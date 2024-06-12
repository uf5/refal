{-# LANGUAGE TupleSections #-}

module Language.Refal.PatternMatching.Implementations.Applicative (matchPattern) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad
import Data.Maybe
import Language.Refal.BasisTypes
import Language.Refal.PatternMatching.Types

data MatcherState = MatcherState
  { input :: [ObjectExpression],
    subs :: Substitutions
  }
  deriving (Show)

newtype Matcher a = Matcher {runMatcher :: MatcherState -> Maybe (a, MatcherState)}

instance Functor Matcher where
  fmap f (Matcher p) = Matcher $ \s -> do
    (a, s') <- p s
    pure (f a, s')

instance Applicative Matcher where
  pure x = Matcher $ pure . (x,)
  (Matcher f) <*> (Matcher a) = Matcher $ \s -> do
    (f', s') <- f s
    (a', s'') <- a s'
    pure (f' a', s'')

instance Monad Matcher where
  return = pure
  (Matcher p) >>= f = Matcher $ \s -> do
    (a, s') <- p s
    runMatcher (f a) s'

instance Alternative Matcher where
  empty = Matcher $ const Nothing
  (Matcher a) <|> (Matcher b) = Matcher $ \s -> a s <|> b s

modifySubs :: (Substitutions -> Substitutions) -> Matcher ()
modifySubs f = Matcher $ \s -> pure ((), s {subs = f (subs s)})

getsSubs :: (Substitutions -> a) -> Matcher a
getsSubs f = Matcher $ \s -> pure (f (subs s), s)

remaining :: Matcher Int
remaining = Matcher $ \s -> pure (length (input s), s)

satisfy :: (ObjectExpression -> Bool) -> Matcher ObjectExpression
satisfy p = Matcher $ \s@(MatcherState {input = inp}) -> case inp of
  [] -> Nothing
  (x : xs)
    | p x -> pure (x, s {input = xs})
    | otherwise -> Nothing

anySingle :: Matcher ObjectExpression
anySingle = satisfy (const True)

satisfy' :: (ObjectExpression -> Maybe a) -> Matcher a
satisfy' p = fromJust . p <$> satisfy (isJust . p)

eof :: Matcher ()
eof = Matcher $ \s@(MatcherState {input = inp}) -> case inp of
  [] -> pure ((), s)
  (_ : _) -> Nothing

runSubMatcher :: Matcher a -> [ObjectExpression] -> Matcher a
runSubMatcher m ss = Matcher $ \s -> do
  (a, s') <- runMatcher m (s {input = ss})
  pure (a, s {subs = subs s'})

mkMatcher :: [PatternExpression] -> Matcher ()
mkMatcher [] = eof
mkMatcher ((PSym x) : xs) = void (satisfy p) *> mkMatcher xs
  where
    p (OSym y) = x == y
    p _ = False
mkMatcher ((PSt x) : xs) = (satisfy' p >>= runSubMatcher (mkMatcher x)) *> mkMatcher xs
  where
    p (OSt y) = Just y
    p _ = Nothing
mkMatcher ((PVar (SType v)) : xs) =
  ( do
      x <- satisfy' p
      getsSubs (lookup v . sType)
        >>= maybe
          (modifySubs (pushSType v x))
          ((`unless` empty) . (x ==))
  )
    *> mkMatcher xs
  where
    p (OSym x) = pure x
    p _ = Nothing
mkMatcher ((PVar (TType v)) : xs) =
  ( do
      x <- anySingle
      getsSubs (lookup v . tType)
        >>= maybe
          (modifySubs (pushTType v x))
          ((`unless` empty) . (x ==))
  )
    *> mkMatcher xs
mkMatcher ((PVar (EType v)) : xs) =
  getsSubs (lookup v . eType)
    >>= maybe
      notDefined
      alreadyDefined
  where
    notDefined = do
      n <- remaining
      choice
        ( map
            ( (*> mkMatcher xs)
                . ( (`replicateM` anySingle)
                      >=> modifySubs . pushEType v
                  )
            )
            [0 .. n]
        )
    alreadyDefined defined = (replicateM (length defined) anySingle >>= ((`unless` empty) . (== defined))) *> mkMatcher xs

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe Substitutions
matchPattern ps os = subs . snd <$> (mkMatcher ps `runMatcher` initialState)
  where
    initialState = MatcherState {input = os, subs = mempty}

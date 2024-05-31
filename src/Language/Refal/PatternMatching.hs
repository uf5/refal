module Language.Refal.PatternMatching (matchPattern, Substitutions (..)) where

import Control.Applicative
import Language.Refal.BasisTypes

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe Substitutions
matchPattern = matchPattern' mempty

data Substitutions = Substitutions
  { sType :: [(SVar, Symbol)],
    tType :: [(TVar, ObjectExpression)],
    eType :: [(EVar, [ObjectExpression])]
  }
  deriving (Show, Eq)

instance Semigroup Substitutions where
  Substitutions {sType = s1, tType = t1, eType = e1}
    <> Substitutions {sType = s2, tType = t2, eType = e2} =
      Substitutions {sType = s1 <> s2, tType = t1 <> t2, eType = e1 <> e2}

instance Monoid Substitutions where
  mempty = Substitutions {sType = mempty, tType = mempty, eType = mempty}

matchPattern' :: Substitutions -> [PatternExpression] -> [ObjectExpression] -> Maybe Substitutions
matchPattern' subs [] [] = pure subs
matchPattern' subs ((PSym p) : ps) ((OSym o) : os)
  | p == o = matchPattern' subs ps os
  | otherwise = Nothing
matchPattern' subs@Substitutions {sType = ss} ((PVar (SType v)) : ps) ((OSym sx) : os) =
  case lookup v ss of
    Just defined
      | sx == defined -> matchPattern' subs ps os
      | otherwise -> Nothing
    Nothing -> matchPattern' (mempty {sType = [(v, sx)]} <> subs) ps os
matchPattern' subs@Substitutions {tType = ts} ((PVar (TType v)) : ps) (ox : os) =
  case lookup v ts of
    Just defined
      | ox == defined -> matchPattern' subs ps os
      | otherwise -> Nothing
    Nothing -> matchPattern' (mempty {tType = [(v, ox)]} <> subs) ps os
matchPattern' subs@Substitutions {eType = es} ((PVar (EType v)) : ps) o =
  case lookup v es of
    Just defined
      | let (elts, rest) = splitAt (length defined) o, elts == defined -> matchPattern' subs ps rest
      | otherwise -> Nothing
    Nothing -> takeUntilNextMatches [] o
  where
    takeUntilNextMatches taken o' =
      matchPattern' (mempty {eType = [(v, taken)]} <> subs) ps o'
        <|> case o' of
          (ox' : os') -> takeUntilNextMatches (taken <> [ox']) os'
          [] -> Nothing
matchPattern' subs ((PSt p) : ps) ((OSt o) : os) = do
  subs' <- matchPattern' subs p o
  matchPattern' subs' ps os
matchPattern' _ _ _ = Nothing

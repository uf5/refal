module Language.Refal.Evaluator (evaluate, Opts (..)) where

import Control.Applicative (Alternative (..))
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Language.Refal.PatternMatching
import Language.Refal.Types

type Functions = [(String, Function)]

type Evaluator = ReaderT Functions (ExceptT EvaluationError Identity)

prelude :: [(String, HFunction)]
prelude = []

newtype Opts = Opts [String]

evaluate :: Program -> Opts -> Either EvaluationError [ObjectExpression]
evaluate (Program p) (Opts opts) = do
  fnMain <- maybe (Left NoMain) (Right . UserDefined) (lookup "main" p)
  runIdentity (runExceptT (runReaderT (apply fnMain optsAsActExpr) withPrelude))
  where
    withPrelude = (second Builtin <$> prelude) <> (second UserDefined <$> p)
    optsAsActExpr = OSt . (OSym . Char <$>) <$> opts

apply :: Function -> [ObjectExpression] -> Evaluator [ObjectExpression]
apply (Builtin (HFunction f)) a = liftEither (f a)
apply (UserDefined (RFunction sents)) a = do
  fromMaybe
    (throwError NoMatchingPattern)
    ( firstJust
        ( map
            (\(Sentence p b) -> (`eval` b) <$> matchPattern p a)
            sents
        )
    )
  where
    firstJust = foldl' (<|>) Nothing

eval :: Substitutions -> [ResultExpression] -> Evaluator [ObjectExpression]
eval subs (RSym x : xs) = (OSym x :) <$> eval subs xs
eval subs (RSt x : xs) = (:) <$> (OSt <$> eval subs x) <*> eval subs xs
eval subs ((RCall f a) : xs) = do
  f' <- lookupFn f
  a' <- eval subs a
  (<>) <$> apply f' a' <*> eval subs xs
eval subs@(Substitutions {sType = ss}) ((RVar w@(SType v)) : xs) =
  (:)
    <$> maybe (throwError (VarNotDefined w)) (pure . OSym) (lookup v ss)
    <*> eval subs xs
eval subs@(Substitutions {tType = ts}) ((RVar w@(TType v)) : xs) =
  (:)
    <$> maybe (throwError (VarNotDefined w)) pure (lookup v ts)
    <*> eval subs xs
eval subs@(Substitutions {eType = es}) ((RVar w@(EType v)) : xs) =
  (<>)
    <$> maybe (throwError (VarNotDefined w)) pure (lookup v es)
    <*> eval subs xs
eval _ [] = pure []

lookupFn :: Monad m => String -> ReaderT Functions (ExceptT EvaluationError m) Function
lookupFn name =
  reader (lookup name)
    >>= maybe (throwError (FnNotDefined name)) pure

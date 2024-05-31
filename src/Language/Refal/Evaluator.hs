{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Refal.Evaluator (evaluate) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Language.Refal.BasisTypes
import Language.Refal.PatternMatching

data EvaluationError
  = FnNotDefined String
  | VarNotDefined Var
  | NoMatchingPattern
  | DivisionByZero
  deriving (Show)

newtype HFunction
  = HFunction
      ([ObjectExpression] -> Either EvaluationError [ObjectExpression])

data Function
  = UserDefined RFunction
  | Builtin HFunction

type Functions = [(String, Function)]

type ProgramField m a = ReaderT Functions m a

type ViewField m a = ReaderT Substitutions m a

newtype Evaluator a = Evaluator (ProgramField (ExceptT EvaluationError Identity) a)
  deriving (Functor, Applicative, Monad, MonadReader Functions, MonadError EvaluationError)

runEvaluator :: Evaluator a -> Functions -> Either EvaluationError a
runEvaluator (Evaluator m) fs = runIdentity (runExceptT (runReaderT m fs))

getFn :: String -> Evaluator Function
getFn name = do
  fn <- reader (lookup name)
  case fn of
    Just fn' -> pure fn'
    Nothing -> throwError (FnNotDefined name)

prelude :: [(String, HFunction)]
prelude = []

evaluate :: Program -> [ObjectExpression] -> Either EvaluationError [ObjectExpression]
evaluate (Program p) args =
  runEvaluator
    ( do
        mainFn <- getFn "main"
        apply mainFn args
    )
    withPrelude
  where
    withPrelude = (second Builtin <$> prelude) <> (second UserDefined <$> p)

apply :: Function -> [ObjectExpression] -> Evaluator [ObjectExpression]
apply (Builtin (HFunction f)) a = liftEither (f a)
apply (UserDefined (RFunction sents)) a =
  fromMaybe
    (throwError NoMatchingPattern)
    (firstJust (map evalSentence sents))
  where
    firstJust [] = Nothing
    firstJust ((Just x) : _) = pure x
    firstJust (Nothing : xs) = firstJust xs
    evalSentence (Sentence p r) = runReaderT (eval r) <$> matchPattern p a

eval :: [ResultExpression] -> ViewField Evaluator [ObjectExpression]
eval [] = pure []
eval (RSym x : xs) = (OSym x :) <$> eval xs
eval (RSt x : xs) = (:) <$> (OSt <$> eval x) <*> eval xs
eval ((RCall f a) : xs) = do
  f' <- lift $ getFn f
  a' <- eval a
  (<>) <$> lift (apply f' a') <*> eval xs
eval ((RVar n@(SType v)) : xs) = do
  v' <- reader (lookup v . sType)
  case v' of
    Just x -> (OSym x :) <$> eval xs
    Nothing -> throwError (VarNotDefined n)
eval ((RVar n@(TType v)) : xs) = do
  v' <- reader (lookup v . tType)
  case v' of
    Just x -> (x :) <$> eval xs
    Nothing -> throwError (VarNotDefined n)
eval ((RVar n@(EType v)) : xs) = do
  v' <- reader (lookup v . eType)
  case v' of
    Just x -> (x <>) <$> eval xs
    Nothing -> throwError (VarNotDefined n)

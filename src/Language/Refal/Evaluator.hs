{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Refal.Evaluator (evaluate) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Bifunctor (second)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Language.Refal.BasisTypes
import Language.Refal.PatternMatching

data EvaluationError
  = FnNotDefined String
  | VarNotDefined Var
  | NoMatchingPattern
  | DivisionByZero
  | BadArgument
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

stdFns :: [(String, HFunction)]
stdFns =
  [ -- arithmetic
    ("Add", intBinOp (+)),
    ("Sub", intBinOp (-)),
    ("Mul", intBinOp (*)),
    ("Div", intBinOpDiv div),
    ("Mod", intBinOpDiv mod),
    ("Compare", intCompare),
    -- char
    ("Symb", int2char),
    ("Numb", char2int),
    ("Upper", charUpper),
    ("Lower", charLower),
    -- type
    ("Type", getType)
  ]
  where
    -- arithmetic
    intBinOp op = HFunction $ \case
      [OSym (Int m), OSym (Int n)] -> pure [symInt (m `op` n)]
      _ -> Left NoMatchingPattern
    intBinOpDiv op = HFunction $ \case
      [OSym (Int _), OSym (Int 0)] -> Left DivisionByZero
      [OSym (Int m), OSym (Int n)] -> pure [symInt (m `op` n)]
      _ -> Left NoMatchingPattern
    intCompare = HFunction $ \args ->
      List.singleton . OSym . Char <$> case args of
        [OSym (Int m), OSym (Int n)]
          | m > n -> pure '+'
          | m < n -> pure '-'
          | otherwise -> pure '0'
        _ -> Left NoMatchingPattern
    -- conversion
    int2char = HFunction $ \case
      [OSym (Int m)] ->
        maybe
          (Left BadArgument)
          (pure . List.singleton . symChar)
          (safeChr m)
      _ -> Left NoMatchingPattern
    char2int = HFunction $ \case
      [OSym (Char m)] -> pure [symInt $ toInteger $ Char.ord m]
      _ -> Left NoMatchingPattern
    -- char
    charUpper = HFunction $ \case
      [OSym (Char m)] -> pure [symChar $ Char.toUpper m]
      _ -> Left NoMatchingPattern
    charLower = HFunction $ \case
      [OSym (Char m)] -> pure [symChar $ Char.toLower m]
      _ -> Left NoMatchingPattern
    -- type
    getType = HFunction $ \case
      [] -> pure $ List.singleton $ symChar '*'
      (x : xs) -> pure $ symChar (getType' x) : xs
    getType' (OSym (Int _)) = 'D'
    getType' (OSym (Char _)) = 'L'
    getType' (OSt _) = 'S'
    -- utility
    safeChr m
      | (toInteger (Char.ord minBound) <= m) && m <= toInteger (Char.ord maxBound) =
          pure (Char.chr (fromInteger m))
      | otherwise = Nothing
    symChar = OSym . Char
    symInt = OSym . Int

evaluate :: Program -> [ObjectExpression] -> Either EvaluationError [ObjectExpression]
evaluate (Program p) args =
  runEvaluator
    ( do
        mainFn <- getFn "main"
        apply mainFn args
    )
    withStdFns
  where
    withStdFns = (second Builtin <$> stdFns) <> (second UserDefined <$> p)

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

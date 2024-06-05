{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Refal.Evaluator (evaluate, EvaluationError (..)) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
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
  | NotAValidFunctionName
  | Info String EvaluationError
  deriving (Show)

newtype HFunction
  = HFunction
      ([ObjectExpression] -> Evaluator [ObjectExpression])

data Function
  = UserDefined RFunction
  | Builtin HFunction

type Functions = [(String, Function)]

type ProgramField m = ReaderT Functions m

type ViewField m = ReaderT Substitutions m

newtype BuryDigState = BuryDigState
  { buried :: [([ObjectExpression], [ObjectExpression])]
  }
  deriving (Semigroup, Monoid)

type BuryDig m = StateT BuryDigState m

newtype Evaluator a = Evaluator (ProgramField (BuryDig (ExceptT EvaluationError IO)) a)
  deriving (Functor, Applicative, Monad, MonadReader Functions, MonadState BuryDigState, MonadError EvaluationError, MonadIO)

runEvaluator :: Evaluator a -> Functions -> BuryDigState -> IO (Either EvaluationError a)
runEvaluator (Evaluator m) fs bs = runExceptT (evalStateT (runReaderT m fs) bs)

stackBury :: [ObjectExpression] -> [ObjectExpression] -> Evaluator ()
stackBury n x = modify (\s -> s {buried = (n, x) : buried s})

stackDig :: [ObjectExpression] -> Evaluator [ObjectExpression]
stackDig n = do
  br <- gets buried
  case (`splitAt` br) <$> List.elemIndex n (map fst br) of
    (Just (ysl, x : ysr)) -> do
      modify (\s -> s {buried = ysl <> ysr})
      pure (snd x)
    (Just (_, [])) -> error "not possible"
    Nothing -> pure []

stackCp :: [ObjectExpression] -> Evaluator [ObjectExpression]
stackCp n = do
  p <- gets (lookup n . buried)
  pure $ case p of
    (Just x) -> x
    Nothing -> []

stackRp :: [ObjectExpression] -> [ObjectExpression] -> Evaluator ()
stackRp n x = do
  br <- gets buried
  case (`splitAt` br) <$> List.elemIndex n (map fst br) of
    (Just (ysl, _ : ysr)) -> do
      modify (\s -> s {buried = ysl <> ((n, x) : ysr)})
      pure ()
    (Just (_, [])) -> error "not possible"
    Nothing -> pure ()

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
    ("Type", getType),
    -- Mu
    ("Mu", mu),
    -- IO
    ("Print", refalPrint),
    ("Prout", refalProut),
    -- Bury/dig
    ("Br", refalBury),
    ("Dg", refalDig),
    ("Cp", refalCp),
    ("Rp", refalRp)
  ]
  where
    intBinOp op = HFunction $ \case
      [OSym (Int m), OSym (Int n)] -> pure [symInt (m `op` n)]
      _ -> throwError NoMatchingPattern

    intBinOpDiv op = HFunction $ \case
      [OSym (Int _), OSym (Int 0)] -> throwError DivisionByZero
      [OSym (Int m), OSym (Int n)] -> pure [symInt (m `op` n)]
      _ -> throwError NoMatchingPattern

    intCompare = HFunction $ \args ->
      List.singleton . OSym . Char <$> case args of
        [OSym (Int m), OSym (Int n)]
          | m > n -> pure '+'
          | m < n -> pure '-'
          | otherwise -> pure '0'
        _ -> throwError NoMatchingPattern

    int2char = HFunction $ \case
      [OSym (Int m)] ->
        maybe
          (throwError BadArgument)
          (pure . List.singleton . symChar)
          (safeChr m)
      _ -> throwError NoMatchingPattern

    char2int = HFunction $ \case
      [OSym (Char m)] -> pure [symInt $ toInteger $ Char.ord m]
      _ -> throwError NoMatchingPattern

    charUpper = HFunction $ \case
      [OSym (Char m)] -> pure [symChar $ Char.toUpper m]
      _ -> throwError NoMatchingPattern

    charLower = HFunction $ \case
      [OSym (Char m)] -> pure [symChar $ Char.toLower m]
      _ -> throwError NoMatchingPattern

    getType = HFunction $ \case
      [] -> pure $ List.singleton $ symChar '*'
      (x : xs) -> pure $ symChar (getType' x) : xs
    getType' (OSym (Int _)) = 'D'
    getType' (OSym (Char _)) = 'L'
    getType' (OSt _) = 'S'

    mu = HFunction $ \case
      ((OSt fname) : args) -> do
        fname' <- maybe (throwError NotAValidFunctionName) pure (asStr fname)
        f <- getFn fname'
        withError (Info fname') (apply f args)
      _ -> throwError NoMatchingPattern

    refalPrint = HFunction $ \args -> args <$ liftIO (putStrLn (showOutput args))

    refalProut = HFunction $ \args -> [] <$ liftIO (putStrLn (showOutput args))

    refalBury = HFunction $ \case
      (OSt n : x) -> [] <$ stackBury n x
      _ -> throwError NoMatchingPattern

    refalDig = HFunction stackDig

    refalCp = HFunction stackCp

    refalRp = HFunction $ \case
      (OSt n : x) -> [] <$ stackRp n x
      _ -> throwError NoMatchingPattern

    safeChr m
      | (toInteger (Char.ord minBound) <= m) && m <= toInteger (Char.ord maxBound) =
          pure (Char.chr (fromInteger m))
      | otherwise = Nothing

    asStr [] = pure []
    asStr (OSym (Char x) : xs) = (x :) <$> asStr xs
    asStr _ = Nothing

    symChar = OSym . Char
    symInt = OSym . Int

evaluate :: Program -> [ObjectExpression] -> IO (Either EvaluationError [ObjectExpression])
evaluate (Program p) args =
  runEvaluator
    ( do
        mainFn <- getFn "main"
        apply mainFn args
    )
    withStdFns
    mempty
  where
    withStdFns = (second UserDefined <$> p) <> (second Builtin <$> stdFns)

apply :: Function -> [ObjectExpression] -> Evaluator [ObjectExpression]
apply (Builtin (HFunction f)) a = f a
apply (UserDefined (RFunction sents)) a =
  fromMaybe
    (throwError NoMatchingPattern)
    (firstJust (map evalSentence sents))
  where
    firstJust [] = Nothing
    firstJust (x@(Just _) : _) = x
    firstJust (Nothing : xs) = firstJust xs
    evalSentence (Sentence p r) = runReaderT (eval r) <$> matchPattern p a

eval :: [ResultExpression] -> ViewField Evaluator [ObjectExpression]
eval [] = pure []
eval (RSym x : xs) = (OSym x :) <$> eval xs
eval (RSt x : xs) = (:) <$> (OSt <$> eval x) <*> eval xs
eval ((RCall f a) : xs) = do
  f' <- lift $ getFn f
  a' <- eval a
  r <- lift $ withError (Info f) (apply f' a')
  (r <>) <$> eval xs
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

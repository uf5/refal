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

type Evaluator = ReaderT Program' (ExceptT EvaluationError Identity)

prelude :: [(String, HFunction)]
prelude = []

newtype Opts = Opts [String]

evaluate :: Program -> Opts -> Either EvaluationError ObjectExpression
evaluate (Program p) (Opts opts) = do
  fnMain <- maybe (Left NoMain) (Right . UserDefined) (lookup "main" p)
  runIdentity (runExceptT (runReaderT (apply fnMain optsAsActExpr) withPrelude))
  where
    withPrelude = Program' ((second Builtin <$> prelude) <> (second UserDefined <$> p))
    optsAsActExpr = OSt . (OSym . Char <$>) <$> opts

apply :: Function -> [ObjectExpression] -> Evaluator ObjectExpression
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

eval :: Substitutions -> ResultExpression -> Evaluator ObjectExpression
eval _ (RSym s) = pure (OSym s)
eval subs (RSt xs) = OSt <$> mapM (eval subs) xs
eval subs (RCall f a) = do
  f' <- lookupFn f
  a' <- mapM (eval subs) a
  apply f' a'
eval subs (RVar v) = lift $ lookupVar v subs

lookupVar :: Monad m => Var -> Substitutions -> ExceptT EvaluationError m ObjectExpression
lookupVar v@(Var k _) s = maybe (throwError (VarNotDefined v)) pure (lookup v associated)
  where
    associated = case k of
      S -> second OSym <$> sType s
      T -> tType s
      E -> second OSt <$> eType s

lookupFn :: Monad m => String -> ReaderT Program' (ExceptT EvaluationError m) Function
lookupFn name =
  reader (lookup name . unwrapProgram')
    >>= maybe (throwError (FnNotDefined name)) pure
  where
    unwrapProgram' (Program' x) = x

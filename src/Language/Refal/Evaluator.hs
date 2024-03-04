module Language.Refal.Evaluator where

import Language.Refal.PatternMatching
import Language.Refal.Types

type Funcs = [(String, Function)]

data Scope = Scope
  { subs :: Substitutions,
    funcs :: Funcs
  }

-- eval :: Scope -> ResultExpression -> Either EvaluationError ObjectExpression
-- eval _ (RSym sym) = pure $ OSym sym
-- eval s (RSt struct) = do
--   x <- traverse (eval s) struct
--   pure $ OSt x
-- eval s (RVar var@(Var _ name)) =
--   maybe
--     (Left (NotDefined name))
--     pure
--     (lookup var (subs s))
-- eval s (RCall name arg) = do
--   fn <-
--     maybe
--       (Left (NotDefined name))
--       pure
--       (lookup name (funcs s))
--   arg' <- eval s arg
--   apply fn arg'

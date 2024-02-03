module Language.Refal.Evaluator where

import Control.Monad (join)
import Language.Refal.Parser
import Language.Refal.Types

type Vars = [(Var, ObjectExpression)]

type Funcs = [(String, Function)]

data Scope = Scope
  { vars :: Vars,
    funcs :: Funcs
  }

eval :: Scope -> ResultExpression -> Either EvaluationError ObjectExpression
eval _ (RSym sym) = pure $ OSym sym
eval s (RSt struct) = do
  x <- traverse (eval s) struct
  pure $ OSt x
eval s (RVar var@(Var _ name)) = lookupScope vars name var s
eval s (RCall name arg) = do
  fn <- lookupScope funcs name name s
  arg' <- eval s arg
  apply fn arg'
  where
    apply (UserDefined (RFunction sents)) arg' = undefined
    apply (Builtin (HFunction f)) arg' = f arg'

lookupScope ::
  Eq a =>
  (Scope -> [(a, b)]) ->
  String ->
  a ->
  Scope ->
  Either EvaluationError b
lookupScope g n x s =
  maybe
    (Left (NotDefined n))
    Right
    (lookup x (g s))

patternToParser :: PatternExpression -> Parser ObjectExpression [(String, Vars)]
patternToParser (PSym s1) = [] <$ satisfy sat
  where
    sat (OSym s2) = s1 == s2
    sat _ = False
patternToParser (PVar v@(Var S _)) = undefined
patternToParser (PVar v@(Var T _)) = undefined
patternToParser (PVar v@(Var E _)) = undefined
patternToParser (PSt xs) = join <$> mapM patternToParser xs

module Language.Refal.Syntax (desugarProgram) where

import Data.Bifunctor
import Language.Refal.Syntax.Types qualified as S
import Language.Refal.Types qualified as T

desugarProgram :: S.Program -> T.Program
desugarProgram (S.Program fs) = T.Program (map (second desugarFunction) fs)

desugarFunction :: S.Function -> T.RFunction
desugarFunction (S.Function ss) = T.RFunction (map desugarSentence ss)

desugarSentence :: S.Sentence -> T.Sentence
desugarSentence (S.Sentence p r) = T.Sentence (desugarPE p) (desugarRE r)

desugarPE :: [S.PatternExpression] -> [T.PatternExpression]
desugarPE = concatMap desugarPE'
  where
    desugarPE' (S.PSym s) =
      case s of
        S.SymBasic x -> [T.PSym x]
        S.SymDQString x -> [T.PSt (map (T.PSym . T.Char) x)]
        S.SymSQString x -> map (T.PSym . T.Char) x
    desugarPE' (S.PSt x) = [T.PSt (desugarPE x)]
    desugarPE' (S.PVar x) = [T.PVar x]

desugarRE :: [S.ResultExpression] -> [T.ResultExpression]
desugarRE = concatMap desugarRE'
  where
    desugarRE' (S.RSym s) =
      case s of
        S.SymBasic x -> [T.RSym x]
        S.SymDQString x -> [T.RSt (map (T.RSym . T.Char) x)]
        S.SymSQString x -> map (T.RSym . T.Char) x
    desugarRE' (S.RSt x) = [T.RSt (desugarRE x)]
    desugarRE' (S.RVar x) = [T.RVar x]
    desugarRE' (S.RCall name args) = [T.RCall name (desugarRE args)]

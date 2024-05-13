module Language.Refal.Sugar (desugarProgram) where

import Data.Bifunctor
import Language.Refal.Sugar.Types qualified as S
import Language.Refal.Types qualified as T

desugarProgram :: S.Program -> T.Program
desugarProgram (S.Program fs) = T.Program (map (second desugarFunction) fs)

desugarFunction :: S.Function -> T.RFunction
desugarFunction (S.Function ss) = T.RFunction (map desugarSentence ss)

desugarSentence :: S.Sentence -> T.Sentence
desugarSentence (S.Sentence p r) = T.Sentence (desugarPE p) (desugarRE r)

desugarPE :: [S.PatternExpression] -> [T.PatternExpression]
desugarPE [] = []
desugarPE ((S.PSym (S.SymBasic x)) : xs) = T.PSym x : desugarPE xs
desugarPE ((S.PSym (S.SymDQString x)) : xs) = T.PSt (map (T.PSym . T.Char) x) : desugarPE xs
desugarPE ((S.PSym (S.SymSQString x)) : xs) = map (T.PSym . T.Char) x <> desugarPE xs
desugarPE ((S.PSt x) : xs) = T.PSt (desugarPE x) : desugarPE xs
desugarPE ((S.PVar x) : xs) = T.PVar x : desugarPE xs

desugarRE :: [S.ResultExpression] -> [T.ResultExpression]
desugarRE [] = []
desugarRE ((S.RSym (S.SymBasic x)) : xs) = T.RSym x : desugarRE xs
desugarRE ((S.RSym (S.SymDQString x)) : xs) = T.RSt (map (T.RSym . T.Char) x) : desugarRE xs
desugarRE ((S.RSym (S.SymSQString x)) : xs) = map (T.RSym . T.Char) x <> desugarRE xs
desugarRE ((S.RSt x) : xs) = T.RSt (desugarRE x) : desugarRE xs
desugarRE ((S.RVar x) : xs) = T.RVar x : desugarRE xs
desugarRE ((S.RCall name args) : xs) = T.RCall name (desugarRE args) : desugarRE xs

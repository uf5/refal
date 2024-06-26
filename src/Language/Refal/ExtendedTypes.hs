module Language.Refal.ExtendedTypes (
  Symbol (..),
  PatternExpression (..),
  ResultExpression (..),
  Sentence (..),
  Function (..),
  Program (..),
)
where

import qualified Language.Refal.BasisTypes as T

data Symbol
  = SymBasic T.Symbol
  | SymQString String
  deriving (Show)

data PatternExpression
  = PSym Symbol
  | PSt [PatternExpression]
  | PVar T.Var
  deriving (Show)

data ResultExpression
  = RSym Symbol
  | RSt [ResultExpression]
  | RCall String [ResultExpression]
  | RVar T.Var
  deriving (Show)

data Sentence
  = Sentence [PatternExpression] [ResultExpression]
  | ClauseSentence [PatternExpression] [ResultExpression] Function
  deriving (Show)

newtype Function = Function [Sentence]
  deriving (Show)

newtype Program = Program [(String, Function)]
  deriving (Show)

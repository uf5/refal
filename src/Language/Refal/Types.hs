module Language.Refal.Types (
  EvaluationError (..),
  VarKind (..),
  Var (..),
  Symbol (..),
  PatternExpression (..),
  ResultExpression (..),
  ObjectExpression (..),
  ActiveExpression (..),
  Sentence (..),
  RFunction (..),
  HFunction (..),
  Function (..),
  Program (..),
)
where

data EvaluationError
  = NotDefined String
  | NoMatchingPattern
  | DivisionByZero

instance Show EvaluationError where
  show (NotDefined name) = "Name `" <> name <> "` is undefined"
  show NoMatchingPattern = "No matching pattern"
  show DivisionByZero = "Division by zero"

data VarKind = S | T | E
  deriving (Show, Eq)

data Var = Var VarKind String
  deriving (Eq)

instance Show Var where
  show (Var k v) = show k <> "." <> v

data Symbol
  = None
  | Int Integer
  | Char Char
  | Bool Bool
  deriving (Eq, Show)

-- Expressions that exist before the evaluation

data PatternExpression
  = PSym Symbol
  | PSt [PatternExpression]
  | PVar Var

data ResultExpression
  = RSym Symbol
  | RSt [ResultExpression]
  | RCall String ResultExpression
  | RVar Var

-- Expressions that exist during the evaluation

data ObjectExpression
  = OSym Symbol
  | OSt [ObjectExpression]

data ActiveExpression
  = ASym Symbol
  | ASt [ActiveExpression]
  | ACall String ActiveExpression

data Sentence = Sentence PatternExpression ActiveExpression

newtype RFunction = RFunction [Sentence]

newtype HFunction
  = HFunction
      (ObjectExpression -> Either EvaluationError ObjectExpression)

data Function
  = UserDefined RFunction
  | Builtin HFunction

newtype Program = Program [(String, RFunction)]

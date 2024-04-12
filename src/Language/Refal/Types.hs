module Language.Refal.Types (
  EvaluationError (..),
  VarKind (..),
  Var (..),
  Symbol (..),
  PatternExpression (..),
  ResultExpression (..),
  ObjectExpression (..),
  Sentence (..),
  RFunction (..),
  HFunction (..),
  Function (..),
  Program (..),
  Program' (..),
)
where

data EvaluationError
  = FnNotDefined String
  | VarNotDefined Var
  | NoMain
  | NoMatchingPattern
  | DivisionByZero

instance Show EvaluationError where
  show NoMain = "Program has no `main' function"
  show (FnNotDefined name) = "Function `" <> name <> "` is undefined"
  show (VarNotDefined var) = "Variable `" <> show var <> "` is undefined"
  show NoMatchingPattern = "No matching pattern"
  show DivisionByZero = "Division by zero"

data VarKind = S | T | E
  deriving (Show, Eq)

data Var = Var VarKind String
  deriving (Eq)

instance Show Var where
  show (Var k v) = show k <> "." <> v

data Symbol
  = Int Integer
  | Char Char
  deriving (Eq, Ord, Show)

-- Expressions that exist before the evaluation

data PatternExpression
  = PSym Symbol
  | PSt [PatternExpression]
  | PVar Var
  deriving (Eq, Show)

data ResultExpression
  = RSym Symbol
  | RSt [ResultExpression]
  | RCall String [ResultExpression]
  | RVar Var
  deriving (Show)

-- Expression that exists during the evaluation

data ObjectExpression
  = OSym Symbol
  | OSt [ObjectExpression]
  deriving (Show, Eq, Ord)

data Sentence = Sentence [PatternExpression] [ResultExpression]
  deriving (Show)

newtype RFunction = RFunction [Sentence]
  deriving (Show)

newtype HFunction
  = HFunction
      ([ObjectExpression] -> Either EvaluationError [ObjectExpression])

data Function
  = UserDefined RFunction
  | Builtin HFunction

newtype Program = Program [(String, RFunction)]
  deriving (Show)

newtype Program' = Program' [(String, Function)]

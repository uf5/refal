module Language.Refal.Types (
  EvaluationError (..),
  SVar (..),
  TVar (..),
  EVar (..),
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

newtype SVar = SVar String
  deriving (Eq)

instance Show SVar where
  show (SVar v) = "s." <> v

newtype TVar = TVar String
  deriving (Eq)

instance Show TVar where
  show (TVar v) = "t." <> v

newtype EVar = EVar String
  deriving (Eq)

instance Show EVar where
  show (EVar v) = "e." <> v

data Var
  = SType SVar
  | TType TVar
  | EType EVar
  deriving (Eq)

instance Show Var where
  show (SType v) = show v
  show (TType v) = show v
  show (EType v) = show v

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

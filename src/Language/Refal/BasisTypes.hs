module Language.Refal.BasisTypes (
  Pretty (..),
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
  Program (..),
  showOutput,
)
where

showOutput :: [ObjectExpression] -> String
showOutput [] = ""
showOutput ((OSym x) : xs) =
  ( case x of
      (Char c) -> [c]
      (Int i) -> show i
  )
    <> showOutput xs
showOutput ((OSt xs) : ys) = "(" <> showOutput xs <> ")" <> showOutput ys

class Pretty a where
  pretty :: a -> String

newtype SVar = SVar String
  deriving (Show, Eq)

instance Pretty SVar where
  pretty (SVar x) = 's' : x

newtype TVar = TVar String
  deriving (Show, Eq)

instance Pretty TVar where
  pretty (TVar x) = 't' : x

newtype EVar = EVar String
  deriving (Show, Eq)

instance Pretty EVar where
  pretty (EVar x) = 'e' : x

data Var
  = SType SVar
  | TType TVar
  | EType EVar
  deriving (Show, Eq)

instance Pretty Var where
  pretty (SType x) = pretty x
  pretty (TType x) = pretty x
  pretty (EType x) = pretty x

data Symbol
  = Int Integer
  | Char Char
  deriving (Eq, Ord, Show)

instance Pretty Symbol where
  pretty (Int x) = show x
  pretty (Char x) = show x

-- Expressions that exist before the evaluation

data PatternExpression
  = PSym Symbol
  | PSt [PatternExpression]
  | PVar Var
  deriving (Eq, Show)

instance Pretty PatternExpression where
  pretty (PSym x) = pretty x
  pretty (PSt xs) = "(" <> unwords (map pretty xs) <> ")"
  pretty (PVar x) = pretty x

data ResultExpression
  = RSym Symbol
  | RSt [ResultExpression]
  | RCall String [ResultExpression]
  | RVar Var
  deriving (Show)

instance Pretty ResultExpression where
  pretty (RSym x) = pretty x
  pretty (RSt xs) = "(" <> unwords (map pretty xs) <> ")"
  pretty (RCall name xs) = "<" <> name <> " " <> unwords (map pretty xs) <> ">"
  pretty (RVar x) = pretty x

-- Expression that exists during the evaluation

data ObjectExpression
  = OSym Symbol
  | OSt [ObjectExpression]
  deriving (Show, Eq, Ord)

data Sentence = Sentence [PatternExpression] [ResultExpression]
  deriving (Show)

newtype RFunction = RFunction [Sentence]
  deriving (Show)

instance Pretty RFunction where
  pretty (RFunction xs) = "{\n" <> unlines (map (\(Sentence p r) -> unwords (map pretty p) <> " = " <> unwords (map ((<> ";") . pretty) r)) xs) <> "}\n"

newtype Program = Program [(String, RFunction)]
  deriving (Show)

instance Pretty Program where
  pretty (Program xs) = unlines $ map (\(name, f) -> name <> " " <> pretty f) xs

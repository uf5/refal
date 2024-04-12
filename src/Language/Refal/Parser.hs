module Language.Refal.Parser (parseProgram, parseProgram') where

import Data.Bifunctor (first)
import Data.Void
import Language.Refal.Types qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    empty
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pVar :: Parser T.Var
pVar = pVarKind <* char '.' <*> pIdent <?> "var"
  where
    pVarKind =
      choice
        [ T.SType . T.SVar <$ char 's',
          T.TType . T.TVar <$ char 't',
          T.EType . T.EVar <$ char 'e'
        ]
        <?> "var kind"

pIdent :: Parser String
pIdent = some allowedChar <?> "identifier"
  where
    allowedChar = noneOf "\n\r\t \"()-=<>;"

pSym :: Parser T.Symbol
pSym = lexeme (choice [pInt, pChar] <?> "symbol")
  where
    pInt = T.Int <$> L.decimal
    pChar = T.Char <$> (char '\'' *> anySingle <* char '\'')

pResExpr :: Parser T.ResultExpression
pResExpr = lexeme (choice [pResSym, pResSt, pResCall, pResVar] <?> "result expr")
  where
    pResSym = T.RSym <$> pSym
    pResSt = T.RSt <$> (char '(' *> many pResExpr <* char ')')
    pResCall = T.RCall <$> (char '<' *> lexeme pIdent) <*> many pResExpr <* char '>'
    pResVar = T.RVar <$> pVar

pPattExpr :: Parser T.PatternExpression
pPattExpr = lexeme (choice [pPattSym, pPattSt, pPattVar] <?> "pattern expr")
  where
    pPattSym = T.PSym <$> pSym
    pPattSt = T.PSt <$> (char '(' *> many pPattExpr <* char ')')
    pPattVar = T.PVar <$> pVar

pSentence :: Parser T.Sentence
pSentence = lexeme (T.Sentence <$> (many pPattExpr <* lexeme (char '=')) <*> (many pResExpr <* lexeme (char ';')) <?> "sentence")

pFunction :: Parser (String, T.RFunction)
pFunction = lexeme ((,) <$> (lexeme pIdent <?> "function name") <*> ((lexeme (char '{') *> (T.RFunction <$> some pSentence) <* lexeme (char '}')) <?> "function body"))

pProgram :: Parser T.Program
pProgram = T.Program <$> (sc *> many pFunction)

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) T.Program
parseProgram = runParser (pProgram <* eof)

parseProgram' :: String -> String -> Either String T.Program
parseProgram' file prog = first errorBundlePretty (parseProgram file prog)

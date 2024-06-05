module Language.Refal.Parser (parseProgram, parseProgram') where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Function
import Data.Void
import qualified Language.Refal.BasisTypes as T
import qualified Language.Refal.ExtendedTypes as S
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "*")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pVar :: Parser T.Var
pVar = pVarKind <*> pIdent <?> "var"
  where
    pVarKind =
      choice
        [ T.SType . T.SVar <$ char 's',
          T.TType . T.TVar <$ char 't',
          T.EType . T.EVar <$ char 'e'
        ]
        <?> "var kind"

betweenLexeme :: Parser sur -> Parser sur -> Parser inner -> Parser inner
betweenLexeme = between `on` lexeme

pIdent :: Parser String
pIdent = some allowedChar <?> "identifier"
  where
    allowedChar = noneOf "\n\r\t \"\'()=<>;,:"

pSym :: Parser S.Symbol
pSym = lexeme (choice [pInt, pSQStr, pDQStr] <?> "symbol")
  where
    pInt = S.SymBasic . T.Int <$> L.decimal
    pSQStr = char '\'' *> (S.SymSQString <$> manyTill L.charLiteral (char '\''))
    pDQStr = char '\"' *> (S.SymDQString <$> manyTill L.charLiteral (char '\"'))

pResExpr :: Parser S.ResultExpression
pResExpr = lexeme (choice [pResSym, pResSt, pResCall, pResVar] <?> "result expr")
  where
    pResSym = S.RSym <$> pSym
    pResSt = betweenLexeme (char '(') (char ')') (S.RSt <$> many pResExpr)
    pResCall = betweenLexeme (char '<') (char '>') (S.RCall <$> lexeme pIdent <*> many pResExpr)
    pResVar = S.RVar <$> pVar

pPattExpr :: Parser S.PatternExpression
pPattExpr = lexeme (choice [pPattSym, pPattSt, pPattVar] <?> "pattern expr")
  where
    pPattSym = S.PSym <$> pSym
    pPattSt = S.PSt <$> betweenLexeme (char '(') (char ')') (many pPattExpr)
    pPattVar = S.PVar <$> pVar

pSentence :: Parser S.Sentence
pSentence = do
  p <- many pPattExpr
  pBasic p <|> pClause p
  where
    pBasic p = do
      void $ lexeme (char '=')
      r <- many pResExpr
      void $ lexeme (char ';')
      pure $ S.Sentence p r
    pClause p = do
      void $ lexeme (char ',')
      x <- many pResExpr
      void $ lexeme (char ':')
      f <- pFunctionBody
      void $ lexeme (char ';')
      pure $ S.ClauseSentence p x f

pFunctionBody :: Parser S.Function
pFunctionBody = betweenLexeme (char '{') (char '}') (S.Function <$> some pSentence) <?> "function body"

pFunction :: Parser (String, S.Function)
pFunction = (,) <$> pFunctionName <*> pFunctionBody
  where
    pFunctionName = lexeme pIdent <?> "function name"

pProgram :: Parser S.Program
pProgram = S.Program <$> (sc *> many pFunction)

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) S.Program
parseProgram = runParser (pProgram <* eof)

parseProgram' :: String -> String -> Either String S.Program
parseProgram' file prog = first errorBundlePretty (parseProgram file prog)

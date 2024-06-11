module Language.Refal.Parser (parseProgram, parseProgram') where

import Control.Monad (void)
import Data.Bifunctor (first)
import qualified Data.Char as Char
import Data.Function
import qualified Data.List as List
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

allowedChar :: Parser Char
allowedChar = noneOf "\n\r\t \"\'{}()=<>;.,:"

pVar :: Parser T.Var
pVar = (pVarKind <?> "var kind") <*> (pVarIdent <?> "var ident") <?> "var"
  where
    pVarKind =
      choice
        [ T.SType . T.SVar <$ char 's',
          T.TType . T.TVar <$ char 't',
          T.EType . T.EVar <$ char 'e'
        ]
    pVarIdent =
      (List.singleton <$> allowedChar) <|> (char '.' *> some allowedChar)

betweenLexeme :: Parser sur -> Parser sur -> Parser inner -> Parser inner
betweenLexeme = between `on` lexeme

pFnName :: Parser String
pFnName = lexeme ((:) <$> satisfy Char.isUpper <*> some allowedChar) <?> "function name"

pSym :: Parser S.Symbol
pSym = lexeme (choice [pInt, pChar, pIdentifier] <?> "symbol")
  where
    pInt = S.SymBasic . T.Int <$> L.decimal

    pChar = S.SymQString <$> (pSQStr <|> pDQStr)
    pSQStr = char '\'' *> manyTill L.charLiteral (char '\'')
    pDQStr = char '\"' *> manyTill L.charLiteral (char '\"')

    pIdentifier = S.SymBasic . T.Identifier <$> some allowedChar

pResExpr :: Parser S.ResultExpression
pResExpr = lexeme (choice [pResVar, pResSt, pResCall, pResSym] <?> "result expr")
  where
    pResSym = S.RSym <$> pSym
    pResSt = betweenLexeme (char '(') (char ')') (S.RSt <$> many pResExpr)
    pResCall = betweenLexeme (char '<') (char '>') (S.RCall <$> pFnName <*> many pResExpr)
    pResVar = S.RVar <$> pVar

pPattExpr :: Parser S.PatternExpression
pPattExpr = lexeme (choice [pPattVar, pPattSt, pPattSym] <?> "pattern expr")
  where
    pPattSym = S.PSym <$> pSym
    pPattSt = S.PSt <$> betweenLexeme (char '(') (char ')') (many pPattExpr)
    pPattVar = S.PVar <$> pVar

pSentence :: Parser S.Sentence
pSentence = do
  p <- many pPattExpr
  let pBasic = lexeme (char '=') *> (S.Sentence p <$> many pResExpr)
  let pClause =
        ( do
            void $ lexeme (char ',')
            x <- many pResExpr
            void $ lexeme (char ':')
            let pClauseFullbody = S.ClauseSentence p x <$> pFunctionBody
            let pClauseShort = S.ClauseSentence p x . S.Function . List.singleton <$> pSentence
            pClauseFullbody <|> pClauseShort
        )
  (pBasic <?> "sentence") <|> (pClause <?> "clause")

pFunctionBody :: Parser S.Function
pFunctionBody = betweenLexeme (char '{') (char '}') (S.Function <$> some (pSentence <* lexeme (char ';'))) <?> "function body"

pFunction :: Parser (String, S.Function)
pFunction = (,) <$> pFunctionName <*> pFunctionBody
  where
    pFunctionName = pFnName <?> "function name"

pProgram :: Parser S.Program
pProgram = S.Program <$> (sc *> many pFunction)

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) S.Program
parseProgram = runParser (pProgram <* eof)

parseProgram' :: String -> String -> Either String S.Program
parseProgram' file prog = first errorBundlePretty (parseProgram file prog)

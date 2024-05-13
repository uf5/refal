module Language.Refal.Parser (parseProgram, parseProgram') where

import Data.Bifunctor (first)
import Data.Void
import Language.Refal.Sugar.Types qualified as S
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

strLiteral :: Char -> Parser String
strLiteral sur = char sur *> many ((char '\\' *> escapeSeq) <|> satisfy (/= sur)) <* char sur
  where
    escapeSeq :: Parser Char
    escapeSeq =
      choice
        [ '\\' <$ char '\\',
          '\'' <$ char '\'',
          '\"' <$ char '\"',
          '\n' <$ char 'n',
          '\t' <$ char 't'
        ]

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

pSym :: Parser S.Symbol
pSym = lexeme (choice [pInt, pChar, pSQStr, pDQStr] <?> "symbol")
  where
    pInt = S.SymBasic . T.Int <$> L.decimal
    pChar = S.SymBasic . T.Char <$> (char '\'' *> anySingle <* char '\'')
    pSQStr = S.SymSQString <$> strLiteral '\''
    pDQStr = S.SymDQString <$> strLiteral '"'

pResExpr :: Parser S.ResultExpression
pResExpr = lexeme (choice [pResSym, pResSt, pResCall, pResVar] <?> "result expr")
  where
    pResSym = S.RSym <$> pSym
    pResSt = S.RSt <$> (char '(' *> many pResExpr <* char ')')
    pResCall = S.RCall <$> (char '<' *> lexeme pIdent) <*> many pResExpr <* char '>'
    pResVar = S.RVar <$> pVar

pPattExpr :: Parser S.PatternExpression
pPattExpr = lexeme (choice [pPattSym, pPattSt, pPattVar] <?> "pattern expr")
  where
    pPattSym = S.PSym <$> pSym
    pPattSt = S.PSt <$> (char '(' *> many pPattExpr <* char ')')
    pPattVar = S.PVar <$> pVar

pSentence :: Parser S.Sentence
pSentence = lexeme (S.Sentence <$> (many pPattExpr <* lexeme (char '=')) <*> (many pResExpr <* lexeme (char ';')) <?> "sentence")

pFunction :: Parser (String, S.Function)
pFunction =
  lexeme
    ( (,)
        <$> (lexeme pIdent <?> "function name")
        <*> ((lexeme (char '{') *> (S.Function <$> some pSentence) <* lexeme (char '}')) <?> "function body")
    )

pProgram :: Parser S.Program
pProgram = S.Program <$> (sc *> many pFunction)

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) S.Program
parseProgram = runParser (pProgram <* eof)

parseProgram' :: String -> String -> Either String S.Program
parseProgram' file prog = first errorBundlePretty (parseProgram file prog)

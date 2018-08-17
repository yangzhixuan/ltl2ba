module Parser where
import LTL
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


languageDef =
  emptyDef { Token.commentStart     = "/*"
           , Token.commentEnd       = "*/"
           , Token.commentLine      = "//"
           , Token.identStart       = lower
           , Token.identLetter      = lower <|> digit
           , Token.reservedNames    = [ "U", "X", "true" , "false" , "not" , "and" , "or" ]
           , Token.reservedOpNames  = [ "U", "X", "and", "or", "not"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser (TFormula String)
whileParser = whiteSpace >> bExpression

bExpression :: Parser (TFormula String)
bExpression = buildExpressionParser bOperators bTerm

bOperators = [ [Prefix (reservedOp "!" >> return TNot),  Prefix (reservedOp "X" >> return TNext)]
             , [Infix  (reservedOp "U" >> return TUntil) AssocLeft]
             , [Infix  (reservedOp "&" >> return TAnd) AssocLeft]
             , [Infix  (reservedOp "|" >> return TOr) AssocLeft]
             ]

bTerm =  parens bExpression
     <|> (reserved "true"  >> return TTrue)
     <|> (reserved "false" >> return TFalse)
     <|> (TAtomic <$> identifier)

parseString :: String -> TFormula String
parseString str = case parse whileParser "" str of
  Left e  -> error $ show e
  Right r -> r

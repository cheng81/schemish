module Parser
    (
     readExpr
    ,readExprList
    ) where

import           Control.Monad                 (void)
import           Control.Monad.Except
import           Numeric                       (readFloat)
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 (void space <|> comment)

spacesM :: Parser ()
spacesM = skipMany (void space <|> comment)

comment :: Parser ()
comment = void $ do
  string ";"
  many $ noneOf "\n"

escapedChar = do
  char '\\'
  c <- oneOf "\"\\nrt"
  case c of
    '\\' -> return "\\"
    '\"' -> return "\""
    'n' -> return "\n"
    'r' -> return "\r"
    't' -> return "\t"
    _ -> unexpected [c]

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (many1 (noneOf "\\\"") <|> escapedChar)
  char '"'
  return $ String $ join x

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  c <- parseCharLiteral <|> anyChar
  return $ Char c

parseCharLiteral = do
  charlit <- string "space" <|> string "newline"
  case charlit of
    "space" -> return ' '
    "newline" -> return '\n'
    s -> unexpected s

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    "#u" -> Unit
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit
-- parseNumber = fmap (Number . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat = do
  real <- many1 digit
  char '.'
  frac <- many1 digit
  return $ (Float . fst . head . readFloat) (real ++ "." ++ frac)

parseList :: Parser LispVal
parseList = List <$> sepEndBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- parseUnquotedExpr :: Parser LispVal
-- parseUnquotedExpr =  try parseUnquoted
--                  <|> parseAtom
--                  <|> parseString
--                  <|> parseChar
--                  <|> try parseFloat
--                  <|> parseNumber
--                  <|> parseQuoted
--                  <|> do char '('
--                         x <- try (parseList parseUnquotedExpr <|> parseDottedList parseUnquotedExpr)
--                         char ')'
--                         return x

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseChar
         <|> try parseFloat
         <|> parseNumber
         <|> parseQuoted
         <|> parseUnquoted
         <|> do char '('
                spacesM
                x <- try parseDottedList <|> parseList
                spacesM
                char ')'
                return x

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow (spacesM >> parseExpr)

readExprList = readOrThrow (spacesM >> endBy parseExpr spaces)
-- readExpr input = case parse parseExpr "lisp" input of
--   Left err -> throwError $ Parser err
--   Right val -> return val

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

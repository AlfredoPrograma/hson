module Parser where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit, isSpace, toUpper)
import Data.Map (Map)
import Parser.Combinators
import Parser.Monad

-- Reference: https://www.crockford.com/mckeeman.html

data JSONValue
  = JSONObject (Map String JSONValue)
  | JSONArray [JSONValue]
  | JSONString String
  | JSONNumber Int
  | JSONBoolean Bool
  | JSONNull
  deriving (Show)

-- parseObject :: Parser JSONValue
-- parseObject = do
--   obj <- bracket (char '{') _ (char '}')
--   return (JSONObject obj)

-- parseArray :: Parser JSONValue
-- parseArray = do
--   arr <- bracket (char '[') _ (char ']')
--   return (JSONArray arr)

parseString :: Parser JSONValue
parseString = do
  content <- bracket (char '"') string (char '"')
  return (JSONString content)

parseNumber :: Parser JSONValue
parseNumber = JSONNumber . read <$> number

parseBoolean :: Parser JSONValue
parseBoolean = do
  bool <- match "true" <|> match "false"
  return (JSONBoolean (read (capitalize bool)))
  where
    capitalize [] = ""
    capitalize (x : xs) = toUpper x : xs

parseNull :: Parser JSONValue
parseNull = do
  _ <- match "null"
  return JSONNull

string :: Parser String
string = do
  x <- charPred isAlphaNum
  acc <- string <|> pure ""
  return (x : acc)

character :: Parser Char
-- TODO: parse character uwu
character = undefined

escape :: Parser String
escape = charAsStr '"' <|> charAsStr '\\' <|> charAsStr '/' <|> charAsStr 'b' <|> charAsStr 'f' <|> charAsStr 'n' <|> charAsStr 'r' <|> charAsStr 't' <|> escapedHex
  where
    charAsStr ch = fmap (: []) (char ch)
    escapedHex = do
      u <- char 'u'
      hx1 <- hex
      hx2 <- hex
      hx3 <- hex
      hx4 <- hex
      return [u, hx1, hx2, hx3, hx4]

hex :: Parser Char
hex = digit <|> lowerHex <|> upperHex
  where
    lowerHex = charPred (\ch -> ch >= 'a' && ch <= 'f')
    upperHex = charPred (\ch -> ch >= 'A' && ch <= 'F')

number :: Parser String
number = do
  int <- integer
  float <- fraction
  exp <- Parser.exponent
  return (int ++ float ++ exp)

integer :: Parser String
integer = negOnenineDigit <|> negDigit <|> onenineDigit <|> strDigit
  where
    strDigit = fmap (: []) digit
    onenineDigit = do
      d <- onenine
      rest <- digits
      return (d : rest)
    negDigit = do
      minus <- char '-'
      d <- digit
      return [minus, d]
    negOnenineDigit = do
      minus <- char '-'
      rest <- digits
      return (minus : rest)

fraction :: Parser String
fraction = float <|> pure ""
  where
    float = do
      dot <- char '.'
      rest <- digits
      return (dot : rest)

digits :: Parser String
digits = many1 digit

digit :: Parser Char
digit = char '0' <|> onenine

onenine :: Parser Char
onenine = charPred (\ch -> ch >= '1' && ch <= '9')

exponent :: Parser String
exponent = exp <|> pure ""
  where
    exp = do
      e <- char 'E' <|> char 'e'
      s <- sign
      rest <- digits
      return (e : s : rest)

sign :: Parser Char
sign = char '+' <|> char '-' <|> pure '\0'

whitespaces :: Parser ()
whitespaces = do
  _ <- many0 $ charPred isSpace
  return ()

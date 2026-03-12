module JSON.Parser where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit, isSpace, toUpper)
import Data.Map qualified as Map
import Parser.Combinators
import Parser.Monad

data JSONValue
  = JSONObject (Map.Map String JSONValue)
  | JSONArray [JSONValue]
  | JSONString String
  | JSONNumber Float
  | JSONBoolean Bool
  | JSONNull
  deriving (Show)

jsonValue :: Parser JSONValue
jsonValue =
  Parser.Combinators.or
    [ jsonObject,
      jsonArray,
      jsonString,
      jsonNumber,
      jsonBool,
      jsonNull
    ]

member :: Parser (String, JSONValue)
member = do
  _ <- whitespaces
  key <- string
  _ <- whitespaces
  _ <- char ':'
  val <- element
  return (key, val)

members :: Parser [(String, JSONValue)]
members = sepBy1 member (char ',')

element :: Parser JSONValue
element = do
  _ <- whitespaces
  el <- jsonValue
  _ <- whitespaces
  return el

elements :: Parser [JSONValue]
elements = sepBy1 element (char ',')

jsonObject :: Parser JSONValue
jsonObject = bracket (char '{') obj (char '}')
  where
    valuesObj = fmap (JSONObject . Map.fromList) members
    emptyObj = fmap (\_ -> JSONObject Map.empty) whitespaces
    obj = valuesObj <|> emptyObj

jsonArray :: Parser JSONValue
jsonArray = bracket (char '[') arr (char ']')
  where
    emptyArr = fmap (\_ -> JSONArray []) whitespaces
    valuesArr = fmap JSONArray elements
    arr = valuesArr <|> emptyArr

jsonString :: Parser JSONValue
jsonString = JSONString <$> string

jsonNumber :: Parser JSONValue
jsonNumber = JSONNumber . read <$> number

jsonBool :: Parser JSONValue
jsonBool = true <|> false
  where
    true = fmap (const $ JSONBoolean True) (match "true")
    false = fmap (const $ JSONBoolean False) (match "false")

jsonNull :: Parser JSONValue
jsonNull = fmap (const JSONNull) (match "null")

string :: Parser String
string = bracket (char '"') characters (char '"')

characters :: Parser String
characters = many0 character

-- TODO: handle escaped characters using `escape` function
character :: Parser Char
character = charPred (\ch -> ch >= '\x0020' && ch <= '\x10FFFF' && ch `notElem` ['"', '\\'])

escape :: Parser String
escape =
  Parser.Combinators.or
    [ charAsStr '"',
      charAsStr '\\',
      charAsStr '/',
      charAsStr 'b',
      charAsStr 'f',
      charAsStr 'n',
      charAsStr 'r',
      charAsStr 't',
      escapedHex
    ]
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
hex = Parser.Combinators.or [digit, lowerHex, upperHex]
  where
    lowerHex = charPred (\ch -> ch >= 'a' && ch <= 'f')
    upperHex = charPred (\ch -> ch >= 'A' && ch <= 'F')

number :: Parser String
number = do
  int <- integer
  float <- fraction
  exp <- jsonExponent
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

jsonExponent :: Parser String
jsonExponent = exp <|> pure ""
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

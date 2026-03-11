module JSONParser where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit, toUpper)
import Data.Map (Map)
import Parser.Combinators
import Parser.Monad

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

number :: Parser String
number = do
  x <- charPred isDigit
  acc <- number <|> pure ""
  return (x : acc)

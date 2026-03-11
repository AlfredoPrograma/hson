{-# LANGUAGE LambdaCase #-}

module Parser.Combinators where

import Control.Applicative (Alternative, empty, (<|>))
import Parser.Monad

-- Single char parsers
char :: Char -> Parser Char
char ch =
  Parser
    ( \case
        [] -> Nothing
        (x : xs) -> if x == ch then Just (x, xs) else Nothing
    )

charPred :: (Char -> Bool) -> Parser Char
charPred f =
  Parser
    ( \case
        [] -> Nothing
        (x : xs) -> if f x then Just (x, xs) else Nothing
    )

-- String char parsers
match :: String -> Parser String
match [] = pure ""
match (x : xs) = do
  _ <- char x
  acc <- match xs
  return (x : acc)

-- Repetition parsers
many1 :: Parser a -> Parser [a]
many1 p = do
  _ <- p
  many1 p <|> pure []

many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> pure []

-- Delimiter parsers
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  _ <- open
  content <- p
  _ <- close
  return content

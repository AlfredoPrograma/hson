{-# LANGUAGE LambdaCase #-}

module Parser.Combinators where

import Control.Applicative (Alternative, empty, (<|>))
import Parser.Monad

-- Single chars
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

-- String
match :: String -> Parser String
match [] = pure ""
match (x : xs) = do
  _ <- char x
  acc <- match xs
  return (x : acc)

-- Repetitions
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  acc <- many1 p <|> pure []
  return (x : acc)

many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> pure []

-- Disjunction
or :: [Parser a] -> Parser a
or = foldl1 (<|>)

-- Delimiters
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  _ <- open
  content <- p
  _ <- close
  return content

-- Separators
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  v <- p
  acc <-
    many1
      ( do
          _ <- sep
          p
      )
      <|> pure []
  return (v : acc)

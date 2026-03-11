module Parser.Monad where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Char (isAlphaNum, isDigit, isSpace, toUpper)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f p =
    Parser
      ( \inp -> case runParser p inp of
          Nothing -> Nothing
          Just (x, rest) -> Just (f x, rest)
      )

instance Applicative Parser where
  pure x = Parser (\inp -> Just (x, inp))
  f <*> p =
    Parser
      ( \inp -> case runParser f inp of
          Nothing -> Nothing
          Just (g, _) -> case runParser p inp of
            Nothing -> Nothing
            Just (x, rest) -> Just (g x, rest)
      )

instance Alternative Parser where
  empty = Parser (const Nothing)
  p <|> q =
    Parser
      ( \inp -> case runParser p inp of
          Just (x, rest) -> Just (x, rest)
          Nothing -> case runParser q inp of
            Just (y, rest') -> Just (y, rest')
            Nothing -> Nothing
      )

instance Monad Parser where
  p >>= f =
    Parser
      ( \inp -> case runParser p inp of
          Nothing -> Nothing
          Just (x, rest) -> runParser (f x) rest
      )

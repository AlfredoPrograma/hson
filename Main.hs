module Main where

import JSON.Parser (jsonValue)
import Parser.Monad (Parser (runParser))
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  case runParser jsonValue contents of
    Nothing -> putStrLn "Invalid JSON"
    Just (value, _) -> print value

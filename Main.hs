module Main where

main :: IO ()
main = do
  contents <- readFile "sample.json"
  putStrLn contents

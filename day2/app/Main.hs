module Main where

import Lib (cordProduct, parseInput, position)

main :: IO ()
main = do
  content <- readFile "input"
  moves <- case parseInput content of
    Right x -> pure x
    Left e -> (error . show) e
  print $ (cordProduct . position) moves

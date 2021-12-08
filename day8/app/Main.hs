module Main where

import Lib (parseInput, part1, part2)

main :: IO ()
main = do
  content <- readFile "input"
  input <- case parseInput content of
    Left e -> error e
    Right i -> pure i
  print $ "part1: " <> show (part1 input)
  print $ "part2: " <> show (part2 input)

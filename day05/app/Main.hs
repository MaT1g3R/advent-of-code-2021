module Main where

import Lib (parseInput, part1, part2)

main :: IO ()
main = do
  content <- readFile "input"
  input <- case parseInput content of
    Left e -> error $ show e
    Right i -> pure i
  let p1 = part1 input
  let p2 = part2 input
  putStrLn $ "part1: " ++ show p1
  putStrLn $ "part2: " ++ show p2

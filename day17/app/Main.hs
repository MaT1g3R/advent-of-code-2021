module Main where

import Lib (parseInput, part1, part2)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  putStrLn $ "part1: " <> (show . part1) input
  putStrLn $ "part2: " <> (show . part2) input

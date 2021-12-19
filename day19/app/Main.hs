module Main where

import Lib (parseInput, part1, part2, solve)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  let solved = solve input
  putStrLn $ "part1: " <> (show . part1) solved
  putStrLn $ "part2: " <> (show . part2) solved

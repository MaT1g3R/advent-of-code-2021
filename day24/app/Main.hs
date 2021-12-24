module Main where

import Control.Parallel
import Lib (parseInput, part1, part2)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  let p1 = part1 input
  let p2 = part1 `par` part2 input
  putStrLn $ "part1: " <> show p1
  putStrLn $ "part2: " <> show p2

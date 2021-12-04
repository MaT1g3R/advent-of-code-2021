module Main where

import GHC.Float (Floating (sqrt))
import Lib (parseInput, playGame, winLast, winningIndicies)

main :: IO ()
main = do
  content <- readFile "input"
  (input, boards) <- case parseInput content of
    Right i -> pure i
    Left e -> error $ show e
  let wins = winningIndicies 5
  print $ winLast wins boards input

module Main where

import Lib (lifeSupport, parseInput)

main :: IO ()
main = do
  content <- readFile "input"
  let input = parseInput content
  print $ lifeSupport input

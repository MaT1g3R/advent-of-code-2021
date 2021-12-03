module Main where

import Lib (binToDec, epsilonFromGamma, gamma, parseInput)

main :: IO ()
main = do
  content <- readFile "input"
  let input = parseInput content
  let gamma_ = gamma input
  let epsilon_ = epsilonFromGamma gamma_
  print (binToDec gamma_ * binToDec epsilon_)

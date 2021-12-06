module Main where

import qualified Data.Text.IO as T
import Lib (parseInput, part1)

main :: IO ()
main = do
  content <- T.readFile "input"
  let input = parseInput content
  putStrLn $ "part1: " ++ show (part1 input)

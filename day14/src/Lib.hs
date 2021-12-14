module Lib (Input, part1, part2, parseInput) where

import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Pair = (Char, Char)

type State = Map Pair Integer

type Instructions = Map Pair Char

type Input = (State, Instructions, Char, Char)

step :: Instructions -> State -> State
step ins = Map.foldlWithKey (\m (a, b) i -> splitPair m (a, b) i) Map.empty
  where
    splitPair m' (a, b) i =
      case Map.lookup (a, b) ins of
        Just c ->
          addCount i (addCount i m' (a, c)) (c, b)
        Nothing -> m'

steps :: Instructions -> Integer -> State -> State
steps ins i m = foldl' (\x _ -> step ins x) m [1 .. i]

count :: Ord a => [a] -> Map a Integer
count = foldl' incCount Map.empty

addCount :: Ord a => Integer -> Map a Integer -> a -> Map a Integer
addCount i m x = Map.insertWith (+) x i m

incCount :: Ord a => Map a Integer -> a -> Map a Integer
incCount = addCount 1

countElements :: Char -> Char -> State -> Map Char Integer
countElements start end m =
  Map.mapWithKey
    (\c i -> i `div` 2 + if c == start || c == end then 1 else 0)
    $ Map.foldlWithKey (\m' (a, b) i -> addCount i (addCount i m' a) b) Map.empty m

solve :: Integer -> Input -> Integer
solve i (m, ins, start, end) = maximum counts - minimum counts
  where
    finalState = steps ins i m
    counts = snd <$> Map.toList (countElements start end finalState)

part1 :: Input -> Integer
part1 = solve 10

part2 :: Input -> Integer
part2 = solve 40

parseInput :: String -> Input
parseInput s = (parseTemplate template, Map.fromList $ parseInstruction <$> tail lns, head template, last template)
  where
    lns = filter (not . null) $ lines s
    template = head lns
    parseTemplate s = count $ s `zip` tail s
    parseInstruction s =
      let k = (head s, s !! 1)
          v = last s
       in (k, v)

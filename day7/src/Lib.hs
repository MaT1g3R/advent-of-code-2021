module Lib (part1, part2, parseInput) where

import Data.Set (Set)
import qualified Data.Set as Set

findKth :: [Int] -> Int -> Int
findKth (x : xs) k
  | k == length l + 1 = x
  | k <= length l = findKth l k
  | k > length l + 1 = findKth r (k - (length l + 1))
  where
    split xs p = ([x | x <- xs, x <= p], [x | x <- xs, x > p])
    (l, r) = split xs x
findKth [a] _ = a
findKth _ _ = -1

median xs = findKth xs $ length xs `div` 2

median' xs = findKth xs $ 1 + length xs `div` 2

move f xs = sum $ f <$> xs

diff a b = abs (a - b)

part1 :: [Int] -> Int
part1 xs = min (move (diff m) xs) (move (diff m') xs)
  where
    m = median xs
    m' = median' xs

part2 :: [Int] -> Int
part2 xs = min (calcFuel avg) (calcFuel avg')
  where
    avg = sum xs `div` length xs
    avg' = succ avg
    fuel p x = d * succ d `div` 2
      where
        d = diff p x

    calcFuel p = move (fuel p) xs

parseInput :: String -> [Int]
parseInput input = read $ "[" <> input <> "]"

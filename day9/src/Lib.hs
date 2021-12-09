module Lib (part1, part2, parseInput) where

import Control.Monad
import Data.Foldable (Foldable (foldl'))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type HeightMap = Vector (Vector Integer)

type IndexedHeightMap = Vector (Vector (Index, Integer))

type Index = (Int, Int)

width :: HeightMap -> Int
width = V.length . V.head

height :: HeightMap -> Int
height = V.length

indexed :: HeightMap -> IndexedHeightMap
indexed h = fmap (\(n, row) -> fmap (\(m, col) -> ((n, m), col)) (V.indexed row)) (V.indexed h)

adjacentIndexes :: Index -> Vector Index
adjacentIndexes (n, m) = V.fromList $ filter (\(x, y) -> x >= 0 && y >= 0) xs
  where
    xs = [(n + 1, m), (n -1, m), (n, m + 1), (n, m - 1)]

access :: IndexedHeightMap -> Index -> Maybe Integer
access h (n, m) = do
  row <- h !? n
  val <- row !? m
  return $ snd val

adjacent :: IndexedHeightMap -> Index -> Vector Integer
adjacent h i = V.mapMaybe (access h) $ adjacentIndexes i

riskLevel :: IndexedHeightMap -> Index -> Integer
riskLevel h (n, m) = risk
  where
    self = access h (n, m)
    neibhours = adjacent h (n, m)
    risk = case self of
      Just x -> if V.all (> x) neibhours then succ x else 0
      _ -> 0

part1 :: IndexedHeightMap -> Integer
part1 h = sum $ (\(i, _) -> riskLevel h i) <$> join h

findBasin :: IndexedHeightMap -> Index -> Set Index -> (Integer, Set Index)
findBasin h i seen = case self of
  Nothing -> (0, seen)
  Just 9 -> (0, seen)
  Just x ->
    if S.member i seen
      then (0, seen)
      else foldl' accum (1, S.insert i seen) neibhours
  where
    self = access h i
    neibhours = adjacentIndexes i

    accum :: (Integer, Set Index) -> Index -> (Integer, Set Index)
    accum (size, seen') i' = (size + size', S.union seen' seen'')
      where
        (size', seen'') = findBasin h i' seen'

part2 :: IndexedHeightMap -> Integer
part2 h = head s * s !! 1 * s !! 2
  where
    accum :: ([Integer], Set Index) -> Index -> ([Integer], Set Index)
    accum (bs, seen) i = (b : bs, S.union seen seen')
      where
        (b, seen') = findBasin h i seen

    (res, _) = foldl' accum ([], S.empty) (fmap fst (join h))
    s = reverse $ sort res

parseInput :: String -> IndexedHeightMap
parseInput s = indexed matrix
  where
    lns = V.filter (not . null) (V.fromList (lines s))
    parseLine :: String -> Vector Integer
    parseLine l = (\c -> read [c]) <$> V.fromList l
    matrix = parseLine <$> lns

module Lib (Input, part1, part2, parseInput) where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue
import Data.Set (Set)
import qualified Data.Set as Set

type Cord = (Int, Int)

type Input = Matrix Int

neighbours :: Cord -> Cord -> [Cord]
neighbours (x, y) (maxX, maxY) = filter (\(x', y') -> x' > 0 && y' > 0 && x' <= maxX && y' <= maxY) [(x - 1, y), (x + 1, y), (x, y -1), (x, y + 1)]

shortestPath :: (Cord -> Int) -> Cord -> Cord -> Int
shortestPath cost start goal = finalDist ! goal
  where
    dist :: Map Cord Int
    dist = Map.singleton start 0

    prev :: Map Cord Cord
    prev = Map.empty

    q :: MinPQueue Int Cord
    q = PQueue.singleton 0 start

    pqueueTryInsert k v q = if v `elem` q then q else PQueue.insert k v q

    inf = 9999999

    go q dist prev
      | PQueue.null q = (dist, prev)
      | otherwise =
        let ((k, u), q') = PQueue.deleteFindMin q
            updateNeighbour (q, dist, prev) v =
              let alt = fromMaybe inf (Map.lookup u dist) + cost v
               in if alt < fromMaybe inf (Map.lookup v dist) then (pqueueTryInsert alt v q, Map.insert v alt dist, Map.insert v u prev) else (q, dist, prev)
            (q'', dist', prev') = foldl' updateNeighbour (q', dist, prev) (neighbours u goal)
         in go q'' dist' prev'

    (finalDist, _) = go q dist prev

part1 :: Input -> Int
part1 input = shortestPath (\(x, y) -> Matrix.getElem x y input) (1, 1) (Matrix.nrows input, Matrix.ncols input)

part2 :: Input -> Int
part2 input = shortestPath cost (1, 1) (5 * Matrix.nrows input, 5 * Matrix.ncols input)
  where
    cost :: Cord -> Int
    cost (x, y) = wrap $ dx + dy + Matrix.getElem (succ mx) (succ my) input
      where
        wrap x
          | x >= 10 = (x `mod` 10) + 1
          | otherwise = x
        (dx, mx) = (x - 1) `divMod` Matrix.nrows input
        (dy, my) = (y - 1) `divMod` Matrix.ncols input

parseInput :: String -> Input
parseInput s = Matrix.fromLists $ fmap digitToInt <$> lns
  where
    lns = filter (not . null) (lines s)

{-# LANGUAGE TupleSections #-}

module Lib (Input, part1, part2, parseInput) where

import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

data SeaCucumber = East | South deriving (Eq)

instance Show SeaCucumber where
  show East = ">"
  show South = "v"

type Input = (Map (Int, Int) SeaCucumber, Int, Int)

down nrows i
  | i >= nrows - 1 = 0
  | otherwise = i + 1

right ncols j
  | j >= ncols -1 = 0
  | otherwise = j + 1

move :: Input -> Maybe Input
move (m, nrows, ncols) = if moved || moved' then Just (m'', nrows, ncols) else Nothing
  where
    moveOneEast m (m', hasMoved) (i, j) South = (Map.insert (i, j) South m', hasMoved)
    moveOneEast m (m', hasMoved) (i, j) East = case Map.lookup (i, right ncols j) m of
      Nothing -> (Map.insert (i, right ncols j) East m', True)
      _ -> (Map.insert (i, j) East m', hasMoved)

    moveOneSouth m (m', hasMoved) (i, j) East = (Map.insert (i, j) East m', hasMoved)
    moveOneSouth m (m', hasMoved) (i, j) South = case Map.lookup (down nrows i, j) m of
      Nothing -> (Map.insert (down nrows i, j) South m', True)
      _ -> (Map.insert (i, j) South m', hasMoved)

    moveEast = Map.foldlWithKey (moveOneEast m) (Map.empty, False)
    (m', moved) = moveEast m

    moveSouth = Map.foldlWithKey (moveOneSouth m') (Map.empty, False)
    (m'', moved') = moveSouth m'

part1 :: Input -> Integer
part1 input = go input 0
  where
    go input i = case move input of
      Just input' -> go input' i + 1
      Nothing -> i + 1

part2 :: Input -> Integer
part2 _ = 0

parseInput :: String -> Input
parseInput s = (m, nrows, ncols)
  where
    lns = filter (not . null) $ lines s
    nrows = length lns
    ncols = length $ head lns

    parse 'v' = Just South
    parse '>' = Just East
    parse _ = Nothing

    m =
      Map.fromList . catMaybes $
        ( \(row, i) ->
            (\(col, j) -> ((i, j),) <$> parse col) <$> row `zip` [0 ..]
        )
          =<< lns `zip` [0 ..]

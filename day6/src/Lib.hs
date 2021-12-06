{-# LANGUAGE OverloadedStrings #-}

module Lib (part1, parseInput) where

import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

type FishState = Map Int Int

insertN :: Int -> Int -> FishState -> FishState
insertN = Map.insertWith (+)

insert1 :: FishState -> Int -> FishState
insert1 m k = insertN k 1 m

count :: [Int] -> FishState
count = foldl' insert1 Map.empty

simDay :: FishState -> FishState
simDay m = Map.mapKeys pred newM
  where
    zeros = Map.lookup 0 m

    newM = case zeros of
      Just z ->
        insertN 7 z $
          insertN 9 z $ Map.filterWithKey (\k _ -> k /= 0) m
      Nothing -> m

simDays :: Int -> FishState -> FishState
simDays days fish = foldl' f fish [1 .. days]
  where
    f m _ = simDay m

parseInput :: T.Text -> [Int]
parseInput input = read . T.unpack <$> T.splitOn "," input

part1 :: [Int] -> Int
part1 input = sum $ snd <$> Map.toList finalState
  where
    state = count input
    finalState = simDays 80 state

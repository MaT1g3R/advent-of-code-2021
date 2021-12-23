{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib (part2) where

import Control.Monad (guard)
import qualified Data.Foldable as F
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PSQueue
import Data.Set (Set)
import qualified Data.Set as Set

type Cell = Maybe Amph

data Burrow = Burrow
  { sideRooms :: Map Int [Amph],
    hall :: Map Int Cell
  }
  deriving (Show, Eq, Ord)

type Energy = Int

data Amph = A | B | C | D
  deriving (Show, Eq, Ord)

roomDepth = 4

initBurrow =
  Burrow
    { sideRooms = Map.fromList [(2, [D, D, D, B]), (4, [C, C, B, C]), (6, [A, B, A, D]), (8, [B, A, C, A])],
      hall = Map.fromList $ zip [0, 1, 3, 5, 7, 9, 10] (repeat Nothing)
    }

finalBurrow =
  Burrow
    { sideRooms = Map.fromList $ zip [2, 4, 6, 8] $ map (replicate 4) [A, B, C, D],
      hall = Map.fromList $ zip [0, 1, 3, 5, 7, 9, 10] (repeat Nothing)
    }

part2 = lowestEnergyFrom initBurrow

lowestEnergyFrom :: Burrow -> Energy
lowestEnergyFrom start = dijk (PSQueue.singleton start 0) Set.empty

type Frontier = PSQueue.PSQ Burrow Energy

dijk :: Frontier -> Set Burrow -> Energy
dijk frontier visited = result
  where
    Just (curBurrow PSQueue.:-> energyToCurBurrow, frontier') = PSQueue.minView frontier
    result
      | curBurrow == finalBurrow = energyToCurBurrow
      | otherwise = dijk frontier'' visited'
    visited' = Set.insert curBurrow visited
    frontier'' = F.foldl' (\q (energyToNeighbor, neighbor) -> PSQueue.insertWith min neighbor (energyToCurBurrow + energyToNeighbor) q) frontier' unexploredNeighbors
    unexploredNeighbors = filter (not . (`Set.member` visited) . snd) $ neighbors curBurrow

neighbors :: Burrow -> [(Energy, Burrow)]
neighbors b = roomToHall b ++ hallToRoom b ++ roomToRoom b

roomToHall :: Burrow -> [(Energy, Burrow)]
roomToHall Burrow {sideRooms, hall} = do
  (roomX, amph : amphs) <- Map.toList sideRooms
  guard $ not $ List.all ((== roomX) . destinationX) (amph : amphs)
  (hallX, Nothing) <- Map.toList hall
  guard $ List.null (hallBlockers roomX hallX hall)
  let distUp = roomDepth - length amphs
  let distOver = abs (roomX - hallX)
  let energy = (distUp + distOver) * amphEnergy amph
  let sideRooms' = Map.insert roomX amphs sideRooms
  let hall' = Map.insert hallX (Just amph) hall
  pure (energy, Burrow sideRooms' hall')

hallToRoom :: Burrow -> [(Energy, Burrow)]
hallToRoom Burrow {sideRooms, hall} = do
  (hallX, Just amph) <- Map.toList hall
  let roomX = destinationX amph
  let amphs = sideRooms Map.! roomX
  guard $ List.all (== amph) amphs
  guard $ List.null (hallBlockers roomX hallX hall)
  let distOver = abs (roomX - hallX)
  let distDown = roomDepth - length amphs
  let energy = (distOver + distDown) * amphEnergy amph
  let sideRooms' = Map.insert roomX (amph : amphs) sideRooms
  let hall' = Map.insert hallX Nothing hall
  pure (energy, Burrow sideRooms' hall')

roomToRoom :: Burrow -> [(Energy, Burrow)]
roomToRoom Burrow {sideRooms, hall} = do
  (roomX1, amph : amphs1) <- Map.toList sideRooms
  guard $ not $ List.all ((== roomX1) . destinationX) (amph : amphs1)
  (roomX2, amphs2) <- Map.toList sideRooms
  guard $ roomX1 /= roomX2
  guard $ List.all (== amph) amphs2
  guard $ List.null (hallBlockers roomX1 roomX2 hall)
  let distUp = roomDepth - length amphs1
  let distOver = abs (roomX1 - roomX2)
  let distDown = roomDepth - length amphs2
  let energy = (distUp + distOver + distDown) * amphEnergy amph
  let sideRooms' =
        sideRooms
          & Map.insert roomX1 amphs1
          & Map.insert roomX2 (amph : amphs2)
  pure (energy, Burrow sideRooms' hall)

hallBlockers :: Int -> Int -> Map Int Cell -> [Amph]
hallBlockers x1 x2 = F.toList . Map.mapMaybeWithKey toBlocker
  where
    travelRange = if x1 < x2 then (x1, x2) else (x2, x1)
    toBlocker x cell = if x1 < x && x < x1 then cell else Nothing

destinationX :: Amph -> Int
destinationX = \case
  A -> 2
  B -> 4
  C -> 6
  D -> 8

amphEnergy :: Amph -> Int
amphEnergy = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

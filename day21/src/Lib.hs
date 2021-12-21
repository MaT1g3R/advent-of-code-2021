{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib (Input, part1, part2, parseInput) where

import Control.Monad.State
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Prelude hiding (init)

type Input = (Integer, Integer)

data Player = Player {score :: Integer, position :: Integer} deriving (Eq, Show, Ord)

type GameState a = (Player, Player, a)

class Die a where
  init :: a
  roll :: Integer -> a -> (Integer, a)
  rolls :: a -> Integer

newtype DeterministicDie = DeterministicDie ([Integer], Integer)

instance Show DeterministicDie where
  show (DeterministicDie (_, n)) = "DeterministicDie: " <> show n

instance Die DeterministicDie where
  init = DeterministicDie (cycle [1 .. 100], 0)
  roll r (DeterministicDie (xs, n)) =
    let (roll, xs') = splitAt (fromInteger r) xs
     in (sum roll, DeterministicDie (xs', n + r))
  rolls (DeterministicDie (_, n)) = n

movePlayer :: Player -> Integer -> Player
movePlayer (Player score position) n = Player {score = score + position', position = position'}
  where
    position' = ([position .. 10] ++ cycle [1 .. 10]) !! fromInteger n

playGame' :: Die a => State (GameState a) (Maybe Player)
playGame' = do
  p <- play movePlayer const
  case p of
    Just p -> return $ Just p
    Nothing -> play const movePlayer
  where
    play modifyPlayer1 modifyPlayer2 = do
      (p1, p2, die) <- get
      let (r, die') = roll 3 die
      let p1' = modifyPlayer1 p1 r
      let p2' = modifyPlayer2 p2 r
      put (p1', p2', die')
      return if score p1' >= 1000 then Just p2' else if score p2' >= 1000 then Just p1' else Nothing

playGame :: Die a => State (GameState a) Player
playGame = playGame' >>= maybe playGame return

part1 :: Input -> Integer
part1 (p1, p2) = rolls die * score losingPlayer
  where
    initState = (Player 0 p1, Player 0 p2, init :: DeterministicDie)
    (losingPlayer, (_, _, die)) = runState playGame initState

playQuantumGame :: Player -> Player -> Integer
playQuantumGame p1 p2 = uncurry max $ evalState (dp (p1, p2)) Map.empty
  where
    dp :: (Player, Player) -> State (Map (Player, Player) (Integer, Integer)) (Integer, Integer)
    dp (p1, p2)
      | score p1 >= 21 = return (1, 0)
      | score p2 >= 21 = return (0, 1)
      | otherwise = do
        m <- get
        case Map.lookup (p1, p2) m of
          Just res -> return res
          Nothing -> do
            res <-
              swap . foldl' (\(a, b) (a', b') -> (a + a', b + b')) (0, 0)
                <$> mapM
                  dp
                  [ (p2, p1')
                    | d <- sum <$> replicateM 3 [1 .. 3],
                      let p1' = movePlayer p1 d
                  ]
            modify (Map.insert (p1, p2) res)
            return res

part2 :: Input -> Integer
part2 (p1, p2) = playQuantumGame (Player 0 p1) (Player 0 p2)

parseInput :: String -> Input
parseInput s = (read [p1], read [p2])
  where
    lns = filter (not . null) $ lines s
    p1 = last $ head lns
    p2 = last $ last lns

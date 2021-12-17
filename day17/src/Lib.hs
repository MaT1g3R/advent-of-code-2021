{-# LANGUAGE BlockArguments #-}

module Lib (Input, part1, part2, parseInput) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P),
    Parsec,
    parse,
  )
import Text.Megaparsec.Char (char, space, string)

type Cord = (Int, Int)

type Velocity = Int

type Input = (Cord, Cord)

arc :: Input -> (Velocity, Velocity) -> Maybe [Cord]
arc ((minx, maxx), (miny, maxy)) (vx, vy) = evalState (sim (0, 0)) (vx, vy)
  where
    sim :: Cord -> State (Velocity, Velocity) (Maybe [Cord])
    sim (x, y) = do
      (vx, vy) <- get
      put (max 0 (vx - 1), vy - 1)
      if (x > maxx) || (y < miny)
        then return Nothing
        else
          if (x, y) `within` ((minx, maxx), (miny, maxy))
            then return $ Just [(x, y)]
            else do
              xs <- sim (x + vx, y + vy)
              return $ ((x, y) :) <$> xs

genArcs :: Input -> [[Cord]]
genArcs ((minx, maxx), (miny, maxy)) = catMaybes [arc ((minx, maxx), (miny, maxy)) (vx, vy) | vx <- [1 .. maxx], vy <- [miny .. negate miny]]

within :: Cord -> Input -> Bool
within (x, y) ((minx, maxx), (miny, maxy)) = x >= minx && x <= maxx && y >= miny && y <= maxy

part1 :: Input -> Int
part1 input = maximum $ maxYForArc <$> arcs
  where
    arcs = genArcs input

    maxYForArc arc = maximum $ snd <$> arc

part2 :: Input -> Int
part2 input = length $ genArcs input

parseInput :: String -> Input
parseInput s = case parse parser "input" s of
  Left e -> (error . show) e
  Right i -> i
  where
    parseInt :: Parsec Void String Int
    parseInt = read <$> takeWhile1P Nothing (\s -> s == '-' || isDigit s)

    parser :: Parsec Void String Input
    parser = do
      string "target area: x="
      x1 <- parseInt
      string ".."
      x2 <- parseInt
      char ','
      space
      string "y="
      y1 <- parseInt
      string ".."
      y2 <- parseInt
      return ((x1, x2), (y1, y2))

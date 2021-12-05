{-# LANGUAGE TupleSections #-}

module Lib (Line, part1, part2, parseInput) where

import Data.Foldable (Foldable (foldl'))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Text.ParserCombinators.Parsec as P

type Point = (Int, Int)

type Line = (Point, Point)

type Covered = Map Point Int

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

points :: Line -> [Point]
points ((x1, y1), (x2, y2))
  | x1 == x2 = (x1,) <$> [(min y1 y2) .. (max y1 y2)]
  | y1 == y2 = (,y1) <$> [(min x1 x2) .. (max x1 x2)]
points ((x1, y1), (x2, y2)) = result
  where
    d1 = [0 ..] `zip` [0 ..]
    d3 = [0, -1 ..] `zip` [0, -1 ..]
    d2 = [0, -1 ..] `zip` [0 ..]
    d4 = [0 ..] `zip` [0, -1 ..]

    move (a, b) (x, y) = (,) (a + x) (b + y)
    end = (x2, y2)

    takeUntil :: (a -> Bool) -> [a] -> [a]
    takeUntil _ [] = []
    takeUntil p (x : xs) =
      x :
      if p x
        then takeUntil p xs
        else []

    l1 = takeUntil (/= end) (move (x1, y1) <$> d1)
    l2 = takeUntil (/= end) (move (x1, y1) <$> d2)
    l3 = takeUntil (/= end) (move (x1, y1) <$> d3)
    l4 = takeUntil (/= end) (move (x1, y1) <$> d4)

    z = List.zip4 l1 l2 l3 l4
    (t1, t2, t3, t4) = last z
    result
      | t1 == end = (\(a, b, c, d) -> a) <$> z
      | t2 == end = (\(a, b, c, d) -> b) <$> z
      | t3 == end = (\(a, b, c, d) -> c) <$> z
      | t4 == end = (\(a, b, c, d) -> d) <$> z
      | otherwise = []

count :: (Ord a) => [a] -> Map a Int
count = foldl' f Map.empty
  where
    f m a = Map.insertWith (+) a 1 m

covered :: [Line] -> Covered
covered lines = count a
  where
    a = lines >>= points

atLeast :: Int -> Covered -> Int
atLeast i c = Map.size $ Map.filter (>= i) c

part1 :: [Line] -> Int
part1 lines = atLeast 2 $ covered $ filter isStraight lines

part2 :: [Line] -> Int
part2 lines = atLeast 2 $ covered lines

parseInput :: String -> Either P.ParseError [Line]
parseInput = P.parse pInput "bad input"

pNum :: P.Parser Int
pNum = read <$> P.many1 P.digit

pPoint :: P.Parser Point
pPoint = do
  x <- pNum
  P.char ','
  y <- pNum
  return (x, y)

pLine :: P.Parser Line
pLine = do
  p1 <- pPoint
  P.spaces
  P.string "->"
  P.spaces
  p2 <- pPoint
  return (p1, p2)

pInput :: P.Parser [Line]
pInput = pLine `P.sepEndBy1` P.char '\n'

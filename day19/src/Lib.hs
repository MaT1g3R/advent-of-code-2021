module Lib (Input, part1, part2, parseInput, solve) where

import Data.Either (partitionEithers)
import Data.Foldable (Foldable (foldl'), maximumBy)
import Data.List (transpose)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Point = (Int, Int, Int)

type Scanner = [Point]

type Input = [Scanner]

orientations :: [Point -> Point]
orientations = flip applyRotations <$> cases
  where
    x (x, y, z) = (x, - z, y)
    y (x, y, z) = (z, y, - x)
    z (x, y, z) = (y, - x, z)

    cases =
      [ [],
        [x],
        [y],
        [z],
        [x, x],
        [x, y],
        [x, z],
        [y, x],
        [y, y],
        [z, y],
        [z, z],
        [x, x, x],
        [x, x, y],
        [x, x, z],
        [x, y, x],
        [x, y, y],
        [x, z, z],
        [y, x, x],
        [y, y, y],
        [z, z, z],
        [x, x, x, y],
        [x, x, y, x],
        [x, y, x, x],
        [x, y, y, y]
      ]

    applyRotations = foldl' (\p f -> f p)

(x, y, z) <+> (x', y', z') = (x + x', y + y', z + z')

(x, y, z) <-> (x', y', z') = (x - x', y - y', z - z')

overlap ::
  (Point, S.Set Point) -> S.Set Point -> Maybe (Point, S.Set Point)
overlap (_, adjusted) points = case found of
  (allAdjustedPoints, pos') : _ -> Just (pos', allAdjustedPoints)
  _ -> Nothing
  where
    found =
      [ (points', offset)
        | a <- take (S.size adjusted - 11) $ S.toList adjusted,
          spin <- orientations,
          p <- S.toList points,
          let offset = a <-> spin p,
          let points' = S.map ((<+>) offset . spin) points,
          S.size (S.intersection adjusted points') >= 12
      ]

triangulate ::
  [(Point, S.Set Point)] -> [S.Set Point] -> [(Point, S.Set Point)]
triangulate adjusted [] = adjusted
triangulate adjusted scanners =
  let (a', s') =
        partitionEithers $
          map
            ( \s -> case mapMaybe (`overlap` s) adjusted of
                [] -> Right s
                (s' : _) -> Left s'
            )
            scanners
   in triangulate (adjusted ++ a') s'

solve (x : xs) = triangulate [((0, 0, 0), S.fromList x)] (S.fromList <$> xs)
solve [] = error "empty input"

part1 solved = (S.size . S.unions) $ snd <$> solved

part2 solved =
  maximum
    [ abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
      | (x1, y1, z1) <- s,
        (x2, y2, z2) <- s
    ]
  where
    s = fst <$> solved

type Parser = Parsec Void String

parseInput :: String -> Input
parseInput s = case parse scanners "input" s of
  Right x -> x
  Left e -> error $ show e
  where
    int :: Parser Int
    int = read <$> some (try $ char '-' <|> digitChar)

    point :: Parser Point
    point = do
      x <- int
      char ','
      y <- int
      char ','
      z <- int
      return (x, y, z)

    scanner :: Parser Scanner
    scanner = do
      string "---"
      space
      string "scanner"
      space
      some digitChar
      space
      string "---"
      space
      ps <- point `sepEndBy` newline
      space
      return ps

    scanners :: Parser [Scanner]
    scanners = some scanner

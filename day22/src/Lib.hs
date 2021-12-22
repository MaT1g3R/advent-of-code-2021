module Lib (Input, part1, part2, parseInput) where

import Control.Monad (foldM)
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    parse,
    sepEndBy,
    some,
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    newline,
    space,
    string,
  )

data Step = On Range Range Range | Off Range Range Range deriving (Eq, Show)

type Input = [Step]

type Point = (Integer, Integer, Integer)

type Range = (Integer, Integer)

type Cube = (Range, Range, Range)

type Generator = (Set Point, Cube)

stepPoints :: Cube -> Step -> [Point]
stepPoints (xrange, yrange, zrange) step = [(x, y, z) | x <- xs, y <- ys, z <- zs]
  where
    chomp (minP, maxP) (a, b)
      | b < minP = Nothing
      | a > maxP = Nothing
      | otherwise = Just (max minP a, min b maxP)

    (x, y, z) = case step of
      (On x y z) -> (x, y, z)
      (Off x y z) -> (x, y, z)

    makeRange Nothing = []
    makeRange (Just (a, b)) = [a .. b]

    xs = makeRange $ chomp xrange x
    ys = makeRange $ chomp yrange y
    zs = makeRange $ chomp zrange z

runStep :: Generator -> Step -> Generator
runStep (g, area) step@On {} = (foldl' (flip Set.insert) g $ stepPoints area step, area)
runStep (g, area) step@Off {} = (foldl' (flip Set.delete) g $ stepPoints area step, area)

runSteps :: Generator -> [Step] -> Generator
runSteps = foldl' runStep

combineRange :: Range -> Range -> Range
combineRange (x, y) (a, b) = (min x a, max y b)

part1 :: Input -> Int
part1 input = Set.size . fst $ runSteps g input
  where
    area = (-50, 50)
    g = (Set.empty, (area, area, area))

part2 :: Input -> Integer
part2 input = sum (MultiSet.map volume on) - sum (MultiSet.map volume off)
  where
    intersectRange :: Range -> Range -> Maybe Range
    intersectRange (x, y) (a, b)
      | max1 <= min2 = Just (max1, min2)
      | otherwise = Nothing
      where
        max1 = max x a
        min2 = min y b

    intersectCube :: Cube -> Cube -> Maybe Cube
    intersectCube (x, y, z) (x', y', z') = (,,) <$> intersectRange x x' <*> intersectRange y y' <*> intersectRange z z'

    volume :: Cube -> Integer
    volume ((x1, x2), (y1, y2), (z1, z2)) = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2)) * (1 + abs (z1 - z2))

    acc :: (MultiSet Cube, MultiSet Cube) -> Step -> (MultiSet Cube, MultiSet Cube)
    acc (on, off) step = case step of
      On {} -> (MultiSet.insert c $ MultiSet.union offIntersects on, MultiSet.union onIntersects off)
      Off {} -> (MultiSet.union offIntersects on, MultiSet.union onIntersects off)
      where
        c = case step of
          (On x y z) -> (x, y, z)
          (Off x y z) -> (x, y, z)
        onIntersects = MultiSet.mapMaybe (intersectCube c) on
        offIntersects = MultiSet.mapMaybe (intersectCube c) off

    (on, off) = foldl' acc (MultiSet.empty, MultiSet.empty) input

type Parser = Parsec Void String

parseInput :: String -> Input
parseInput s = case parse (pStep `sepEndBy` newline) "input" s of
  Right i -> i
  Left e -> error (show e)
  where
    pDigit :: Parser Char
    pDigit = try $ char '-' <|> digitChar

    pInt :: Parser Integer
    pInt = read <$> some pDigit

    pRange :: Parser Range
    pRange = do
      a <- pInt
      string ".."
      b <- pInt
      return (min a b, max a b)

    pStep :: Parser Step
    pStep = do
      constructor <- On <$ try (string "on") <|> Off <$ string "off"
      space
      string "x="
      x <- pRange
      string ",y="
      y <- pRange
      string ",z="
      constructor x y <$> pRange

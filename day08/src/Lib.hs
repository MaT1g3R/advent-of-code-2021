{-# LANGUAGE TupleSections #-}

module Lib (Entry, parseInput, part1, part2) where

import Control.Monad
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Entry = ([String], [String])

part1 :: [Entry] -> Int
part1 input = length a
  where
    a = filter (\s -> Set.member (length s) $ Set.fromList [2, 3, 4, 7]) . snd =<< input

part2 :: [Entry] -> Int
part2 input = sum e
  where
    assignments = tryAssigns . possibleLetters' . fst <$> input
    b = (snd <$> input) `zip` (fst <$> assignments)
    c = (\(ss, s) -> decode s <$> ss) <$> b
    d a = foldr (\(pow, d) a -> a + d * 10 ^ pow) 0 ([0 ..] `zip` reverse a)
    e = d <$> c

decode' :: Map Char (Set Char) -> String -> Int
decode' m s = c
  where
    a = (\c -> fromMaybe Set.empty $ Map.lookup c m) <$> s
    b = foldl' Set.union Set.empty a
    c = fromMaybe (-1) $ Map.lookup b lettersToDigitMap

decode :: [Map Char (Set Char)] -> String -> Int
decode xs s = case r of
  [] -> -1
  (x : xs) -> x
  where
    r = filter (>= 0) $ (`decode'` s) <$> xs

countToDigitMap :: Map Int [Int]
countToDigitMap =
  Map.fromList
    [ (6, [0, 6, 9]),
      (2, [1]),
      (5, [2, 3, 5]),
      (4, [4]),
      (3, [7]),
      (7, [8])
    ]

digitToLettersMap :: Map Int (Set Char)
digitToLettersMap =
  Map.fromList
    [ (0, Set.fromList "abcefg"),
      (1, Set.fromList "cf"),
      (2, Set.fromList "acdeg"),
      (3, Set.fromList "acdfg"),
      (4, Set.fromList "bcdf"),
      (5, Set.fromList "abdfg"),
      (6, Set.fromList "abdefg"),
      (7, Set.fromList "acf"),
      (8, Set.fromList "abcdefg"),
      (9, Set.fromList "abcdfg")
    ]

lettersToDigitMap :: Map (Set Char) Int
lettersToDigitMap = Map.fromList $ swap <$> Map.toList digitToLettersMap

possibleLetters :: String -> Map Char (Set Char)
possibleLetters s = Map.fromList $ (,possibleLetters) <$> s
  where
    possibleDigits = fromMaybe [] $ Map.lookup (length s) countToDigitMap
    possibleLetters = foldl' Set.union Set.empty $ fromMaybe Set.empty . (`Map.lookup` digitToLettersMap) <$> possibleDigits

possibleLetters' :: [String] -> Map Char (Set Char)
possibleLetters' s = a
  where
    merge :: Map Char (Set Char) -> Map Char (Set Char) -> Map Char (Set Char)
    merge a b =
      foldl' (\m (k, v) -> Map.insertWith Set.intersection k v m) a $ Map.toList b

    a = foldl' merge Map.empty $ possibleLetters <$> s

tryAssign :: Char -> Char -> Map Char (Set Char) -> Map Char (Set Char)
tryAssign c to m = result
  where
    (assigned, rest) = Map.partitionWithKey (\k a -> k == c) m
    rest' = Set.delete to <$> rest
    result = Map.insert c (Set.fromList [to]) rest'

tryAssigns :: Map Char (Set Char) -> ([Map Char (Set Char)], Bool)
tryAssigns m
  | not $ validate m = ([], False)
  | done m = ([m], True)
  | otherwise = (result, True)
  where
    validate :: Map Char (Set Char) -> Bool
    validate m = Map.null empties && isUnique
      where
        empties = Map.filter Set.null m
        ones = snd <$> Map.toList (Map.filter (\a -> Set.size a == 1) m)
        isUnique = length ones == Set.size (Set.fromList ones)

    notDone = Map.filter (\s -> Set.size s > 1)
    done x = Map.null $ notDone x

    (from, tos) = head $ Map.toList (notDone m)

    ms = (\to -> tryAssign from to m) <$> Set.toList tos

    goodMs = filter validate ms

    go :: [Map Char (Set Char)] -> [Map Char (Set Char)]
    go [] = []
    go ms' = r'
      where
        r = fst =<< filter snd (tryAssigns <$> ms')
        finished = filter done r
        notFinished = filter (not . done) r
        r' = if not (null finished) then finished else go notFinished

    result = go goodMs

type Parser = Parsec Void String

pSegment :: Parser String
pSegment = some alphaNumChar

pSegments :: Parser [String]
pSegments = pSegment `sepEndBy` char ' '

pEntry :: Parser Entry
pEntry = do
  a <- pSegments
  skipMany $ char ' '
  char '|'
  skipMany $ char ' '
  b <- pSegments
  return (a, b)

pInput :: Parser [Entry]
pInput = pEntry `sepEndBy` newline

parseInput :: String -> Either String [Entry]
parseInput input = case parse pInput "input" input of
  Left e -> Left $ show e
  Right i -> Right i

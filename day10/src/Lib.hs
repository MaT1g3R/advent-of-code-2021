module Lib (part1, part2, parseInput) where

import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable (foldl'))
import Data.List (sort)
import Data.Maybe (catMaybes, isNothing, mapMaybe)

match :: Char -> Maybe Char
match '(' = Just ')'
match '[' = Just ']'
match '{' = Just '}'
match '<' = Just '>'
match _ = Nothing

isOpen :: Char -> Bool
isOpen c = c `elem` "([{<"

isClose :: Char -> Bool
isClose = not . isOpen

points :: Char -> Integer
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = 0

parseLine :: String -> ([Char], Maybe Char)
parseLine = foldl' f ([], Nothing)
  where
    f (xs, Just c') c = (xs, Just c')
    f ([], _) c
      | isOpen c = ([c], Nothing)
      | otherwise = ([], Just c)
    f (x : xs, _) c
      | isOpen c = (c : x : xs, Nothing)
      | otherwise = case match x of
        Nothing -> (x : xs, Just x)
        Just c' -> if c == c' then (xs, Nothing) else (x : xs, Just c)

parseLines :: [String] -> [([Char], Maybe Char)]
parseLines lines = parseLine <$> lines

part1 :: [String] -> Integer
part1 lines = sum $ points <$> catMaybes (snd <$> parseLines lines)

part2 :: [String] -> Integer
part2 lines = (middle . sort) allScores
  where
    incompleteLines = fst <$> filter (\(xs, c) -> isNothing c && not (null xs)) (parseLines lines)

    point '(' = 1
    point '[' = 2
    point '{' = 3
    point '<' = 4
    point _ = 0

    score :: String -> Integer
    score = foldl' (\i c -> i * 5 + point c) 0

    allScores = score <$> incompleteLines

    middle xs = xs !! (length xs `div` 2)

parseInput :: String -> [String]
parseInput = filter (not . null) . lines

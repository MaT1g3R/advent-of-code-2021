{-# LANGUAGE RankNTypes #-}

module Lib (part1, part2, parseInput) where

import Data.Char (isUpper)
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

data Cave = Small String | Big String | Start | End deriving (Eq, Show, Ord)

type Connection = (Cave, Cave)

type CaveMap = Map Cave (Set Cave)

data Path a = Incomplete [Cave] a | Complete [Cave] deriving (Show)

instance forall a. Eq (Path a) where
  (==) (Complete _) (Incomplete _ _) = False
  (==) (Incomplete _ _) (Complete _) = False
  (==) (Complete xs) (Complete xs') = xs == xs'
  (==) (Incomplete xs _) (Incomplete xs' _) = xs == xs'

instance forall a. Ord (Path a) where
  compare (Complete _) (Incomplete _ _) = LT
  compare (Incomplete _ _) (Complete _) = GT
  compare (Incomplete xs _) (Incomplete xs' _) = compare xs xs'
  compare (Complete xs) (Complete xs') = compare xs xs'

class Visited a where
  empty :: a
  add :: Cave -> a -> Maybe a

newtype AtMostOnce = AtMostOnce (Set Cave)

instance Visited AtMostOnce where
  empty = AtMostOnce S.empty
  add (Big _) s = Just s
  add c (AtMostOnce s) =
    if S.member c s
      then Nothing
      else Just . AtMostOnce $ S.insert c s

newtype AtMostTwice = AtMostTwice (Map Cave Int)

instance Visited AtMostTwice where
  empty = AtMostTwice M.empty
  add (Big _) a = Just a
  add c (AtMostTwice m) = if (M.size gt1 > 1) || (M.size gt2 > 0) then Nothing else Just (AtMostTwice m')
    where
      m' = M.insertWith (+) c 1 m
      gt1 = M.filter (> 1) m'
      gt2 = M.filter (> 2) gt1

parseInput :: String -> CaveMap
parseInput s =
  let lns = filter (not . null) (lines s)
      parseLine l = (parseCave start, parseCave end)
        where
          start = takeWhile (/= '-') l
          end = tail $ dropWhile (/= '-') l
          parseCave "end" = End
          parseCave "start" = Start
          parseCave s
            | isUpper $ head s = Big s
            | otherwise = Small s
   in buildMap $ parseLine <$> lns

buildMap :: [Connection] -> CaveMap
buildMap =
  let acc m (a, b) = M.insertWith S.union b (S.singleton a) m'
        where
          m' = M.insertWith S.union a (S.singleton b) m
   in foldl' acc M.empty

addToPath :: Visited a => Path a -> Cave -> Maybe (Path a)
addToPath (Complete p) _ = Nothing
addToPath _ End = Nothing
addToPath (Incomplete xs _) Start = Just $ Complete (Start : xs)
addToPath (Incomplete xs seen) c = Incomplete (c : xs) <$> add c seen

completed :: Path a -> Bool
completed (Complete _) = True
completed _ = False

next :: CaveMap -> Path a -> Set Cave
next _ (Complete _) = S.empty
next m (Incomplete xs _) = fromMaybe S.empty $ M.lookup (head xs) m

expandPath :: Visited a => CaveMap -> Path a -> Set (Path a)
expandPath _ (Complete p) = S.singleton (Complete p)
expandPath m p = S.fromList $ mapMaybe (addToPath p) n
  where
    n = S.toList $ next m p

findPaths :: Visited a => CaveMap -> Set (Path a)
findPaths m = go m fromEndPaths
  where
    basePath = Incomplete [End] empty

    fromEnd = next m basePath
    fromEndPaths = S.fromList $ mapMaybe (addToPath basePath) $ S.toList fromEnd
    go m xs
      | all completed xs = xs
      | otherwise = go m xs'
      where
        xs' = foldl' (\ps p -> S.union ps $ expandPath m p) S.empty xs

part1 :: CaveMap -> Integer
part1 = toInteger . S.size . (findPaths :: CaveMap -> Set (Path AtMostOnce))

part2 :: CaveMap -> Integer
part2 = toInteger . S.size . (findPaths :: CaveMap -> Set (Path AtMostTwice))

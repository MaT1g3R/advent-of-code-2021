{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( gamma,
    epsilonFromGamma,
    Bin (..),
    parseInput,
    binToDec,
  )
where

import Control.Applicative (Alternative (empty))
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (catMaybes, mapMaybe)

data Bin = One | Zero deriving (Eq, Show)

type BinNum = [Bin]

binToDec :: BinNum -> Int
binToDec = fst . go
  where
    digitToDec One pow = 2 ^ pow
    digitToDec Zero _ = 0
    acc d (sum, pow) = (sum + digitToDec d pow, pow + 1)
    go = foldr acc (0, 0)

gamma :: [BinNum] -> BinNum
gamma b = case go b of
  (Just lst, length) -> fmap (checkMajority length) lst
  (Nothing, _) -> []
  where
    digitToDec One = 1
    digitToDec Zero = 0
    countOnes = fmap digitToDec
    sumOnes x y = uncurry (+) <$> zip x y
    acc (Just sum, count) n = (Just $ sumOnes sum (countOnes n), count + 1)
    acc (Nothing, count) n = (Just $ countOnes n, count + 1)
    go = foldl' acc (Nothing, 0)
    checkMajority length count =
      if count > length / 2 then One else Zero

epsilonFromGamma :: BinNum -> BinNum
epsilonFromGamma = fmap inverse
  where
    inverse Zero = One
    inverse One = Zero

parseInput :: String -> [BinNum]
parseInput = filter (/= []) . fmap parseLine . lines
  where
    parseLine = mapMaybe charToDigit
      where
        charToDigit '0' = Just Zero
        charToDigit '1' = Just One
        charToDigit _ = Nothing

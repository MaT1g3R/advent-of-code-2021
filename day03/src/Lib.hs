{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( gamma,
    epsilonFromGamma,
    Bin (..),
    parseInput,
    binToDec,
    oxygen,
    co2,
    lifeSupport,
  )
where

import Control.Applicative (Alternative (empty))
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (catMaybes, mapMaybe)

data Bin = One | Zero deriving (Eq, Show)

type BinNum = [Bin]

binToDec :: BinNum -> Integer
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

bitFilter compare xs = go xs 0
  where
    go [] _ = []
    go [x] _ = x
    go xs idx = go partition (idx + 1)
      where
        acc :: BinNum -> ([BinNum], [BinNum], Integer, Integer) -> ([BinNum], [BinNum], Integer, Integer)
        acc num (ones, zeros, oneCount, zeroCount) =
          case num !! idx of
            Zero -> (ones, num : zeros, oneCount, zeroCount + 1)
            One -> (num : ones, zeros, oneCount + 1, zeroCount)
        partition = compare $ foldr acc ([], [], 0, 0) xs

oxygen :: [BinNum] -> BinNum
oxygen = bitFilter compare
  where
    compare (ones, zeros, oneCount, zeroCount) = if oneCount >= zeroCount then ones else zeros

co2 :: [BinNum] -> BinNum
co2 = bitFilter compare
  where
    compare (ones, zeros, oneCount, zeroCount) = if zeroCount <= oneCount then zeros else ones

lifeSupport :: [BinNum] -> Integer
lifeSupport xs = (binToDec . oxygen) xs * (binToDec . co2) xs

parseInput :: String -> [BinNum]
parseInput = filter (/= []) . fmap parseLine . lines
  where
    parseLine = mapMaybe charToDigit
      where
        charToDigit '0' = Just Zero
        charToDigit '1' = Just One
        charToDigit _ = Nothing

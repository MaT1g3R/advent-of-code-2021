module Lib
  ( DepthChange (..),
    depthChanges,
    countDepthIncrease,
    measurementWindows,
  )
where

data DepthChange = Increase | Decrease | NoChange | NoMeasurement deriving (Eq, Show)

depthChanges :: [Int] -> [DepthChange]
depthChanges [] = []
depthChanges [x] = [NoMeasurement]
depthChanges (x : xs) =
  NoMeasurement :
  fmap
    compare
    (zip xs (x : xs))
  where
    compare (x, y)
      | x > y = Increase
      | x < y = Decrease
    compare _ = NoChange

countDepthIncrease :: [DepthChange] -> Int
countDepthIncrease xs = length (filter (== Increase) xs)

measurementWindows :: [Int] -> [Int]
measurementWindows (x : y : z : xs) = x + y + z : measurementWindows (y : z : xs)
measurementWindows _ = []

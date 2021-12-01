module Lib
  ( DepthChange (..),
    depthChanges,
    countDepthIncrease,
  )
where

data DepthChange = Increase | Decrease | NoMeasurement deriving (Eq, Show)

depthChanges :: [Int] -> [DepthChange]
depthChanges [] = []
depthChanges [x] = [NoMeasurement]
depthChanges xs =
  NoMeasurement :
  fmap
    (\(cur, prev) -> if cur > prev then Increase else Decrease)
    (zip (tail xs) xs)

countDepthIncrease :: [DepthChange] -> Int
countDepthIncrease xs = length (filter (== Increase) xs)

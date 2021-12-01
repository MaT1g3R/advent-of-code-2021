{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
  ( countDepthIncrease,
    depthChanges,
  )

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  let nums :: [Int] = fmap read lns
  print $ Lib.countDepthIncrease . Lib.depthChanges $ nums

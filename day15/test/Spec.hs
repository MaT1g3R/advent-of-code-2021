module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( parseInput,
    part1,
    part2,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    (@=?),
  )

exampleInput =
  parseInput
    "1163751742\n\
    \1381373672\n\
    \2136511328\n\
    \3694931569\n\
    \7463417111\n\
    \1319128137\n\
    \1359912421\n\
    \3125421639\n\
    \1293138521\n\
    \2311944581\n"

testPart1 = 40 @=? part1 exampleInput

testPart2 = 315 @=? part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "part1" testPart1,
      testCase "part2" testPart2
    ]
    mempty

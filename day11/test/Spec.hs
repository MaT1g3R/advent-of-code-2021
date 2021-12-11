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
    assertEqual,
  )

exampleInput =
  parseInput
    "5483143223\n\
    \2745854711\n\
    \5264556173\n\
    \6141336146\n\
    \6357385478\n\
    \4167524645\n\
    \2176841721\n\
    \6882881134\n\
    \4846848554\n\
    \5283751526\n"

testPart1 = assertEqual "part1 works with example input" 1656 $ part1 exampleInput

testPart2 = assertEqual "part2 works with example input" 195 $ part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPart1" testPart1,
      testCase "testPart2" testPart2
    ]
    mempty

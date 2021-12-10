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
  "2199943210\n\
  \3987894921\n\
  \9856789892\n\
  \8767896789\n\
  \9899965678\n"

testPart1 =
  assertEqual "part1 works with example input" 15 $
    (part1 . parseInput) exampleInput

testPart2 =
  assertEqual "part2 works with example input" 1134 $
    (part2 . parseInput) exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPart1" testPart1,
      testCase "testPart2" testPart2
    ]
    mempty

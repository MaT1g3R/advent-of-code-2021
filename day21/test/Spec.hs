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
    "Player 1 starting position: 4\n\
    \Player 2 starting position: 8\n"

testPart1 = 739785 @=? part1 exampleInput

testPart2 = 444356092776315 @=? part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "part1" testPart1,
      testCase "part2" testPart2
    ]
    mempty

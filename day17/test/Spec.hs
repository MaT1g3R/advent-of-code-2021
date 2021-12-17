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

exampleInput = parseInput "target area: x=20..30, y=-10..-5"

testPart1 = 45 @=? part1 exampleInput

testPart2 = 112 @=? part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "part1" testPart1,
      testCase "part2" testPart2
    ]
    mempty

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

exampleInput = parseInput <$> readFile "input-small"

testPart1 input = 35 @=? part1 input

testPart2 input = 3351 @=? part2 input

main :: IO ()
main = do
  input <- exampleInput
  defaultMainWithOpts
    [ testCase "part1" $ testPart1 input,
      testCase "part2" $ testPart2 input
    ]
    mempty

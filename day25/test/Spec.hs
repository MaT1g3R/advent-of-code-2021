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

testPart1 = do
  i <- exampleInput
  58 @=? part1 i

testPart2 = do
  i <- exampleInput
  0 @=? part2 i

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "part1" testPart1,
      testCase "part2" testPart2
    ]
    mempty

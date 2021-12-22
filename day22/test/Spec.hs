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

exampleInput2 = parseInput <$> readFile "input-big"

testPart1 i = 590784 @=? part1 i

testPart2 i = 2758514936282235 @=? part2 i

main :: IO ()
main =
  do
    i <- exampleInput
    i2 <- exampleInput2
    defaultMainWithOpts
      [ testCase "part1" $ testPart1 i,
        testCase "part2" $ testPart2 i2
      ]
      mempty

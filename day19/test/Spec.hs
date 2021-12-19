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
    solve,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    (@=?),
  )

testPart1 input = 79 @=? part1 input

testPart2 input = 3621 @=? part2 input

main :: IO ()
main = do
  content <- readFile "input-small"
  let input = solve $ parseInput content
  defaultMainWithOpts
    [ testCase "part1" $ testPart1 input,
      testCase "part2" $ testPart2 input
    ]
    mempty

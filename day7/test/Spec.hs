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

exampleInput = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

exampleString = "16,1,2,0,4,2,7,1,2,14"

testParseInput :: Assertion
testParseInput = assertEqual "parse input works" exampleInput $ parseInput exampleString

testPart1 :: Assertion
testPart1 = assertEqual "part1 works with example input" 37 $ part1 exampleInput

testPart2 :: Assertion
testPart2 = assertEqual "part1 works with example input" 168 $ part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPart1" testPart1,
      testCase "testPart2" testPart2,
      testCase "testParseInput" testParseInput
    ]
    mempty

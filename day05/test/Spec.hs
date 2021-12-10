module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( Line,
    parseInput,
    part1,
    part2,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

exampleInput :: [Line]
exampleInput =
  [ ((0, 9), (5, 9)),
    ((8, 0), (0, 8)),
    ((9, 4), (3, 4)),
    ((2, 2), (2, 1)),
    ((7, 0), (7, 4)),
    ((6, 4), (2, 0)),
    ((0, 9), (2, 9)),
    ((3, 4), (1, 4)),
    ((0, 0), (8, 8)),
    ((5, 5), (8, 2))
  ]

exampleString =
  "0,9 -> 5,9\n\
  \8,0 -> 0,8\n\
  \9,4 -> 3,4\n\
  \2,2 -> 2,1\n\
  \7,0 -> 7,4\n\
  \6,4 -> 2,0\n\
  \0,9 -> 2,9\n\
  \3,4 -> 1,4\n\
  \0,0 -> 8,8\n\
  \5,5 -> 8,2\n\
  \"

testPart1 :: Assertion
testPart1 = assertEqual "part1 works with example input" 5 $ part1 exampleInput

testParseInput :: Assertion
testParseInput = assertEqual "parse input works with example input" (Right exampleInput) $ parseInput exampleString

testPart2 :: Assertion
testPart2 = assertEqual "part2 works with example input" 12 $ part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPart1" testPart1,
      testCase "testParseInput" testParseInput,
      testCase "testPart2" testPart2
    ]
    mempty

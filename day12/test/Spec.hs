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

input1 =
  parseInput
    "start-A\n\
    \start-b\n\
    \A-c\n\
    \A-b\n\
    \b-d\n\
    \A-end\n\
    \b-end\n"

input2 =
  parseInput
    "dc-end\n\
    \HN-start\n\
    \start-kj\n\
    \dc-start\n\
    \dc-HN\n\
    \LN-dc\n\
    \HN-end\n\
    \kj-sa\n\
    \kj-HN\n\
    \kj-dc\n"

input3 =
  parseInput
    "fs-end\n\
    \he-DX\n\
    \fs-he\n\
    \start-DX\n\
    \pj-DX\n\
    \end-zg\n\
    \zg-sl\n\
    \zg-pj\n\
    \pj-he\n\
    \RW-he\n\
    \fs-DX\n\
    \pj-RW\n\
    \zg-RW\n\
    \start-pj\n\
    \he-WI\n\
    \zg-he\n\
    \pj-fs\n\
    \start-RW\n"

testPart1 :: Assertion
testPart1 = do
  assertEqual "input1" 10 $ part1 input1
  assertEqual "input2" 19 $ part1 input2
  assertEqual "input3" 226 $ part1 input3

testPart2 :: Assertion
testPart2 = do
  assertEqual "input1" 36 $ part2 input1
  assertEqual "input2" 103 $ part2 input2
  assertEqual "input3" 3509 $ part2 input3

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPart1" testPart1,
      testCase "testPart2" testPart2
    ]
    mempty

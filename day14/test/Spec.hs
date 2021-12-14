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
    "NNCB\n\
    \\n\
    \CH -> B\n\
    \HH -> N\n\
    \CB -> H\n\
    \NH -> C\n\
    \HB -> C\n\
    \HC -> B\n\
    \HN -> C\n\
    \NN -> C\n\
    \BH -> H\n\
    \NC -> B\n\
    \NB -> B\n\
    \BN -> B\n\
    \BB -> N\n\
    \BC -> B\n\
    \CC -> N\n\
    \CN -> C\n"

testPart1 = 1588 @=? part1 exampleInput

testPart2 = 2188189693529 @=? part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "part1" testPart1,
      testCase "part2" testPart2
    ]
    mempty

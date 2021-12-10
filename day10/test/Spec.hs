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

exampleInput =
  "[({(<(())[]>[[{[]{<()<>>\n\
  \[(()[<>])]({[<{<<[]>>(\n\
  \{([(<{}[<>[]}>{[]{[(<()>\n\
  \(((({<>}<{<{<>}{[]{[]{}\n\
  \[[<[([]))<([[{}[[()]]]\n\
  \[{[{({}]{}}([{[{{{}}([]\n\
  \{<[[]]>}<{[{[{[]{()[[[]\n\
  \[<(<(<(<{}))><([]([]()\n\
  \<{([([[(<>()){}]>(<<{{\n\
  \<{([{{}}[<[[[<>{}]]]>[]]\n"

testPart1 = assertEqual "part1 works with example input" 26397 $ (part1 . parseInput) exampleInput

testPart2 = assertEqual "part2 works with example input" 288957 $ (part2 . parseInput) exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPart1" testPart1,
      testCase "testPart2" testPart2
    ]
    mempty

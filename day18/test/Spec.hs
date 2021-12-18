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
    "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n\
    \[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n\
    \[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n\
    \[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n\
    \[7,[5,[[3,8],[1,4]]]]\n\
    \[[2,[2,2]],[8,[8,1]]]\n\
    \[2,9]\n\
    \[1,[[[9,3],9],[[9,0],[0,7]]]]\n\
    \[[[5,[7,4]],7],1]\n\
    \[[[[4,2],2],6],[8,7]]\n"

exampleInput2 =
  parseInput
    "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
    \[[[5,[2,8]],4],[5,[[9,9],0]]]\n\
    \[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
    \[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
    \[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
    \[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
    \[[[[5,4],[7,7]],8],[[8,3],8]]\n\
    \[[9,3],[[9,9],[6,[4,9]]]]\n\
    \[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
    \[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]\n"

testPart1 = do
  3488 @=? part1 exampleInput
  4140 @=? part1 exampleInput2

testPart2 = 3993 @=? part2 exampleInput2

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "part1" testPart1,
      testCase "part2" testPart2
    ]
    mempty

module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( Board,
    parseInput,
    playGame,
    winLast,
    winningIndicies,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

exampleInput = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

exampleBoards :: [Board]
exampleBoards =
  [ [ 22,
      13,
      17,
      11,
      0,
      8,
      2,
      23,
      4,
      24,
      21,
      9,
      14,
      16,
      7,
      6,
      10,
      3,
      18,
      5,
      1,
      12,
      20,
      15,
      19
    ],
    [ 3,
      15,
      0,
      2,
      22,
      9,
      18,
      13,
      17,
      5,
      19,
      8,
      7,
      25,
      23,
      20,
      11,
      10,
      24,
      4,
      14,
      21,
      16,
      12,
      6
    ],
    [ 14,
      21,
      17,
      24,
      4,
      10,
      16,
      15,
      9,
      19,
      18,
      8,
      23,
      26,
      20,
      22,
      11,
      13,
      6,
      5,
      2,
      0,
      12,
      3,
      7
    ]
  ]

exampleString =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \ 8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \ 6 10  3 18  5\n\
  \ 1 12 20 15 19\n\
  \\n\
  \ 3 15  0  2 22\n\
  \ 9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \ 2  0 12  3  7\n\
  \\n\n"

testPlayGame :: Assertion
testPlayGame = assertEqual "play game works" 4512 $ playGame (winningIndicies 5) exampleBoards exampleInput

testParseInput :: Assertion
testParseInput = assertEqual "parse input works" (Right (exampleInput, exampleBoards)) (parseInput exampleString)

testWinLast :: Assertion
testWinLast = assertEqual "win last works" 1924 $ winLast (winningIndicies 5) exampleBoards exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "play game works with example input" testPlayGame,
      testCase "parse input works with example input" testParseInput,
      testCase "win last works with example input" testWinLast
    ]
    mempty

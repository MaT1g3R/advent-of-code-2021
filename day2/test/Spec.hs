module Main
  ( main,
  )
where

import Control.Monad ()
import Data.List (intercalate)
import Data.Monoid ()
import Lib
  ( Coordinate (..),
    Direction (..),
    Move (..),
    cordProduct,
    parseInput,
    position,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

sampleInput =
  [ Move Forward 5,
    Move Down 5,
    Move Forward 8,
    Move Up 3,
    Move Down 8,
    Move Forward 2
  ]

sampleLines =
  [ "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2",
    ""
  ]

sampleString = intercalate "\n" sampleLines

testPosition :: Assertion
testPosition = assertEqual "position works with sample input" (Coordinate 15 10) $ position sampleInput

testProduct = assertEqual "product works" 150 $ cordProduct (Coordinate 15 10)

testParseInput = assertEqual "prase input works" (Right sampleInput) $ parseInput sampleString

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPosition" testPosition,
      testCase "testProduct" testProduct,
      testCase "testParseInput" testParseInput
    ]
    mempty

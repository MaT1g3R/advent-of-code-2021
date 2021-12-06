module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( part1,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

exampleInput = [3, 4, 3, 1, 2]

testPart1 :: Assertion
testPart1 = assertEqual "part1 works with example input" 5934 $ part1 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testPart1" testPart1
    ]
    mempty

module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( Bin (..),
    binToDec,
    epsilonFromGamma,
    gamma,
    parseInput,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

exampleString =
  "00100\n\
  \11110\n\
  \10110\n\
  \10111\n\
  \10101\n\
  \01111\n\
  \00111\n\
  \11100\n\
  \10000\n\
  \11001\n\
  \00010\n\
  \01010"

exampleInput = parseInput exampleString

testGamma :: Assertion
testGamma = assertEqual "gamma works with example input" 22 $ binToDec (gamma exampleInput)

testEpsilon :: Assertion
testEpsilon = assertEqual "epsilon works with example input" 9 $ (binToDec . epsilonFromGamma . gamma) exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "gamma works" testGamma,
      testCase "epsilon works" testEpsilon
    ]
    mempty

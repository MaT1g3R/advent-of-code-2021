module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( Bin (..),
    binToDec,
    co2,
    epsilonFromGamma,
    gamma,
    lifeSupport,
    oxygen,
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

testOxygen :: Assertion
testOxygen = assertEqual "oxygen works with example input" 23 $ (binToDec . oxygen) exampleInput

testCo2 :: Assertion
testCo2 = assertEqual "co2 works with example input" 10 $ (binToDec . co2) exampleInput

testLifeSupport :: Assertion
testLifeSupport = assertEqual "lifeSupport works with example input" 230 $ lifeSupport exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "gamma works" testGamma,
      testCase "epsilon works" testEpsilon,
      testCase "oxygen works" testOxygen,
      testCase "co2 works" testCo2,
      testCase "life support works" testLifeSupport
    ]
    mempty

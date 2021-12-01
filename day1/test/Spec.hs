module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( DepthChange (..),
    countDepthIncrease,
    depthChanges,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

exampleInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

depthChangesTest :: Assertion
depthChangesTest =
  assertEqual
    "depth change should work correctly"
    [ NoMeasurement,
      Increase,
      Increase,
      Increase,
      Decrease,
      Increase,
      Increase,
      Increase,
      Decrease,
      Increase
    ]
    $ depthChanges exampleInput

countDepthIncreaseTest :: Assertion
countDepthIncreaseTest =
  assertEqual "count depth change should work correctly" 7 $
    countDepthIncrease
      [ NoMeasurement,
        Increase,
        Increase,
        Increase,
        Decrease,
        Increase,
        Increase,
        Increase,
        Decrease,
        Increase
      ]

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "depthChangesTest" depthChangesTest,
      testCase "countDepthIncreaseTest" countDepthIncreaseTest
    ]
    mempty

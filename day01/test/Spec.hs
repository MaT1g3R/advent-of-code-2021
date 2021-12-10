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
    measurementWindows,
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

measurementWindowsTest :: Assertion
measurementWindowsTest =
  assertEqual
    "Measurement windows should work correctly"
    [607, 618, 618, 617, 647, 716, 769, 792]
    $ measurementWindows exampleInput

depthWindowChangesTest :: Assertion
depthWindowChangesTest =
  assertEqual
    "Depth changes should work with sliding windows"
    [NoMeasurement, Increase, NoChange, Decrease, Increase, Increase, Increase, Increase]
    $ depthChanges . measurementWindows $ exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "depthChangesTest" depthChangesTest,
      testCase
        "countDepthIncreaseTest"
        countDepthIncreaseTest,
      testCase
        "measurementWindowsTest"
        measurementWindowsTest,
      testCase
        "depthWindowChangesTest"
        depthWindowChangesTest
    ]
    mempty

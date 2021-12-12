module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  (
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

itWorks :: Assertion
itWorks = assertEqual "it works" 42 42

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "itWorks" itWorks
    ]
    mempty

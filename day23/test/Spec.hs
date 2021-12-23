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
    (@=?),
  )

main :: IO ()
main =
  defaultMainWithOpts
    []
    mempty

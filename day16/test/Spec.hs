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

input1 = parseInput "8A004A801A8002F478"

input2 = parseInput "620080001611562C8802118E34"

input3 = parseInput "C0015000016115A2E0802F182340"

input4 = parseInput "A0016C880162017C3686B18A3D4780"

testPart1 = do
  16 @=? part1 input1
  12 @=? part1 input2
  23 @=? part1 input3
  31 @=? part1 input4

testPart2 = do
  3 @=? part2 (parseInput "C200B40A82")
  54 @=? part2 (parseInput "04005AC33890")
  7 @=? part2 (parseInput "880086C3E88112")
  9 @=? part2 (parseInput "CE00C43D881120")
  1 @=? part2 (parseInput "D8005AC2A8F0")
  0 @=? part2 (parseInput "F600BC2D8F")
  0 @=? part2 (parseInput "9C005AC2F8F0")
  1 @=? part2 (parseInput "9C0141080250320F1802104A08")

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "part1" testPart1,
      testCase "part2" testPart2
    ]
    mempty

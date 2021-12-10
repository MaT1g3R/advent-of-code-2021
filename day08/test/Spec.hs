module Main
  ( main,
  )
where

import Control.Monad ()
import Data.Monoid ()
import Lib
  ( Entry,
    parseInput,
    part2,
    part1,
  )
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( Assertion,
    assertEqual,
  )

exampleInput :: [Entry]
exampleInput =
  [ ( ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"],
      ["fdgacbe", "cefdb", "cefbgd", "gcbe"]
    ),
    ( ["edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"],
      ["fcgedb", "cgb", "dgebacf", "gc"]
    ),
    ( ["fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"],
      ["cg", "cg", "fdcagb", "cbg"]
    ),
    ( ["fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"],
      ["efabcd", "cedba", "gadfec", "cb"]
    ),
    ( ["aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"],
      ["gecf", "egdcabf", "bgf", "bfgea"]
    ),
    ( ["fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"],
      ["gebdcfa", "ecba", "ca", "fadegcb"]
    ),
    ( ["dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"],
      ["cefg", "dcbef", "fcge", "gbcadfe"]
    ),
    ( ["bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"],
      ["ed", "bcgafe", "cdgba", "cbgef"]
    ),
    ( ["egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"],
      ["gbdfcae", "bgc", "cg", "cgb"]
    ),
    ( ["gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"],
      ["fgae", "cfgab", "fg", "bagce"]
    )
  ]

exampleString =
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
  \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
  \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
  \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
  \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
  \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
  \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
  \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
  \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
  \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n"

testParseInput = assertEqual "parse input works" (Right exampleInput) (parseInput exampleString)

testPart1 :: Assertion
testPart1 = assertEqual "part1 works with example input" 26 $ part1 exampleInput

testPart2 = assertEqual "part2 works with example input" 61229 $ part2 exampleInput

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "testParseInput" testParseInput,
      testCase "testPart1" testPart1,
      testCase "testPart2" testPart2
    ]
    mempty

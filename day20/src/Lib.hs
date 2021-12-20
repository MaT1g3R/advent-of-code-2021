module Lib (Input, part1, part2, parseInput) where

import Control.Applicative ((<|>))
import Data.Foldable (Foldable (foldl'))
import Data.List (iterate')
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import qualified Data.Matrix as Matrix
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    parse,
    sepEndBy,
    some,
  )
import Text.Megaparsec.Char (char, newline, space)

type Input = (Matrix Int, Vector Int, Int)

image (i, _, _) = i

bin2dec :: [Int] -> Int
bin2dec xs = foldl' (\s (d, pow) -> s + if d == 0 then 0 else 2 ^ pow) 0 $ reverse xs `zip` [0 ..]

computePixel :: (Int, Int) -> Input -> Int
computePixel (x, y) (image, alg, defaultPixel) = alg ! bin2dec grid
  where
    grid =
      [ fromMaybe defaultPixel $ M.safeGet (i + x) (j + y) image
        | i <- [-1 .. 1],
          j <- [-1 .. 1]
      ]

enhance :: Input -> Input
enhance (image, alg, defaultPixel) = (image', alg, computePixel (-2, -2) (image, alg, defaultPixel))
  where
    image' =
      Matrix.fromLists
        [ (\j -> computePixel (i, j) (image, alg, defaultPixel)) <$> [0 .. M.ncols image + 1]
          | i <- [0 .. M.nrows image + 1]
        ]

enhanceN :: Int -> Input -> Input
enhanceN n input = iterate enhance input !! n

part1 :: Input -> Int
part1 = sum . Matrix.toList . image . enhanceN 2

part2 :: Input -> Int
part2 = sum . Matrix.toList . image . enhanceN 50

type Parser = Parsec Void String

parseInput :: String -> Input
parseInput s = case parse pInput "input" s of
  Right x -> x
  Left e -> error (show e)
  where
    pPixel :: Parser Int
    pPixel = 0 <$ try (char '.') <|> 1 <$ char '#'

    pLine :: Parser [Int]
    pLine = some pPixel

    pAlg :: Parser (Vector Int)
    pAlg = Vector.fromList <$> pLine

    pImage :: Parser (Matrix Int)
    pImage = do
      lines <- pLine `sepEndBy` newline
      return $ Matrix.fromLists lines

    pInput :: Parser Input
    pInput = do
      alg <- pAlg
      space
      image <- pImage
      return (image, alg, 0)

module Lib (parseInput, part1, part2) where

import Data.Char (isDigit)
import Data.Foldable (Foldable (foldl'))
import Data.Set (Set)
import qualified Data.Set as S

type Cord = (Int, Int)

data Fold = Vertical Int | Horizontal Int deriving (Eq, Show)

type Input = (Set Cord, [Fold])

foldPaper :: Set Cord -> Fold -> Set Cord
foldPaper xs (Vertical y) = S.map f xs
  where
    updateY y' = if y' <= y then y' else y' - 2 * (y' - y)
    f (x', y') = (x', updateY y')
foldPaper xs (Horizontal x) = S.map f xs
  where
    updateX x' = if x' <= x then x' else x' - 2 * (x' - x)
    f (x', y') = (updateX x', y')

runFolds :: Input -> Set Cord
runFolds (cords, folds) = foldl' foldPaper cords folds

showCords :: Set Cord -> String
showCords s = unlines ls
  where
    (x, y) = foldl' (\(x, y) (x', y') -> (max x x', max y y')) (0, 0) s
    showLine y = (\x -> if S.member (x, y) s then '█' else ' ') <$> [0 .. x]
    ls = showLine <$> [0 .. y]

part1 :: Input -> Int
part1 (cords, folds) = S.size $ runFolds (cords, take 1 folds)

part2 :: Input -> String
part2 = showCords . runFolds

parseInput :: String -> Input
parseInput s = foldr acc (S.empty, []) ls
  where
    ls = filter (not . null) $ lines s
    acc line (cords, folds) =
      if ',' `elem` line
        then (S.insert (parseCord line) cords, folds)
        else (cords, parseFold line : folds)

    parseCord s = read $ "(" <> s <> ")"

    parseFold s =
      let i = read $ dropWhile (not . isDigit) s
       in if 'x' `elem` s then Horizontal i else Vertical i

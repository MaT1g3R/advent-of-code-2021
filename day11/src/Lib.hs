module Lib (parseInput, part1, part2) where

import Data.Foldable (Foldable (foldl'))
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Maybe (isJust, mapMaybe)

type Index = (Int, Int)

data Octopus = Unflashed Integer Index | Flashed Index deriving (Eq, Show)

type Grid = Matrix Octopus

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

index :: Octopus -> Index
index (Unflashed _ i) = i
index (Flashed i) = i

incEnergy :: Octopus -> Octopus
incEnergy (Unflashed x i) = Unflashed (succ x) i
incEnergy f = f

reset :: Octopus -> Octopus
reset (Flashed i) = Unflashed 0 i
reset u = u

neighbours :: Grid -> Index -> [Index]
neighbours g (n, m) = idxes
  where
    valid (i, j) = (i, j) /= (n, m) && isJust (M.safeGet i j g)
    idxes = filter valid $ cartProd [n -1 .. n + 1] [m -1 .. m + 1]

unflashed :: Octopus -> Bool
unflashed (Unflashed _ _) = True
unflashed _ = False

energy :: Octopus -> Integer
energy (Unflashed i _) = i
energy _ = 0

flash :: Grid -> Index -> (Grid, Integer)
flash g (i, j) = r
  where
    o = M.getElem i j g

    r = case o of
      (Flashed _) -> (g, 0)
      (Unflashed x (n, m)) -> go x (n, m)

    go f (n, m)
      | f < 10 = (g, 0)
      | otherwise = foldl' acc (g'', 1) neighbours'
      where
        neighbours' = neighbours g (n, m)

        g' = M.setElem (Flashed (n, m)) (n, m) g
        g'' = foldl' (\m (i, j) -> M.setElem (incEnergy (M.getElem i j m)) (i, j) m) g' neighbours'

        acc (m, fs) o = let (m', fs') = flash m o in (m', fs + fs')

unflash :: Grid -> Grid
unflash = M.mapPos (\_ o -> reset o)

step :: Grid -> (Grid, Integer)
step g = (unflash next, count)
  where
    g' = M.mapPos (\(_, _) o -> incEnergy o) g
    acc (m, fs) o = let (m', fs') = flash m (index o) in (m', fs + fs')
    (next, count) = foldl' acc (g', 0) g'

steps :: Grid -> Int -> (Grid, Integer)
steps g x = foldl' acc (g, 0) [1 .. x]
  where
    acc (m, fs) _ = let (m', fs') = step m in (m', fs + fs')

part1 :: Grid -> Integer
part1 g = snd $ steps g 100

part2 :: Grid -> Integer
part2 g = go g 1
  where
    go grid s
      | count == toInteger (M.nrows g * M.ncols g) = s
      | otherwise = go next $ succ s
      where
        (next, count) = step grid

parseInput :: String -> Grid
parseInput s = M.fromLists cols
  where
    lns = filter (not . null) (lines s) `zip` [1 ..]
    cols = (\(s, n) -> (\(c, m) -> Unflashed (read [c]) (n, m)) <$> s `zip` [1 ..]) <$> lns

module Lib (Input, part1, part2, parseInput) where

import Control.Applicative ((<|>))
import Data.List (foldl1')
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    parseMaybe,
    some,
  )
import Text.Megaparsec.Char (char, digitChar)

type Input = [Tree]

data Tree = Pair Tree Tree | Leaf Integer deriving (Eq)

instance Show Tree where
  show (Leaf i) = show i
  show (Pair a b) = "[" <> show a <> "," <> show b <> "]"

(<+>) :: Tree -> Tree -> Tree
t1 <+> t2 = reduce $ Pair t1 t2

sumTrees :: [Tree] -> Tree
sumTrees = foldl1' (<+>)

magnitude :: Tree -> Integer
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Leaf l) = l

reduce :: Tree -> Tree
reduce t = maybe t reduce (explode t <|> split t)

explode :: Tree -> Maybe Tree
explode t = (\(t, _, _) -> t) <$> go 0 t
  where
    go :: Integer -> Tree -> Maybe (Tree, Maybe Integer, Maybe Integer)
    go _ (Leaf i) = Nothing
    go depth (Pair (Leaf l) (Leaf r))
      | depth >= 4 = Just (Leaf 0, Just l, Just r)
    go depth (Pair l r) = explodeLeft <$> go (succ depth) l <|> explodeRight <$> go (succ depth) r
      where
        explodeLeft (newLeft, addL, addR) =
          let (newRight, newAddR) = addLeftMost addR r
           in (Pair newLeft newRight, addL, newAddR)
        explodeRight (newRight, addL, addR) =
          let (newLeft, newAddL) = addRightMost addL l
           in (Pair newLeft newRight, newAddL, addR)

    addLeftMost :: Maybe Integer -> Tree -> (Tree, Maybe Integer)
    addLeftMost Nothing tree = (tree, Nothing)
    addLeftMost (Just i) (Leaf l) = (Leaf (i + l), Nothing)
    addLeftMost (Just i) (Pair l r) = case addLeftMost (Just i) l of
      (t, Nothing) -> (Pair t r, Nothing)
      (t, Just i) ->
        let (t', i') = addLeftMost (Just i) r
         in (Pair l t', i')

    addRightMost :: Maybe Integer -> Tree -> (Tree, Maybe Integer)
    addRightMost Nothing tree = (tree, Nothing)
    addRightMost (Just i) (Leaf r) = (Leaf (i + r), Nothing)
    addRightMost (Just i) (Pair l r) = case addRightMost (Just i) r of
      (t, Nothing) -> (Pair l t, Nothing)
      (t, Just i) ->
        let (t', i') = addRightMost (Just i) l
         in (Pair t' r, i')

split :: Tree -> Maybe Tree
split (Leaf i)
  | i >= 10 = Just $ Pair (Leaf (i `div` 2)) (Leaf (i - i `div` 2))
  | otherwise = Nothing
split (Pair l r) = (`Pair` r) <$> split l <|> Pair l <$> split r

part1 :: Input -> Integer
part1 = magnitude . sumTrees

part2 :: Input -> Integer
part2 xs = maximum $ magnitude <$> sums
  where
    withIdx = xs `zip` [0 ..]
    sums = withIdx >>= (\(x, i) -> mapMaybe (\(y, i') -> if i == i' then Nothing else Just $ x <+> y) withIdx)

type Parser = Parsec Void String

pInteger :: Parser Integer
pInteger = read <$> some digitChar

pLeaf :: Parser Tree
pLeaf = Leaf <$> pInteger

pPair :: Parser Tree
pPair = do
  char '['
  p1 <- pTree
  char ','
  p2 <- pTree
  char ']'
  return $ Pair p1 p2

pTree :: Parser Tree
pTree = try pPair <|> pLeaf

parseInput :: String -> Input
parseInput s = mapMaybe parseTree $ filter (not . null) (lines s)
  where
    parseTree s = parseMaybe pTree s

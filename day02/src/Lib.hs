{-# LANGUAGE BlockArguments #-}

module Lib (position, Direction (..), Move (..), Coordinate (..), cordProduct, parseInput, positionWithAim) where

import Control.Monad.State
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    char,
    digit,
    endBy,
    eof,
    many1,
    newline,
    parse,
    skipMany,
    skipMany1,
    string,
    try,
    (<|>),
  )

data Direction = Forward | Down | Up deriving (Eq, Show)

data Move = Move Direction Integer deriving (Eq, Show)

data Coordinate = Coordinate Integer Integer deriving (Eq, Show)

instance Semigroup Coordinate where
  (Coordinate x y) <> (Coordinate a b) = Coordinate (x + a) (y + b)

instance Monoid Coordinate where
  mempty = Coordinate 0 0

data Position = Position Coordinate Integer

moveWithAim :: Position -> Move -> Position
moveWithAim (Position (Coordinate a b) aim) (Move dir x) =
  case dir of
    Up -> Position (Coordinate a b) (aim - x)
    Down -> Position (Coordinate a b) (aim + x)
    Forward -> Position (Coordinate (a + x) (b + x * aim)) aim

positionWithAim :: [Move] -> Coordinate
positionWithAim xs =
  let (Position c _) = foldl' moveWithAim (Position (Coordinate 0 0) 0) xs
   in c

move :: Move -> Coordinate
move (Move Forward i) = Coordinate i 0
move (Move Down i) = Coordinate 0 i
move (Move Up i) = Coordinate 0 (- i)

position :: [Move] -> Coordinate
position = mconcat . fmap move

cordProduct :: Coordinate -> Integer
cordProduct (Coordinate x y) = x * y

pInteger :: Parser Integer
pInteger = read <$> many1 digit

pDirection :: Parser Direction
pDirection = try (string "forward" >> return Forward) <|> try (string "down" >> return Down) <|> try (string "up" >> return Up)

pMove :: Parser Move
pMove = do
  d <- pDirection
  _ <- skipMany $ char ' '
  Move d <$> pInteger

pInput :: Parser [Move]
pInput = catMaybes <$> lns
  where
    lns = (line <|> emptyLine) `endBy` newline <* eof
    line = Just <$> pMove
    emptyLine = spaces1 >> pure Nothing
    spaces1 = skipMany1 (char '\n')

parseInput :: String -> Either ParseError [Move]
parseInput = parse pInput "bad input"

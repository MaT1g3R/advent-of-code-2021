module Lib (position, Direction (..), Move (..), Coordinate (..), cordProduct, parseInput) where

import Data.Maybe (catMaybes)
import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec

data Direction = Forward | Down | Up deriving (Eq, Show)

data Move = Move Direction Integer deriving (Eq, Show)

data Coordinate = Coordinate Integer Integer deriving (Eq, Show)

instance Semigroup Coordinate where
  (Coordinate x y) <> (Coordinate a b) = Coordinate (x + a) (y + b)

instance Monoid Coordinate where
  mempty = Coordinate 0 0

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

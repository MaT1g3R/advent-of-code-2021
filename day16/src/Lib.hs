{-# LANGUAGE LambdaCase #-}

module Lib (Input, part1, part2, parseInput) where

import Data.Foldable (Foldable (foldl'))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = String

type Version = Integer

data Type = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving (Eq, Show)

data Packet = Literal Version Integer | Operator Version Type [Packet] deriving (Eq, Show)

type Parser = Parsec Void String

bin2dec :: String -> Integer
bin2dec s = foldl' (\s (c, i) -> s + if c == '1' then 2 ^ i else 0) 0 $ reverse s `zip` [0 ..]

pVersion :: Parser Version
pVersion = bin2dec <$> count 3 anySingle

pType :: Parser Type
pType =
  \case
    0 -> Sum
    1 -> Product
    2 -> Minimum
    3 -> Maximum
    5 -> GreaterThan
    6 -> LessThan
    7 -> EqualTo
    x -> error $ "invalid type: " <> show x
    <$> (bin2dec <$> count 3 anySingle)

pHeader :: Parser (Version, Type)
pHeader = (,) <$> pVersion <*> pType

pGroup :: Parser String
pGroup = count 5 anySingle

pGroups :: Parser [String]
pGroups = go []
  where
    go xs = do
      group <- pGroup
      let xs' = group : xs
      if head group == '0' then return $ reverse xs' else go xs'

pLiteral :: Parser Packet
pLiteral = do
  version <- pVersion
  string "100"
  Literal version . bin2dec . concatMap tail <$> pGroups

pOp :: Parser Packet
pOp = do
  (version, typ) <- pHeader
  mode <- anySingle
  case mode of
    '0' -> do
      size <- bin2dec <$> count 15 anySingle
      bits <- count (fromInteger size) anySingle
      case runParser (some pPacket) "Parser" bits of
        Left e -> error $ show e
        Right x -> return $ Operator version typ x
    _ -> do
      num <- bin2dec <$> count 11 anySingle
      sub <- count (fromInteger num) pPacket
      return $ Operator version typ sub

pPacket :: Parser Packet
pPacket = try pLiteral <|> pOp

runP = runParser pPacket "input"

part1 :: Input -> Integer
part1 input = case runP input of
  Right p -> sumVersions p
  Left e -> error $ show e
  where
    sumVersions (Literal version _) = version
    sumVersions (Operator version _ xs) = version + sum (sumVersions <$> xs)

part2 :: Input -> Integer
part2 input = case runP input of
  Right p -> evalP p
  Left e -> error $ show e
  where
    evalP (Literal _ x) = x
    evalP (Operator _ Sum xs) = sum $ evalP <$> xs
    evalP (Operator _ Product xs) = product $ evalP <$> xs
    evalP (Operator _ Minimum xs) = minimum $ evalP <$> xs
    evalP (Operator _ Maximum xs) = maximum $ evalP <$> xs
    evalP (Operator _ GreaterThan xs) = if evalP (head xs) > evalP (last xs) then 1 else 0
    evalP (Operator _ LessThan xs) = if evalP (head xs) < evalP (last xs) then 1 else 0
    evalP (Operator _ EqualTo xs) = if evalP (head xs) == evalP (last xs) then 1 else 0

parseInput :: String -> Input
parseInput = concatMap hex2bin
  where
    hex2bin '0' = "0000"
    hex2bin '1' = "0001"
    hex2bin '2' = "0010"
    hex2bin '3' = "0011"
    hex2bin '4' = "0100"
    hex2bin '5' = "0101"
    hex2bin '6' = "0110"
    hex2bin '7' = "0111"
    hex2bin '8' = "1000"
    hex2bin '9' = "1001"
    hex2bin 'A' = "1010"
    hex2bin 'B' = "1011"
    hex2bin 'C' = "1100"
    hex2bin 'D' = "1101"
    hex2bin 'E' = "1110"
    hex2bin 'F' = "1111"
    hex2bin _ = error "invalid input"

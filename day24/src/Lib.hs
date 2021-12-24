module Lib (Input, part1, part2, parseInput) where

import Control.Monad.State
import Control.Parallel
import Control.Parallel.Strategies
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    parse,
    sepEndBy,
    some,
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    newline,
    space,
    string,
  )

data ALU = ALU
  { w :: Int,
    x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Eq, Show)

setW :: ALU -> Int -> ALU
setW alu w = ALU {w = w, x = x alu, y = y alu, z = z alu}

setX :: ALU -> Int -> ALU
setX alu x = ALU {w = w alu, x = x, y = y alu, z = z alu}

setY :: ALU -> Int -> ALU
setY alu y = ALU {w = w alu, x = x alu, y = y, z = z alu}

setZ :: ALU -> Int -> ALU
setZ alu z = ALU {w = w alu, x = x alu, y = y alu, z = z}

setRegister :: ALU -> Register -> Int -> ALU
setRegister alu reg i =
  case reg of
    W -> setW alu i
    X -> setX alu i
    Y -> setY alu i
    Z -> setZ alu i
    _ -> error "cannot set const register"

readRegister :: ALU -> Register -> Int
readRegister alu r =
  case r of
    W -> w alu
    X -> x alu
    Y -> y alu
    Z -> z alu
    Const i -> i

type Block = ALU -> Int -> ALU

data Register = W | X | Y | Z | Const Int deriving (Eq, Show)

data Instruction
  = Inp Register
  | Add Register Register
  | Mul Register Register
  | Div Register Register
  | Mod Register Register
  | Eql Register Register
  deriving (Eq, Show)

type Input = [Instruction]

type ComputeState = (ALU, Input, [Int])

runCompute :: Input -> State (ALU, [Int]) ALU
runCompute [] = gets fst
runCompute (i : is) = do
  (alu, digits) <- get
  let (alu', digits') = runInstruction (alu, digits) i
  put (alu', digits')
  runCompute is
  where
    runInstruction :: (ALU, [Int]) -> Instruction -> (ALU, [Int])
    runInstruction (alu, x : xs) (Inp reg) = (setRegister alu reg x, xs)
    runInstruction (alu, []) (Inp reg) = error "Input instruction on empty input"
    runInstruction (alu, xs) (Add a b) = (runBinOp alu (+) a b, xs)
    runInstruction (alu, xs) (Mul a b) = (runBinOp alu (*) a b, xs)
    runInstruction (alu, xs) (Div a b) = (runBinOp alu div a b, xs)
    runInstruction (alu, xs) (Mod a b) = (runBinOp alu mod a b, xs)
    runInstruction (alu, xs) (Eql a b) = (runBinOp alu (\a b -> fromEnum (a == b)) a b, xs)

    runBinOp :: ALU -> (Int -> Int -> Int) -> Register -> Register -> ALU
    runBinOp alu op a b = setRegister alu a (op vala valb)
      where
        vala = readRegister alu a
        valb = readRegister alu b

part1 :: Input -> Integer
part1 input = go allNumbers
  where
    allNumbers = replicateM 14 [9, 8 .. 1]

    go [] = 0
    go nums = case foldl' acc Nothing results of
      Just i -> i
      Nothing -> go rest
      where
        (ns, rest) = splitAt 12 nums
        results :: [(ALU, [Int])]
        results = parMap rpar (\num -> (evalState (runCompute input) (ALU {w = 0, x = 0, y = 0, z = 0}, num), num)) ns

        acc :: Maybe Integer -> (ALU, [Int]) -> Maybe Integer
        acc (Just i) _ = Just i
        acc Nothing (alu, num) = if readRegister alu Z == 0 then Just $ read (concatMap show num) else Nothing

part2 :: Input -> Integer
part2 input = go allNumbers
  where
    allNumbers = replicateM 14 [1 .. 9]

    go [] = 0
    go nums = case foldl' acc Nothing results of
      Just i -> i
      Nothing -> go rest
      where
        (ns, rest) = splitAt 12 nums
        results :: [(ALU, [Int])]
        results = parMap rpar (\num -> (evalState (runCompute input) (ALU {w = 0, x = 0, y = 0, z = 0}, num), num)) ns

        acc :: Maybe Integer -> (ALU, [Int]) -> Maybe Integer
        acc (Just i) _ = Just i
        acc Nothing (alu, num) = if readRegister alu Z == 0 then Just $ read (concatMap show num) else Nothing

type Parser = Parsec Void String

parseInput :: String -> Input
parseInput s = case parse pInput "input" s of
  Right i -> i
  Left e -> error $ show e
  where
    pInt :: Parser Int
    pInt = read <$> (try (string "-") <|> string "" >>= (\sign -> (sign <>) <$> some digitChar))

    pRegister' :: Parser Register
    pRegister' =
      W <$ try (char 'w')
        <|> X <$ try (char 'x')
        <|> Y <$ try (char 'y')
        <|> Z <$ char 'z'

    pRegister :: Parser Register
    pRegister = try pRegister' <|> Const <$> pInt

    pInp :: Parser Instruction
    pInp = do
      string "inp"
      space
      Inp <$> pRegister'

    pBinIns :: String -> (Register -> Register -> Instruction) -> Parser Instruction
    pBinIns str ins = do
      string str
      space
      r1 <- pRegister
      space
      ins r1 <$> pRegister

    pIns :: Parser Instruction
    pIns =
      try pInp
        <|> try (pBinIns "add" Add)
        <|> try (pBinIns "mul" Mul)
        <|> try (pBinIns "div" Div)
        <|> try (pBinIns "mod" Mod)
        <|> try (pBinIns "eql" Eql)

    pInput :: Parser Input
    pInput = pIns `sepEndBy` newline

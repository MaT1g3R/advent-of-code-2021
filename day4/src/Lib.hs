module Lib
  ( Board,
    playGame,
    winningIndicies,
    parseInput,
    winLast,
  )
where

import Control.Monad
import Data.Foldable (foldl')
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec

type Board = [Int]

data BoardState = BoardState
  { board :: Board,
    marked :: Set Int,
    scoreSum :: Int
  }

data GameState = GameState [BoardState] (Maybe Int)

winningIndicies :: Int -> [Set Int]
winningIndicies n = winningRows n ++ winningCols n

winningRows :: Int -> [Set Int]
winningRows n = fmap row [0 .. n -1]
  where
    row i = Set.fromList $ fmap (+ (i * n)) [0 .. n -1]

winningCols :: Int -> [Set Int]
winningCols n = fmap col [0 .. n -1]
  where
    col i = Set.fromList $ fmap (\a -> a * 5 + i) [0 .. n -1]

hasWon :: [Set Int] -> Set Int -> Bool
hasWon wins idxes = any (`Set.isSubsetOf` idxes) wins

playBoard :: Int -> BoardState -> BoardState
playBoard call boardState = case List.elemIndex call $ board boardState of
  Just i -> BoardState {board = board boardState, marked = Set.insert i $ marked boardState, scoreSum = scoreSum boardState - call}
  Nothing -> boardState

getScore :: [Set Int] -> Int -> BoardState -> Maybe Int
getScore wins call boardState =
  if hasWon wins $ marked boardState then Just $ call * scoreSum boardState else Nothing

initBoardState board = BoardState {board = board, marked = Set.empty, scoreSum = sum board}

head' [] = Nothing
head' xs = Just $ head xs

last' [] = Nothing
last' xs = Just $ last xs

playGame :: [Set Int] -> [Board] -> [Int] -> Int
playGame wins boards calls =
  let playBoards (GameState bs (Just score)) _ = GameState bs $ Just score
      playBoards (GameState bs _) call =
        let newBs = playBoard call <$> bs
            score = head' $ mapMaybe (getScore wins call) newBs
         in GameState newBs score
      (GameState _ score) = foldl' playBoards (GameState (initBoardState <$> boards) Nothing) calls
   in fromMaybe 0 score

winLast :: [Set Int] -> [Board] -> [Int] -> Int
winLast wins boards calls =
  let playBoards (GameState bs oldScore) call =
        let newBs = playBoard call <$> bs
            boardHasWon b = hasWon wins (marked b)
            score = last' $ mapMaybe (getScore wins call) newBs
         in GameState (filter (not . boardHasWon) newBs) (if isNothing score then oldScore else score)
      (GameState _ score) = foldl' playBoards (GameState (initBoardState <$> boards) Nothing) calls
   in fromMaybe 0 score

pInt :: Parser Int
pInt = read <$> many1 digit

pRow :: Parser [Int]
pRow = do
  skipMany $ char ' '
  pInt `sepBy1` many1 (char ' ')

pBoard :: Parser Board
pBoard = join <$> pRow `endBy1` char '\n'

pBoards :: Parser [Board]
pBoards = pBoard `endBy` string "\n"

pCalls :: Parser [Int]
pCalls = pInt `sepBy` char ','

pInput :: Parser ([Int], [Board])
pInput = do
  calls <- pCalls
  spaces
  boards <- pBoards
  spaces
  return (calls, boards)

parseInput :: String -> Either ParseError ([Int], [Board])
parseInput = parse pInput "bad input"

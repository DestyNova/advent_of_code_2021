module Main where

import Text.Parsec
import Data.List ((\\), transpose, intercalate)

type Board = [[Int]]

main = do
  txt <- readFile "input.txt"
  let (Right (nums,boards)) = parse parser "" txt
  print $ getScore boards nums

getScore boards nums = sum unmarked * lastNum
  where (unmarked,lastNum) = runGame boards nums [] (length boards)

runGame :: [Board] -> [Int] -> [Int] -> Int -> ([Int],Int)
runGame _ [] _ _ = error "Ran out of numbers."
runGame boards (x:xs) called winsLeft = case (winsLeft, winner) of
                                             (_,[])  -> runGame boards xs (x:called) winsLeft
                                             (1,[w]) -> (concat w \\ (x:called), x)
                                             (_,ws) -> runGame (removeWinners ws boards) xs (x:called) (winsLeft - length ws)
  where winner = filter (isWinner (x:called)) boards
        removeWinners ws b = b \\ ws

isWinner :: [Int] -> Board -> Bool
isWinner nums board = isRow || isColumn
  where isRow = isLine board
        isColumn = isLine $ transpose board
        isLine = any (all (`elem` nums))

parser :: Parsec String () ([Int],[Board])
parser = do
  nums <- number `sepBy` char ','
  newline
  newline
  boards <- parseBoard `sepBy1` newline
  return (nums, boards)

number :: Parsec String () Int
number = read <$> many1 digit

parseBoard :: Parsec String () Board
parseBoard = many1 $ do
  nums <- try (do
             many spaceChars
             xs <- number `sepEndBy1` (many1 spaceChars)
             newline
             return xs)
  return nums

spaceChars = char ' '

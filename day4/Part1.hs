module Main where

import Text.Parsec
import Data.List ((\\), transpose, intercalate)

type Board = [[Int]]

main = do
  txt <- readFile "input.txt"
  let (Right (nums,boards)) = parse parser "" txt
  print $ getScore boards nums

getScore boards nums = sum unmarked * lastNum
  where (unmarked,lastNum) = runGame boards nums []

runGame :: [Board] -> [Int] -> [Int] -> ([Int],Int)
runGame _ [] _ = error "Ran out of numbers."
runGame boards (x:xs) called = case winner of
                                  []  -> runGame boards xs (x:called)
                                  [w] -> (concat w \\ (x:called), x)
  where winner = filter (isWinner (x:called)) boards

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
  return (nums, map (filter (not . null)) boards) -- why?

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

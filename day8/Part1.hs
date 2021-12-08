module Main where

import Text.Parsec
import Data.List ((\\), transpose)

type Board = [[Int]]

main = do
  txt <- readFile "input.txt"
  let (Right inp) = parse parser "" txt
  print $ inp
  -- 0: 6, 1: 2, 2: 5, 3: 5, 4: 4, 5: 5:, 6: 6, 7: 3, 8: 7, 9: 6
  print $ length $ filter (`elem` [2,4,3,7]) $ concat inp

parser :: Parsec String () [[Int]]
parser = (do
  word `sepEndBy1` char ' '
  string "| "
  val <- word `sepBy1` char ' '
  return $ map length val
  ) `sepEndBy` newline

number :: Parsec String () Int
number = read <$> many1 digit

word = many1 letter

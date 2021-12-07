module Main where

import Text.Parsec
import Data.List (sort)

type Board = [[Int]]

main = do
  txt <- readFile "input.txt"
  let (Right crabs) = parse parser "" txt
  print $ getCost crabs (median crabs)

getCost xs x = sum $ map (\c -> abs $ c - x) xs
median = midVal . sort
  where midVal xs = head $ drop (length xs `div` 2) xs

parser :: Parsec String () [Int]
parser = number `sepBy` char ','

number :: Parsec String () Int
number = read <$> many1 digit

module Main where

import Text.Parsec
import Data.List (minimumBy)
import Data.Ord (comparing)

type Board = [[Int]]

main = do
  txt <- readFile "input.txt"
  let (Right crabs) = parse parser "" txt
  print $ getCost crabs

getCost :: [Int] -> (Int,Int)
getCost crabs = minimumBy (comparing snd) fuelCosts
  where fuelCosts = map (\loc -> (loc, totalCost loc)) [0..maximum crabs]
        totalCost loc = sum [ n*(n+1) `div` 2 | c <- crabs, let n = abs (c - loc) ]

parser :: Parsec String () [Int]
parser = number `sepBy` char ','

number :: Parsec String () Int
number = read <$> many1 digit

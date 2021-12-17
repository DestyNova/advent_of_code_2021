module Main where

import Text.Parsec
import Data.List (nub)

type Coord = (Int,Int)
type Grid = [[Char]]

main = do
  txt <- readFile "input.txt"
  let (Right area) = parse parser "" txt
  print area
  print $ uncurry solve area

solve :: Coord -> Coord -> Int
solve (x1,x2) (y1,y2) = length $ nub xSteps
  where ySteps = solveYs y1 y2
        xSteps = concatMap (\(_,y,steps) -> map (\(x,d) -> (x,y)) $ getXsForSteps steps x1 x2 [minX..x2]) ySteps
        minX = truncate $ sqrt $ fromIntegral (x1*2)

solveYs :: Int -> Int -> [(Int,Int,Int)]
solveYs y1 y2 = getPossibleYSteps y1 y2 [minY..maxY]
  where minY = y1
        maxY = negate y1 - 1

getPossibleYSteps :: Int -> Int -> [Int] -> [(Int,Int,Int)]
getPossibleYSteps y1 y2 ys = filter possibleY $ concatMap yVals ys
  where possibleY (d,_,_) = d >= y1 && d <= y2
        yVals y' = scanl (\(acc,_,i) y -> (acc+y,y',i+1)) (0,0,0) (take 10000 $ iterate pred y')

getXsForSteps :: Int -> Int -> Int -> [Int] -> [(Int,Int)]
getXsForSteps steps x1 x2 xs = filter possibleX $ map xVals xs
  where possibleX (_,d) = d >= x1 && d <= x2
        xVals x' = (x', foldl (\d v -> d+v) 0 (take steps $ reverse [1..x']))

parser :: Parsec String () (Coord,Coord)
parser = do
  string "target area: x="
  x1 <- number
  string ".."
  x2 <- number
  string ", y="
  y1 <- number
  string ".."
  y2 <- number

  return ((x1,x2),(y1,y2))

number :: Parsec String () Int
number = read <$> many1 (digit <|> char '-')

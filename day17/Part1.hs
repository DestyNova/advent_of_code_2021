module Main where

import Text.Parsec
import Data.List (nub)

type Coord = (Int,Int)

main = do
  txt <- readFile "input.txt"
  let (Right area) = parse parser "" txt
  print area
  print $ uncurry solve area

solve (x1,x2) (y1,y2) = getMaxY yv
  where possibleYs = solveYs y1 y2 possibleSteps
        possibleSteps = nub $ map snd possibleXSteps
        possibleXSteps = getPossibleXSteps x1 x2 [minX..x2]
        minX = truncate $ sqrt $ fromIntegral (x1*2)
        yv = maximum $ map snd $ solveYs y1 y2 possibleSteps

getMaxY yv = last $ takeWhile (\(y,v) -> v >= 0) $ iterate (\(y,v) -> (y+v,v-1)) (0,yv)

solveYs :: Int -> Int -> [Int] -> [(Int,Int)]
solveYs y1 y2 steps = getPossibleYSteps y1 y2 (maximum steps) [minY..maxY]
  where minY = y1
        maxY = negate y1 - 1

getPossibleYSteps :: Int -> Int -> Int -> [Int] -> [(Int,Int)]
getPossibleYSteps y1 y2 steps ys = filter possibleY $ concatMap yVals ys
  where possibleY (d, _) = d >= y1 && d <= y2
        yVals y' = scanl (\(acc,_) y -> (acc+y,y')) (0,0) (take (1000*steps) $ iterate pred y')

getPossibleXSteps :: Int -> Int -> [Int] -> [(Int,Int)]
getPossibleXSteps x1 x2 xs = filter possibleX $ concatMap xVals xs
  where possibleX (d, _) = d >= x1 && d <= x2
        xVals x' = scanr (\x (acc,i) -> (acc+x,i+1)) (0,0) [1..x']

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

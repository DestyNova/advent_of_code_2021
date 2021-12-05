module Main where

import Text.Parsec
import qualified Data.Map as M

type Segment = (Int,Int,Int,Int)

main = do
  txt <- readFile "input.txt"
  let (Right segments) = parse parser "" txt
  --print segments
  let straightLines = filter (\(a,b,c,d) -> a == c || b == d) segments
  let grid = makeGrid straightLines
  --print grid
  let overlaps = M.size $ M.filter (>=2) grid
  print overlaps

makeGrid segments = foldr insertPoints M.empty segments

insertPoints segment m = foldr (\p m' -> M.insertWith (+) p 1 m') m (getPoints segment)

getPoints (x1,y1,x2,y2) = [(x,y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

parser :: Parsec String () [Segment]
parser = (do
  x1 <- number
  char ','
  y1 <- number

  string " -> "

  x2 <- number
  char ','
  y2 <- number

  return (x1,y1,x2,y2)) `sepEndBy` newline

number :: Parsec String () Int
number = read <$> many1 digit

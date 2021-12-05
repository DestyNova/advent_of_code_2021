module Main where

import Text.Parsec
import qualified Data.Map as M

type Segment = (Int,Int,Int,Int)

main = do
  txt <- readFile "input.txt"
  let (Right segments) = parse parser "" txt
  let grid = makeGrid segments
  let overlaps = M.size $ M.filter (>=2) grid
  print overlaps

makeGrid segments = foldr insertPoints M.empty segments

insertPoints segment m = foldr (\p m' -> M.insertWith (+) p 1 m') m (getPoints segment)

-- This feels like it should be waaaaay simpler. What have I done?
getPoints (x1,y1,x2,y2) = [(x1 + i*dirX, y1 + i*dirY) | let diff = max (abs (x2 - x1)) (abs (y2 - y1)),
                                              let dirX = signum (x2 - x1),
                                              let dirY = signum (y2 - y1),
                                              i <- if x1 == x2 && y1 == y2
                                                      then [0]
                                                      else [0..diff]]

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

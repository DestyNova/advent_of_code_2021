module Main where

import Text.Parsec
import qualified Data.Array as A
import Data.Array (Array, (!), (//))
import Data.List (sort, intercalate)

type Coord = (Int,Int,Int)
type Range = (Int,Int)
type RangeSpec = (Range,Range,Range)
type Flip = (Bool,RangeSpec)
type Grid = Array Coord Bool

main = do
  txt <- readFile "input.txt"
  let (Right flips) = parse parser "" txt
  print flips
  let g' = solve flips
  print $ countOn g'

solve :: [Flip] -> Grid
solve = applyFlips initialG

applyFlips :: Grid -> [Flip] -> Grid
applyFlips g [] = g
applyFlips g ((v,(xRange,yRange,zRange)):fs) | any oob [xRange,yRange,zRange] = applyFlips g fs
applyFlips g ((v,(xRange,yRange,zRange)):fs) = applyFlips g' fs
  where g' = g // updates
        updates = [((x,y,z),v) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
        (x1,x2) = maxMin xRange
        (y1,y2) = maxMin yRange
        (z1,z2) = maxMin zRange

oob (a,b) = max (abs a) (abs b) > 50
maxMin (a,b) | a <= b = (a,b)
             | otherwise = (b,a)

countOn g = length [True | let r = [-50..50], x <- r, y <- r, z <- r, g ! (x,y,z)]

initialG = A.array gBounds [((x,y,z),False) | let r = [-50..50], x <- r, y <- r, z <- r]

gBounds = (negLim,posLim)
  where negLim = (-50,-50,-50)
        posLim = (50,50,50)

parser :: Parsec String () [Flip]
parser = parseFlip `endBy1` newline

parseFlip = do
  action <- try (string "off") <|> string "on"
  let a = case action of
               "off" -> False
               _ -> True
  char ' '
  [xr,yr,zr] <- parseRange `sepBy` char ','
  return (a, (xr,yr,zr))

parseRange = do
  letter
  char '='
  n1 <- number
  string ".."
  n2 <- number
  return (n1,n2)

number :: Parsec String () Int
number = read <$> many1 (digit <|> char '-')

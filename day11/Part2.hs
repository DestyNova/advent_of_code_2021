module Main where

import Text.Parsec
import Data.List (intercalate, nub, transpose)
import qualified Data.Map as M
import Data.Map ((!))

type Coord = (Int,Int)
type Grid = M.Map Coord Int

main = do
  txt <- readFile "input.txt"
  let (Right inp) = parse parser "" txt
  let g = transpose inp
  print $ run g

gridToMap g = M.fromList [((i,j), g !! j !! i) | i <- [0..w-1], j <- [0..h-1]]
  where w = length (head g)
        h = length g

run g = run' (gridToMap g) 0 w h 0
  where w = length (head g)
        h = length g

run' :: Grid -> Int -> Int -> Int -> Int -> Int
run' grid n w h flashes | allZeroes grid = n
                        | otherwise = run' g' (n+1) w h flashes'
  where flashes' = flashes + M.size (M.filter (>9) flashed)
        incremented = step grid
        flashed = doFlashes incremented w h []
        g' = reset flashed

step = M.map succ
reset = M.map (\i -> if i > 9 then 0 else i)

allZeroes g = M.size (M.filter (/=0) g) == 0

doFlashes :: Grid -> Int -> Int -> [Coord] -> Grid
doFlashes g w h toFlash = g'
  where flashing = nub $ toFlash ++ M.keys (M.filter (==10) g)
        g' = case flashing of
                  [] -> g
                  (f:fs) -> doFlashes (applyFlash f w h g) w h fs

applyFlash (x,y) w h g = foldr (\c g' -> M.insertWith (+) c 1 g') g neighbours
  where neighbours = getNeighbours x y w h

getNeighbours x y w h = [(x',y') | i <- [-1,0,1], j <- [-1,0,1],
                                   let x' = x+i,
                                   let y' = y+j,
                                   x' >= 0 && x' < w,
                                   y' >= 0 && y' < h]

parser :: Parsec String () [[Int]]
parser =
  many1 (read . (:[]) <$> digit) `sepEndBy1` newline

number :: Parsec String () Int
number = read <$> many1 digit

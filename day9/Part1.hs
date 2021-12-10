module Main where

import Text.Parsec
import Data.List ((\\), transpose)

main = do
  txt <- readFile "input.txt"
  let (Right inp) = parse parser "" txt
  let lowPoints = getLowPoints inp
  print $ sum $ map (+1) lowPoints

getLowPoints xs = [p | x <- [0..w-1], y <- [0..h-1],
                         let neighbours = getNeighbours x y w h,
                         let p = xs !! y !! x,
                         all (\(nx,ny) -> p < (xs !! ny !! nx)) neighbours ]
  where w = length (head xs)
        h = length xs

getNeighbours x y w h = [(x',y') | (i,j) <- [(-1,0),(0,-1),(1,0),(0,1)],
                                   let x' = x+i,
                                   let y' = y+j,
                                   x' >= 0 && x' < w,
                                   y' >= 0 && y' < h
                        ]

parser :: Parsec String () [[Int]]
parser =
  many1 (read . (:[]) <$> digit) `sepEndBy1` newline

number :: Parsec String () Int
number = read <$> many1 digit

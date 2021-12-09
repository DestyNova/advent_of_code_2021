module Main where

import Text.Parsec
import Data.List (nub, sortBy)

type HeightMap = [[Int]]

main = do
  txt <- readFile "input.txt"
  let (Right inp) = parse parser "" txt
  print $ product $ take 3 $ sortBy (flip compare) $ getLowPoints inp

getLowPoints xs = [getBasinSize (getFlowPoints xs x y w h p) | x <- [0..w-1], y <- [0..h-1],
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

getBasinSize = (+1) . length . nub . map fst

getFlowPoints :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [((Int,Int),Int)]
getFlowPoints xs x y w h currentHeight | currentHeight >= 8 = []
                                       | otherwise = higherNeighbours ++ theirFlowPoints
  where higherNeighbours = filter (\(_,n) -> n > currentHeight && n < 9) zipped
        currentHeight = xs !! y !! x
        neighbourCoords = getNeighbours x y w h
        neighbours = map (\(i,j) -> xs !! j !! i) neighbourCoords
        zipped = zip neighbourCoords neighbours
        theirFlowPoints = concat $ zipWith (\(i,j) nHeight ->
                                            if nHeight < 8 && nHeight > currentHeight
                                               then getFlowPoints xs i j w h nHeight
                                               else []) neighbourCoords neighbours

parser :: Parsec String () HeightMap
parser =
  many1 (read . (:[]) <$> digit) `sepEndBy1` newline

number :: Parsec String () Int
number = read <$> many1 digit

module Main where

import Text.Parsec
import Data.List (intercalate)

type Grid = [[Char]]

main = do
  txt <- readFile "input.txt"
  let (Right (alg,g)) = parse parser "" txt
  putStrLn alg

  -- putStrLn $ printGrid g
  -- putStrLn "\n----\n"
  let n = 50
  let g' = run alg g n
  -- putStrLn $ printGrid g'
  putStrLn $ "d: " ++ show (length g')
  print $ pixelCount g'

run alg g n = run' alg g n True

run' alg g 0 _ = g
run' alg g n b = run' alg g' (n-1) (not b)
  where g' = [[process alg w | i <- [0..d + 1], let w = getWindow g i j d b]
                             | j <- [0..d + 1]]
        d = length g

printGrid = intercalate "\n"

process alg w = alg !! w

getWindow :: Grid -> Int -> Int -> Int -> Bool -> Int
getWindow g x y d b = toBin $ concat [[ c | i <- [x-1..x+1],
                                            let oob = i < 1 || i > d || j < 1 || j > d,
                                            let u = if b then '.' else '#',
                                            let c = if oob then u else lookupGrid g i j ] | j <- [y-1..y+1]]

lookupGrid g i j = g !! (j-1) !! (i-1)

toBin = foldl (\acc c -> acc*2 + if c == '#' then 1 else 0) 0

pixelCount = length . filter (=='#') . concat

getNeighbours x y w h = [(x',y') | (i,j) <- [(-1,0),(0,-1),(1,0),(0,1)],
                                   let x' = x+i,
                                   let y' = y+j,
                                   x' >= 0 && x' < w,
                                   y' >= 0 && y' < h
                        ]

parser :: Parsec String () ([Char], Grid)
parser = do
  alg <- many1 dot
  newline
  newline
  grid <- many1 dot `endBy1` newline
  return (alg,grid)

dot = char '.' <|> char '#'

number :: Parsec String () Int
number = read <$> many1 digit

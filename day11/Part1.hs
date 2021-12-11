{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.Parsec
import Control.Monad.ST
import Data.Array.ST
import Debug.Trace (trace)
import Data.Array.Unboxed (UArray)
import Data.List (intercalate)

main = do
  txt <- readFile "sample-small.txt"
  let (Right inp) = parse parser "" txt
  print inp
  print $ runST $ run inp 0

--run :: [[Int]] -> Int -> ST s [[Int]]
run s n = do
  grid <- newArray ((1,1),(w,h)) 0
  mapM_ (\(r,j) -> mapM_ (\(x,i) -> writeArray grid (i,j) x) (zip r [1..])) (zip s [1..])
  run' grid n 0
  where w = length (head s)
        h = length s

run' :: forall s. STUArray s (Int,Int) Int -> Int -> Int -> ST s Int
run' grid steps flashes | trace (toList grid) False = undefined
run' grid 0 flashes = pure flashes

toList :: forall s. STUArray s (Int,Int) Int -> String
toList grid = 
  intercalate "\n" [
    r | i <- [1..5],
        let r = concat [showVal x | j <- [1..5], let x = getVal grid i j]
    ] ++ "\n"
  where getVal :: STUArray s (Int,Int) Int -> Int -> Int -> ST s Int
        getVal grid i j = do
          readArray grid (i,j)
        showVal :: ST s Int -> String
        showVal x = runST $ do
          show x

-- toList grid = getElems grid -- runST $ do
  --xs <- getElems grid
  --pure [1]

  -- pure $ group 5 xs
-- group _ [] = []
-- group n xs = (take n xs) : (group n (drop n xs))

-- step s = sReset
--   where w = length (head s)
--         h = length s
--         sReset = [if e > 9 then 0 else e | x <- [0..w-1], y <- [0..h-1],
--                        let e = sFlashed !! y !! x]
--         sFlashed = doFlashes s' w h
--         s' = [e' | x <- [0..w-1], y <- [0..h-1],
--                    let neighbours = getNeighbours x y w h,
--                    let e' = (s !! y !! x) + 1,
--                    all (\(nx,ny) -> p < (s !! ny !! nx)) neighbours ]
-- 
-- 
-- doFlashes s w h = if s' == s then s else doFlashes s' w h
--   where s' = [e' | x <- [0..w-1], y <- [0..h-1],
-- getNeighbours x y w h = [(x',y') | (i,j) <- [(-1,-1),(0,-1),(1,-1)
--                                             ,(-1, 0),       (1, 0)
--                                             ,(-1, 1),(0, 1),(1, 1)],
--                                    let x' = x+i,
--                                    let y' = y+j,
--                                    x' >= 0 && x' < w,
--                                    y' >= 0 && y' < h
--                         ]

parser :: Parsec String () [[Int]]
parser =
  many1 (read . (:[]) <$> digit) `sepEndBy1` newline

number :: Parsec String () Int
number = read <$> many1 digit

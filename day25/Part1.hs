module Main where

import Data.List (sort, intercalate, transpose)
import qualified Data.Array.Unboxed as A
import Data.Array (Array, (//), (!))

type Coord = (Int,Int)
type Grid = Array (Int,Int) Int

main = do
  txt <- readFile "input.txt"
  let inp = lines txt
  let d = solve inp
  print d

printGrid :: Grid -> String
printGrid g = intercalate "\n" [[deconvertChar (g ! (i,j)) | i <- [0..w]] | j <- [0..h]]
  where (_,(w,h)) = A.bounds g

solve :: [[Char]] -> Int
solve inp = solve' g w h 0
  where h = length inp
        w = length $ head inp
        g = A.listArray ((0,0),(w-1,h-1)) (map convertChar $ concat $ transpose inp)

solve' :: Grid -> Int -> Int -> Int -> Int
solve' g w h steps = if stepped > 0 && steps < 1000
                      then solve' g' w h (steps+1)
                      else steps + 1
  where (stepped,g') = doStep g w h

doStep g w h = (updatedCount, g3)
  where rightMoves = concat [[(pos,0),(nextPos,c)] | i <- [0..w-1],
                                                     j <- [0..h-1],
                                                     let pos = (i,j),
                                                     let nextPos = ((i+1) `mod` w,j),
                                                     let c = g ! (i,j),
                                                     let c2 = g ! nextPos,
                                                     canMoveRight c c2]
        g2 = g // rightMoves

        downMoves = concat [[(pos,0),(nextPos,c)] | i <- [0..w-1],
                                                    j <- [0..h-1],
                                                    let pos = (i,j),
                                                    let nextPos = (i,(j+1) `mod` h),
                                                    let c = g2 ! (i,j),
                                                    let c2 = g2 ! nextPos,
                                                    canMoveDown c c2]

        g3 = g2 // downMoves

        updatedRight = length rightMoves
        updatedDown = length downMoves
        updatedCount = updatedRight + updatedDown

canMoveRight 0 _ = False
canMoveRight 2 _ = False
canMoveRight _ 0 = True
canMoveRight _ _ = False

canMoveDown 0 _ = False
canMoveDown 1 _ = False
canMoveDown _ 0 = True
canMoveDown _ _ = False

convertChar c = case c of
                     '.' -> 0
                     '>' -> 1
                     'v' -> 2
                     _ -> error "Wat?"

deconvertChar i = case i of
                     0 -> '.'
                     1 -> '>'
                     2 -> 'v'
                     _ -> error "Wat?"

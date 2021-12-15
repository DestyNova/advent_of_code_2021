module Main where

-- Day 15 part 2, using Bellman-Ford with the ST monad and STUArrays.
-- I tested it on the sample input with immutable arrays, and it took
-- about 4 seconds for the small sample input. Now it takes about
-- 3.3 seconds for the full input, so I'd estimate a speedup of at
-- least 3 or 4 orders of magnitude. Imperative Haskell FTW!

import Text.Parsec
import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((//), (!))
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)

type Coord = (Int,Int)
type Weights = A.Array (Int,Int) Int

data Heap = Node Int Int Heap Heap | NoNode deriving (Eq,Show)

main = do
  inp <- lines <$> readFile "sample.txt"
  let w = length (head inp) * 5
  let h = length inp * 5
  let g = parseInts inp
  let g' = multiplyGrid g
  let ws = parseGrid g'
  let ds = runST $ solve ws w h
  print ds

parseInts = map (map (read . (:[])))

multiplyGrid :: [[Int]] -> [[Int]]
multiplyGrid xs = concatMap (incAll megaRow) [0..4]
  where megaRow = transpose $ concatMap (incAll (transpose xs)) [0..4]

incAll :: [[Int]] -> Int -> [[Int]]
incAll xs n = [[if r > 9 then r - 9 else r | i <- rs, let r = i + n] | rs <- xs]

solve :: Weights -> Int -> Int -> ST s Int
solve ws w h = do
  ds <- newArray ((0,0),(w-1,h-1)) 10000000
  writeArray ds (0,0) 0
  solve2' ws ds (allVertices w h) w h

allVertices w h = [(i,j) | i <- [0..w-1], j <- [0..h-1]]

solve2' :: Weights -> STUArray s (Int,Int) Int -> [Coord] -> Int -> Int -> ST s Int
solve2' ws ds [] w h = error "Empty vertex list."
solve2' ws ds vs w h = do
  update' <- foldM (\updated v ->
                      let ns = getNeighbours v w h
                        in (do
                           newDs <- catMaybes <$> mapM (readNeighbour ws ds v) ns
                           mapM (\(u,ud') -> writeArray ds u ud') newDs
                           return $ updated || (not $ null newDs)))
                           False vs

  if (not update')
     then do
       res <- readArray ds (w-1,h-1)
       return res
     else do
       solve2' ws ds vs w h

readNeighbour :: Weights -> STUArray s (Int,Int) Int -> Coord -> Coord -> ST s (Maybe (Coord,Int))
readNeighbour ws ds v u = do
  vd <- readArray ds v
  ud <- readArray ds u
  let ud' = (ws ! u) + vd
  return $ if ud' < ud then Just (u,ud') else Nothing

getNeighbours (i,j) w h = [(a,b) | (a,b) <- [(i-1,j),(i,j-1),(i+1,j),(i,j+1)],
                                   a >= 0, a < w,
                                   b >= 0, b < h]

parseGrid :: [[Int]] -> Weights
parseGrid rs = arr // [((0,0),0)]
  where w = length $ head rs
        h = length rs
        arr = parseGrid' 0 rs (A.listArray ((0,0),(w-1,h-1)) [])

parseGrid' :: Int -> [[Int]] -> Weights -> Weights
parseGrid' _ [] acc = acc
parseGrid' j (r:rs) acc = parseGrid' (j+1) rs acc'
  where acc' = acc // [((i,j),w) | (i,w) <- zip [0..] r]

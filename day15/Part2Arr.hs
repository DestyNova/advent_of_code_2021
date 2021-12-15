module Main where

-- Day 15 part 2 using immutable arrays to implement Djikstra's pathing
-- algorithm using leftist heaps as a priority queue. Note that I just
-- allowed the heaps to be leaky by adding a new duplicate node with a
-- lower key (distance) when a shorter path is found. That's fine in this
-- case because running through the old values later has no negative
-- effects other than warming the planet slightly.
-- This version took about 9 minutes compared to the 2 hours needed by the
-- Map/list/minimumBy version in Part2.hs.
-- But the slower Bellman-Ford algorithm with mutable unboxed arrays in the
-- ST monad took a ridiculous 3.3 seconds. Do that instead.

import Text.Parsec
import Data.List (minimumBy, transpose, (\\))
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Array.Unboxed as A
import Data.Array (Array, (//), (!))

type Coord = (Int,Int)
type Weights = Array (Int,Int) Int

data Heap a = Node Int a Int (Heap a) (Heap a) | NoNode deriving (Eq,Show)

main = do
  inp <- lines <$> readFile "input.txt"
  let w = length (head inp) * 5
  let h = length inp * 5
  let g = parseInts inp
  let g' = multiplyGrid g
  let ws = parseGrid g'
  putStrLn "solving..."
  let ds = solve ws w h
  print $ ds ! (w-1,h-1)

parseInts = map (map (read . (:[])))

multiplyGrid :: [[Int]] -> [[Int]]
multiplyGrid xs = concatMap (incAll megaRow) [0..4]
  where megaRow = transpose $ concatMap (incAll (transpose xs)) [0..4]

incAll :: [[Int]] -> Int -> [[Int]]
incAll xs n = [[if r > 9 then r - 9 else r | i <- rs, let r = i + n] | rs <- xs]

solve :: Weights -> Int -> Int -> Weights
solve ws w h = solve' ws (initialDists w h) (initialQ w h) w h

initialDists w h = (A.listArray ((0,0),(w-1,h-1)) [10000000 | i <- [0..w-1], j <- [0..h-1]]) // [((0,0),0)]
initialQ w h = foldr (\(i,j) acc -> insertNode 10000000 (i,j) acc) NoNode [(i,j) | i <- [0..w-1], j <- [0..h-1]]

solve' ws ds NoNode _ _ = ds
solve' ws ds q w h = solve' ws ds' qUpdated w h
  where (v,q') = getClosest ds q
        ns = getNeighbours v w h
        newDs = mapMaybe (\u -> let ud' = (ws ! u) + (ds ! v)
                                    ud = ds ! u
                                    in if ud' < ud then Just (u,ud') else Nothing) ns
        ds' = ds // newDs
        qUpdated = foldr (\(u,ud') acc -> insertNode ud' u acc) q' newDs

getClosest :: Weights -> Heap Coord -> (Coord,Heap Coord)
getClosest ds q = (v,q')
  where (v,q') = deleteMin q

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

-- Leftist trees have a pretty big limitation for this use-case:
-- Removing an arbitrary node, or updating its key, seems to require
-- a complete tree traversal just to find the node, making it O(n) again.
insertNode k v h = mergeNodes h (Node k v 1 NoNode NoNode)

mergeNodes :: (Eq a) => Heap a -> Heap a -> Heap a
mergeNodes NoNode h = h
mergeNodes h NoNode = h
mergeNodes na@(Node ak av as al ar) nb@(Node bk bv bs bl br)
  | ak > bk = mergeNodes nb na
  | otherwise = if al == NoNode then Node ak av 1 arb al else h'

  where arb = mergeNodes ar nb
        h' = if ars > als
                then (Node ak av (als+1) arb al)
                else (Node ak av (ars+1) al arb)
        als = getHeapS al
        ars = getHeapS arb
        getHeapS (Node _ _ s _ _) = s

testMergeNodes = foldr (\i acc -> insertNode i "dud" acc) NoNode [12,7,13,19,8,5]

deleteMin (Node k v _ l r) = (v, mergeNodes l r)

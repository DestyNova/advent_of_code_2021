module Main where

-- Day 15 part 2 using mutable arrays to implement Djikstra's pathing
-- algorithm with leftist heaps as a priority queue.
--
import Text.Parsec
import Data.List (minimumBy, transpose, (\\))
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Array.Unboxed as A
import Data.Array (Array, (//), (!))
import qualified Data.Set as S
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)

type Coord = (Int,Int)
type Weights = Array (Int,Int) Int
type WeightQ = S.Set (Int,Coord)

data Heap a = Node Int a Int (Heap a) (Heap a) | NoNode deriving (Eq,Show)

main = do
  inp <- lines <$> readFile "input.txt"
  let w = length (head inp) * 5
  let h = length inp * 5
  let g = parseInts inp
  let g' = multiplyGrid g
  let ws = parseGrid g'
  putStrLn "solving..."
  let d = runST $ solve ws w h
  print d

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
  solve' ws ds (initialQ w h) w h

initialQ w h = foldr (\(i,j) acc -> insertNode 10000000 (i,j) acc) NoNode [(i,j) | i <- [0..w-1], j <- [0..h-1]]

solve' :: Array (Int, Int) Int -> STUArray s (Int, Int) Int -> Heap Coord -> Int -> Int -> ST s Int
solve' ws ds NoNode w h = readArray ds (w-1,h-1)
solve' ws ds q w h = do
  let (v,q') = getClosest ds q
  let ns = getNeighbours v w h

  -- read and update distances of any neighbours who can be reached through this node via a shorter path
  newDs <- catMaybes <$> mapM (readNeighbour ws ds v) ns
  mapM_ (uncurry (writeArray ds)) newDs

  -- update the priority queue (immutably, for now)
  let qUpdated = foldr (\(u,ud') acc -> insertNode ud' u acc) q' newDs
  solve' ws ds qUpdated w h

readNeighbour :: Weights -> STUArray s (Int,Int) Int -> Coord -> Coord -> ST s (Maybe (Coord,Int))
readNeighbour ws ds v u = do
  vd <- readArray ds v
  ud <- readArray ds u
  let ud' = (ws ! u) + vd
  return $ if ud' < ud then Just (u,ud') else Nothing

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

insertNode k v h = mergeNodes h (Node k v 1 NoNode NoNode)

mergeNodes :: (Eq a) => Heap a -> Heap a -> Heap a
mergeNodes NoNode h = h
mergeNodes h NoNode = h
mergeNodes na@(Node ak av as al ar) nb@(Node bk bv bs bl br)
  | ak > bk = mergeNodes nb na
  | otherwise = if al == NoNode then Node ak av 1 arb al else h'

  where arb = mergeNodes ar nb
        h' = if ars > als
                then Node ak av (als+1) arb al
                else Node ak av (ars+1) al arb
        als = getHeapS al
        ars = getHeapS arb
        getHeapS (Node _ _ s _ _) = s

testMergeNodes = foldr (`insertNode` "dud") NoNode [12,7,13,19,8,5]

deleteMin (Node k v _ l r) = (v, mergeNodes l r)

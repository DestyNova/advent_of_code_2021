module Main where

-- I didn't get this approach working at all. Might come back to this when I'm less tired and frustrated.

import Data.List (sort, intercalate, nub)
import Data.Map (Map, (!), (\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.Maybe (isNothing, isJust, catMaybes)
import Control.Monad (foldM)
import Control.Monad.ST
import qualified Data.Array.Unboxed as A
import Data.Array.ST
import Data.Bits

type Coord = (Int,Int)
type Grid = Map Coord Char
type Move = (Coord,Coord)
type Table s = STArray s Int (Int,Grid)
type WeightQ = S.Set (Int,Grid)

main = do
  txt <- readFile "sample2.txt"
  let g = parse $ drop 2 $ lines txt
  print g
  putStrLn $ showGrid g
  let res = runST $ solve g
  print res

cacheSize :: Integer
cacheSize = 2^28

solve :: Grid -> ST s Int
solve g = do
  ds <- newArray (0,fromInteger $ cacheSize-1) (maxCost,mempty)
  writeArray ds (encodeState g) (0,g)
  res <- dfs ds (initialQ g)
  return res

initialQ g = S.fromList [(0,g)]

maxCost = 100000
testG :: Grid
testG = M.fromList [((0,0),'A'),((2,2),'B'),((1,0),'A'),((2,4),'A'),((5,0),'B'),((2,3),'A'),((4,3),'B'),((4,4),'B'),((6,1),'C'),((6,2),'C'),((6,3),'C'),((6,4),'C'),((8,1),'D'),((8,2),'D'),((8,3),'D'),((8,4),'D')]

solved = M.fromList [((i,j),c) | i <- [2,4,6,8], j <- [1,2,3,4], let c = "ABCD" !! ((i `div` 2) - 1)]

dfs :: Table s -> S.Set (Int,Grid) -> ST s Int
dfs ds q
  | S.null q = do
    (cost,g) <- readArray ds (encodeState solved)
    return $ if g == solved
                then cost
                else error "Missing end state in distance table."

  | otherwise = do
    let (v,q') = getClosest q
    let ns = validMoves v
    (vd,_) <- readArray ds (encodeState v)
    newDs <- catMaybes <$> mapM (readNeighbour ds vd) ns
    mapM_ (\(u,ud') -> writeArray ds (encodeState u) (ud',u)) newDs

    -- update priority queue
    let qUpdated = foldr (\(u,ud') acc -> S.insert (ud',u) acc) q' (tmp $ newDs)
    dfs ds qUpdated

readNeighbour :: Table s -> Int -> (Move,Int,Grid) -> ST s (Maybe (Grid,Int))
readNeighbour ds vd (_,uw,g) = do
  (ud,g') <- tmp <$> readArray ds (encodeState g)
  let ud' = vd + uw
  return $ if ud' < ud then Just (g,ud') else Nothing

getClosest :: WeightQ -> (Grid,WeightQ)
getClosest q = case S.minView q of
                       Nothing -> error "Empty vertex queue."
                       Just ((_,v),q') -> (v,q')

encodeState :: Grid -> Int
encodeState g = fromInteger $ (encodeState' g) `mod` cacheSize

-- something is probably badly wrong with this?
encodeState' :: Grid -> Integer
encodeState' g = foldl (\acc (sw,c) -> acc .|. (getCellState g c `shiftL` sw)) 0 combo
  where hallway = [(0,i) | i <- [0,1,3,5,7,9,10]]
        rooms = [(i,j) | i <- [2,4,6,8], j <- [1..4]]
        combo = zip [0,3..] (hallway ++ rooms)

getCellState g c = case M.lookup c g of
                        Nothing -> 0
                        Just 'A' -> 1
                        Just 'B' -> 2
                        Just 'C' -> 3
                        Just 'D' -> 4

validMoves :: Grid -> [(Move,Int,Grid)]
validMoves g = filter (\((c1,c2),_,g) -> isLegal g c1 c2)
                      (concatMap (\c1 -> let a = unitCost (g ! c1)
                                             in map (\c2 -> ((c1,c2), getCost a c1 c2, applyMove g c1 c2)) (validMovesFrom g c1)) positions)
  where positions = M.keys g

getHomeCost c 'A' = minimum [getCost 1    c (2,j) | j <- [1..4]]
getHomeCost c 'B' = minimum [getCost 10   c (4,j) | j <- [1..4]]
getHomeCost c 'C' = minimum [getCost 100  c (6,j) | j <- [1..4]]
getHomeCost c 'D' = minimum [getCost 1000 c (8,j) | j <- [1..4]]

getCost cost (i1,j1) (i2,j2) = cost * distance
  where distance = abs (i1-i2) + yDist
        yDist = if i1 /= i2 && (j1 > 0 || j2 > 0)
                   then j1 + j2
                   else abs (j1-j2)

unitCost 'A' = 1
unitCost 'B' = 10
unitCost 'C' = 100
unitCost 'D' = 1000

applyMove :: Grid -> Coord -> Coord -> Grid
-- applyMove g c1 c2 | trace ("move " ++ show (g ! c1) ++ " from " ++ show c1 ++ " to " ++ show c2) False = undefined
applyMove g c1 c2 = M.insert c2 v (M.delete c1 g)
  where v = g ! c1

validMovesFrom g c = nub $ possibleMovesFrom g c

isLegal g c1 c2 = nobodyInDeadSpaces && isNotBlocking g c2
  where nobodyInDeadSpaces = not $ any (`M.member` g) [(2,0),(4,0),(6,0),(8,0)]

isSafeDestination g (i,j) (i2,j2) =
  (j > 0 && i == i2) || j2 == 0 || (isCorrectRoom amphipod i2 && isCompatible)
    where amphipod = g ! (i,j)
          isCorrectRoom 'A' 2 = True
          isCorrectRoom 'B' 4 = True
          isCorrectRoom 'C' 6 = True
          isCorrectRoom 'D' 8 = True
          isCorrectRoom _ _ = False
          isCompatible = all (\jx -> case M.lookup (i2,jx) g of
                             Nothing -> True
                             Just other -> amphipod == other) [2,3,4]

isNotBlocking g (i2,j2) =
  j2 == 0 || j2 == 4 ||
  (j2 == 1 && full2 && full3 && full4) ||
  (j2 == 2 && full3 && full4) ||
  (j2 == 3 && full4)
  where full1 = isJust (M.lookup (i2,1) g)
        full2 = isJust (M.lookup (i2,2) g)
        full3 = isJust (M.lookup (i2,3) g)
        full4 = isJust (M.lookup (i2,4) g)

isRoom (_,j) = j > 0

possibleMovesFrom :: Grid -> Coord -> [Coord]
-- possibleMovesFrom g c | trace ("possibleMovesFrom " ++ show c) False = undefined
possibleMovesFrom g c = filter hallwayToRoom moves
  where moves = possibleMovesFrom' g c c (S.fromList [c]) []
        hallwayToRoom c2 = isRoom c || isRoom c2

-- possibleMovesFrom' g c1 (i,j) visited acc | trace ("exploring " ++ show (c1,(i,j),visited)) False = undefined
possibleMovesFrom' g c1 (i,j) visited acc | null moves = concat acc
                                          | otherwise =
  concatMap (\c2 -> possibleMovesFrom' g c1 c2 visited' acc') moves
  where visited' = S.union (S.fromList moves) visited
        acc' = moves : acc
        moves = filter (\c2 ->
                        not (M.member c2 g) &&
                        not (S.member c2 visited) &&
                        isSafeDestination g c1 c2
                       ) (xMoves ++ yMoves)
        xMoves = if j == 0
                    then filter (\(x,_) -> x >= 0 && x <= 10) [(i-1,j),(i+1,j)]
                    else []
        yMoves = if inSideColumn i
                    then filter (\(_,y) -> y >= 0 && y <= 4) [(i,j-1),(i,j+1)]
                    else []

roomCols = [2,4,6,8]

inSideColumn = (`elem` roomCols)

showGrid g = intercalate "\n" g'
  where g' = [[c | i <- [0..10],
                   let c = case M.lookup (i,j) g of
                                Nothing -> if j == 0 || inSideColumn i
                                              then '.'
                                              else '#'
                                Just v -> v] | j <- [0..4]]

parse (x:y:_) = M.fromList positions
  where rows = concatMap (zip roomCols . filter (`elem` "ABCD")) [x,"ABCD","ABCD",y]
  -- where rows = concatMap (zip roomCols . filter (`elem` "ABCD")) [x,"DCBA","DBAC",y]
        positions = zipWith (\j (i,c) -> ((i,j),c)) [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4] rows

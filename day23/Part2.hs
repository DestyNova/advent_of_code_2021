module Main where

-- Iterative deepening search. Didn't have any success with this.

import Data.List (sort, intercalate, nub)
import Data.Map (Map, (!), (\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.Maybe (isNothing, isJust)
import Control.Monad (foldM)

type Coord = (Int,Int)
type Grid = Map Coord Char
type Move = (Coord,Coord)

main = do
  txt <- readFile "sample.txt"
  let g = parse $ drop 2 $ lines txt
  print g
  putStrLn $ showGrid g
  Just res <- ids g
  print res

maxCost = 50000
testG :: Grid
testG = M.fromList [((2,1),'B'),((2,2),'D'),((2,3),'D'),((2,4),'A'),((4,1),'C'),((4,2),'C'),((4,3),'B'),((4,4),'D'),((6,1),'B'),((6,2),'B'),((6,3),'A'),((6,4),'C'),((8,1),'D'),((8,2),'A'),((8,3),'C'),((8,4),'A')]

ids g = foldM (\found d ->
              case found of
                   Nothing -> do
                     putStrLn $ "Running IDS at depth " ++ show d
                     let res = dfs maxCost 0 g d
                     if res < maxCost
                        then pure (Just res)
                        else pure Nothing

                   Just res -> pure $ Just res) Nothing [7..40]

dfs :: Int -> Int -> Grid -> Int -> Int
-- dfs bestCost cost moves g d | trace ("dfs " ++ show (bestCost,cost,g,d,moves) ++ "\n" ++ showGrid g) False = undefined
dfs bestCost cost g d | isSolved g = cost
                      | d == 0 = maxCost
                      | bestCost < cost + heuristicCost = maxCost
                      | heuristicMoves > d = maxCost
                      | otherwise = res
  where res = foldr (\(move,moveCost,g') acc ->
                           if cost + moveCost > acc
                              then acc
                              else let acc' = dfs acc (cost+moveCost) g' (d-1)
                                       in if acc' < acc
                                             then acc'
                                             else acc
                              ) bestCost nextMoves
        nextMoves = validMoves g
        (heuristicCost, heuristicMoves) = minHeuristicMoves g

minHeuristicMoves :: Grid -> (Int,Int)
minHeuristicMoves g = foldr (\(c,a) (cost,moves) -> let hc = getHomeCost c a in (cost + hc, moves + if hc > 0 then 1 else 0)) (0,0) (M.toList g)

isSolved g = and $ zipWith (\i c ->
                            M.lookup (i,1) g == Just c &&
                            M.lookup (i,2) g == Just c &&
                            M.lookup (i,3) g == Just c &&
                            M.lookup (i,4) g == Just c) roomCols "ABCD"

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

tmp x | trace ("tmp " ++ show x) False = undefined
tmp x = True

isLegal g c1 c2 = nobodyInDeadSpaces && isNotBlocking g c2
  where nobodyInDeadSpaces = not $ any (`M.member` g) [(2,0),(4,0),(6,0),(8,0)]

-- isSafeDestination g a b | trace ("isSafeDestination " ++ show (a,b)) False = undefined
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

exitingRoomFully (i,j) (i2,j2) =
  i /= i2 || j2 == 0 || j == 0

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
                        -- tmp (c1,c2,i,j,visited) &&
                        not (M.member c2 g) &&
                        not (S.member c2 visited) &&
                        isSafeDestination g c1 c2
                        -- exitingRoomFully c1 c2 &&
                        -- isNotBlocking g c2
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
  -- where rows = concatMap (zip roomCols . filter (`elem` "ABCD")) [x,"ABCD","ABCD",y]
  where rows = concatMap (zip roomCols . filter (`elem` "ABCD")) [x,"BACD","ABCD",y]
        positions = zipWith (\j (i,c) -> ((i,j),c)) [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4] rows

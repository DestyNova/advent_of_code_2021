module Main where

import Data.List (sort, intercalate, nub)
import Data.Map (Map, (!), (\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)

type Coord = (Int,Int)
type Grid = Map Coord Char
type Move = (Coord,Coord)

main = do
  txt <- readFile "input.txt"
  let g = parse $ drop 2 $ lines txt
  print g
  putStrLn $ showGrid g
  -- let moves = validMoves inp
  -- putStrLn $ show (length moves) ++ " moves found."
  -- mapM (\(cost,g) -> do
  --      putStrLn $ "Cost: " ++ show cost
  --      putStrLn $ showGrid g) moves
  let d = 12
  putStrLn $ "Searching with depth " ++ show d
  print $ ids (2^32) 0 [] g d

-- [((7,0),(2,1)),((2,1),(4,1)),((4,1),(7,0))]
testG :: Grid
testG = M.fromList [((0,0),'B'),((2,2),'A'),((3,0),'A'),((4,2),'B'),((6,1),'C'),((6,2),'C'),((8,1),'D'),((8,2),'D')]

ids :: Int -> Int -> [Move] -> Grid -> Int -> (Int,[Move])
-- ids bestCost cost moves g d | trace ("ids " ++ show (bestCost,cost,g,d,moves) ++ "\n" ++ showGrid g) False = undefined
ids bestCost cost moves g d | isSolved g = (cost,moves)
                            | d == 0 = (2^32,moves)
                            | bestCost < cost + heuristicCost = (2^32,moves)
                            | heuristicMoves > d = (2^32,moves)
                            | otherwise = res
  where res = foldr (\(move,moveCost,g') (acc,accMoves) ->
                           if cost + moveCost > acc
                              then (acc,moves)
                              else let (acc',moves') = ids acc (cost+moveCost) (move:moves) g' (d-1)
                                       in if acc' < acc
                                             then (acc',moves')
                                             else (acc,accMoves)
                              ) (bestCost,moves) nextMoves
        nextMoves = validMoves g
        (heuristicCost, heuristicMoves) = minHeuristicMoves g

minHeuristicMoves g = go' g 0
  where go' g cost = foldr (\(c,a) (cost,moves) -> let hc = getHomeCost c a in (cost + hc, moves + if hc > 0 then 1 else 0)) (0,0) (M.toList g)

minHeuristicCost g = go' g 0
  where go' g cost = foldr (\(c,a) acc -> acc + getHomeCost c a) 0 (M.toList g)

isSolved g = and $ zipWith (\i c ->
                            M.lookup (i,1) g == Just c &&
                            M.lookup (i,2) g == Just c) roomCols "ABCD"

validMoves :: Grid -> [(Move,Int,Grid)]
validMoves g = filter (\(_,_,g) -> isLegal g)
                      (concatMap (\c1 -> let a = unitCost (g ! c1)
                                             in map (\c2 -> ((c1,c2), getCost a c1 c2, applyMove g c1 c2)) (validMovesFrom g c1)) positions)
  where positions = M.keys g

getHomeCost c 'A' = min (getCost 1 c (2,1)) (getCost 1 c (2,2))
getHomeCost c 'B' = min (getCost 10 c (4,1)) (getCost 10 c (4,2))
getHomeCost c 'C' = min (getCost 100 c (6,1)) (getCost 100 c (6,2))
getHomeCost c 'D' = min (getCost 1000 c (8,1)) (getCost 1000 c (8,2))

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
tmp x = x

isLegal g = nobodyInDeadSpaces
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
          isCompatible = case M.lookup (i2,2) g of
                              Nothing -> True
                              Just other -> amphipod == other

isRoom (_,j) = j > 0

possibleMovesFrom :: Grid -> Coord -> [Coord]
-- possibleMovesFrom g c | trace ("possibleMovesFrom " ++ show c) False = undefined
possibleMovesFrom g c = filter hallwayToRoom moves
  where moves = possibleMovesFrom' g c c (S.fromList [c]) []
        hallwayToRoom c2 = isRoom c || isRoom c2

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
                    then filter (\(_,y) -> y >= 0 && y <= 2) [(i,j-1),(i,j+1)]
                    else []

roomCols = [2,4,6,8]

inSideColumn = (`elem` roomCols)

showGrid g = intercalate "\n" g'
  where g' = [[c | i <- [0..10],
                   let c = case M.lookup (i,j) g of
                                Nothing -> if j == 0 || inSideColumn i
                                              then '.'
                                              else '#'
                                Just v -> v] | j <- [0..2]]

-- parse grid
-- IDS?
-- determine heuristic cost of current position + all moves possible from here
-- pc = minimum cost of (any) unexplored ancestor position
-- if solved && pc >= current minimum cost, finished
-- else 
getNeighbours x y w h = [(x',y') | (i,j) <- [(-1,0),(0,-1),(1,0),(0,1)],
                                   let x' = x+i,
                                   let y' = y+j,
                                   x' >= 0 && x' < w,
                                   y' >= 0 && y' < h
                        ]

parse (x:y:_) = M.fromList positions
  where rows = concatMap (zip roomCols . filter (`elem` "ABCD")) [x,y]
        positions = zipWith (\j (i,c) -> ((i,j),c)) [1,1,1,1,2,2,2,2] rows

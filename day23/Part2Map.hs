module Main where

import Data.List (sort, intercalate, nub)
import Data.Map (Map, (!), (\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isJust, mapMaybe)

type Coord = (Int,Int)
type Grid = Map Coord Char
type Move = (Coord,Coord)
type Table = Map Grid Int
type WeightQ = S.Set (Int,Grid)

main = do
  txt <- readFile "input.txt"
  let g = parse $ drop 2 $ lines txt
  putStrLn $ showGrid g
  let res = solve g
  print res

solve :: Grid -> Int
solve g = dijkstra (M.insert g 0 mempty) (initialQ g)

initialQ g = S.fromList [(0,g)]

solved :: Grid
solved = M.fromList [((i,j),c) | i <- [2,4,6,8], j <- [1..4], let c = "ABCD" !! ((i `div` 2) - 1)]

dijkstra :: Table -> S.Set (Int,Grid) -> Int
dijkstra ds q
  | S.null q = case M.lookup solved ds of
                    Just c -> c
                    Nothing -> error $ "Nothing. " ++ show (M.size ds)
  | otherwise = dijkstra ds' qUpdated

  where (v,q') = getClosest q
        ns = validMoves v
        newDs = mapMaybe (\(_,ud,u) -> let ud' = ud + getDs ds v
                               in case M.lookup u ds of
                                       Nothing -> Just (ud',u)
                                       Just d | ud' < d -> Just (ud',u)
                                       _ -> Nothing) ns

        -- update distance table
        ds' = foldr addToDists ds newDs
        -- update priority queue
        qUpdated = foldr (\(ud',u) acc -> S.insert (ud',u) acc) q' newDs


addToDists (d,u) = M.insert u d

getClosest :: WeightQ -> (Grid,WeightQ)
getClosest q = case S.minView q of
                       Nothing -> error "Empty vertex queue."
                       Just ((_,v),q') -> (v,q')

getDs ds v = case M.lookup v ds of
                  Just d -> d
                  _ -> error $ "Couldn't find " ++ show v ++ " in dists."

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
  where rows = concatMap (zip roomCols . filter (`elem` "ABCD")) [x,"DCBA","DBAC",y]
        positions = zipWith (\j (i,c) -> ((i,j),c)) [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4] rows

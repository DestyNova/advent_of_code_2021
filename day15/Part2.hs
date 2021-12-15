module Main where

-- Day 15 part 2 using immutable Data.Map and minimumBy over gigantic lists that
-- are generated and discarded at each step. Don't do it like this.

import Text.Parsec
import Data.List (minimumBy, transpose, (\\))
import qualified Data.Map as M
import Data.Map (Map, (!))

type Coord = (Int,Int)
type Weights = Array (Int,Int) Int

main = do
  inp <- lines <$> readFile "input.txt"
  let w = length (head inp) * 5
  let h = length inp * 5
  let g = parseInts inp
  let g' = multiplyGrid g
  let ws = parseGrid g'
  let ds = solve ws w h
  print $ ds ! (w-1,h-1)

parseInts = map (map (read . (:[])))

multiplyGrid :: [[Int]] -> [[Int]]
multiplyGrid xs = concatMap (incAll megaRow) [0..4]
  where megaRow = transpose $ concatMap (incAll (transpose xs)) [0..4]

incAll :: [[Int]] -> Int -> [[Int]]
incAll xs n = [[if r > 9 then r - 9 else r | i <- rs, let r = i + n] | rs <- xs]

solve :: Weights -> Int -> Int -> Weights
solve ws w h = solve' ws (M.insert (0,0) 0 mempty) (M.keys ws) w h

solve' ws ds [] _ _ = ds
solve' ws ds q w h = solve' ws ds' q' w h
  where (v,q') = getClosest ds q
        ns = getNeighbours v w h
        newDs = map (\u -> let ud = (getWs ws u) + (getDs ds v)
                               in case M.lookup u ds of
                                       Nothing -> ud
                                       Just d -> min ud d) ns
        ds' = foldr addToDists ds (zip newDs ns)

addToDists (d,u) m = M.insert u d m

getWs ws u = case M.lookup u ws of
                  Nothing -> error $ "Couldn't find " ++ show u ++ " in weights."
                  Just d -> d

getDs ds v = case M.lookup v ds of
                  Nothing -> error $ "Couldn't find " ++ show v ++ " in dists."
                  Just d -> d

getClosest :: Weights -> [Coord] -> (Coord,[Coord])
getClosest ds q = (v,q')
  where q' = q \\ [v]
        (_,v) = minimumBy (\(a,_) (b,_) -> compare a b) dists
        dists = map (\u -> (ds ! u, u)) knownQs
        knownQs = filter (\u -> M.member u ds) q

getNeighbours (i,j) w h = [(a,b) | (a,b) <- [(i-1,j),(i,j-1),(i+1,j),(i,j+1)],
                                   a >= 0, a < w,
                                   b >= 0, b < h]

parseGrid :: [[Int]] -> Weights
parseGrid rs = M.insert (0,0) 0 (parseGrid' 0 rs mempty)

parseGrid' :: Int -> [[Int]] -> Weights -> Weights
parseGrid' _ [] acc = acc
parseGrid' j (r:rs) acc = parseGrid' (j+1) rs acc'
  where acc' = foldr (\(i,w) m -> M.insert (i,j) w m) acc (zip [0..] r)

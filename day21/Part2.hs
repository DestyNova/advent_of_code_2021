module Main where

import Text.Parsec
import Data.List (group, sort, genericLength, foldl')
import qualified Data.Map as M
import Data.Map (Map)

type Table = Map (Integer,Integer,Integer,Integer,Bool) (Integer,Integer)

main = do
  txt <- readFile "input.txt"
  let (Right (p1,p2)) = parse parser "" txt
  print $ solve p1 p2

solve :: Integer -> Integer -> Integer
solve p1 p2 = let ((u1,u2),_) = solve' p1 p2 0 0 True mempty
                  in max u1 u2

solve' :: Integer -> Integer -> Integer -> Integer -> Bool -> Table -> ((Integer, Integer), Table)
solve' p1 p2 s1 s2 turn dp | s1 >= 21 = ((1,0),dp)
                           | s2 >= 21 = ((0,1),dp)
                           | otherwise = case M.lookup (p1,p2,s1,s2,turn) dp of
                                              Just w -> (w,dp)
                                              Nothing -> (wUpdated, dpUpdated)
  where (wUpdated, dpUpdated) = foldl' (\(wAcc,dpAcc) (m,i) ->
                                       let (w,dp') = update i dpAcc
                                           in (addWins wAcc m w, M.union dpAcc dp')) ((0,0),dp) rolls
        updateP p d = (((p-1) + d) `mod` 10) + 1
        update d dpu = let (p1',p2') = if turn
                                          then (updateP p1 d, p2)
                                          else (p1, updateP p2 d)
                           (s1',s2') = if turn
                                          then (s1 + p1', s2)
                                          else (s1, s2 + p2')
                           turn' = not turn
                           ((r1,r2),dp') = solve' p1' p2' s1' s2' turn' dpu
                           in ((r1,r2), M.insert (p1',p2',s1',s2',turn') (r1,r2) dp')

rolls = map (\xs -> (genericLength xs,head xs)) $ group $ sort [a+b+c | let d = [1,2,3], a <- d, b <- d, c <- d]

addWins (a1,a2) m (b1,b2) = (a1+m*b1,a2+m*b2)

parser :: Parsec String () (Integer,Integer)
parser = do
  p1 <- parsePlayer
  newline
  p2 <- parsePlayer
  return (p1,p2)

fac 1 = 1
fac n = n * fac (n-1)

parsePlayer = do
  string "Player "
  number
  string " starting position: "
  number

number :: Parsec String () Integer
number = read <$> many1 digit

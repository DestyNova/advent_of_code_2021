module Main where

import Text.Parsec
import Data.List (group, sort, intercalate, genericLength, foldl')
import qualified Data.Map as M
import Data.Map (Map)
import Debug.Trace (trace)

type Table = Map (Integer,Integer,Integer,Integer,Bool) (Integer,Integer)

main = do
  txt <- readFile "input.txt"
  let (Right (p1,p2)) = parse parser "" txt
  print $ solve p1 p2

solve :: Integer -> Integer -> Integer
solve p1 p2 = let ((u1,u2),_) = solve' p1 p2 0 0 True mempty
                  in max u1 u2

-- use DP to cache the number of wins for each player for given values of (p1,p2,s1,s2,d)?
-- how do you combine those values though?
--
-- maybe solve naievely first for w = score >= 2
--
-- wins = p1 rolls 1, 1 + p1 r 1, 2 + p1 r 1,3 + p1 r 2 + p1 r 3
--      = (5,0)
--
-- for w = score >= 4
--
-- wins = wins where p1 rolls 1 on t1 + ww p1 rolls 2 on t1 + ww p1 rolls 3 on t1
--      = (p1 rolls 1 on t1 && (p1 rolls 1 on t2 + 
-- solve' p1 p2 s1 s2 d | s1 >= 1000 = (p1,p2,s1,s2,d)
--                      | s2 >= 1000 = (p1,p2,s1,s2,d)
-- want to say "number of wins where s1 == 0, s2 == 0, p1 == X. p2 == Y, turn == 0, rolled a 1"
-- turn 0: 1
-- t 1: 1
-- t 2: 1
-- solve' p1 p2 s1 s2 turn | trace ("solve' " ++ show (p1,p2,s1,s2,turn)) False = undefined
-- win at >=5 = (4383,324)
-- win at >=13 = (152998146,5950827) -- 0.01s
solve' :: Integer -> Integer -> Integer -> Integer -> Bool -> Table -> ((Integer, Integer), Table)
-- solve' p1 p2 s1 s2 turn dp | trace ("solve' " ++ show (p1,p2,s1,s2,turn,M.size dp)) False = undefined
solve' p1 p2 s1 s2 turn dp | s1 >= 21 = ((1,0),dp)
                           | s2 >= 21 = ((0,1),dp)
                           | otherwise = case M.lookup (p1,p2,s1,s2,turn) dp of
                                              Just w -> (w,dp)
                                              --Nothing -> (addWins w1 w2 w3, foldl1 M.union [dp1,dp2,dp3])
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

-- addWins a b c | trace ("addWins " ++ show (a,b,c)) False = undefined
addWins (a1,a2) m (b1,b2) = (a1+m*b1,a2+m*b2)

solvex' p1 p2 s1 s2 d | s1 >= 2 = (1,0)
                     | s2 >= 2 = (0,1)
                     | otherwise = solvex' p1' p2' s1' s2' d'
  where p1' = (((p1-1) + d) `mod` 10) + 1
        p2' = (((p2-1) + 3*(d+3) + 6) `mod` 10) + 1
        s1' = s1 + p1'
        s2' = if s1' >= 1000 then s2 else s2 + p2'
        d' = if s1' >= 1000 then d+3 else d+6

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

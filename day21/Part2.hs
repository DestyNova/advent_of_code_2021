module Main where

import Text.Parsec
import Data.List (sort, intercalate)
import Debug.Trace (trace)

main = do
  txt <- readFile "input.txt"
  let (Right (p1,p2)) = parse parser "" txt
  print $ solve p1 p2

solve p1 p2 = let (u1,u2) = solve' p1 p2 0 0 0
                  in (u1,u2)

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
solve' p1 p2 s1 s2 turn | s1 >= 5 = (1,0)
                        | s2 >= 5 = (0,1)
                        | otherwise = addWins w1 w2 w3
  where w1 = update 1
        w2 = update 2
        w3 = update 3
        updateP p d = (((p-1) + d) `mod` 10) + 1
        update d = let (p1',p2') = case turn of
                                        2 -> (updateP p1 d, p2)
                                        5 -> (p1, updateP p2 d)
                                        _ -> (p1,p2)
                       (s1',s2') = case turn of
                                      2 -> (s1 + p1', s2)
                                      5 -> (s1, s2 + p2')
                                      _ -> (s1,s2)
                       in solve' p1' p2' s1' s2' ((turn+1) `mod` 6)

-- addWins a b c | trace ("addWins " ++ show (a,b,c)) False = undefined
addWins (a1,a2) (b1,b2) (c1,c2) = (a1+b1+c1,a2+b2+c2)

solvex' p1 p2 s1 s2 d | s1 >= 2 = (1,0)
                     | s2 >= 2 = (0,1)
                     | otherwise = solvex' p1' p2' s1' s2' d'
  where p1' = (((p1-1) + d) `mod` 10) + 1
        p2' = (((p2-1) + 3*(d+3) + 6) `mod` 10) + 1
        s1' = s1 + p1'
        s2' = if s1' >= 1000 then s2 else s2 + p2'
        d' = if s1' >= 1000 then d+3 else d+6

parser :: Parsec String () (Int,Int)
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

number :: Parsec String () Int
number = read <$> many1 digit

module Main where

import Text.Parsec
import Data.List (sort, intercalate)

main = do
  txt <- readFile "input.txt"
  let (Right (p1,p2)) = parse parser "" txt
  print $ solve p1 p2

solve p1 p2 = solve' p1 p2 0 0 0

-- solve' p1 p2 s1 s2 d | s1 >= 1000 = (p1,p2,s1,s2,d)
--                      | s2 >= 1000 = (p1,p2,s1,s2,d)
solve' p1 p2 s1 s2 d | s1 >= 1000 = d * s2
                     | s2 >= 1000 = d * s1
                     | otherwise = solve' p1' p2' s1' s2' d'
  where p1' = (((p1-1) + 3*d + 6) `mod` 10) + 1
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

parsePlayer = do
  string "Player "
  number
  string " starting position: "
  number


number :: Parsec String () Int
number = read <$> many1 digit

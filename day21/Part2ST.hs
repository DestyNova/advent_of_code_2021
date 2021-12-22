module Main where

import Text.Parsec
import Data.List (group, sort)
import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((//), (!))
import Data.Array.ST
import Control.Monad.ST
import Data.Bits ((.|.), shiftL)
import Control.Monad (foldM)

type Table s = STArray s Int (Int,Int)
type Score = (Int, Int)

main = do
  txt <- readFile "input.txt"
  let (Right (p1,p2)) = parse parser "" txt
  print $ runST $ solve p1 p2

solve :: Int -> Int -> ST s Int
solve p1 p2 = do
  ds <- newArray (0,2^(4+4+5+5+1)-1) (0,0)
  (u1,u2) <- solve' p1 p2 0 0 True ds
  return $ max u1 u2

solve' :: Int -> Int -> Int -> Int -> Bool -> Table s -> ST s Score
solve' p1 p2 s1 s2 turn dp | s1 >= 21 = return (1,0)
                           | s2 >= 21 = return (0,1)
                           | otherwise = do
  cachedScore <- readArray dp index
  case cachedScore of
                (0,0) -> wUpdated
                _ -> pure cachedScore

  where wUpdated = do
                    (a,b) <- foldM (\wAcc (m,i) -> do
                                updated <- update i
                                return $ addWins wAcc m updated) (0,0) rolls
                    writeArray dp index (a,b)
                    return (a,b)

        index = getIndex p1 p2 s1 s2 turn
        updateP p d = (((p-1) + d) `mod` 10) + 1
        update d = do
          let (p1',p2') = if turn
                             then (updateP p1 d, p2)
                             else (p1, updateP p2 d)
          let (s1',s2') = if turn
                             then (s1 + p1', s2)
                             else (s1, s2 + p2')
          let turn' = not turn
          solve' p1' p2' s1' s2' turn' dp

getIndex p1 p2 s1 s2 turn =
  p1 .|.
  (p2 `shiftL` 4) .|.
  (s1 `shiftL` 8) .|.
  (s2 `shiftL` 13) .|.
  ((if turn then 1 else 0) `shiftL` 18)

rolls = map (\xs -> (length xs,head xs)) $ group $ sort [a+b+c | let d = [1,2,3], a <- d, b <- d, c <- d]

addWins (a1,a2) m (b1,b2) = (a1+m*b1,a2+m*b2)

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

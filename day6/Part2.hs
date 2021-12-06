module Main where

import Text.Parsec
import qualified Data.IntMap as M
import Data.IntMap ((!?))

main = do
  txt <- readFile "input.txt"
  let (Right fish) = parse parser "" txt
  let m = foldr (\x m -> M.insertWith (+) x 1 m) M.empty fish
  print $ run m 256

run m 0 = M.foldr (+) 0 m
run m n = run mReset (n-1)
  where mDec = M.mapKeys pred m
        numKids = case m !? 0 of
                       Nothing -> 0
                       Just n  -> n
        mNew = M.insert 8 numKids mDec
        mReset = M.insertWith (+) 6 numKids (M.delete (-1) mNew)

parser :: Parsec String () [Int]
parser = number `sepBy` char ','

number :: Parsec String () Int
number = read <$> many1 digit

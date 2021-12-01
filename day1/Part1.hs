module Main where

main = do
  txt <- readFile "input.txt"
  let nums = read <$> lines txt
  let n = countIncreases nums
  print n

countIncreases :: [Int] -> Int
countIncreases nums = sum [if x>y then 1 else 0 | (y,x) <- zip nums (tail nums)]

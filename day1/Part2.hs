module Main where

main = do
  txt <- readFile "input.txt"
  let nums = read <$> lines txt
  let sums = slidingSums nums
  let n = countIncreases sums
  print n

countIncreases :: [Int] -> Int
countIncreases nums = sum [if x>y then 1 else 0 | (y,x) <- zip nums (tail nums)]

slidingSums :: [Int] -> [Int]
slidingSums xs = zipWith3 (\a b c -> a+b+c) xs (tail xs) (tail $ tail xs)

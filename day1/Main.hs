module Main where

main = do
  txt <- readFile "input.txt"
  let nums = read <$> lines txt
  print $ head [ x*y*z | x <- nums, y <- nums, z <- nums, x+y+z == 2021 ]

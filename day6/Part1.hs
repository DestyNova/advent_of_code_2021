module Main where

import Text.Parsec

main = do
  txt <- readFile "input.txt"
  let (Right fish) = parse parser "" txt
  print fish
  print $ run fish 80

run xs 0 = length xs
run xs n = run xs'' (n-1)
  where xs'' = map (\x -> if x < 0 then 6 else x) xs' ++ replicate newFish 8
        xs' = map (\x -> x - 1) xs
        newFish = length $ filter (<0) xs'

parser :: Parsec String () [Int]
parser = number `sepBy` char ','

number :: Parsec String () Int
number = read <$> many1 digit

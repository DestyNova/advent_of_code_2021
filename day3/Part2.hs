module Main where

import Data.List (transpose)

main = do
  txt <- lines <$> readFile "input.txt"
  let oxygen = fromBinary $ head $ filterValues (>=) 0 txt
  let scrubber = fromBinary $ head $ filterValues (<) 0 txt
  let lifeSupport = oxygen*scrubber
  print lifeSupport

calcOxygen = filterValues (>=) 0

filterValues p i xs | i >= length (head xs) = xs
                    | length xs == 1 = xs
                    | otherwise = filterValues p (i+1) xs'
  where xs' = filter (\r -> r !! i == keptBit) xs
        keptBit = if ones `p` zeroes then '1' else '0'
        (ones,zeroes) = calcColumn $ map (!! i) xs

calcColumn cs = foldr (\b (os,zs) ->
                       if b == '1'
                          then (os+1,zs)
                          else (os,zs+1)) (0,0) cs

fromBinary = foldl (\acc d -> acc*2 + if d == '1' then 1 else 0) 0

module Main where

import Data.List (transpose)

main = do
  txt <- readFile "input.txt"
  let gamma = calc $ lines txt
  print gamma

calc xs = gamma * epsilon
  where counts = map calcColumn (transpose xs)
        gamma = fromBinary maxes
        epsilon = fromBinary mins
        maxes = map (\(o,z) -> if o > z then 1 else 0) counts
        mins = [if d == 1 then 0 else 1 | d <- maxes]

calcColumn cs = foldr (\b (os,zs) ->
                       if b == '1'
                          then (os+1,zs)
                          else (os,zs+1)) (0,0) cs

fromBinary = foldl (\acc d -> acc*2 + d) 0

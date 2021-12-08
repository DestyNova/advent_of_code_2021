module Main where

import Data.List (elemIndex, findIndex, permutations)
import Data.Maybe (catMaybes)
import Data.Set (fromList, Set)

main = do
  txt <- readFile "input.txt"
  let inp = map (filter (/="|")) $ map words $ lines txt
  print $ sum [n | s <- inp, let [m] = findMapping s, let n = decode s m]

  --  8:     
  --   0000  
  --  1    2
  --  1    2
  --   3333  
  --  4    5 
  --  4    5 
  --   6666  

findMapping :: [String] -> [String]
findMapping inp = filter (isValid inp) allMappings
  where allMappings = permutations "abcdefg"

isValid :: [String] -> String -> Bool
isValid inp m = all (isNum m) inp

isNum :: String -> String -> Bool
isNum m v = any (\p -> (indices m v) == p) digitPatterns

indices :: String -> String -> Set Int
indices m v = fromList $ catMaybes $ map (\c -> elemIndex c m) v

decode :: [String] -> String -> Int
decode inp m = foldl (\a b -> a*10 + b) 0 digits
  where digits = map (decodeDigit m) (drop (length inp - 4) inp)

decodeDigit m code = case findIndex (\d -> (indices m code) == d) digitPatterns of
                          Nothing -> error $ "Unknown code: " ++ code ++ ", patterns: " ++ m
                          Just i -> i

digitPatterns :: [Set Int]
digitPatterns = map fromList [
                              [0,1,2,4,5,6] -- 0
                             ,[2,5]         -- 1
                             ,[0,2,3,4,6]   -- 2
                             ,[0,2,3,5,6]   -- 3
                             ,[1,2,3,5]     -- 4
                             ,[0,1,3,5,6]   -- 5
                             ,[0,1,3,4,5,6] -- 6
                             ,[0,2,5]       -- 7
                             ,[0..6]        -- 8
                             ,[0,1,2,3,5,6] -- 9
                             ]

module Main where

import Data.List (sort)

main = do
  txt <- lines <$> readFile "input.txt"
  let incompletes = filter (not . null) $ map getEndStack txt
  let points = map (getPoints 0) incompletes
  print $ head $ drop (length points `div` 2) $ sort points

getPoints :: Int -> String -> Int
getPoints s [] = s
getPoints s ('(':xs) = getPoints (s*5 + 1) xs
getPoints s ('[':xs) = getPoints (s*5 + 2) xs
getPoints s ('{':xs) = getPoints (s*5 + 3) xs
getPoints s ('<':xs) = getPoints (s*5 + 4) xs

getEndStack s = go s []
  where go [] stack = stack
        go (x:xs) stack | isOpener x = go xs (x:stack)
                        | otherwise = if canClose x stack
                                         then go xs (tail stack)
                                         else []

isOpener = (`elem` "([{<")

canClose ')' ('(':_) = True
canClose ']' ('[':_) = True
canClose '}' ('{':_) = True
canClose '>' ('<':_) = True
canClose _ _ = False

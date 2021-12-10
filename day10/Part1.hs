module Main where

import Data.Maybe (catMaybes)

main = do
  txt <- lines <$> readFile "input.txt"
  let errorChars = catMaybes $ map getErrors txt
  print $ getPoints 0 errorChars

getPoints :: Int -> String -> Int
getPoints s [] = s
getPoints s (')':xs) = getPoints (s+3) xs
getPoints s (']':xs) = getPoints (s+57) xs
getPoints s ('}':xs) = getPoints (s+1197) xs
getPoints s ('>':xs) = getPoints (s+25137) xs

getErrors s = go s []
  where go [] _ = Nothing
        go (x:xs) stack | isOpener x = go xs (x:stack)
                        | otherwise = if canClose x stack
                                         then go xs (tail stack)
                                         else Just x

isOpener = (`elem` "([{<")

canClose ')' ('(':_) = True
canClose ']' ('[':_) = True
canClose '}' ('{':_) = True
canClose '>' ('<':_) = True
canClose _ _ = False

module Main where

import Text.Parsec
import Data.List (sort, group)
import qualified Data.Map as M
import Data.Map (Map, (!))

type Rules = Map String Char

main = do
  txt <- readFile "input.txt"
  let (Right (template,rules)) = parse parser "" txt
  print $ solve 10 template rules

solve 0 template _ = hiFreq - lowFreq
  where lowFreq = head grouped
        hiFreq = last grouped
        grouped = sort $ map length $ group $ sort template

solve n template rules = solve (n-1) (solve' template rules []) rules

solve' :: String -> Rules -> String -> String
solve' [] rules acc = reverse $ tail acc
solve' (x:y:xs) rules [] = solve' (y:xs) rules [y, rules ! [x, y], x]
solve' (x:y:xs) rules acc = solve' (y:xs) rules (y:(rules ! [x,y]):acc)
solve' [x] rules acc = solve' [] rules (x:acc)

parser :: Parsec String () (String, Map String Char)
parser = do
  template <- many1 letter
  newline
  newline
  rules <- rule `sepEndBy1` newline
  return (template, M.fromList rules)

rule = do
  ab <- many1 letter
  string " -> "
  c <- letter
  return (ab,c)

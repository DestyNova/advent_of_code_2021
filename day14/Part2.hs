module Main where

import Text.Parsec
import Data.List (sort, group, nub)
import qualified Data.Map as M
import Data.Map (Map, (!))

type Rules = Map String Char

main = do
  txt <- readFile "input.txt"
  let (Right (template,rules)) = parse parser "" txt
  print $ solve 40 template rules (M.empty)

getCounts freqs = hiFreq - loFreq
  where fs = sort $ map (getCount freqs) (getSyms freqs)
        hiFreq = last fs
        loFreq = head fs

getSyms = nub . concat . M.keys

getCount freqs c = max (f firsts) (f ends)
  where f pos = sum $ map snd $ M.toList $ M.filterWithKey pos freqs
        firsts = (\[a,b] _ -> b == c)
        ends = (\[a,b] _ -> a == c)

solve n template rules freqs = getCounts f'
  where f' = solve' n rules (makePairs template freqs)

solve' :: Int -> Rules -> Map String Integer -> Map String Integer
solve' 0 _ freqs = freqs
solve' n rules freqs = solve' (n-1) rules freqs'
  where freqs' = M.foldrWithKey (\pair num acc ->
                                 foldr
                                 (\p m -> M.insertWith (+) p num m)
                                 acc
                                 (rulePairs pair rules)) mempty freqs

makePairs (x:y:xs) freqs = makePairs (y:xs) (M.insertWith (+) [x,y] 1 freqs)
makePairs _ freqs = freqs

rulePairs [x,y] rules = [[x,c],[c,y]]
  where c = rules ! [x,y]

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

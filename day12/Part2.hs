module Main where

import Text.Parsec
import Data.Char (isLower)
import Data.List (nub)
import qualified Data.Map as M
import Data.Map ((!))

type Graph = M.Map String [String]

main = do
  txt <- readFile "input.txt"
  let (Right g) = parse parser "" txt
  print $ dfs g

dfs = dfs' "start" []

dfs' "end" _ _ = 1
dfs' from smalls g | cannotVisit from smalls = 0
                     | otherwise = sum paths
  where paths = map (\to -> dfs' to smalls' g) destinations
        destinations = g ! from
        smalls' = if smallCave from
                     then from:smalls
                     else smalls

cannotVisit from smalls = seenTwice || (seenOnce && (from == "start" || anySeenTwice))
  where seenTwice = seen == 2
        seenOnce = seen == 1
        seen = length (filter (==from) smalls)
        anySeenTwice = length smalls > length (nub smalls)

smallCave = all isLower

parser :: Parsec String () Graph
parser = do
  edges <- (do
           from <- many1 letter
           char '-'
           to <- many1 letter
           return [(from,to), (to,from)]) `sepEndBy1` newline
  return $ foldr (\(from,to) m -> M.insertWith (++) from [to] m) mempty $ concat edges

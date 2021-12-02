module Main where

import Text.Parsec

data Dir = Forward | Down | Up deriving (Show,Eq)

main = do
  txt <- readFile "input.txt"
  let (Right dirs) = parse parser "" txt
  let pos = calcPos dirs
  print pos

calcPos = uncurry (*) . foldr (\(dir,n) (h,d) -> case dir of
                     Forward -> (h+n,d)
                     Down -> (h,d+n)
                     Up -> (h,d-n)) (0,0)

parser :: Parsec String () [(Dir,Int)]
parser = do
  (do
    dir <- getDir <$> many1 letter
    space
    n <- number
    return (dir,n)) `sepEndBy` newline

number = read <$> many1 digit

getDir "forward" = Forward
getDir "down" = Down
getDir "up" = Up
getDir s = error $ "Invalid direction specified: " ++ s

module Main where

import Text.Parsec

data Dir = Forward | Down | Up deriving (Show,Eq)

main = do
  txt <- readFile "input.txt"
  let (Right dirs) = parse parser "" txt
  print $ calcPos dirs

calcPos dirs = h*d
  where (h,d,a) = foldl (\(h,d,a) (dir,n) -> case dir of
                        Forward -> (h+n,d+n*a,a)
                        Down -> (h,d,a+n)
                        Up -> (h,d,a-n)) (0,0,0) dirs

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

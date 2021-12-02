module Main where

import Text.Parsec

main = do
  txt <- readFile "input.txt"
  --putStrLn txt
  let (Right dirs) = parse parser "" txt
  --print dirs
  let pos = calcPos dirs
  print pos

calcPos dirs = h*d
  where (h,d,a) = foldl (\(h,d,a) (dir,n) -> case dir of
                        "forward" -> (h+n,d+n*a,a)
                        "down" -> (h,d,a+n)
                        "up" -> (h,d,a-n)) (0,0,0) dirs

parser :: Parsec String () [(String,Int)]
parser = do
  (do
    dir <- many1 letter
    space
    n <- number
    return (dir,n)) `sepEndBy` newline

number = read <$> many1 digit
--number = many1 digit

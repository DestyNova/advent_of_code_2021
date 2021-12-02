module Main where

import Text.Parsec

main = do
  txt <- readFile "input.txt"
  putStrLn txt
  let (Right dirs) = parse parser "" txt
  print dirs
  let pos = calcPos dirs
  print pos

calcPos = uncurry (*) . foldr (\(dir,n) (h,d) -> case dir of
                     "forward" -> (h+n,d)
                     "down" -> (h,d+n)
                     "up" -> (h,d-n)) (0,0)

parser :: Parsec String () [(String,Int)]
parser = do
  (do
    dir <- many1 letter
    space
    n <- number
    return (dir,n)) `sepEndBy` newline

number = read <$> many1 digit
--number = many1 digit

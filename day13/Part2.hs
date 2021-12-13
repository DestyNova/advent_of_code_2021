module Main where

import Text.Parsec
import Data.List (sort, intercalate)

data Fold = FoldUp Int | FoldLeft Int deriving (Eq, Show)
type Coord = (Int,Int)
type Grid = [[Char]]

main = do
  txt <- readFile "input.txt"
  let (Right (coords,folds)) = parse parser "" txt
  print folds
  let g = makeGrid coords
  let g' = solve g folds
  putStrLn $ printGrid g'
  print $ countDots g'

printGrid = intercalate "\n"

makeGrid :: [Coord] -> Grid
makeGrid coords = [[ if (j,i) `elem` sorted then '#' else '.' | i <- [0..maxX]] | j <- [0..maxY]]
  where sorted = sort $ map (\(a,b) -> (b,a)) coords
        maxX = maximum $ map snd sorted
        maxY = maximum $ map fst sorted

countDots g = length $ filter (=='#') $ concat g

solve :: Grid -> [Fold] -> Grid
solve g [] = g
solve g (f:fs) = solve (solve' g f) fs

solve' :: Grid -> Fold -> Grid
solve' g (FoldLeft x) = overlay l r
  where l = map (take x) g
        r = map (take x . reverse) g
solve' g (FoldUp x) = overlay h l
  where h = take x g
        l = reverse $ drop (x+1) g

overlay :: Grid -> Grid -> Grid
overlay l r = zipWith (\a b -> combineRows a b) l r

combineRows :: [Char] -> [Char] -> [Char]
combineRows l r = zipWith (\a b -> case (a,b) of
                          ('#',_) -> '#'
                          (_,'#') -> '#'
                          _ -> '.') l r

getNeighbours x y w h = [(x',y') | (i,j) <- [(-1,0),(0,-1),(1,0),(0,1)],
                                   let x' = x+i,
                                   let y' = y+j,
                                   x' >= 0 && x' < w,
                                   y' >= 0 && y' < h
                        ]

parser :: Parsec String () ([Coord], [Fold])
parser = do
  coords <- (do
            [x,y] <- number `sepBy1` char ','
            return (x,y)) `sepEndBy1` newline
  newline
  folds <- foldInstruction `sepEndBy1` newline
  return (coords, folds)

foldInstruction = do
  string "fold along "
  dir <- string "x=" <|> string "y="
  v <- number

  return $ case dir of
           "y=" -> FoldUp v
           _ -> FoldLeft v

number :: Parsec String () Int
number = read <$> many1 digit

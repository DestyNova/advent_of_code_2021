module Main where

import Text.Parsec
import Data.List (nub, sort, intersect, sortBy, intercalate)

type Coord = (Int,Int,Int)
type Scanner = [[Coord]]

main = do
  txt <- readFile "input.txt"
  let (Right inp) = parse parser "" txt

  let res = findOverlaps inp
  print res
  print $ maxManhattan res

maxManhattan xs = maximum [d | a <- xs, b <- xs, let d = manhattan a b]

manhattan (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)

findOverlaps scanners = match (head scanners) allOrientations
  where allOrientations = map spin (tail scanners) -- don't spin scanner 0

match :: [Coord] -> [Scanner] -> [Coord]
match root scanners = match' [sortedRoot] scanners []
  where sortedRoot = sort root

match' [] _ acc = error "Ran out of root nodes"
match' roots [] acc = acc
match' roots scanners acc = match' roots' scanners' acc'
  where acc' = loc : acc
        roots' = root' : roots
        (i,loc,root') = transformed
        transformed = matchAny roots scanners
        scanners' = dropElem i scanners

showRoots [] _ = ""
showRoots (r:rs) i = "--- root " ++ show i ++ "---\n" ++ intercalate "\n" (map show r) ++ "\n\n" ++ showRoots rs (i+1)

matchAny [] scanners = error ("Ran out of roots to match against " ++ show (length scanners) ++ " scanners")
matchAny (r:rs) scanners = case takeFirst (getMatchingBeacons r) 0 scanners of
                                (_,_,[]) -> matchAny rs scanners
                                res -> res

takeFirst f _ [] = (0,(0,0,0),[])
takeFirst f i (x:xs) = case f x of
                            (_,[]) -> takeFirst f (i+1) xs
                            (loc,m) -> (i,loc,m)

getMatchingBeacons root scannerFlips = (loc,res)
  where (loc,res) = case filter (not . null . snd) $ map (match1 root) scannerFlips of
                         [] -> ((0,0,0),[])
                         m -> head m

findNewRoot transformed = transformed

dropElem i scanners = take i scanners ++ drop (i+1) scanners

match1 bs scanner = match1' bs scanner scanner

-- return points seen by the scanner, transformed to scanner 0's POV
match1' bs _ _ | length bs < 12 = ((0,0,0),[])
match1' (b:bs) scanner s' | length shared >= 12 = (diff, transformed) -- return all transformed points
                          | length s' > 12 = match1' (b:bs) scanner (tail s')
                          | otherwise = match1' bs scanner scanner
  where
        shared = (b:bs) `intersect` transformed
        transformed = map (`subCoord` diff) scanner
        diff = b1 `subCoord` b
        b1 = head s'

subCoord (x,y,z) (i,j,k) = (x-i, y-j, z-k)

spin scanner = map (\f -> sort $ map f scanner) cubeRotations

cubeRotations = concat [t1,t2,t3,t4,t5,t6]
  where t1 = repeatF 4 xTurn
        t2 = map (. zTurn) t1
        t3 = map (. (zTurn . zTurn)) t1
        t4 = map (. (zTurn . zTurn . zTurn)) t1
        t5 = map (. yTurn) t1
        t6 = map (. (yTurn . yTurn . yTurn)) t1

repeatF n f = scanl (\acc _ -> f . acc) id [0..(n-2)]

xTurn (x,y,z) = (x,z,-y)
yTurn (x,y,z) = (-z,y,x)
zTurn (x,y,z) = (y,-x,z)

parser :: Parsec String () [[Coord]]
parser = do
  parseScanner
  ((do
    [x,y,z] <- number `sepBy1` char ','
    return (x,y,z)) `endBy` newline) `sepBy` (newline >> parseScanner)

parseScanner = do
  string "--- scanner "
  number
  string " ---"
  newline

number :: Parsec String () Int
number = read <$> many1 (digit <|> char '-')

module Main where

import Text.Parsec
import Data.List (sort, intercalate, tails, foldl')
import Debug.Trace (trace)

type Coord = (Integer,Integer,Integer)
type Range = (Integer,Integer)
type Region = (Range,Range,Range)
type Flip = (Bool,Region)

main = do
  txt <- readFile "input.txt"
  -- txt <- readFile "sample3.txt"
  let (Right flips) = parse parser "" txt
  print flips
  let g' = solve flips
  print $ countOn g'

solve :: [Flip] -> [Region]
solve = applyFlips []

applyFlips :: [Region] -> [Flip] -> [Region]
applyFlips rs [] = rs
applyFlips rs ((v,rangeSpec):fs) = applyFlips rs' fs
  where rs' = if v
                 then addRegion rangeSpec rs
                 else subtractRegion rangeSpec rs

addRegion (xr,yr,zr) rs = if containsRegion r rs
                             then rs
                             else r:rs
  where r = (maxMin xr,maxMin yr,maxMin zr)

containsRegion (xr,yr,zr) rs =
  or [within xr xr' && within yr yr' && within zr zr' | (xr',yr',zr') <- rs]

within (a1,a2) (b1,b2) = a1 >= b1 && a2 <= b2

subtractRegion range = concatMap (subtractRegion' range)

subtractRegion' :: Region -> Region -> [Region]
-- subtractRegion' d r | getIntersections d r /= 0 && trace ("subtractRegion' " ++ show (d,r,getIntersections d r)) False = undefined
subtractRegion' d@((xc,xd),(yc,yd),(zc,zd)) r@((xa,xb),(ya,yb),(za,zb)) =
  if overlaps == 0
     then [r]
     else keepNonEmpty [fl,fr,di,ui,fi,bi]

  where overlaps = getIntersections d r
        fl = ((xa, min (xc-1) xb),(ya,yb),(za,zb))
        fr = ((max (xd+1) xa, xb),(ya,yb),(za,zb))
        di = ((max xa xc,min xb xd),(ya, min (yc-1) yb),(za,zb))
        ui = ((max xa xc,min xb xd),(max (yd+1) ya, yb),(za,zb))
        fi = ((max xa xc,min xb xd),(max ya yc,min yb yd),(za, min (zc-1) zb))
        bi = ((max xa xc,min xb xd),(max ya yc,min yb yd),(max (zd+1) za, zb))

keepNonEmpty = filter (\((xa,xb),(ya,yb),(za,zb)) -> xa <= xb && ya <= yb && za <= zb)

maxMin (a,b) | a <= b = (a,b)
             | otherwise = (b,a)

countOn rs | trace ("countOn " ++ show rs) False = undefined
countOn rs = foldCount 0 rs

-- foldCount c rs | trace ("foldCount " ++ show (c,rs)) False = undefined
foldCount c [] = c
foldCount c (r@((x1,x2),(y1,y2),(z1,z2)):rs) = foldCount ((x2-x1+1)*(y2-y1+1)*(z2-z1+1) + c) rs'
  where rs' = subtractRegion r rs

getIntersections :: Region -> Region -> Integer
-- getIntersections r1 r2 | trace ("getIntersections " ++ show (r1,r2)) False = undefined
getIntersections ((xa,xb),(ya,yb),(za,zb)) ((xc,xd),(yc,yd),(zc,zd)) =
  max (min (xd+1) (xb+1) - max xc xa) 0 *
  max (min (yd+1) (yb+1) - max yc ya) 0 *
  max (min (zd+1) (zb+1) - max zc za) 0

parser :: Parsec String () [Flip]
parser = parseFlip `endBy1` newline

parseFlip = do
  action <- try (string "off") <|> string "on"
  let a = case action of
               "off" -> False
               _ -> True
  char ' '
  [xr,yr,zr] <- parseRange `sepBy` char ','
  return (a, (xr,yr,zr))

parseRange = do
  letter
  char '='
  n1 <- number
  string ".."
  n2 <- number
  return (n1,n2)

number :: Parsec String () Integer
number = read <$> many1 (digit <|> char '-')

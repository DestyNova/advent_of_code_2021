module Main where

import Text.Parsec
import Data.List (sort, intercalate)

main = do
  txt <- getContents
  let inp = concatMap hex2bin txt
  print $ solve inp Nothing

solve [] _ = (0,[])
solve (v1:v2:v3:t1:t2:t3:xs) totalBytes =
  case totalBytes of
       Nothing -> (version + solvedInner, xs')
       Just k | k > bitsParsed -> let (recSolve, recXs) = solve xs' (Just $ k - bitsParsed)
                                      in (version + solvedInner + recSolve, recXs)
  where version = bin2dec [v1,v2,v3]
        typeId = bin2dec [t1,t2,t3]
        (solvedInner,xs') = solve' typeId xs (fmap (\bc -> bc - 6) totalBytes)
        bitsParsed = length xs' - (6 + length xs) -- yes it sucks
solve xs _ = (0,xs)

solve' _ [] _ = (0,[])
solve' typeId (lengthType:xs) totalBytes
  | typeId == 4 = parseLiteral (lengthType:xs) 0
  | lengthType == '0' = solveBits (drop 15 xs) (Just bitLength)
  | otherwise = foldr (\_ (acc,bs) -> let (v,bs') = solve bs Nothing
                                          in (acc+v,bs')) (0,drop 11 xs) [0..packetsToParse]
  where
        bitLength = bin2dec $ take 15 xs
        packetsToParse = bin2dec $ take 11 xs

solveBits :: [Char] -> Maybe Int -> (Int, [Char])
solveBits xs bitLength = solve xs bitLength

parseLiteral :: [Char] -> Int -> (Int,[Char])
parseLiteral (a:b:c:d:e:xs) acc | a == '1' = parseLiteral xs acc'
                                | otherwise = (acc',xs)
  where acc' = 0 -- oops... acc*16 + bin2dec [b,c,d,e]
parseLiteral xs acc = (acc,xs)

bin2dec = foldl (\acc d -> acc*2 + if d == '1' then 1 else 0) 0

hex2bin c = case c of
                 '0' -> "0000"
                 '1' -> "0001"
                 '2' -> "0010"
                 '3' -> "0011"
                 '4' -> "0100"
                 '5' -> "0101"
                 '6' -> "0110"
                 '7' -> "0111"
                 '8' -> "1000"
                 '9' -> "1001"
                 'A' -> "1010"
                 'B' -> "1011"
                 'C' -> "1100"
                 'D' -> "1101"
                 'E' -> "1110"
                 'F' -> "1111"
                 '\n' -> ""
                 _ -> error "Not hex input: " ++ [c]

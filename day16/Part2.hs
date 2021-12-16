module Main where

import Text.Parsec
import Data.List (sort, intercalate)

main = do
  txt <- getContents
  let inp = concatMap hex2bin txt
  print $ solve inp Nothing

solve :: [Char] -> Maybe Int -> ([Int], [Char])
solve [] _ = ([],[])
solve (v1:v2:v3:t1:t2:t3:xs) totalBits =
  case totalBits of
       Just k | k > bitsParsed -> let (recSolve, recXs) = solve xs' (Just $ k - bitsParsed)
                                      in (solvedInner : recSolve, recXs)
       Just k | k > bitsParsed -> error ("Parsed too many bits: " ++ show (bitsParsed,k))
       _ -> ([solvedInner], xs')
  where version = bin2dec [v1,v2,v3]
        typeId = bin2dec [t1,t2,t3]
        (solvedInner,xs') = solve' typeId xs (fmap (\bc -> bc - 6) totalBits)
        bitsParsed = 6 + (length xs - length xs') -- yes it sucks
solve xs _ = ([],xs)

solve' :: Int -> [Char] -> Maybe Int -> (Int, [Char])
solve' _ [] _ = error "empty solve' call"
solve' typeId (lengthType:xs) totalBits
  | typeId == 4 = parseLiteral (lengthType:xs) 0
  | otherwise =
      let (results,xs') =
                  case lengthType of
                       '0' -> solve (drop 15 xs) (Just bitLength)
                       '1' -> foldl (\(acc,bs) _ ->
                                     let (v,bs') = solve bs Nothing
                                     in (acc ++ v,bs')) ([],drop 11 xs) [0..packets-1]
                in (combine results,xs')
  where
        bitLength = bin2dec $ take 15 xs
        packets = bin2dec $ take 11 xs
        combine = getCombiner typeId

getCombiner :: Int -> [Int] -> Int
getCombiner 0 = sum
getCombiner 1 = product
getCombiner 2 = minimum
getCombiner 3 = maximum
getCombiner 5 = \[x,y] -> if x > y then 1 else 0
getCombiner 6 = \[x,y] -> if x < y then 1 else 0
getCombiner 7 = \[x,y] -> if x == y then 1 else 0

parseLiteral :: [Char] -> Int -> (Int,[Char])
parseLiteral (a:b:c:d:e:xs) acc | a == '1' = parseLiteral xs acc'
                                | otherwise = (acc',xs)
  where acc' = acc*16 + bin2dec [b,c,d,e]
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

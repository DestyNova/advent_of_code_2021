module Main where

import Text.Parsec

data SnailNum = Pair SnailNum SnailNum | RegNum Int deriving (Eq,Show)

main = do
  txt <- getContents
  let (Right nums) = parse parser "" txt
  print nums
  putStrLn "--------"
  print $ solve nums

magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b
magnitude (RegNum x) = x

solve nums = foldl (\mx i ->
                    maximum (mx:map (combinedMagnitude (nums !! i)) (drop (i+1) nums)))
                    0 [0..length nums]

combinedMagnitude a b = max ma mb
  where ma = magnitude $ reduce $ Pair a b
        mb = magnitude $ reduce $ Pair b a

reduce n = split $ explode n

explode n = case explode' 0 0 n of
                 (_, Nothing, n') -> n
                 (i, Just (a, b), n') -> reduce $ snd $ increment i 0 (a,b) n'

explode' :: Int -> Int -> SnailNum -> (Int, Maybe (Int,Int), SnailNum)
explode' d i (Pair (RegNum a) (RegNum b)) | d == 4 = (i, Just (a,b), RegNum 0)
explode' d i a@(RegNum _) = (i+1, Nothing, a)
explode' d i (Pair a b) | updA /= Nothing = aResult
                        | otherwise = bResult
  where aResult = (ia, updA, Pair a' b)
        bResult = (ib, updB, Pair a b')
        (ia, updA, a') = explode' (d+1) i a
        (ib, updB, b') = explode' (d+1) ia b

increment c i (lt,rt) (RegNum a) | (c-1) == i = (i+1,RegNum (a+lt))
                                 | c+1 == i = (i+1,RegNum (a+rt))
                                 | otherwise = (i+1, RegNum a)
increment c i u@(lt,rt) (Pair a b) = (ib,Pair a' b')
  where (ia,a') = increment c i u a
        (ib,b') = increment c ia u b

split n = if didSplit
             then reduce x
             else x
  where (didSplit,x) = split' n

split' n@(RegNum x) | x >= 10 = (True, Pair (RegNum $ x `div` 2) (RegNum $ (x+1) `div` 2))
                    | otherwise = (False, n)
split' (Pair a b) = (didSplit', Pair a' b')
  where (didSplit, a') = split' a
        (didSplit', b') = if didSplit
                             then (True, b)
                             else split' b


parser :: Parsec String () [SnailNum]
parser = parseSnailNum `sepEndBy` newline

parseSnailNum = try parsePair <|> (RegNum <$> number)

parsePair = do
  char '['
  a <- parseSnailNum
  char ','
  b <- parseSnailNum
  char ']'
  return $ Pair a b

number :: Parsec String () Int
number = read <$> many1 digit

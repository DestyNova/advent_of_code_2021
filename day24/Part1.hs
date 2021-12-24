module Main where

import Text.Parsec
import Data.List (maximumBy)
import Debug.Trace (trace)
import Data.Ord (comparing)

data Val = Const Int | Reg Char deriving (Eq,Show)
data Insn = Inp Val | Add Val Val | Mul Val Val | Div Val Val | Mod Val Val | Eql Val Val deriving (Eq,Show)
data ALU = ALU { stateW :: Int, stateX :: Int, stateY :: Int, stateZ :: Int } deriving (Eq,Show)

main = do
  txt <- readFile "input.txt"
  let (Right prog) = parse parser "" txt
  -- print prog
  -- print $ solve prog
  print $ testBacktracking prog

testBacktracking ps = search ps [0] [9]

search :: [Insn] -> [Int] -> [Int] -> [Int]
search _ _ _ = error "Couldn't get this working properly..."
--search [] _ acc = acc
-- search ((c1,c2):ps) zs (d:acc) = res
--   where res = case (d,findWZ c1 c2 zs) of
--                 (0,[]) -> []
--                 (_,[]) -> search ((c1,c2):ps) zs ((d-1):acc)
--                 (_,ns) -> search ps (map snd $ filter ((==d) . fst) ns) (9:d:acc)

readProg = do
  txt <- readFile "input.txt"
  let (Right prog) = parse parser "" txt
  return prog

testRun prog inp = do
  return $ run prog inp

testLoad = do
  prog <- readProg
  return $ extract prog []

-- solve prog = foldl (\(acc,prev) (c1,c2) -> let res = findWZ c1 c2 [prev]
--                                                (d,z) = maximumBy (comparing fst) res
--                                                in (d:acc,z)) ([],0) nums
--   where nums = take 5 $ extract prog []

extract [] acc = acc
extract prog acc = extract prog' acc'
  where ((Add _ (Const a)):d1) = drop 5 prog
        ((Add _ (Const b)):d2) = drop 9 d1
        prog' = drop 2 d2
        acc' = (a,b):acc

-- solve prog = solve' prog 1

solve' prog n | n < 10 = if z == 0
                            then (state,n)
                            else solve' prog (n+1)
  where state = tmp $ run prog (show n)
        z = stateZ state

solve' prog n = error "Failed to find solution."

tmp x | trace ("state: " ++ show x) False = undefined
      | otherwise = x

-- ps <- testLoad
findWZ c1 c2 zb zc = [(w,z) | w <- [1..9],
                             z2 <- zc,
                             z <- [cz*zb + i|let cz = z2, i <- [0..25]],
                             checkW w z c1 c2 `elem` zc]

findWZold c1 c2 zb zs = [(w, z) | w <- [1 .. 9],
                            z <- [zb*i| i <- [1..26]],
                            let res = checkW w z c1 c2,
                            res `elem` zs]

-- findWZ' c1 c2 zs = [(w, lower,upper) | let mz = minimum zs,
--                             w <- [1 .. 9],
--                             let (lower,upper) = getBounds w c1 c2 100 -- checkW w z c1 c2,
--                   ]
--                             -- res `elem` zs]

-- getBounds w c1 c2 range = getBounds' w c1 c2 0
-- 
-- getBounds' w c1 c2 n | valid n && not (valid (n+1)) = n
--                      | valid n = getBounds' w c1 c2 (n*2)

checkLevel :: [(Int,Int)]
checkLevel = [(w,z) | w <- [1..9], z2 <- [15..23], z <- [cz*26 + i|let cz = z2, i <- [0..25]], checkW w z (-16) 10 `elem` [15..23]]

checkW w z c1 c2 = z'
  where x = if (z `mod` 26 + c1) == w then 0 else 1
        z' = (z `div` 26) * (25 * x + 1) + x*(w + c2)

run prog = run' prog initialState

initialState = ALU { stateW = 0, stateX = 0, stateY = 0, stateZ = 0 }

run' [] s _ = s
run' (insn:rs) s inps = run' rs s' inps'
  where (s',inps') = case insn of
                          Add a b -> (setReg a (getVal a + getVal b), inps)
                          Mul a b -> (setReg a (getVal a * getVal b), inps)
                          Div a b -> (setReg a (safeDiv (getVal a) (getVal b)), inps)
                          Mod a b -> (setReg a (getVal a `mod` getVal b), inps)
                          Eql a b -> (setReg a (if getVal a == getVal b then 1 else 0), inps)
                          Inp a ->   (setReg a (read [head inps]), tail inps)

        setReg (Reg 'w') a = s { stateW = a }
        setReg (Reg 'x') a = s { stateX = a }
        setReg (Reg 'y') a = s { stateY = a }
        setReg (Reg 'z') a = s { stateZ = a }

        getVal (Const x) = x
        getVal (Reg 'w') = stateW s
        getVal (Reg 'x') = stateX s
        getVal (Reg 'y') = stateY s
        getVal (Reg 'z') = stateZ s

safeDiv :: Int -> Int -> Int
safeDiv a b = if a*b < 0
                 then negate $ abs a `div` abs b
                 else a `div` b

parser :: Parsec String () [Insn]
parser = parseInstruction `sepEndBy` newline

parseInstruction = do
  op <- choice $ map (try . string) ["inp","add","mul","div","mod","eql"]
  char ' '
  args <- parseVal `sepBy` char ' '

  return $ case (op,args) of
                ("inp",[a]) -> Inp a
                ("add",[a,b]) -> Add a b
                ("mul",[a,b]) -> Mul a b
                ("div",[a,b]) -> Div a b
                ("mod",[a,b]) -> Mod a b
                ("eql",[a,b]) -> Eql a b

parseVal = do
  v <- optionMaybe number
  case v of
       Just n -> return $ Const n
       _ -> Reg <$> letter

number :: Parsec String () Int
number = read <$> many1 (digit <|> char '-')

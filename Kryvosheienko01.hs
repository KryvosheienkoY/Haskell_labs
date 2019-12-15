{-# OPTIONS_GHC -Wall #-}
module Kryvosheienko01 where

-- Task 1 -----------------------------------------
power3 :: [Integer]
power3 = [x^(3::Integer)|x<-[(1::Integer)..]]

-- Task 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [(3::Integer)^x|x<-[(1::Integer)..]]

-- Task 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [(3::Integer)^i | i<-[(1::Integer)..n]]

-- Task 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m^i | i<-[1..n], m>=0]

-- Task 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe lst = [countLess x lst | x <- lst] 

countLess :: Int -> [Int] -> Int
countLess n lst = sum [if x < n then 1 else 0 | x <- lst]
 
-- Task 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency xs = unique[(x,(length.filter (==x)) xs ) | x<-xs]

unique :: [(Int,Int)] -> [(Int,Int)]
unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)

-- Task 7 -----------------------------------------
hailstone :: Int -> Int
hailstone x = if even x then x `div` 2 else x*3+1

-- Task 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq 1 = [1]
hailSeq n = n : hailSeq (hailstone n)

-- Task 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x| x<-[(1::Int)..]]

-- Task 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = mySeq l allHailSeq

mySeq :: Int -> [[Int]] -> Int
mySeq l x
                |length (head x)==l = (head (head x))
                |otherwise           = mySeq l (tail x )
                


type Expr = [Term]
type Term = [String]

build :: String -> [Expr]
--build ds = concat [ breaks xss | xss <-breaks ds]
--build ds = concatMap breaks (breaks ds)
build = concatMap breaks . breaks

breaks :: [a] -> [[[a]]]
breaks [] = [[[]]]
breaks [v] = [[[v]]]
breaks (x:xs) = let xsss = breaks xs 
                    ysss1 = map(\(ys:yss) -> [x]:(ys:yss)) xsss
                    ysss2 = map(\(ys:yss) -> (x:ys):yss) xsss
                in  ysss1 ++ ysss2
                
eval :: Expr -> Int
eval = sum . map evalT where evalT = product . map read

find :: Int -> String -> [Expr]
find v ds = filter (\e -> eval e == v) (build ds)
showT :: Term -> String
showT = tail . concatMap('*':)
  
showE :: Expr -> String
showE = tail . concatMap (('+':) . showT)
  
findG :: Int -> String -> [String]
findG v = map showE . find v
  
  
  
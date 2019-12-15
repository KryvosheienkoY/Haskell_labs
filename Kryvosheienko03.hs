{-# OPTIONS_GHC -Wall #-}
module Kryvosheienko03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] xs = True
isPrefix (x:xs) [] = False
isPrefix (b:bs) (x:xs) = (b == x) && isPrefix bs xs

-- Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (x, y, _) i w = let a = take i w 
                               b = if(isPrefix x (drop i w)) then y++drop (i+length x) w else drop i w
                           in a++b 
-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)]
findPosition str (a, b, c) =filter (\(_, x) -> x > (-1)) [if isPrefix a (drop n str) then ((a, b, c), n)else ((a, b, c), -1)| n <- [0 .. length str]]


-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]
findAll algo w = concatMap(findPosition w) algo

-- Задача 5 ------------------------------------

first :: Substitution -> String
first (f, s, t) = f
second :: Substitution -> String
second (f, s, t) = s
third :: Substitution -> Bool
third (f, s, t) = t

stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (bt, st, word) =(not (third (fst (head (findAll algo word)))), st+1, changeStr (second (fst (head (findAll algo word)))) (first (fst (head (findAll algo word)))) word)

changeStr :: String -> String -> String -> String
changeStr xs [] ls = xs++ls
changeStr xs (y:ys) (l:ls) |l==y = changeStr xs ys ls
                         |l/=y = l : changeStr xs (y:ys) ls
changeStr _ _ _ = ""

-- Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String
evalA _ 0 _ = Nothing
evalA algo m word =
  if getFirstConfigA (stepA algo (False, 0, word))
    then evalA algo (m - 1) (getThirdConfigA (stepA algo (False, 0, word)))
    else Just (getThirdConfigA (stepA algo (False, 0, word)))

getFirstConfigA::ConfigA->Bool
getFirstConfigA (first, _, _)= first
getThirdConfigA::ConfigA->String
getThirdConfigA (_, _, third) = third

-- Задача 7 ------------------------------------
maximReg :: Program -> Int 
maximReg pr = maxNumber pr 0

maxNumber :: [Command]->Int->Int
maxNumber [] n = n
maxNumber (Z a:xs) n = maxNumber xs (max a n)
maxNumber (S b:xs) n = maxNumber xs (max b n)
maxNumber (T _ c:xs) n = maxNumber xs (max c n)
maxNumber (J {}:xs) n = maxNumber xs n

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int]
ini pr ir= if(length ir<maximReg pr) then ini pr (ir++[0]) else ir

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v= take r reg ++ [v] ++ drop (r + 1) reg

-- Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
stepC pr (nm, st, rg)= countReg (pr!!(nm-1)) (nm, st, rg)

countReg:: Command ->ConfigC -> ConfigC
countReg (Z a) reg = (getFirstConfigC reg+1, getSecondConfigC reg+1, upd (getListConfigC reg) (a-1) 0)
countReg (S a) reg = (getFirstConfigC reg+1, getSecondConfigC reg+1, upd (getListConfigC reg) (a-1) (getListConfigC reg!!(a-1)+1))
countReg (T a b) reg = (getFirstConfigC reg+1, getSecondConfigC reg+1, upd (getListConfigC reg) (b-1) (getListConfigC reg!!(a-1)))
countReg (J a b c) reg = if (getListConfigC reg!!(a-1))==(getListConfigC reg!!(b-1)) then (c, getSecondConfigC reg+1, getListConfigC reg) else (getFirstConfigC reg+1, getSecondConfigC reg+1, getListConfigC reg)

getFirstConfigC:: ConfigC->Int
getFirstConfigC(a, _, _)= a

getSecondConfigC:: ConfigC->Int
getSecondConfigC(_, b, _)= b

getListConfigC:: ConfigC->[Int]
getListConfigC(_, _, c)= c


-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int
evalC _ 0 _ = Nothing
evalC pr mx ir =
  if getFirstConfigC (stepC pr (0, 0, ir)) <= mx
    then evalC pr (mx - 1) (getListConfigC (stepC pr (0, 0, ir)))
    else Just (head (getListConfigC (stepC pr (0, 0, ir))))
--



---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ]

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ]
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False)
          , ("db", "bd", False)
          , ("d", "", True)
          , ("caa", "aca", False)
          , ("cab", "bca", False)
          , ("cba", "acb", False)
          , ("cbb", "bcb", False)
          , ("", "c", False)
          ]

-- добуток натуральних чисел
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1]

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]













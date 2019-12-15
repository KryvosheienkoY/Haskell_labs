{-# OPTIONS_GHC -Wall #-}
module Kryvosheienko04 where

import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG str = s str == Just "" 

s :: String -> Maybe String
s ls |null ls = Nothing
     |head(ls) =='b' = Just (tail ls)
     |head(ls) =='a'=let r= match 'b' (s (tail ls))
                    in if(r==Nothing) then Nothing else match 'a' (a  $ specifyJust r) 
     |otherwise = Nothing

a :: String -> Maybe String
a ls |null ls = Nothing
       |head(ls) =='a' = Just (tail ls)
       |head(ls) =='b'=let res= match 'a' (Just (tail ls))
                           res2=if (res==Nothing) then Nothing else a (specifyJust res)
                       in if(res2==Nothing) then Nothing else (s (specifyJust res2)) 
       |otherwise = Nothing
   
specifyJust:: Maybe a -> a
specifyJust (Just x) =x
specifyJust Nothing = undefined    
   
   
-- Задача 2 ----------------------------------------

b ::String -> Maybe String
b('(':st1)= case b st1 of
      Just(')':st2)-> b st2
      _           -> Nothing
b('[':st1)= case b st1 of
      Just(']':st2)-> b st2
      _           -> Nothing
b('{':st1)= case b st1 of
      Just('}':st2)-> b st2
      _           -> Nothing
b(' ':st1)= b st1
b st = Just st

balance::String ->Bool
balance st1 = case b st1 of
       Just st2 -> null st2
       Nothing -> False


-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr str = ae str == Just ""

ae :: String -> Maybe String 
ae str = let res = af str 
         in if (res == Nothing) then Nothing else aa $ specifyJust res

aa :: String -> Maybe String 
aa str | null str = Just str
       | head(str)=='*' || head(str)=='-' || head(str)=='+' = ae $tail str 
       | otherwise = Just str

af :: String -> Maybe String 
af str |null str = Nothing
       |isDigit $ head str = Just $ tail str
       |head str=='(' = match ')' $ ae $ tail str
       |otherwise = Nothing
-- Задача 4 -----------------------------------------
evalLeft :: String -> Maybe Int 
evalLeft st1 = case le st1 of
   Just (v,st2)| null st2 -> Just v
   _                      -> Nothing

le :: String -> Maybe (Int,String)
le st1 =  case lf st1 of
     Just (v1,st2) -> la (v1,st2)
     Nothing       -> Nothing

la :: (Int,String) -> Maybe (Int,String) 
la (v1, d:st1)
  | d `elem` "+-*" =
    case lf st1 of
      Just (v2, st2) ->
        let v
              | d == '+' = v1 + v2
              | d == '-' = v1 - v2
              | otherwise = v1 * v2
         in la (v, st2)
      Nothing -> Nothing
la (v1,st1)                  = Just (v1,st1)


lf :: String -> Maybe (Int,String)
lf ('(':st1) =
  case le st1 of
    Just (v, ')':st2) -> Just (v, st2)
    _ -> Nothing

lf (d:st1)
  | d `elem` "0123456789" = Just (digitToInt d, st1)
lf _            = Nothing

-- Задача 5 -----------------------------------------

evalRigth :: String -> Maybe Int 
evalRigth st1 = case re st1 of
   Just (v,st2)| null st2 -> Just v
   _                      -> Nothing

re :: String -> Maybe (Int,String)
re st1 =  case rf st1 of
     Just (v1,st2) -> ra (v1,st2)
     Nothing       -> Nothing

ra :: (Int,String) -> Maybe (Int,String) 
ra (v1, d:st1)
  | d `elem` "+-*" =
    case re st1 of
      Just (v2, st2) ->
        let v
              | d == '+' = v1 + v2
              | d == '-' = v1 - v2
              | otherwise = v1 * v2
         in ra (v, st2)
      Nothing -> Nothing
ra (v1,st1)                  = Just (v1,st1)


rf :: String -> Maybe (Int,String)
rf ('(':st1) =
  case re st1 of
    Just (v, ')':st2) -> Just (v, st2)
    _ -> Nothing

rf (d:st1)
  | d `elem` "0123456789" = Just (digitToInt d, st1)
rf _            = Nothing

-- Задача 6 -----------------------------------------
evalPrior :: String -> Maybe Int 
evalPrior st1 = case pe st1 of
             Just (v,st2)| null st2 -> Just v
             _                      -> Nothing

pt :: String -> Maybe (Int,String) 
pt st1 =  case pf st1 of
     Just (v1,st2) -> pb (v1,st2)
     Nothing       -> Nothing

pb :: (Int,String) -> Maybe (Int,String) 
pb (v1, d:st1)
  | d == '*' =
    case pf st1 of
      Just (v2, st2) ->
        let v= v1 * v2
         in pb (v, st2)
      Nothing -> Nothing
pb (v1,st1)                  = Just (v1,st1)


pe :: String -> Maybe (Int,String)
pe st1 =  case pt st1 of
     Just (v1,st2) -> pa (v1,st2)
     Nothing       -> Nothing

pa :: (Int,String) -> Maybe (Int,String) 
pa (v1, d:st1)
  | d `elem` "+-" =
    case pt st1 of
      Just (v2, st2) ->
        let v
              | d == '+' = v1 + v2
              | otherwise = v1 - v2
         in pa (v, st2)
      Nothing -> Nothing
pa (v1,st1)                  = Just (v1,st1)


pf :: String -> Maybe (Int,String)
pf ('(':st1) =
  case pe st1 of
    Just (v, ')':st2) -> Just (v, st2)
    _ -> Nothing

pf (d:st1)
  | d `elem` "0123456789" = Just (digitToInt d, st1)
pf _            = Nothing

------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match c (Just (t:st)) | c==t = Just st
match _ _                    = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp c = case c of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}




putN::Int->String->IO()
putN n str = if(n<=1) then (do putStrLn str )else do  
    putStrLn str
    putN (n-1) str

type FilePath = String
--readFile:: FilePath ->IO String
--writeFile:: FilePath ->String->IO()

--firstN::IO()
firtsN = do
        putStr "Enter file name:"
        line <- getLine
	putStr "Enter num:"
	num <- getLine
	--putStrLn (line++" "++num)
        f <- readFile line
	putStrLn (takeN f num)

takeN::String->String ->String
takeN file ns = unlines (take (read ns) (lines file))

lastN = do
        putStr "Enter file name:"
        line <- getLine
	putStr "Enter num:"
	num <- getLine
	--putStrLn (line++" "++num)
        f <- readFile line
	putStrLn (takeLN f num)

takeLN::String->String ->String

takeLN file ns = unlines (reverse (take (read ns) (reverse (lines file))))
--takeLN file ns = (unlines. reverse. ((take.read ns). reverse .lines) file

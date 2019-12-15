{-# OPTIONS_GHC -Wall #-}

module Kryvosheienko08 where

--import Text.ParserCombinators.Parsec
import Data.Maybe

data Recur = Zero | Succ | Sel Int Int
           | Super Recur [Recur]
           | Prim Recur Recur
           | Mini Recur Int
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]

-- ������ 1 ------------------------------------
isNumbConst ::System->Recur -> Bool
isNumbConst sys (Name f) = let funct = filter (\(x,_) -> x==f) sys
                          in isNumbConst sys (snd(head funct))
isNumbConst _ Zero = True
isNumbConst _ Succ = False
isNumbConst _ (Sel _ _)= False
isNumbConst _ (Super _ _)= False
isNumbConst _ (Prim _ _)= False
isNumbConst _ (Mini _ _)= False


-- ������ 2 ------------------------------------
evRank :: System -> Recur -> Int
evRank _ Succ = 1
evRank _ Zero= 1
evRank _ (Sel n _) = n
evRank sys (Super _ al) = evRank sys (head al)
evRank sys (Prim _ st) = evRank sys st -1
evRank sys (Mini b _) = evRank sys b -1
evRank sys (Name f) = evRank sys (findFunction sys f)

findFunction::System->String->Recur
findFunction [] _= undefined
findFunction (x:sys) f = if fst x == f  then snd x else findFunction sys f

-- ������ 3 ------------------------------------
isNames :: System -> Bool
isNames syst= check syst && checkN (reverse syst)

check::System->Bool
check [] = True
check (x:sys) = not (checkIfHasDuplicates sys (fst x))  && check sys


checkN::System->Bool
checkN [] = True
checkN (x:sys) = isRecur sys (snd x) && checkN sys

checkIfHasDuplicates::System->String->Bool
checkIfHasDuplicates [] _ = False
checkIfHasDuplicates (x:sys) f
                                |fst x == f =  True
                                |otherwise = checkIfHasDuplicates sys f

checkRecurName::System->String->Bool
checkRecurName [] _ = False
checkRecurName (x:sys) f = (fst x == f) || checkRecurName sys f



-- ������ 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel _ _)  = True
isRecur sys (Prim a b)  = isRecur sys a && isRecur sys b
isRecur sys (Mini a _)  = isRecur sys a
isRecur sys (Super a b) = isRecur sys a && and [isRecur sys x | x <- b]
isRecur sys (Name str) = checkRecurName (reverse sys) str

-- ������ 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int
eval _ Zero _= 0
eval _ Succ x = head x+1 
eval _ (Sel _ k) xs =xs!!(k-1)
eval sys (Prim g h) xs =
    if last xs /= 0
    then eval sys h (xxs ++ [eval sys (Prim g h) (init xxs)])
    else eval sys g xs
    where x = [last xs - 1]
          xxs= xs ++ x
eval sys (Mini f a)xs  = undefined
eval sys (Super a b) xs= eval sys a (map (\x -> eval sys x xs) b)
eval sys (Name str) xs = eval sys (snd (head (filter(\(x, _) -> x==str) sys))) xs

-- ������ 6 ------------------------------------
--evalPart :: System -> Recur -> [Int] -> Maybe  Int
--evalPart sys Zero x= Just (eval sys Zero x)
--evalPart sys Succ x = Just (eval sys Succ x)
--evalPart sys (Sel n k) xs =Just (eval sys (Sel n k) xs)
--evalPart sys (Prim g h) xs =Just (eval sys (Prim g h) xs)
--evalPart sys (Super a b) xs= Just (eval sys (Super a b) xs)
--evalPart sys (Name str) xs = Just (eval sys (Name str) xs)
--evalPart sys (Mini f a)xs = evalPartX sys (Mini f a) (xs++[0])


--evalPartX :: System -> Recur -> [Int] -> Maybe  Int
--evalPartX sys (Mini f a) xs |last xs >= a = Nothing
--                            |evalPart sys f xs== Just 0 = Just(last xs) 
--                            |otherwise = evalPart sys (Mini f a) (tail xs ++ [last xs+1])

                        
-- ������ 7 ------------------------------------
parseRec :: String -> Maybe System
parseRec = undefined
---------------------------
syst1, syst2 :: System
syst1 = [("const0", Zero)
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
   , ("const2", Super Succ [Super Succ [Zero]])
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
   , ("subtract1", Prim Zero (Sel 2 1))
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	      \  notSignum = [(a1:z1),(z1:s21)];\n\
		  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
		  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"

sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"

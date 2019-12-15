{-# OPTIONS_GHC -Wall #-}
module Kryvosheienko10 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE
simplify Null = Null
simplify (Term a) = Term a
simplify (Seq a b) = Seq (simplify a) (simplify b)
simplify (Alt a b) = Alt (simplify a) (simplify b)
simplify (Rep a) = Rep (simplify a)
simplify (Plus a) = Seq (simplify a) (Rep (simplify a))
simplify (Opt a) = Alt (simplify a) Null


-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool
isTerminal aut a = a `elem` getTerminalStates aut

getTransition:: Automation->[Transition]
getTransition (_,_, tr) = tr

containsC::[Transition]->State->Bool
containsC [] _ = False
containsC ((s1, _, C _):tr) st = (s1 == st) || containsC tr st
containsC ((_,_, _):tr) st = containsC tr st

getTerminalStates :: Automation -> [State]
getTerminalStates (_, b, _) = b

isEssential :: Automation -> State -> Bool
isEssential aut a =  containsC (getTransition aut) a  || isTerminal aut a

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom aut s = formTransitionsList (getTransition aut) s

formTransitionsList::[Transition]->State->[Transition]
formTransitionsList [] _ = []
formTransitionsList ((s1, s2, l):tr) st = 
                                           if s1 == st
                                           then (s1, s2, l):formTransitionsList tr st 
                                           else formTransitionsList tr st



-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels tr =  nub (map getLabel (filter (\t -> getLabel t /= Eps) tr))

getLabel :: Transition -> Label
getLabel (_, _, l) = l

-- Задача 5 ----- ------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA daut st=  acceptsDAhelper (getTransition daut) st 1

acceptsDAhelper :: [Transition] -> String-> Int-> Bool
acceptsDAhelper _ [] _ = True
acceptsDAhelper tr (s:st) num = (n /=  (-1)) && acceptsDAhelper tr st n
                                where
                                  n = checkSymbol tr s num

checkSymbol::[Transition]->Char->Int->Int
checkSymbol [] _ _ = -1
checkSymbol ((_, _, Eps):tr) c num = checkSymbol tr c num
checkSymbol ((s1,s2,C l):tr) c num = if s1==num && l==c then s2 else checkSymbol tr c num

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
stStep aut st mc = map getSndState (filter (\t -> getLabel t == mc) (transitionsFrom aut st))

setStep :: Automation -> [State] -> Label -> [State]
setStep _ [] _ = []
setStep aut (s:bs) mc = nub (stStep aut s mc ++ setStep aut bs mc)

closure :: Automation -> [State] -> [State]
closure naut ss = nub (closureHelper naut ss [])

closureHelper :: Automation -> [State] -> [State] -> [State]
closureHelper _ [] _ = []
closureHelper naut ss r = current ++ closureHelper naut newSt (current ++ r)
                          where current = setStep naut ss Eps
                                newSt = filter (`notElem` r) current

getSndState :: Transition -> State
getSndState (_, s2, _) = s2

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut s = acceptsHelper aut s [getFirstSt aut]

acceptsHelper :: Automation -> String -> [State] -> Bool
acceptsHelper _ _ [] = False
acceptsHelper aut [] st = True `elem` map (isTerminal aut) st
acceptsHelper aut (c:s) currStates = acceptsHelper aut s (setStep aut avSt (C c))
                                 where avSt = closure aut currStates

getFirstSt::Automation->State
getFirstSt (s1,_,_)=s1

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null str end n = ([(str, end, Eps)], n)
make (Term a) str end n = ([(str, end, C a)], n)

make (Seq re1 re2) str end n = (trx++trx1++[(n, n+1, Eps)], nxt1)
                              where  (trx, nxt) = make re1 str n (n+2)
                                     (trx1, nxt1) = make re2 (n+1) end nxt
                                            
make (Alt re1 re2) str end n =  (trx++trx1++[(str, n, Eps), (n+1, end, Eps), (str, n+2, Eps), (n+3, end, Eps)], nxt1)
                               where (trx, nxt) = make re1 n (n+1) (n+4)
                                     (trx1, nxt1) = make re2 (n+2) (n+3) nxt
                                             
make (Rep re) str end n = (trx++[(str, n, Eps), (n+1, n, Eps), (n+1, end, Eps), (str, end, Eps)], nxt)
                           where (trx, nxt) = make re n (n+1) (n+2)

make (Plus re) str end n = make (simplify re) str end n
make (Opt re) str end n =  make (simplify re) str end n
-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE
parseReg  = undefined
-- Задача 10 -----------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--знаходить всі стани автомату
states::Automation->[State]
states (ist, fsx, trx)= (sort . nub . concat) [[ist],fsx, concatMap(\(s1,s2,_) -> [s1,s2]) trx]

-- перевіряє чи автомат  детермінований
--isDeter::Automation->Bool
--isDeter (_,_,trx)=(null.(filter (\(_,_,l)-> l == Eps)))trx &&
--                  (null.(filter ((>1)length)) .group . sort . (map (\(s,_,l) -> (s,l))))trx

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps),
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)])
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
			  
			  

leastKelem::[Int] -> String
leastKelem xs = xs!!2
			  
			  
			  

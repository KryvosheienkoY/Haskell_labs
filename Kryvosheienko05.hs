{-# OPTIONS_GHC -Wall #-}

module Kryvosheienko05 where

import           Data.Char (isUpper)
import           Data.List

type Grammar = [Production] -- 
type Production = (Char, String) -- 
type Predict = [(Char, String)] -- 
type Control = [((Char, Char), Int)] -- 


--  1 ------------------------------------
addOne :: String -> Char -> String
addOne st c =
  let n = findSymbol st c 0
   in if n == (-1)
        then st ++ [c]
        else st

findSymbol :: String -> Char -> Int -> Int
findSymbol [] _ _ = -1
findSymbol (x:st) c n
  | x == c = n
  | otherwise = findSymbol st c (n + 1)

addAll :: String -> String -> String
addAll st [] = st
addAll st wd =
  let line = removeDuplicates wd
   in if findSymbol st (head line) 0 == (-1)
        then addAll (st ++ [head line]) (drop 1 line)
        else addAll st (drop 1 line)

removeDuplicates :: String -> String
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

addWithout :: String -> String -> String
addWithout st [] = st
addWithout st wd = addAll st (filter (/= '$') wd)

inter :: String -> String -> String
inter [] _ = []
inter (x:xs) l
  | x `elem` l = x : inter xs (delete x l)
  | otherwise = inter xs l

--  2 ------------------------------------
tkPredict :: Predict -> Char -> String
tkPredict [] _ = ""
tkPredict pt n =
  if fst (head pt) == n
    then snd (head pt)
    else tkPredict (drop 1 pt) n

upPredict :: Predict -> Char -> String -> Predict
upPredict pt n st =
  let index = findNoTerminalNum pt n 0
   in if index == (-1)
        then insertionSort (pt ++ [(n, st)])
        else insertionSort (take index pt ++ [(n, st)] ++ drop (index + 1) pt)

findNoTerminalNum :: Predict -> Char -> Int -> Int
findNoTerminalNum [] _ _ = -1
findNoTerminalNum pt n num =
  if fst (head pt) == n
    then num
    else findNoTerminalNum (drop 1 pt) n (num + 1)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []

-- Задача 3--
parse :: Grammar -> Control -> String -> Maybe [Int]
parse gr ctl word = analyzeLL gr ctl (word ++ "$") "S$" (Just [])

analyzeLL :: Grammar -> Control -> String -> String -> Maybe [Int] -> Maybe [Int]
analyzeLL gr ctl word st res
  | (st /= "$" && word == "$") || (st == "$" && word /= "$") = Nothing
  | head st == '$' = res
  | otherwise =
    analyzeLL
      gr
      ctl
      (getFirst (step gr ctl (word, st, res)))
      (getSecond (step gr ctl (word, st, res)))
      (getThird (step gr ctl (word, st, res)))

step :: Grammar -> Control -> (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gr ctl (word, st, res)
  | head st == head word = (tail word, tail st, res)
  | null curCtl = (word, st, res)
  | otherwise = (word, snd (gr !! specCurCtl) ++ tail st, Just (specifyJust res ++ [specCurCtl]))
  where
    ls = [n | n <- ctl, fst (fst n) == head st, snd (fst n) == head word]
    curCtl =
      if null ls
        then Nothing
        else Just (snd (head ls))
    specCurCtl = specifyJust curCtl

getFirst :: (String, String, Maybe [Int]) -> String
getFirst (x, _, _) = x

getSecond :: (String, String, Maybe [Int]) -> String
getSecond (_, x, _) = x

getThird :: (String, String, Maybe [Int]) -> Maybe [Int]
getThird (_, _, x) = x

specifyJust :: Maybe a -> a
specifyJust (Just x) = x
specifyJust Nothing  = undefined

--Задача 4--
first :: Predict -> String -> String
first pFst st
  | null st = "$"
  | not (isUpper (head st)) = [head st]
  | length st == 1 = curPredict
  | '$' `notElem` curPredict = curPredict
  | otherwise = insertionSort (addWithout (first pFst (tail st)) curPredict)
  where
    curPredict = snd (head [n | n <- pFst, fst n == head st])


--Задача 5--

buildingControl :: Grammar -> Predict -> Predict -> Control
buildingControl gr pFst pNxt = sortBy (\(a, _) (b, _) -> compare a b) (buildTem gr pFst pNxt 0)

buildTem :: Grammar -> Predict -> Predict -> Int -> Control
buildTem gr pFst pNxt n
  | null gr = []
  | chPFst == "$" = built ++ buildCl1 (fst (head gr)) (tkPredict pNxt (fst (head gr))) pNxt n
  | chPFst /= "$" = built ++ buildCl1 (fst (head gr)) chPFst pFst n
  | otherwise = built
  where
    chPFst = first pFst (snd (head gr))
    built = buildTem (tail gr) pFst pNxt (n + 1)



buildCl1 :: Char -> String -> Predict -> Int -> [((Char, Char), Int)]
buildCl1 ch str pr n =
  if fst (head pr) == ch
    then [((ch, x), n) | x <- str]
    else buildCl1 ch str (tail pr) n

--  6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pFst pNxt = if notElem False (ar (fromGrammar gr)) then True else False  
                         where ar =  map (\x -> (if testFst [first pFst f | f <- snd x] && if elem "" (snd x)  then testFollow (tkPredict pNxt (fst x)) (snd x) else True  then True else False))

uniqueChar :: [Char] -> [Char]
uniqueChar [] = []
uniqueChar (x : xs) = x : uniqueChar (filter (x /=) xs)

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar gr =  [ (nontermAr!!n, [snd g | g <- gr, fst g == nontermAr!!n]) | n<-[0..(length nontermAr-1)] ]
                  where nontermAr = uniqueChar [fst x |x<-gr]

testFst :: [String] -> Bool
testFst rls =   length ls == length (filter (=="") ls)
                where ls = [inter (rls!!a) (rls!!b) |a<- [0..(length rls-1)], b<- reverse [0..(length(rls)-1)], a/=b]


testFollow :: String -> [String] -> Bool
testFollow fs rls =  length ls == length (filter (=="") ls)
                     where ls = [inter a fs |a<- rls ]


--  7 ------------------------------------

buildFst :: Grammar -> Predict
buildFst gr = predict gr (mPr gr [])

evalFst :: Grammar -> Predict -> Predict
evalFst _ pFst = pFst
evalFst (g:gr) pFst = evalFst gr (extandFst pFst g)


extandFst :: Predict -> Production -> Predict
extandFst pFst (n, nl)= upPredict pFst n (addAll (first pFst nl) (tkPredict pFst n))


predict :: Grammar -> Predict -> Predict
predict gr pr = if pr == evalFst gr pr then pr else predict gr (evalFst gr pr)


mPr:: Grammar -> Predict -> Predict
mPr gr pr
            |null gr = pr
            |null ( snd ( head gr)) && elem  (fst ( head gr)) (map fst pr) = mPr (tail gr) (upPredict pr (fst $ head gr) "$" )
            |notElem (fst (head gr)) (map fst pr) && null (snd (head gr)) = mPr (tail gr) (upPredict pr (fst $ head gr) "" )
            |otherwise = mPr (tail gr) (upPredict pr (fst ( head gr)) "" )


--  8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict
buildNxt = undefined

nontermTails :: Grammar -> [(Char, String)]
nontermTails = undefined

evalNxt :: [(Char, String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

--------------------------------------------------------------
gr0, gr1, gr2, gr3, gr4, gr5 :: Grammar
gr0 = [('S', "aAS"), ('S', "b"), ('A', "a"), ('A', "bSA")]

gr1 = [('S', "TV"), ('T', "d"), ('T', "(S)"), ('V', "+TV"), ('V', "-TV"), ('V', "")]

gr2 =
  [ ('E', "TU")
  , ('U', "")
  , ('U', "+TU")
  , ('U', "-TU")
  , ('T', "FV")
  , ('V', "")
  , ('V', "*FV")
  , ('V', "%FV")
  , ('V', "/FV")
  , ('F', "d")
  , ('F', "(E)")
  ]

gr3 = [('S', "aAS"), ('S', "a"), ('A', "SbA"), ('A', "ba"), ('S', "")]

gr4 = [('E', "E+T"), ('E', "T"), ('T', "T*F"), ('T', "F"), ('F', "d"), ('F', "(E)")]

gr5 =
  [ ('E', "E+T")
  , ('E', "E-T")
  , ('E', "T")
  , ('T', "T*F")
  , ('T', "T%F")
  , ('T', "T/F")
  , ('T', "F")
  , ('F', "d")
  , ('F', "(E)")
  ]

pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A', "ab"), ('S', "ab")]

pFst1 = [('S', "(d"), ('T', "(d"), ('V', "$+-")]

pFst2 = [('E', "(d"), ('F', "(d"), ('T', "(d"), ('U', "$+-"), ('V', "$%*/")]

pFst3 = [('A', "ab"), ('S', "$a")]

pFst4 = [('E', "(d"), ('F', "(d"), ('T', "(d")]

pFst5 = [('E', "(d"), ('F', "(d"), ('T', "(d")]

pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A', "ab"), ('S', "$ab")]

pNxt1 = [('S', "$)"), ('T', "$)+-"), ('V', "$)")]

pNxt2 = [('E', "$)"), ('F', "$%)*+-/"), ('T', "$)+-"), ('U', "$)"), ('V', "$)+-")]

pNxt3 = [('A', "$ab"), ('S', "$b")]

pNxt4 = [('E', "$)+"), ('F', "$)*+"), ('T', "$)*+")]

pNxt5 = [('E', "$)+-"), ('F', "$%)*+-/"), ('T', "$%)*+-/")]

ctl0, ctl1, ctl2 :: Control
ctl0 = [(('A', 'a'), 2), (('A', 'b'), 3), (('S', 'a'), 0), (('S', 'b'), 1)]

ctl1 =
  [ (('S', '('), 0)
  , (('S', 'd'), 0)
  , (('T', '('), 2)
  , (('T', 'd'), 1)
  , (('V', '$'), 5)
  , (('V', ')'), 5)
  , (('V', '+'), 3)
  , (('V', '-'), 4)
  ]

ctl2 =
  [ (('E', '('), 0)
  , (('E', 'd'), 0)
  , (('F', '('), 10)
  , (('F', 'd'), 9)
  , (('T', '('), 4)
  , (('T', 'd'), 4)
  , (('U', '$'), 1)
  , (('U', ')'), 1)
  , (('U', '+'), 2)
  , (('U', '-'), 3)
  , (('V', '$'), 5)
  , (('V', '%'), 7)
  , (('V', ')'), 5)
  , (('V', '*'), 6)
  , (('V', '+'), 5)
  , (('V', '-'), 5)
  , (('V', '/'), 8)
  ]

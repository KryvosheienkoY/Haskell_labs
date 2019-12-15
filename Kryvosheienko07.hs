{-# OPTIONS_GHC -Wall #-}
module Kryvosheienko07 where

import Data.List (sort)

data BinTreeM a = EmptyM
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq)

-- B-������ ������� t (NodeB kl tl) =>
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- ������� �������������� B-������  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- ������ 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM h n l r) = checkN (<=h) l && checkN (>=h) r && isSearch l && isSearch r && n/=0
   
checkN::(a->Bool)->BinTreeM a-> Bool
checkN _ EmptyM = True
checkN c (NodeM h1 n1 l1 r1) = c h1 && checkN c l1 && checkN c r1 && n1/=0

-- ������ 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM h _ l r) v 
                           |h==v = True
                           |v<h = elemSearch l v
                           |otherwise = elemSearch r v

-- ������ 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM v= NodeM v 1 EmptyM EmptyM
insSearch (NodeM h n l r) v
                                | v == h = NodeM h (n+1) l r
                                | v < h = NodeM h n (insSearch l v) r
                                | otherwise = NodeM h n l (insSearch r v)

-- ������ 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _=  EmptyM
delSearch (NodeM h n l r) v
                                | v == h = if n>1 then NodeM h (n-1) l r else deleteNode (NodeM h n l r )
                                | v < h = NodeM h n (delSearch l v) r
                                | otherwise = NodeM h n l (delSearch r v)

deleteNode :: (Ord a) => BinTreeM a -> BinTreeM a
deleteNode (NodeM _ _ EmptyM  r) = r
deleteNode (NodeM _ _ l  EmptyM) = l
deleteNode (NodeM _ n l r) = NodeM (lastLeftElem r) n l r


lastLeftElem :: (Ord a) => BinTreeM a -> a
lastLeftElem (NodeM v _ EmptyM  _) = v
lastLeftElem (NodeM _ _ t1 _) = lastLeftElem t1


-- ������ 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList ls = treeToList (buildTree ls)

buildTree::(Ord a) => [a] ->BinTreeM a
buildTree = foldl insSearch EmptyM

treeToList :: BinTreeM a -> [a]
treeToList EmptyM = []
treeToList (NodeM h n l r) = treeToList l ++ replicate n h ++ treeToList r

-- ������ 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform tr = BInform (height tr) (minItem tr) (maxItem tr)

minItem::Btree a ->a
minItem (NodeB a []) = head a
minItem (NodeB _ tree)=minItem (head tree)

maxItem::Btree a ->a
maxItem (NodeB a []) = last a
maxItem (NodeB _ tree)=maxItem (last tree)

height::Btree a ->Int
height (NodeB _[]) = 0
height (NodeB _ tree)=1+height (head tree)

-- ������ 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool
isBtree = undefined

checkKeyNum ::(Bounded a, Ord a) => Int -> Btree a -> Bool
checkKeyNum _ (NodeB [] _) = False
checkKeyNum t (NodeB h []) = length h<=(2*t-1) || length h >=(t-1)
checkKeyNum t (NodeB h (x:tr)) =
  not (length h>(2*t-1) || length h <(t-1)) && (checkKeyNum t x && checkKeyNum t (NodeB h tr))
  

-- ������ 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool
eqBtree _ tr1 tr2 = areEqual (bTreetoList tr1) (bTreetoList tr2)

bTreetoList::(Bounded a, Ord a) =>Btree a ->[a]
bTreetoList (NodeB [] _) = []
bTreetoList (NodeB h []) = h
bTreetoList (NodeB h (x:tr))= bTreetoList x ++ bTreetoList (NodeB h tr)

areEqual::(Ord a)=>[a]->[a]->Bool
areEqual a b = sort a == sort b

-- ������ 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree (NodeB [] _) _= False
elemBtree (NodeB h []) a = a `elem` h
elemBtree (NodeB h (x:tr)) a = elemBtree x a || elemBtree (NodeB h tr) a

position :: Ord a => a -> [a] -> Int
position _ []= 0
position a (x:xs)
  | a < x = 0
  | a == x = 0
  | otherwise = 1 + position a xs

-- ������ 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t tr v
                | isFull t tr = insertInTree t (NodeB [v1] [tr1, tr2]) v
                | otherwise = insertInTree t tr v
                where (tr1, v1, tr2) = splitAtB t tr 

insertInTree :: Ord a => Int -> Btree a -> a -> Btree a
insertInTree _ (NodeB k1 []) v = NodeB (insertKey v k1) []
insertInTree t (NodeB k1 t1) v =
                                if isFull t bt
                                then  NodeB (k11 ++ (k : k12)) (t11 ++ (btr1 : (btr2 : t12)))
                                else NodeB k1 (t11 ++ (insertInTree t bt v : t12))
                                where (k11, k12, t11, bt, t12) = decomposeNodeB v k1 t1
                                      (bt1, k, bt2) = splitAtB t bt
                                      btr1 = if v > k  then bt1 else insertInTree t bt1 v
                                      btr2 = if v > k  then insertInTree t bt2 v else bt2

isFull :: Ord a => Int -> Btree a -> Bool
isFull n (NodeB v _) = 2*n-1==length v


decomposeNodeB :: Ord a => a -> [a] -> [Btree a] ->
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB v k1 t1 =(k11, k12, t11, bt, t12)
                        where n = position v k1
                              k11 = [k1!!i | i <-[0..n - 1]]
                              k12 = [k1!!i | i <-[n..length k1 - 1]]
                              t11 = [t1!!i | i <-[0..n - 1]]
                              t12 = [t1!!i | i <-[n + 1..length t1 - 1]]
                              bt = t1!!n


splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB k1 t1) = (NodeB k11 t11, k1 !! (t - 1), NodeB k12 t12)
                           where t11 = if null t1 then [] else [t1!!n | n <-[0..t-1]]
                                 t12 = if null t1 then [] else [t1!!n | n <-[t..(2*t)-1]]
                                 k11 = [k1!!n | n <-[0..t-2]]
                                 k12 = [k1!!n | n <-[t..(2*t)-2]]



insertKey :: Ord a => a -> [a] -> [a]
insertKey a [] = [a]
insertKey a (b:ls)=if a>b then b: insertKey a ls else a:(b:ls)

-------------------
bm = NodeM  't' 2
            (NodeM 'a' 1  EmptyM
                    (NodeM 'e' 1
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            )
            (NodeM 'w' 2  EmptyM EmptyM)
{-
tiB1 :: Btree Char
tiB1 = NodeB ['G','M','P','X']
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]

tBtr1 :: Btree Int
tBtr1 = NodeB [5,10,12] [ts0,ts1,ts2,ts3]
   where ts0 = NodeB [1,3  ] []   --- ,4,5] []  --
         ts1 = NodeB [6,6 ,8,9,10] [] --- ,8,9,10] []  -- ] []
         ts2 = NodeB [11,11,12,12] []
         ts3 = NodeB [16,16] [] -- ,18,19,20] []

tBtr2 :: Btree Int
tBtr2 = NodeB [15] [ts10,ts11]
  where ts10 = NodeB [11,13] []
        ts11 = NodeB [21,22] []
-}
tBt1 :: Btree Char
tBt1 = NodeB "L"
       [ NodeB "DG"
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU"
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX"
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX"
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]



--binary tree with duplicates---

--
--infOp::String -> (a->a->a)-> Parser(a->a->a)
--infOp x f = do _ <-string x
--               return f
----infOp x f = string x >> return f
--
--
--mulop::Parser (Int->Int->Int)
--mulop = infOp "*" (*)
----mulop= try (infOp ":%" mod)
----            <|> (infOp ":/" div)
----            <|> (infOp "*" (*))
--
--paren::Parser a -> Parser a
--paren p = do _ <-string "("
--             v <-p
--             _ <-string ")"
--             return v
----paren p = string "(" *> p <* string ")"
--
--addOp ::Parser (Int->Int->Int)
--addOp = infOp "+" (+) <|> infOp "-" (-)
--
--factor ::Parser Int
--factor = num <|> paren num
--
--
--term, expr ::Parser Int
--term = chainl1 factor mulop
--expr = chainl1 factor addOp
--
--
--
--full ::Parser Int
--full = do v <-expr
--          eof
--          return v
--
--num ::Parser Int
--num = read <$> (many1 digit)

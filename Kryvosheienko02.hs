{-# OPTIONS_GHC -Wall #-}
module Kryvosheienko02 where

-- Task 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs
  
-- Task 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- Task 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs


-- Task 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert xs = foldl insert [] xs 

insert ::[Int]->Int->[Int]
insert [] x  = [x]
insert (y:ys) x = if x > y
                  then y : insert ys x
                  else x:y:ys

-- Task 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int]
findIndices p xs =
  filter
    (>= 0)
    [ if p (xs !! x)
      then x
      else -1
    | x <- [0 .. length xs - 1]
    ]
-- Task 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = reverse (map reverse xss)

-- Task 7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (not . (`elem` "1234567890")) xs

-- Task 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v =
  sum
    [ if (ps !! x) v
      then 1
      else 0
    | x <- [0 .. length ps - 1]
    ]

-- Task 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate (\x -> zipWith (+) (0 : x) (x ++ [0])) [1]

-- Task 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM =  1 : 2 : zipWith (*) [3..] (tail factorialsM)


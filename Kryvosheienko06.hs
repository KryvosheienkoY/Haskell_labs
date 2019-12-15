{-# OPTIONS_GHC -Wall #-}
module Kryvosheienko06 where

import Data.List (nub, sortOn, sort)

type GraphS = (Int,[(Int,Int)])
type Graph  = [[Int]]

-- ������ 1 ------------------------------------
isOrdinary :: Graph -> Bool
isOrdinary gr = False `notElem` [a `elem` gr!!b | a<- nodes gr, b<-gr!!a]

nodes::Graph-> [Int]
nodes gr=[0..(length gr-1)]

-- ������ 2 ------------------------------------
fromGraph :: Graph -> GraphS
fromGraph gr =(length gr-1, [(a,b) | a<- nodes gr, b<-gr!!a])

-- ������ 3 ------------------------------------
toGraph :: GraphS -> Graph
toGraph grS = [map snd y | y<-ar]
  where
    ls = snd grS
    ar = [filter (\n -> fst n == x) ls | x <- [0 .. fst grS]]

-- ������ 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b = if null w then [] else head w
                  where w = (getWays gr a b)

getWays::Graph->Int->Int->[[Int]]
getWays gr a b  = sortOn length ( filter (not . null) (unwrap3 (getPathsBetweenAB gr a b)))

getPathsBetweenAB ::Graph -> Int -> Int->[[[Int]]]
getPathsBetweenAB gr a b = map (\y -> filter (\x -> head x == a && last x == b) y) (tail (allWays gr b))

unwrap3::[[[Int]]] -> [[Int]]
unwrap3  = foldr (++) [[]]


allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW::[[[Int]]] -> Bool
condW vsss = null (head vsss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW  _ [] = [[[]]]
stepW gr (ways:vss) = let noCycle = filter (not . isCycle) ways
                        in  [concatMap (\e -> (findWays gr e)) noCycle ] ++ (ways:vss)

findWays::Graph->[Int]->[[Int]]
findWays gr (v:vs) =
  let neighbour = adj gr v
   in [x : (v : vs) | x <- neighbour]
findWays _ [] = [[]]

adj :: Graph -> Int -> [Int]
adj g v =
  if length g > v && v >= 0
    then g !! v
    else []

isCycle :: [Int] -> Bool
isCycle vs = (length vs /= 1) && (length vs /= length (nub vs))


-- ������ 5 ------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = length ar == length (filter (/= []) ar)
  where
    ar = getAllPaths gr

getAllPaths::Graph -> [[Int]]
getAllPaths gr = [shortWay gr a b |  a <- nodes gr, b <- nodes gr , a/=b ]

-- ������ 6 ------------------------------------
components :: Graph -> [[Int]]
components gr =undefined

-- ������ 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v= if lngth== 0 then lngth else lngth-1
                   where lngth =length (last(sortOn length ([shortWay gr v a |  a <- nodes gr , a/=v ])))

-- ������ 8 ------------------------------------
findDiameter :: Graph -> Int
findDiameter gr = last (getEccentricities gr)

findRadius :: Graph -> Int
findRadius gr= head (getEccentricities gr)

getEccentricities:: Graph -> [Int]
getEccentricities gr = sort [eccentricity gr x | x<-[0..length gr-1]]

-- ������ 9 ------------------------------------
findCenter :: Graph -> [Int]
findCenter gr= [x | x<-[0..length gr-1], eccentricity gr x == findRadius gr]

-- ������ 10 ------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]]
shortWays gr a b = [x | x <- ar, length x <= length (head ar)]
  where
    ar = getWays gr a b

---------------------------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]





{-# LANGUAGE NoMonomorphismRestriction,TupleSections #-}
module NanoUtils.List
    (
      eqClasses
    , rcompare
    , rsort
    , sortOn
    , rsortOn
    , printList
    , sum'
    , sumBy'
    , countBy
    , foldWhile
    , pushFixed
    , unfoldr'
    , choose2
    , chooseSortedPairs
    , leaveOneOuts
    , removeAt
    , pairUp
    , multiplicity
    , chunk
    ) where

import NanoUtils.Tuple (ascOrder)
import Data.List
import Data.Function
import qualified Data.Map as M

-- |Chunk the list
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (l,r) = splitAt n xs
             in l : chunk n r

-- |Count the number of times each unique item appears
multiplicity :: Ord a => [a] -> [(a,Int)]
multiplicity = M.toList.M.fromListWith (+).flip zip (repeat 1)

-- |All 2-subsets of xs
choose2 [] = []
choose2 (x:xs) = [[x,y]|y<-xs] ++ choose2 xs

-- |pair up elements. If odd number the last one will be on its own
pairUp [] = []
pairUp (x:[]) = [[x]]
pairUp (x:y:xs) = [x,y] : pairUp xs

-- |removeAt
removeAt i = (\(l,r) -> (head l,r)).
             foldr (\(j,v) (l,r) -> if i==j then (v:l,r) else (l,v:r)) ([],[]).
             zip [0..]

-- |leaveOneOuts
leaveOneOuts xs = leaveOneOuts' [] xs
leaveOneOuts' ls [] = []
leaveOneOuts' ls rs = (ls ++ tail rs) : leaveOneOuts' (ls++[head rs]) (tail rs)

-- |Choose pairs of the form (a,b) such that
-- a is taken from list 1
-- b is taken from list 2
-- and, a <= b
-- Requirement: set1 `intersection` set2 == empty. For clean semantics.
chooseSortedPairs :: Ord a => [a] -> [a] -> [(a,a)]
chooseSortedPairs xs ys = concat.map (\x -> map (ascOrder.(x,)) ys) $ xs

-- |unfold an infinite list. You don't have to deal with the Maybe in unfoldr
unfoldr' :: (b -> (a,b)) -> b -> [a]
unfoldr' f = unfoldr (\b -> Just (f b))

-- |pushFixed
pushFixed n x xs = take n (x:xs)

-- |foldWhile
foldWhile _ _ acc [] = acc
foldWhile p f acc (x:xs)
  | p acc == True = foldWhile p f (f acc x) xs
  | otherwise = acc

-- |count the number of elements satisfying predicate
countBy f = foldl' (\acc x -> if f x then acc+1 else acc) 0

-- |strict sum
sum' = foldl' (+) 0

-- |strict sum by
sumBy' f = foldl' (\acc v -> acc + f v) 0

-- |print list
printList delim = intercalate delim.map show

-- |sort on f of the element
sortOn f = sortBy (compare`on`f)

-- |reverse sort on f of the element
rsortOn f = sortBy (rcompare`on`f)

-- |reverse sort
rsort = sortBy rcompare

-- |flip the compare function
rcompare x y = flip compare x y

-- |equivalence classes
eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses r xs = part xs []
    where part (a:as) acc = let (p1,p2) = partition (r a) as
                            in part p2 ((a:p1):acc)
          part [] acc = acc

--TODO: add the eqClassesT algorithm present in NanoUtils.Monadic.List
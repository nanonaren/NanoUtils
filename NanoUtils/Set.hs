{-# LANGUAGE TupleSections #-}
module NanoUtils.Set
    (
      randCoprimeFactors
    , randPartition
    , randFixedPartition
    ) where

import Control.Monad.Random
import Control.Monad (liftM)
import Data.Int
import Data.List (partition)
import NanoUtils.List (sortOn,removeAt)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.HashTable (hashString)

hashSet :: Show a => S.Set a -> Int32
hashSet = hashString.concat.map show.S.toList

randPartition :: (RandomGen g,Ord a) => Int -> S.Set a -> Rand g [S.Set a]
randPartition n s = do
  binMap <- mapM (\x -> getRandomR (1,n) >>= \r -> return (r,[x])).S.toList $ s
  return.map S.fromList.M.elems.M.fromListWith (++) $ binMap

-- |Doc
randFixedPartition :: RandomGen g => Int -> Int -> [a] -> Rand g [[a]]
randFixedPartition 0 _ _ = return []
randFixedPartition len n xs = do
  (xs',p) <- randPicks len n' xs
  liftM (p:) $ randFixedPartition (len-n') n xs'
     where n' = if n > len then len else n

randPicks _ 0 xs = return (xs,[])
randPicks len num xs = do
  (r,xs') <- randPick len xs
  liftM (fmap (r:)) $ randPicks (len-1) (num-1) xs'

randPick len xs = do
  num <- getRandomR (0,len-1)
  return.removeAt num $ xs

randAnnotate m = mapM (\x -> m >>= \r -> (r,x))

randCoprimeFactors :: (RandomGen g,Ord a) =>
                      S.Set a
                   -> S.Set a
                   -> Rand g (Maybe (S.Set a,S.Set a))
randCoprimeFactors iden d = do
  let diff = S.difference iden d
  case S.size diff of
    1 -> return Nothing
    _ -> do
      parts <- randPartition 2 diff
      return (Just $ addDiv $ balance parts)
    where balance ps
              | length ps == 1 = moveOne S.empty (head ps)
              | otherwise = (head ps,(head.tail) ps)
          moveOne s1 s2 = let (a,s2') = S.deleteFindMin s2
                          in (S.insert a s1,s2')
          addDiv (a,b) = (S.union d a,S.union d b)

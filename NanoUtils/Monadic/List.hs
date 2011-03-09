module NanoUtils.Monadic.List
    (
      eqClassesM
    , eqClassesTM
    , partitionM
    ) where

import NanoUtils.Tuple (mapHomTup)
import Control.Monad
import Data.List (nub)

-- |equivalence classes
eqClassesM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
eqClassesM r xs = part xs []
    where part (a:as) acc = do
            (p1,p2) <- partitionM (r a) as
            part p2 ((a:p1):acc)
          part [] acc = return acc

-- |partition a list
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = foldM (flip (select p)) ([],[]) xs

-- |equivalence classes where the given relation does
-- not have to be closed under transitivity. This algorithm
-- will compensate for that and produce
-- the correct equivalence classes. Hence,
-- > eqClassesTM f xs == eqClassesM f xs
eqClassesTM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
eqClassesTM r [] = return []
eqClassesTM r (x:xs) = do
  (acc,xs') <- close' r [x] xs [x]
  liftM (acc:) $ eqClassesTM r xs'
close' _ [] xs acc = return (acc,xs)
close' r as xs acc = do
  (as',xs') <- foldM (\(ls,rs) a -> do
                        (ls',rs') <- partitionM (r a) rs
                        return (ls++ls',rs') ) ([],xs) as
  close' r as' xs' (acc ++ as')

select :: Monad m => (a -> m Bool) -> a -> ([a], [a]) -> m ([a], [a])
select p x ~(ts,fs) = do
  b <- p x
  return $ case b of 
             True -> (x:ts,fs)
             False -> (ts, x:fs)
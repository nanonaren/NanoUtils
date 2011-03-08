module NanoUtils.Monadic.List
    (
      eqClassesM
    , partitionM
    ) where

import Control.Monad

-- |equivalence classes
eqClassesM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
eqClassesM r xs = part xs []
    where part (a:as) acc = do
            (p1,p2) <- partitionM (r a) as
            part p2 ((a:p1):acc)
          part [] acc = return acc

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = foldM (flip (select p)) ([],[]) xs

select :: Monad m => (a -> m Bool) -> a -> ([a], [a]) -> m ([a], [a])
select p x ~(ts,fs) = do
  b <- p x
  return $ case b of 
             True -> (x:ts,fs)
             False -> (ts, x:fs)
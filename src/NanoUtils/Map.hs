module NanoUtils.Map
    (
      topWithKey
    , unionWithMonoid
    ) where

import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Monoid

-- |Retrieve the top k values with their key
topWithKey :: M.Map k a -> Int -> [(k,a)]
topWithKey mp n = take n $ unfoldr M.maxViewWithKey mp

-- |Union two maps and merge values that extend the Monoid interface
unionWithMonoid :: (Ord k,Monoid a) => M.Map k a -> M.Map k a -> M.Map k a
unionWithMonoid = M.unionWith mappend

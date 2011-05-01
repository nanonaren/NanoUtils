module NanoUtils.Container
    (
      normalize
    , foldMapM
    ) where

import Prelude hiding (foldr)
import Data.Foldable
import Data.Monoid
import Control.Monad

-- | Normalize values in a container, that is, each value is
-- divided by the total
normalize :: (Foldable t,Functor t,Fractional a) => t a -> t a
normalize t = let total = foldl' (+) 0 t
              in fmap (/total) t

-- | Monaidic version of foldMap
foldMapM :: (Foldable t,Monoid v,Monad m) => (a -> m v) -> t a -> m v
foldMapM f = foldr (liftM2 mappend.f) (return mempty)
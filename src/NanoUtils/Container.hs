module NanoUtils.Container
    (
      normalize
    ) where

import Data.Foldable

-- | Normalize values in a container, that is, each value is
-- divided by the total
normalize :: (Foldable t,Functor t,Fractional a) => t a -> t a
normalize t = let total = foldl' (+) 0 t
              in fmap (/total) t
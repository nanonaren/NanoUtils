module NanoUtils.Tuple
    (
      swap
    , mapFst
    , mapSnd
    , mapHomTup
    , ascOrder
    ) where

-- |swap tuple elements
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- |apply function to the first element of tuple
mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a,b)

-- |apply function to the second element of tuple
mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a,f b)

-- |apply function to a homogeneous tuple
mapHomTup :: (a -> b) -> (a,a) -> (b,b)
mapHomTup f (a,b) = (f a,f b)

-- |order elements of tuple in ascending order
ascOrder :: Ord a => (a,a) -> (a,a)
ascOrder (a,b) | a <= b = (a,b)
               | otherwise = (b,a)

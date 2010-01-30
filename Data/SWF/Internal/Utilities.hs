module Data.SWF.Internal.Utilities (
    module Data.SWF.Internal.Utilities,
    
    module Control.Monad,
    module Data.Maybe,
    module Data.List
  ) where

import Control.Monad

import Data.Maybe
import Data.List


orElse = flip fromMaybe

fst3 (a, _, _) = a
snd3 (_, b, _) = b
thd3 (_, _, c) = c

fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thd4 (_, _, c, _) = c
fth4 (_, _, _, d) = d

assertM True  _ = return ()
assertM False s = fail s

condM :: Monad m => m Bool -> m a -> m a -> m a
condM mcond mt mf = do
    cond <- mcond
    if cond then mt else mf


maybeHasM :: Monad m => m Bool -> m b -> m (Maybe b)
maybeHasM ma mb = ma >>= \a -> maybeHas a mb

maybeHas :: Monad m => Bool -> m b -> m (Maybe b)
maybeHas flag act
  | flag      = liftM Just act
  | otherwise = return Nothing

genericReplicateM :: (Integral a, Monad m) => a -> m b -> m [b]
genericReplicateM n act = sequence $ genericReplicate n act

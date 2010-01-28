module Utilities (
    module Utilities,
    
    module Control.Monad,
    module Data.Maybe
  ) where

import Control.Monad

import Data.Maybe


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


maybeHasF :: (Num a, Monad m) => a -> m b -> m (Maybe b)
maybeHasF = maybeHas . (/= 0)

maybeHas :: Monad m => Bool -> m b -> m (Maybe b)
maybeHas flag act
  | flag      = liftM Just act
  | otherwise = return Nothing


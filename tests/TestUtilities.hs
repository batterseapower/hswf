module TestUtilities (
    module TestUtilities,
    
    module Data.SWF
  ) where

import Data.SWF
import Data.SWF.Internal.Binary

import qualified Data.ByteString.Lazy as BS


run :: [Word8] -> SwfGet a -> a
run = runSwfGet emptySwfGetEnv . BS.pack

assertEquals :: (Eq a, Show a) => a -> a -> IO ()
assertEquals x y = if x == y then return () else error (unlines [show x, "/=", show y])

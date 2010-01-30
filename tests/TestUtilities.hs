module TestUtilities (
    module TestUtilities,
    
    module Data.SWF,
    module Data.SWF.Internal.Binary,
    module System.FilePath
  ) where

import Data.SWF
import Data.SWF.Internal.Binary

import qualified Data.ByteString.Lazy as BS

import System.FilePath


readFileWords :: FilePath -> IO [Word8]
readFileWords = fmap BS.unpack . BS.readFile

run :: [Word8] -> SwfGet a -> a
run = runSwfGet emptySwfEnv . BS.pack

runGetSwf :: [Word8] -> Swf
runGetSwf = getSwf . BS.pack

assertEquals :: (Eq a, Show a) => a -> a -> IO ()
assertEquals x y = if x == y then return () else error (unlines [show x, "/=", show y])

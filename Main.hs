module Main (main) where

import Data.SWF

import qualified Data.ByteString.Lazy as BS

import System.Environment


main :: IO ()
main = do
    [file] <- getArgs
    bs <- BS.readFile file
    let swf = getSwf bs
    print swf
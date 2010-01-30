module Main (main) where

import Data.SWF

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import System.Environment


main :: IO ()
main = do
    [file] <- getArgs
    bs <- BS.readFile file
    
    let swf = getSwf bs
    
    {-
    print $ swf { tags = [] }
    forM_ (tags swf) $ \tag -> print (rECORD_recordHeader tag) >> print (rECORD_recordTag tag)
    -}
    
    print swf
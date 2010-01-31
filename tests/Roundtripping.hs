module Roundtripping where

import TestUtilities

import Control.Monad

import Data.List

import System.Directory


main :: IO ()
main = do
    roundtripPrimitives
    roundtripFiles


roundtripPrimitives :: IO ()
roundtripPrimitives = do
    quickCheck $ roundtrips getUI8 putUI8
    quickCheck $ roundtrips getUI16 putUI16
    quickCheck $ roundtrips getUI32 putUI32
    
    quickCheck $ roundtrips getSI8 putSI8
    quickCheck $ roundtrips getSI16 putSI16
    quickCheck $ roundtrips getSI32 putSI32
    
    quickCheck $ roundtrips getFLOAT16 putFLOAT16 . FLOAT16
    quickCheck $ roundtrips getFLOAT putFLOAT
    quickCheck $ roundtrips getDOUBLE putDOUBLE
    quickCheck $ roundtrips getEncodedU32 putEncodedU32
    
    let fitInto x n = x `mod` 2 ^ (n :: Int)
        fitIntoS _ 0 = 0
        fitIntoS x n = x `mod` 2 ^ (n - 1 :: Int)
        
        roundtripsBits' :: Eq a => (Int -> SwfGet a) -> (Int -> a -> SwfPut) -> Int -> a -> Bool
        roundtripsBits' getter putter n x = roundtrips (aligned $ getter n) (flushed . putter n) x
    quickCheck $ \n x -> (n >= 0 && n <= 32) ==> roundtripsBits' getUB putUB n (x `fitInto` n)
    quickCheck $ \n x -> (n >= 0 && n <= 32) ==> roundtripsBits' getUB putUB n (x `fitIntoS` n)
    quickCheck $ \n i d -> (n >= 0 && n <= 16) ==> roundtripsBits' getFB putFB (n + 16) (FIXED { fIXED_integer = i `fitIntoS` n, fIXED_decimal = d })


roundtrips :: Eq a => SwfGet a -> (a -> SwfPut) -> a -> Bool
roundtrips getter putter what
  = runSwfGet emptySwfEnv (runSwfPut emptySwfEnv (putter what)) getter == what


findFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFiles p dir = do
    entries <- fmap (filter (not . flip elem [".", ".."])) $ getDirectoryContents dir
    fmap concat $ forM (map (dir </>) entries) $ \entry -> do
        is_file <- doesFileExist entry
        if is_file
         then return [entry | p entry]
         else findFiles p entry

roundtripFiles :: IO ()
roundtripFiles = do
    files <- findFiles (".swf" `isSuffixOf`) "examples"
    forM_ files $ \file -> do
        bs <- readFileBS file
        let swf = getSwf bs
            bs' = putSwf swf
            swf' = getSwf bs'
        unless (bs == bs') $ sayNotEqual ["In " ++ file] swf' swf

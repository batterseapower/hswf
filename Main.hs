module Main (main) where

import Data.SWF

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import System.Console.CmdArgs


data HSwf = HSwf {
    files :: [FilePath],
    incremental :: Bool
  } deriving (Show, Data, Typeable)

hswf = mode $ HSwf {
    files       = def   &= args & text "Files to read",
    incremental = False &=        text "Output displayed incrementally"
  }


main :: IO ()
main = do
    HSwf{..} <- cmdArgs "HSwf v0.1, (C) 2010 Max Bolingbroke" [hswf]
    forM_ files $ \file -> do
        putStrLn $ "### Reading " ++ file
        
        swf <- fmap getSwf $ BS.readFile file
    
        if incremental
         then do
          print $ swf { tags = [] }
          forM_ (tags swf) print
         else
          print swf
module Main (main) where

import Data.SWF
import Data.SWF.Internal.Binary

import qualified Data.ByteString.Lazy as BS

import Test.QuickCheck


main :: IO ()
main = do
    let assertEquals x y = if x == y then return () else error (unlines [show x, "/=", show y])
        run = runSwfGet emptySwfGetEnv . BS.pack
    run [0x78] (getUB 5) `assertEquals` 15
    run [0x78, 0x00, 0x05, 0x5F, 0x00, 0x00, 0x0F, 0xA0, 0x00] getRECT
      `assertEquals` RECT { rECT_nbits = 15, rECT_xmin = 0, rECT_xmax = 11000, rECT_ymin = 0, rECT_ymax = 8000 }
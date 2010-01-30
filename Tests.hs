module Main (main) where

import Data.SWF
import Data.SWF.Internal.Binary

import qualified Data.ByteString.Lazy as BS

import Test.QuickCheck


main :: IO ()
main = do
    let assert s True  = return ()
        assert s False = error s
        run = runSwfGet emptySwfGetEnv . BS.pack
    assert "getUB 5" $ run [0x78] (getUB 5) == 15
    assert "getRECT" $ run [0x78, 0x00, 0x5F, 0x00, 0x00, 0x0F, 0xA0, 0x00] getRECT
                        == RECT { rECT_nbits = 5, rECT_xmin = 0, rECT_xmax = 11000, rECT_ymin = 0, rECT_ymax = 8000 }
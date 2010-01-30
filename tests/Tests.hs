import Data.SWF
import Data.SWF.Internal.Binary

import Numeric

import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List

import Test.QuickCheck


-- Useful for constructing tests:

showHexList :: Integral a => [a] -> String
showHexList xs = "[" ++ intercalate ", " ["0x" ++ padTo 2 '0' (map toUpper $ showHex x "") | x <- xs] ++ "]"

padTo :: Int -> a -> [a] -> [a]
padTo n c xs = replicate (n - length xs) c ++ xs

showListBinary :: Integral a => [a] -> String
showListBinary xs = intercalate " " [padTo 8 '0' (showBinary x "") | x <- xs]

showBinary :: Integral a => a -> ShowS
showBinary x = showIntAtBase 2 (\d -> toEnum (fromEnum '0' + d)) (fromIntegral x)


-- The actual tests:

vanilla_rect_bytes = [
    0x78, 0x00, 0x05, 0x5F, 0x00, 0x00, 0x0F, 0xA0, 0x00
  ]

vanilla_rect = RECT { rECT_nbits = 15, rECT_xmin = 0, rECT_xmax = 11000, rECT_ymin = 0, rECT_ymax = 8000 }


define_shape_bytes = [
    0x16, 0x00, 0x86, 0x4A, 0xF2, 0xD9, 0xAE, 0x3E,
    0x71, 0xBB, 0xD8, 0x01, 0x10, 0x95, 0x2D, 0xC3,
    0x0A, 0x79, 0x81, 0x91, 0xE4, 0xE2, 0xAB, 0xE1,
    0x80, 0x02, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x10,
    0x59, 0x9C, 0x00, 0x10, 0x16, 0x0B, 0x66, 0xA6,
    0xB5, 0x3B, 0xAC, 0xAA, 0x40, 0xCC, 0xAE, 0xE0,
    0x3F, 0x65, 0x7E, 0x64, 0x57, 0xBF, 0x12, 0x3A,
    0xFF, 0x2D, 0xBB, 0x60, 0x00
  ]

define_shape = DefineShape {}

main :: IO ()
main = do
    let assertEquals x y = if x == y then return () else error (unlines [show x, "/=", show y])
        run = runSwfGet emptySwfGetEnv . BS.pack
    run [0x78] (getUB 5) `assertEquals` 15
    run vanilla_rect_bytes getRECT `assertEquals` vanilla_rect
    run define_shape_bytes getDefineShape `assertEquals` define_shape
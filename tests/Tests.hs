import qualified Primitives
import qualified VanillaRect
import qualified DefineShapeAlignment

import Numeric

import Data.Char
import Data.List

import Test.QuickCheck


-- The actual tests:
main :: IO ()
main = do
    Primitives.main
    VanillaRect.main
    DefineShapeAlignment.main


-- Useful for constructing new tests:
showHexList :: Integral a => [a] -> String
showHexList xs = "[" ++ intercalate ", " ["0x" ++ padTo 2 '0' (map toUpper $ showHex x "") | x <- xs] ++ "]"

padTo :: Int -> a -> [a] -> [a]
padTo n c xs = replicate (n - length xs) c ++ xs

showListBinary :: Integral a => [a] -> String
showListBinary xs = intercalate " " [padTo 8 '0' (showBinary x "") | x <- xs]

showBinary :: Integral a => a -> ShowS
showBinary x = showIntAtBase 2 (\d -> toEnum (fromEnum '0' + d)) (fromIntegral x)

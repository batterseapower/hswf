module TestUtilities (
    module TestUtilities,
    
    module Data.SWF,
    module Data.SWF.Internal.Binary,
    module System.FilePath,
    module Test.QuickCheck
  ) where

import Data.SWF
import Data.SWF.Internal.Binary

import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as BS

import System.FilePath
import System.Random

import Test.QuickCheck (Arbitrary(..), Gen, Property, (==>), quickCheck, sized, choose, variant)


arbitraryIntegral :: (Random a, Integral a, Bounded a, Ord a) => Gen a
arbitraryIntegral = sized $ \n -> choose (0, min (fromIntegral n) maxBound)

coarbitraryIntegral :: (Random a, Integral a, Bounded a, Ord a) => a -> Gen b -> Gen b
coarbitraryIntegral n = variant (if n >= 0 then 2 * x else 2 * x + 1)
  where x = abs . fromIntegral $ n

randomREnum (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g

randomEnum g = randomR (minBound, maxBound) g

instance Arbitrary Word8 where
    arbitrary = arbitraryIntegral
    coarbitrary = coarbitraryIntegral

instance Random Word8 where
    randomR = randomREnum
    random = randomEnum

instance Arbitrary Word16 where
    arbitrary = arbitraryIntegral
    coarbitrary = coarbitraryIntegral

instance Random Word16 where
    randomR = randomREnum
    random = randomEnum

instance Arbitrary Word32 where
    arbitrary = arbitraryIntegral
    coarbitrary = coarbitraryIntegral

instance Random Word32 where
    randomR = randomREnum
    random = randomEnum

instance Arbitrary Word64 where
    arbitrary = arbitraryIntegral
    coarbitrary = coarbitraryIntegral

instance Random Word64 where
    randomR = randomREnum
    random = randomEnum

instance Arbitrary Int8 where
    arbitrary = arbitraryIntegral
    coarbitrary = coarbitraryIntegral

instance Random Int8 where
    randomR = randomREnum
    random = randomEnum

instance Arbitrary Int16 where
    arbitrary = arbitraryIntegral
    coarbitrary = coarbitraryIntegral

instance Random Int16 where
    randomR = randomREnum
    random = randomEnum

instance Arbitrary Int32 where
    arbitrary = arbitraryIntegral
    coarbitrary = coarbitraryIntegral

instance Random Int32 where
    randomR = randomREnum
    random = randomEnum

deriving instance Arbitrary EncodedU32
deriving instance Random EncodedU32


aligned :: SwfGet a -> SwfGet a
aligned what = do { x <- what; byteAlign; return x }

flushed :: SwfPutM a -> SwfPutM a
flushed what = do { x <- what; flushBits; return x }

readFileWords :: FilePath -> IO [Word8]
readFileWords = fmap BS.unpack . BS.readFile

readFileBS :: FilePath -> IO ByteString
readFileBS = BS.readFile

run :: [Word8] -> SwfGet a -> a
run = runSwfGet emptySwfEnv . BS.pack

runGetSwf :: [Word8] -> Swf
runGetSwf = getSwf . BS.pack

assertEquals :: (Eq a, Show a) => a -> a -> IO ()
assertEquals x y = if x == y then return () else sayNotEqual ["assertEquals failure"] x y

sayNotEqual :: Show a => [String] -> a -> a -> b
sayNotEqual msg x y = error (unlines $ msg ++ [show x, "/=", show y])

module Data.SWF.Internal.Binary (
    module Data.SWF.Internal.Binary,
    
    module Data.Bits,
    module Data.ByteString.Lazy,
    module Data.Int,
    module Data.Word
  ) where

import Data.SWF.Internal.Utilities

import Codec.Compression.Zlib

import qualified Data.Binary as B
import qualified Data.Binary.Get as B

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word


newtype SwfGetEnv = SwfGetEnv {
    swfVersion :: Word8
  }

emptySwfGetEnv :: SwfGetEnv
emptySwfGetEnv = SwfGetEnv { swfVersion = error "swfVersion not known yet!" }

newtype SwfGet a = SwfGet { unSwfGet :: SwfGetEnv -> Word8 -> Int -> B.Get (Word8, Int, a) }

instance Functor SwfGet where
    fmap = liftM

instance Monad SwfGet where
    return x = SwfGet $ \_ byte nbits -> return (byte, nbits, x)
    mx >>= fxmy = SwfGet $ \env byte nbits -> unSwfGet mx env byte nbits >>= \(byte, nbits, x) -> unSwfGet (fxmy x) env byte nbits

runSwfGet :: SwfGetEnv -> ByteString -> SwfGet a -> a
runSwfGet env bs mx = thd3 $ B.runGet (unSwfGet (checkConsumesAll mx) env 0 0) bs

checkConsumesAll :: SwfGet a -> SwfGet a
checkConsumesAll mx = SwfGet $ \env byte nbits -> do
    (byte, nbits, x) <- unSwfGet mx env byte nbits
    nbytes <- B.remaining
    if nbytes /= 0
     then error $ show nbytes ++ " trailing bytes - likely to be an error"
     else -- Allow trailing bits, because the alternative is too ugly
          {- if nbits /= 0
           then error $ show nbits ++ " trailing bits - likely to be an error"
           else -} return (byte, nbits, x)


nest' :: B.Get ByteString -> SwfGet a -> SwfGet a
nest' mrest mx = SwfGet $ \env _ nbits -> do
  if nbits /= 0
   then error "Nesting off a byte boundary - likely to be an error"
   else do
     rest <- mrest
     return (0, 0, runSwfGet env rest mx)

nestSwfGet :: Int64 -> SwfGet a -> SwfGet a
nestSwfGet len = nest' (B.getLazyByteString len)

decompressRemainder :: Int -> SwfGet a -> SwfGet a
decompressRemainder size_hint = nest' (fmap decompress B.getRemainingLazyByteString)
  where decompress = decompressWith (defaultDecompressParams { decompressBufferSize = size_hint })


liftGet :: Bool -> B.Get a -> SwfGet a
liftGet resetbits get = SwfGet $ \_ byte nbits -> fmap (if resetbits then (0, 0,) else (byte, nbits,)) get

getBits :: Integral a => a -> SwfGet Word32
getBits n | n <  0    = error "getBits: negative bits"
          | n > 32    = error "getBits: bit count greater than 32"
          | otherwise = SwfGet $ \_ byte nbits -> go byte nbits (fromIntegral n)
  where
    go byte nbits want_nbits
       -- Can we satisfy ourselves with just the bits from this byte?
      | want_nbits <= nbits = return (byte, nbits - want_nbits, fromIntegral (byte `shiftR` fromIntegral (nbits - want_nbits)) .&. (2 ^ want_nbits - 1))
       -- We need at least some of the next byte
      | otherwise           = do
        let want_nbits' = want_nbits - nbits
            this = fromIntegral (byte .&. (2 ^ nbits - 1)) `shiftL` fromIntegral want_nbits'
        byte <- B.getWord8
        (byte, nbits, rest) <- go byte 8 want_nbits'
        return (byte, nbits, this .|. rest)


getWord8 = liftGet True B.getWord8

getWord16 = liftGet True B.getWord16le

getWord32 = liftGet True B.getWord32le

getWord64 = liftGet True B.getWord64le

getLazyByteString = liftGet True . B.getLazyByteString

getLazyByteStringNul = liftGet True B.getLazyByteStringNul

getRemainingLazyByteString = liftGet True B.getRemainingLazyByteString

isEmpty = liftGet False B.isEmpty

lookAhead :: SwfGet a -> SwfGet a
lookAhead mx = SwfGet $ \env byte nbits -> fmap ((byte, nbits,) . thd3) (B.lookAhead (unSwfGet mx env byte nbits))

getToEnd :: SwfGet a -> SwfGet [a]
getToEnd mx = condM isEmpty (return []) $ do
                  x <- mx
                  fmap (x:) $ getToEnd mx

byteAlign :: SwfGet ()
byteAlign = SwfGet $ \env bytes nbits -> do
    (_, 0, remaining) <- unSwfGet (getBits nbits) env bytes nbits
    if remaining /= 0
     then error "Byte alignment discarded non-zero bits - probably an error"
     else return (0, 0, ())


modify :: (SwfGetEnv -> SwfGetEnv) -> SwfGet a -> SwfGet a
modify f act = SwfGet $ \env byte nbits -> unSwfGet act (f env) byte nbits

get :: SwfGet SwfGetEnv
get = SwfGet $ \env byte nbits -> return (byte, nbits, env)

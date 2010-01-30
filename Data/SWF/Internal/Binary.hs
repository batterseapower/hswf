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
import qualified Data.Binary.Put as B

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word


newtype SwfEnv = SwfEnv {
    swfVersion :: Word8
  }

emptySwfEnv :: SwfEnv
emptySwfEnv = SwfEnv { swfVersion = error "swfVersion not known yet!" }


-- Getting data

newtype SwfGet a = SwfGet { unSwfGet :: SwfEnv -> Word8 -> Int -> B.Get (Word8, Int, a) }

instance Functor SwfGet where
    fmap = liftM

instance Monad SwfGet where
    return x = SwfGet $ \_ byte nbits -> return (byte, nbits, x)
    mx >>= fxmy = SwfGet $ \env byte nbits -> unSwfGet mx env byte nbits >>= \(byte, nbits, x) -> unSwfGet (fxmy x) env byte nbits

modify :: (SwfEnv -> SwfEnv) -> SwfGet a -> SwfGet a
modify f act = SwfGet $ \env byte nbits -> unSwfGet act (f env) byte nbits

get :: SwfGet SwfEnv
get = SwfGet $ \env byte nbits -> return (byte, nbits, env)

runSwfGet :: SwfEnv -> ByteString -> SwfGet a -> a
runSwfGet env bs mx = thd3 $ B.runGet (unSwfGet (checkConsumesAll mx) env 0 0) bs


checkConsumesAll :: SwfGet a -> SwfGet a
checkConsumesAll mx = SwfGet $ \env byte nbits -> do
    (byte, nbits, x) <- unSwfGet mx env byte nbits
    nbytes <- B.remaining
    if nbytes /= 0
     then error $ show nbytes ++ " trailing bytes - likely to be an error"
     else if nbits /= 0
           then error $ show nbits ++ " trailing bits - likely to be an error"
           else return (byte, nbits, x)


nestSwfGetBS :: B.Get ByteString -> SwfGet a -> SwfGet a
nestSwfGetBS mrest mx = SwfGet $ \env _ nbits -> do
  if nbits /= 0
   then error "Nesting off a byte boundary - likely to be an error"
   else do
     rest <- mrest
     return (0, 0, runSwfGet env rest mx)

nestSwfGet :: Int64 -> SwfGet a -> SwfGet a
nestSwfGet len = nestSwfGetBS (B.getLazyByteString len)

decompressRemainder :: Int -> SwfGet a -> SwfGet a
decompressRemainder size_hint = nestSwfGetBS (fmap decompress B.getRemainingLazyByteString)
  where decompress = decompressWith (defaultDecompressParams { decompressBufferSize = size_hint })


liftGet :: B.Get a -> SwfGet a
liftGet get = SwfGet $ \_ byte nbits -> fmap (byte, nbits,) get

getWord8 = byteAlign >> liftGet B.getWord8

getWord16 = byteAlign >> liftGet B.getWord16le

getWord32 = byteAlign >> liftGet B.getWord32le

getWord64 = byteAlign >> liftGet B.getWord64le

getLazyByteString len = byteAlign >> liftGet (B.getLazyByteString len)

getLazyByteStringNul = byteAlign >> liftGet B.getLazyByteStringNul

getRemainingLazyByteString = byteAlign >> liftGet B.getRemainingLazyByteString

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

getToEnd :: SwfGet a -> SwfGet [a]
getToEnd mx = condM isEmpty (return []) $ do
                  x <- mx
                  fmap (x:) $ getToEnd mx

isEmpty = liftGet B.isEmpty

lookAhead :: SwfGet a -> SwfGet a
lookAhead mx = SwfGet $ \env byte nbits -> fmap ((byte, nbits,) . thd3) (B.lookAhead (unSwfGet mx env byte nbits))

byteAlign :: SwfGet ()
byteAlign = SwfGet $ \env bytes nbits -> do
    (_, 0, remaining) <- unSwfGet (getBits nbits) env bytes nbits
    if remaining /= 0
     then error "Byte alignment discarded non-zero bits - probably an error"
     else return (0, 0, ())

-- Putting data pack

type SwfPut = SwfPutM ()

newtype SwfPutM a = SwfPutM { unSwfPutM :: SwfEnv -> Word8 -> Int -> B.PutM (Word8, Int, a) }

instance Functor SwfPutM where
    fmap = liftM

instance Monad SwfPutM where
    return x = SwfPutM $ \_ byte nbits -> return (byte, nbits, x)
    mx >>= fxmy = SwfPutM $ \env byte nbits -> do
        (byte, nbits, x) <- unSwfPutM mx env byte nbits
        unSwfPutM (fxmy x) env byte nbits

runSwfPutM :: SwfEnv -> SwfPutM a -> (a, ByteString)
runSwfPutM env mx = first thd3 $ B.runPutM (unSwfPutM (checkFlushesAll mx) env 0 8)


checkFlushesAll :: SwfPutM a -> SwfPutM a 
checkFlushesAll mx = SwfPutM $ \env byte nbits -> do
    (byte, nbits, x) <- unSwfPutM mx env byte nbits
    if nbits /= 0
     then error $ show nbits ++ " unwritten bits - almost certainly an error"
     else return (byte, nbits, x)


nestSwfPutMBS :: (ByteString -> ByteString) -> SwfPutM a -> SwfPutM a
nestSwfPutMBS f mx = SwfPutM $ \env _ nbits ->
    if nbits /= 8
     then error $ show nbits ++ " desired bits when we reach a nested write - probably an error"
     else do
      let (x, bs) = runSwfPutM env mx
      B.putLazyByteString (f bs)
      return (0, 8, x)

compressRemainder :: Int -> SwfPutM a -> SwfPutM a
compressRemainder size_hint = nestSwfPutMBS compress
 where compress = compressWith (defaultCompressParams { compressBufferSize = size_hint })


liftPut :: B.PutM a -> SwfPutM a
liftPut put = SwfPutM $ \_ byte nbits -> fmap (byte, nbits,) put

putWord8 x = flushBits >> liftPut (B.putWord8 x)

putWord16 x = flushBits >> liftPut (B.putWord16le x)

putWord32 x = flushBits >> liftPut (B.putWord32le x)

putWord64 x = flushBits >> liftPut (B.putWord64le x)

putLazyByteString x = flushBits >> liftPut (B.putLazyByteString x)

putLazyByteStringNul x = flushBits >> liftPut (B.putLazyByteString x >> B.putWord8 0)

putBits :: Integral a => a -> Word32 -> SwfPut
putBits n x | n <  0    = error "putBits: negative bits"
            | n > 32    = error "putBits: bit count greater than 32"
            | otherwise = SwfPutM $ \_ byte nbits -> go byte nbits (fromIntegral n) x
  where
    go :: Word8 -> Int -> Int -> Word32 -> B.PutM (Word8, Int, ())
    go byte want_nbits nbits x
       -- Can we now output a complete byte?
      | want_nbits <= nbits = do
        B.putWord8 (byte .|. fromIntegral (x `shiftR` (nbits - want_nbits)))
        let nbits' = nbits - want_nbits
            rest_mask = (1 `shiftL` nbits') - 1
        go 0 8 nbits' (x .&. rest_mask)
       -- We need at least 1 more bit to output something
      | otherwise = return (byte .|. fromIntegral (x `shiftL` (want_nbits - nbits)), want_nbits - nbits, ())

flushBits :: SwfPut
flushBits = SwfPutM $ \_ byte _nbits -> B.putWord8 byte >> return (0, 8, ())

{-# LANGUAGE GADTs, Rank2Types #-}
module Main where

import Control.Arrow ((***))
import Control.Monad

import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy
import Data.Int
import Data.Ratio
import Data.Word
import Data.Maybe

import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

import System.IO.Unsafe


orElse = flip fromMaybe

fst3 (a, _, _) = a
snd3 (_, b, _) = b
thd3 (_, _, c) = c


type Twips = Integer
type LogicalPixels = Integer

twipsToLogicalPixels :: Twips -> LogicalPixels
twipsToLogicalPixels = (`div` 20)


data Point = Point { x :: Twips, y :: Twips }


-- p12: Integer types and byte order

type SI8 = Int8
type SI16 = Int16
type SI32 = Int32

getSI8 :: Get SI8
getSI8 = fmap fromIntegral getWord8

getSI16 :: Get SI16
getSI16 = fmap fromIntegral getWord16le

getSI32 :: Get SI32
getSI32 = fmap fromIntegral getWord32le

--type SI18[n]
--type SI16[n]

type UI8 = Word8
type UI16 = Word16
type UI32 = Word32

getUI8 :: Get UI8
getUI8 = fmap fromIntegral getWord8

getUI16 :: Get UI16
getUI16 = fmap fromIntegral getWord16le

getUI32 :: Get UI32
getUI32 = fmap fromIntegral getWord32le

--type UI8[n]
--type U116[n]
--type U124[n]
--type U132[n]
--type U164[n]


-- p12: Fixed-point numbers

data FIXED = FIXED SI16 UI16
data FIXED8 = FIXED8 SI8 UI8

getFIXED :: Get FIXED
getFIXED = liftM2 (flip FIXED) getUI16 getSI16

getFIXED8 :: Get FIXED8
getFIXED8 = liftM2 (flip FIXED8) getUI8 getSI8

fIXEDToRational (FIXED whole decimal) = fromIntegral whole + (fromIntegral decimal % 65535)
fIXED8ToRational (FIXED8 whole decimal) = fromIntegral whole + (fromIntegral decimal % 255)

fIXEDToFractional :: Fractional a => FIXED -> a
fIXEDToFractional = fromRational . fIXEDToRational

fIXED8ToFractional :: Fractional a => FIXED8 -> a
fIXED8ToFractional = fromRational . fIXED8ToRational


-- Page 13: Floating-point numbers

newtype FLOAT16 = FLOAT16 Float
type FLOAT = Float
type DOUBLE = Double

getFLOAT16 :: Get FLOAT16
getFLOAT16 = do
    w <- getWord16le
    let sign  = (w `shiftR` 15) .&. 0x1
        expon = (w `shiftR` 10) .&. 0x1F - 16
        manti = (w `shiftR` 0)  .&. 0x3FF
        promote = (realToFrac :: CFloat -> Float) . storableCast
    return $ FLOAT16 $ (1 + (promote manti / 0x400)) * 2 ^^ expon

getFLOAT :: Get FLOAT
getFLOAT = fmap word32ToDouble getWord32le

getDOUBLE :: Get DOUBLE
getDOUBLE = fmap word64ToDouble getWord64le

word32ToDouble :: Word32 -> Float
word32ToDouble = (realToFrac :: CFloat -> Float) . storableCast

word64ToDouble :: Word64 -> Double
word64ToDouble = (realToFrac :: CDouble -> Double) . storableCast

storableCast :: (Storable a, Storable b) => a -> b
storableCast w = unsafePerformIO $ with w $ peek . castPtr


-- Page 14: encoded integers

newtype ENCODEDU32 = ENCODEDU32 UI32

getENCODEDU32 :: Get ENCODEDU32
getENCODEDU32 = fmap ENCODEDU32 $ do
    i0@res <- fmap fromIntegral getWord8
    if i0 .&. 0x80 == 0
     then return res
     else do
       i1 <- fmap fromIntegral getWord8
       res <- return $ (i0 .&. 0x7F) .|. (i1 `shiftL` 7)
       if i1 .&. 0x80 == 0
        then return res
        else do
          i2 <- fmap fromIntegral getWord8
          res <- return $ (res .&. 0x3FFF) .|. (i2 `shiftL` 14)
          if i2 .&. 0x80 == 0
           then return res
           else do
             i3 <- fmap fromIntegral getWord8
             res <- return $ (res .&. 0x1FFFFF) .|. (i3 `shiftL` 21)
             if i3 .&. 0x80 == 0
              then return res
              else do
                i4 <- fmap fromIntegral getWord8
                return $ (res .&. 0xFFFFFFF) .|. (i4 `shiftL` 28)


-- Page 15: bit values

type NBits = UI32

data BVDesc a where
    SBDesc :: BVDesc SB
    UBDesc :: BVDesc UB
    FBDesc :: BVDesc FB

data BS a where
    BSNil :: BS ()
    BSCons :: NBits -> BVDesc a -> (a -> BS b) -> BS (a, b)

newtype SB = SB SI32
newtype UB = UB UI32
newtype FB = FB FIXED

getBS :: BS a -> Get a
getBS = go 0xCC 0
  where
    go :: Word8 -> NBits -> BS a -> Get a
    go byte nbits BSNil = return ()
    go byte nbits (BSCons want_nbits bvdesc bs_fn) = do
      (byte, nbits, bits) <- accumulate byte nbits want_nbits
      let this = interpret bvdesc nbits bits
      rest <- go byte nbits (bs_fn this)
      return (this, rest)

    accumulate :: Word8 -> NBits -> NBits -> Get (Word8, NBits, Word32)
    accumulate byte nbits want_nbits
       -- Can we satisfy ourselves with just the bits from this byte?
      | want_nbits <= nbits = return (byte, nbits - want_nbits, fromIntegral (byte `shiftR` fromIntegral (nbits - want_nbits)) .&. (2 ^ want_nbits - 1))
       -- We need at least some of the next byte
      | otherwise           = do
        let want_nbits' = want_nbits - nbits
            this = fromIntegral (byte .&. (2 ^ nbits - 1)) `shiftL` fromIntegral want_nbits'
        byte <- getWord8
        (byte, nbits, rest) <- accumulate byte 8 want_nbits'
        return (byte, nbits, this .|. rest)

    interpret :: BVDesc a -> NBits -> Word32 -> a
     -- 1110b = -2
     -- 0x30000 (19 bit) = 196608
    interpret SBDesc nbits bits = SB $ signextend nbits bits
     -- 1110b = 14
    interpret UBDesc _     bits = UB bits
     -- 0x30000 (19 bit) = 3.0
    interpret FBDesc nbits bits = FB $ FIXED (signextend (nbits - 16) $ bits `shiftR` 16) (fromIntegral $ bits .&. 0xFFFF)

    signextend nbits bits = fromIntegral $ bits .|. complement (2 ^ nbits - 1)


-- p17: String values

-- SWF <= 5: ANSI or shift-JIS encoding. No way to tell.
-- SWF >  5: UTF-8
type STRING = ByteString

getSTRING :: Get STRING
getSTRING = getLazyByteStringNul


-- p18: Language code

data LANGCODE = None | Latin | Japanese | Korean | SimplifiedChinese | TraditionalChinese | Unrecognized UI8

getLANGCODE :: Get LANGCODE
getLANGCODE = do
    n <- getUI8
    return $ case n of
      0 -> None
      1 -> Latin
      2 -> Japanese
      3 -> Korean
      4 -> SimplifiedChinese
      5 -> TraditionalChinese
      _ -> Unrecognized n


-- p18: RGB color record

data RGB = RGB { red :: UI8, green :: UI8, blue :: UI8 }

getRGB = liftM3 RGB getUI8 getUI8 getUI8


-- p19: RGBA color record/ARGB color record

data RGBA = RGBA { rgb :: RGB, alpha :: UI8 }
type ARGB = RGBA

getRGBA = liftM2 RGBA getRGB getUI8

getARGB = liftM2 (flip RGBA) getUI8 getRGB


-- p20: Rectangle record

data RECT = RECT { xMin :: SI32, xMax :: SI32, yMin :: SI32, yMax :: SI32 }

getRECT = do
    (_, (SB xmin, (SB xmax, (SB ymin, (SB ymax, ()))))) <- getBS $
        BSCons 5 UBDesc $ \(UB nbits) ->
        BSCons nbits SBDesc $ const $
        BSCons nbits SBDesc $ const $
        BSCons nbits SBDesc $ const $
        BSCons nbits SBDesc $ const $
        BSNil
    return $ RECT xmin xmax ymin ymax


-- p20: MATRIX record

data MATRIX = MATRIX { scale :: Maybe (FIXED, FIXED), rotateSkew :: Maybe (FIXED, FIXED), translate :: (SI32, SI32) }

getMATRIX = do
    (UB hasScale, (_, (FB scaleX, (FB scaleY, (UB hasRotate, (_, (FB rotateSkew0, (FB rotateSkew1, (_, (SB translateX, (SB translateY, ()))))))))))) <- getBS $
        BSCons 1 UBDesc $ \(UB hasScale) ->
        BSCons (if hasScale /= 0 then 5 else 0) UBDesc $ \(UB nScaleBits) ->
        BSCons nScaleBits FBDesc $ const $
        BSCons nScaleBits FBDesc $ const $
        BSCons 1 UBDesc $ \(UB hasRotate) ->
        BSCons (if hasRotate /= 0 then 5 else 0) UBDesc $ \(UB nRotateBits) ->
        BSCons nRotateBits FBDesc $ const $
        BSCons nRotateBits FBDesc $ const $
        BSCons 5 UBDesc $ \(UB nTranslateBits) ->
        BSCons nTranslateBits SBDesc $ const $
        BSCons nTranslateBits SBDesc $ const $
        BSNil
    return $ MATRIX (if hasScale /= 0 then Just (scaleX, scaleY) else Nothing)
                    (if hasRotate /= 0 then Just (rotateSkew0, rotateSkew1) else Nothing)
                    (translateX, translateY)

transformByMATRIX :: Fractional a => (a, a) -> MATRIX -> (a, a)
transformByMATRIX (x, y) m = (x *  (fst scale') + y * snd rotateSkew' + fromIntegral (fst (translate m)),
                              x * fst rotateSkew' + y * snd scale' + fromIntegral (snd (translate m)))
  where scale' = maybe (0, 0) (fIXEDToFractional *** fIXEDToFractional) $ scale m
        rotateSkew' = maybe (0, 0) (fIXEDToFractional *** fIXEDToFractional) $ rotateSkew m


-- p22: Color transform record

data CXFORM = CXFORM { multTerms :: Maybe (SI32, SI32, SI32), addTerms :: Maybe (SI32, SI32, SI32) }

getCXFORM = do
    (UB hasAddTerms, (UB hasMultTerms, (_, (SB redMultTerm, (SB greenMultTerm, (SB blueMultTerm, (SB redAddTerm, (SB greenAddTerm, (SB blueAddTerm, ()))))))))) <- getBS $
        BSCons 1 UBDesc $ \(UB hasAddTerms) ->
        BSCons 1 UBDesc $ \(UB hasMultTerms) ->
        BSCons 4 UBDesc $ \(UB nbits) ->
        BSCons (hasMultTerms * nbits) SBDesc $ const $
        BSCons (hasMultTerms * nbits) SBDesc $ const $
        BSCons (hasMultTerms * nbits) SBDesc $ const $
        BSCons (hasAddTerms * nbits) SBDesc $ const $
        BSCons (hasAddTerms * nbits) SBDesc $ const $
        BSCons (hasAddTerms * nbits) SBDesc $ const $
        BSNil
    return $ CXFORM (if hasMultTerms /= 0 then Just (redMultTerm, greenMultTerm, blueMultTerm) else Nothing)
                    (if hasAddTerms /= 0 then Just (redAddTerm, greenAddTerm, blueAddTerm) else Nothing)

transformByCXFORM :: Integral a => (a, a, a) -> CXFORM -> (a, a, a)
transformByCXFORM rgb c = (component fst3, component snd3, component thd3)
   where multTerms' = multTerms c `orElse` (1, 1, 1)
         addTerms' = addTerms c `orElse` (0, 0, 0)
         clamp x = round $ max 0 $ min 255 $ x
         component :: Integral a => (forall b. (b, b, b) -> b) -> a
         component sel = clamp ((fromIntegral (sel rgb) * fromIntegral (sel multTerms') / (256.0 :: Rational)) + fromIntegral (sel addTerms'))


-- p23: Color transform with alpha record

data CXFORMWITHALPHA = CXFORMWITHALPHA { }


main :: IO ()
main = return ()

\begin{code}
module Data.SWF.Format where

import Data.SWF.Internal.Binary
import Data.SWF.Internal.Utilities

import Data.Char
import Data.Data
import Data.Ratio

import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

import System.IO.Unsafe

\end{code}

TODOS
~~~~~

1) Turn more bitfields into proper enumerations
2) Expand some of those BYTE[] fields 
  * In particular, the zlib compressed fields in the image chapter
  * Perhaps also those embedding sound and video formats
3) Reduce semantic junk
  * NBits fields, even those in CXFORM and stuff like mATRIX_scaleBits
  * "NumFillBits" and the like...
5) Simplify away generated consistency checks that are trivially true
7) Generate comments on constructor fields and add them to custom ones
8) Represent some [UI8] as ByteString?
10) Clean up names in putters and getters: don't give excluded fields record-prefixed names
11) Review all handled tags to ensure that changing the size due to the roundtrip won't screw up any offset fields (hmm!)


Roundtripping
~~~~~~~~~~~~~

We do *not* implement perfect roundtripping -- but we guarantee to
roundtrip anything about the file that affects how it plays back.
The things preventing us from having perfect roundtripping are:
1) Fields that control the number of bits used to encode other fields.
   We discard this information and write back using the minimum number of bits.
   (TODO: implement this)
2) Long vs. short count fields. For example, the RECORDHEADER length field
   can be coded using either a long or short count field if the length is less
   than 0x3F. We always write back using the shortest possible representation.
3) ZLib (compress . decompress) may not be the identity (haven't observed this
   in practice though)


Chapter 1: Basic Data Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

p12: Integer types and byte order
\begin{code}

type SI8 = Int8
type SI16 = Int16
type SI32 = Int32

getSI8 :: SwfGet SI8
getSI8 = fmap fromIntegral getWord8

putSI8 :: SI8 -> SwfPut
putSI8 = putWord8 . fromIntegral

getSI16 :: SwfGet SI16
getSI16 = fmap fromIntegral getWord16

putSI16 :: SI16 -> SwfPut
putSI16 = putWord16 . fromIntegral

getSI32 :: SwfGet SI32
getSI32 = fmap fromIntegral getWord32

putSI32 :: SI32 -> SwfPut
putSI32 = putWord32 . fromIntegral

type UI8 = Word8
type UI16 = Word16
type UI32 = Word32

getUI8 :: SwfGet UI8
getUI8 = getWord8

putUI8 :: UI8 -> SwfPut
putUI8 = putWord8

getUI16 :: SwfGet UI16
getUI16 = getWord16

putUI16 :: UI16 -> SwfPut
putUI16 = putWord16

getUI32 :: SwfGet UI32
getUI32 = getWord32

putUI32 :: UI32 -> SwfPut
putUI32 = putWord32

\end{code}

p12: Fixed-point numbers
\begin{code}
 
data FIXED = FIXED{fIXED_decimal :: UI16, fIXED_integer :: SI16}
           deriving (Eq, Show, Typeable, Data)
getFIXED
  = do fIXED_decimal <- getUI16
       fIXED_integer <- getSI16
       return (FIXED{..})
putFIXED FIXED{..}
  = do putUI16 fIXED_decimal
       putSI16 fIXED_integer
       return ()

\end{code}

\begin{code}

fIXEDToRational :: FIXED -> Rational
fIXEDToRational fixed = fromIntegral (fIXED_integer fixed) + (fromIntegral (fIXED_decimal fixed) % 65535)

fIXEDToFractional :: Fractional a => FIXED -> a
fIXEDToFractional = fromRational . fIXEDToRational

\end{code}

\begin{code}
 
data FIXED8 = FIXED8{fIXED8_decimal :: UI8, fIXED8_integer :: SI8}
            deriving (Eq, Show, Typeable, Data)
getFIXED8
  = do fIXED8_decimal <- getUI8
       fIXED8_integer <- getSI8
       return (FIXED8{..})
putFIXED8 FIXED8{..}
  = do putUI8 fIXED8_decimal
       putSI8 fIXED8_integer
       return ()

\end{code}

\begin{code}

fIXED8ToRational :: FIXED8 -> Rational
fIXED8ToRational fixed8 = fromIntegral (fIXED8_integer fixed8) + (fromIntegral (fIXED8_decimal fixed8) % 255)

fIXED8ToFractional :: Fractional a => FIXED8 -> a
fIXED8ToFractional = fromRational . fIXED8ToRational

\end{code}

Page 13: Floating-point numbers
\begin{code}

newtype FLOAT16 = FLOAT16 Word16
                deriving (Eq, Show, Typeable, Data)

storableCast :: (Storable a, Storable b) => a -> b
storableCast w = unsafePerformIO $ with w $ peek . castPtr

getFLOAT16 :: SwfGet FLOAT16
getFLOAT16 = fmap FLOAT16 getWord16

putFLOAT16 :: FLOAT16 -> SwfPut
putFLOAT16 (FLOAT16 x) = putWord16 x

fLOAT16ToFloat :: FLOAT16 -> Float
fLOAT16ToFloat (FLOAT16 w)
  | expon == 0x00 -- Zero or denorm (TODO: produce denorms)
  = 0.0
  | expon == 0x1F -- Infinity or NaN
  = if manti /= 0 then 0/0 else (signer 1 / 0)
  | otherwise -- Simple number
  = signer $ (1.0 + (fromIntegral manti / 1024.0)) * 2 ^ (expon - 16)
  where
    sign  = (w `shiftR` 15) .&. 0x001
    expon = (w `shiftR` 10) .&. 0x01F
    manti = w               .&. 0x3FF

    signer = if sign /= 0 then negate else id

floatToFLOAT16 :: Float -> FLOAT16
floatToFLOAT16 x
  | isNaN x = FLOAT16 0x7FFF -- Extra NaN handling because it seems NaN mantissa bits can be wrong...
  | otherwise = FLOAT16 w'
  where
    -- CFloat:  seeeeeeeemmmmmmmmmmmmmmmmmmmmmmm (1, 8, 23), bias = 127
    -- FLOAT16: seeeeemmmmmmmmmm                 (1, 5, 10), bias = 16
    depromote = (storableCast :: CFloat -> Word32) . realToFrac
    w = depromote x
    sign          = w `shiftR` 31 .&. 0x00000001
    biased_expon  = w `shiftR` 23 .&. 0x000000FF
    manti         = w             .&. 0x007FFFFF
  
    trimmed_manti = manti `shiftR` 13
  
    sign'         = sign
    (biased_expon', manti')
      | biased_expon == 0 -- Zero or denorm. May turn denorms into zeroes if they are too small - just accept that
      = (0x0, trimmed_manti)
      | biased_expon == 0xFF -- Infinity or NaN. Doesn't preserve which specific NaN we had.
      = (0x1F, if manti == 0 then 0x000 else 0x3FF)
      | biased_expon <= 127 - 16 -- Too small: (-)0
      = (0x00, 0x000)
      | biased_expon >= (127 - 16 + ((2^5) - 1)) -- Too big: (-)Infinity
      = (0x1F, 0x000)
      | otherwise -- Just right!
      = (biased_expon - (127 - 16), trimmed_manti)

    w' = fromIntegral $ (sign'         `shiftL` 15) .|.
                        (biased_expon' `shiftL` 10) .|.
                        manti'

    {-
    trace (unlines [showBinary w "" ++ " (" ++ show x ++ ")" ++ " ==> " ++ showBinary w' "",
                    showBinary sign "" ++ " ==> " ++ showBinary sign' "",
                    showBinary biased_expon' "" ++ " (" ++ show biased_expon ++ ")" ++ " ==> " ++ showBinary biased_expon' "" ++ " (" ++ show biased_expon' ++ ")",
                    showBinary manti "" ++ " ==> " ++  showBinary manti' ""]) $ return ()
    -}

\end{code}

\begin{code}

type FLOAT = Float

getFLOAT :: SwfGet FLOAT
getFLOAT = fmap word32ToFloat getWord32

putFLOAT :: FLOAT -> SwfPut
putFLOAT = putWord32 . floatToWord32

word32ToFloat :: Word32 -> Float
word32ToFloat = (realToFrac :: CFloat -> Float) . storableCast

floatToWord32 :: Float -> Word32
floatToWord32 = storableCast . (realToFrac :: Float -> CFloat)

\end{code}

\begin{code}

type DOUBLE = Double

getDOUBLE :: SwfGet DOUBLE
getDOUBLE = fmap word64ToDouble getWord64

putDOUBLE :: DOUBLE -> SwfPut
putDOUBLE = putWord64 . doubleToWord64

word64ToDouble :: Word64 -> Double
word64ToDouble = (realToFrac :: CDouble -> Double) . storableCast

doubleToWord64 :: Double -> Word64
doubleToWord64 = storableCast . (realToFrac :: Double -> CDouble)

\end{code}

Page 14: encoded integers
\begin{code}

newtype EncodedU32 = EncodedU32 UI32
                   deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Typeable, Data)

getEncodedU32 :: SwfGet EncodedU32
getEncodedU32 = fmap EncodedU32 $ do
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

putEncodedU32 :: EncodedU32 -> SwfPut
putEncodedU32 (EncodedU32 x) = do
    x <- put x
    when (x /= 0) $ do
      x <- put x
      when (x /= 0) $ do
        x <- put x
        when (x /= 0) $ do
          x <- put x
          when (x /= 0) $ do
            0 <- put x
            return ()
  where
    put x = do
      let x' = x `shiftR` 7
      putWord8 (fromIntegral (x .&. 0x7F) .|. (if x' /= 0 then 0x80 else 0x00))
      return x'

\end{code}

Page 15: bit values
\begin{code}

type UB = UI32

 -- 1110b = 14
getUB :: Integral a => a -> SwfGet UB
getUB = getBits

putUB :: Integral a => a -> UB -> SwfPut
putUB = putBits

getFlag :: SwfGet Bool
getFlag = fmap (/= 0) (getUB 1)

putFlag :: Bool -> SwfPut
putFlag x = putUB 1 (if x then 1 else 0)

requiredBitsUB :: Integral a => UB -> a
requiredBitsUB = ceiling . logBase 2 . (+1) . fromIntegral

\end{code}

\begin{code}

type SB = SI32

signextend :: (Integral a, Integral b) => a -> Word32 -> b
signextend nbits bits
  -- From http://graphics.stanford.edu/~seander/bithacks.html#VariableSignExtend
  -- We want to copy the sign bit into all of bits higher than nbits in the Word32
  -- before we convert that to the desired integral type.
  = fromIntegral ((bits `xor` signbitmask) - signbitmask)
  where signbitmask = 1 `shiftL` (fromIntegral $ nbits - 1)

 -- 1110b = -2
 -- 0x30000 (19 bit) = 196608
getSB :: Integral a => a -> SwfGet SB
getSB nbits = fmap (signextend nbits) (getBits nbits)

signretract :: (Integral a, Integral b) => a -> b -> Word32
signretract nbits num
  -- From http://homepage.mac.com/randyhyde/webster.cs.ucr.edu/www.artofasm.com/Linux/HTML/DataRepresentation5.html
  -- This will overflow if the bits above the new sign bit are actually different
  -- from the sign bit. TODO: check for that condition.
  = lowerbitsmask .&. (fromIntegral num)
  where lowerbitsmask = (1 `shiftL` fromIntegral nbits) - 1

putSB :: Integral a => a -> SB -> SwfPut
putSB nbits = putBits nbits . signretract nbits

-- An n bit number can store values in the range:
--   -2^(n-1) to 2^(n-1)-1
requiredBitsSB :: Integral a => SB -> a
requiredBitsSB 0 = 0
requiredBitsSB x = 1 + ceiling (logBase 2 (fromIntegral $ if x < 0 then negate x else x + 1))

\end{code}

\begin{code}

type FB = FIXED

 -- 0x30000 (19 bit) = 3.0
getFB :: Integral a => a -> SwfGet FB
getFB nbits = fmap parse (getBits nbits)
  where parse bits = FIXED { fIXED_integer = signextend (nbits - 16) (bits `shiftR` 16), fIXED_decimal = fromIntegral (bits .&. 0xFFFF) }

putFB :: Integral a => a -> FB -> SwfPut
putFB nbits fixed = putBits nbits $ ((signretract (nbits - 16) $ fIXED_integer fixed) `shiftL` 16) .|. fromIntegral (fIXED_decimal fixed)

requiredBitsFB :: Integral a => FB -> a
requiredBitsFB (FIXED {..})
  | fIXED_integer == 0 = requiredBitsUB . fromIntegral $ fIXED_decimal
  | otherwise          = (+16) . requiredBitsSB . fromIntegral $ fIXED_integer

\end{code}

p17: String values
\begin{code}

-- SWF <= 5: ANSI or shift-JIS encoding. No way to tell.
-- SWF >  5: UTF-8
type STRING = ByteString

getSTRING :: SwfGet STRING
getSTRING = getLazyByteStringNul

putSTRING :: STRING -> SwfPut
putSTRING = putLazyByteStringNul

\end{code}

p18: Language code
\begin{code}

data LANGCODE = NoLanguage | LatinLanguage | JapaneseLanguage | KoreanLanguage | SimplifiedChineseLanguage | TraditionalChineseLanguage | UnknownLanguage UI8
              deriving (Eq, Show, Typeable, Data)

getLANGCODE :: SwfGet LANGCODE
getLANGCODE = do
    n <- getUI8
    return $ case n of
      0 -> NoLanguage
      1 -> LatinLanguage
      2 -> JapaneseLanguage
      3 -> KoreanLanguage
      4 -> SimplifiedChineseLanguage
      5 -> TraditionalChineseLanguage
      _ -> UnknownLanguage n

putLANGCODE :: LANGCODE -> SwfPut
putLANGCODE lc = case lc of
    NoLanguage                 -> putUI8 0
    LatinLanguage              -> putUI8 1
    JapaneseLanguage           -> putUI8 2
    KoreanLanguage             -> putUI8 3
    SimplifiedChineseLanguage  -> putUI8 4
    TraditionalChineseLanguage -> putUI8 5
    UnknownLanguage x          -> putUI8 x

\end{code}

p18: RGB color record
\begin{code}
 
data RGB = RGB{rGB_red :: UI8, rGB_green :: UI8, rGB_blue :: UI8}
         deriving (Eq, Show, Typeable, Data)
getRGB
  = do rGB_red <- getUI8
       rGB_green <- getUI8
       rGB_blue <- getUI8
       return (RGB{..})
putRGB RGB{..}
  = do putUI8 rGB_red
       putUI8 rGB_green
       putUI8 rGB_blue
       return ()

\end{code}

p19: RGBA color record/ARGB color record
\begin{code}
 
data RGBA = RGBA{rGBA_red :: UI8, rGBA_green :: UI8,
                 rGBA_blue :: UI8, rGBA_alpha :: UI8}
          deriving (Eq, Show, Typeable, Data)
getRGBA
  = do rGBA_red <- getUI8
       rGBA_green <- getUI8
       rGBA_blue <- getUI8
       rGBA_alpha <- getUI8
       return (RGBA{..})
putRGBA RGBA{..}
  = do putUI8 rGBA_red
       putUI8 rGBA_green
       putUI8 rGBA_blue
       putUI8 rGBA_alpha
       return ()

\end{code}

\begin{code}
 
data ARGB = ARGB{aRGB_alpha :: UI8, aRGB_red :: UI8,
                 aRGB_green :: UI8, aRGB_blue :: UI8}
          deriving (Eq, Show, Typeable, Data)
getARGB
  = do aRGB_alpha <- getUI8
       aRGB_red <- getUI8
       aRGB_green <- getUI8
       aRGB_blue <- getUI8
       return (ARGB{..})
putARGB ARGB{..}
  = do putUI8 aRGB_alpha
       putUI8 aRGB_red
       putUI8 aRGB_green
       putUI8 aRGB_blue
       return ()

\end{code}

p20: Rectangle record
\begin{code}
 
data RECT = RECT{rECT_xmin :: SB, rECT_xmax :: SB, rECT_ymin :: SB,
                 rECT_ymax :: SB}
          deriving (Eq, Show, Typeable, Data)
getRECT
  = do rECT_nbits <- getUB 5
       rECT_xmin <- getSB rECT_nbits
       rECT_xmax <- getSB rECT_nbits
       rECT_ymin <- getSB rECT_nbits
       rECT_ymax <- getSB rECT_nbits
       discardReserved "_reserved (x :: ?)" byteAlign
       return (RECT{..})
putRECT RECT{..}
  = do let rECT_nbits
             = requiredBitsSB rECT_xmin `max` requiredBitsSB rECT_xmax `max`
                 requiredBitsSB rECT_ymin
                 `max` requiredBitsSB rECT_ymax
       if requiredBitsUB rECT_nbits <= 5 then putUB 5 rECT_nbits else
         inconsistent "x :: RECT"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB rECT_nbits) ++
                " bits to store the value " ++
                  show rECT_nbits ++ ", but only have available " ++ show 5)
       if requiredBitsSB rECT_xmin <= rECT_nbits then
         putSB rECT_nbits rECT_xmin else
         inconsistent "rECT_xmin (x :: RECT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB rECT_xmin) ++
                " bits to store the value " ++
                  show rECT_xmin ++ ", but only have available " ++ show rECT_nbits)
       if requiredBitsSB rECT_xmax <= rECT_nbits then
         putSB rECT_nbits rECT_xmax else
         inconsistent "rECT_xmax (x :: RECT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB rECT_xmax) ++
                " bits to store the value " ++
                  show rECT_xmax ++ ", but only have available " ++ show rECT_nbits)
       if requiredBitsSB rECT_ymin <= rECT_nbits then
         putSB rECT_nbits rECT_ymin else
         inconsistent "rECT_ymin (x :: RECT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB rECT_ymin) ++
                " bits to store the value " ++
                  show rECT_ymin ++ ", but only have available " ++ show rECT_nbits)
       if requiredBitsSB rECT_ymax <= rECT_nbits then
         putSB rECT_nbits rECT_ymax else
         inconsistent "rECT_ymax (x :: RECT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB rECT_ymax) ++
                " bits to store the value " ++
                  show rECT_ymax ++ ", but only have available " ++ show rECT_nbits)
       let rECT_padding = reservedDefault
       const flushBits (rECT_padding :: ())
       return ()

\end{code}

p20: MATRIX record

\begin{code}
 
data MATRIX = MATRIX{mATRIX_scale :: Maybe (FB, FB),
                     mATRIX_rotate :: Maybe (FB, FB), mATRIX_translateX :: SB,
                     mATRIX_translateY :: SB}
            deriving (Eq, Show, Typeable, Data)
getMATRIX
  = do mATRIX_hasScale <- getFlag
       mATRIX_scale <- maybeHas mATRIX_hasScale
                         (do mATRIX_scaleBits <- getUB 5
                             mATRIX_scaleX <- getFB mATRIX_scaleBits
                             mATRIX_scaleY <- getFB mATRIX_scaleBits
                             return (mATRIX_scaleX, mATRIX_scaleY))
       mATRIX_hasRotate <- getFlag
       mATRIX_rotate <- maybeHas mATRIX_hasRotate
                          (do mATRIX_rotateBits <- getUB 5
                              mATRIX_rotateSkew0 <- getFB mATRIX_rotateBits
                              mATRIX_rotateSkew1 <- getFB mATRIX_rotateBits
                              return (mATRIX_rotateSkew0, mATRIX_rotateSkew1))
       mATRIX_translateBits <- getUB 5
       mATRIX_translateX <- getSB mATRIX_translateBits
       mATRIX_translateY <- getSB mATRIX_translateBits
       discardReserved "_reserved (x :: ?)" byteAlign
       return (MATRIX{..})
putMATRIX MATRIX{..}
  = do let mATRIX_hasScale = isJust mATRIX_scale
       putFlag mATRIX_hasScale
       case mATRIX_scale of
           Just x | mATRIX_hasScale ->
                    case x of
                        (mATRIX_scaleX, mATRIX_scaleY) -> do let mATRIX_scaleBits
                                                                   = requiredBitsFB mATRIX_scaleX
                                                                       `max`
                                                                       requiredBitsFB mATRIX_scaleY
                                                             if requiredBitsUB mATRIX_scaleBits <= 5
                                                               then putUB 5 mATRIX_scaleBits else
                                                               inconsistent
                                                                 "fromJust (mATRIX_scale (x :: MATRIX))"
                                                                 ("Bit count incorrect: required "
                                                                    ++
                                                                    show
                                                                      (requiredBitsUB
                                                                         mATRIX_scaleBits)
                                                                      ++
                                                                      " bits to store the value " ++
                                                                        show mATRIX_scaleBits ++
                                                                          ", but only have available "
                                                                            ++ show 5)
                                                             if
                                                               requiredBitsFB mATRIX_scaleX <=
                                                                 mATRIX_scaleBits
                                                               then
                                                               putFB mATRIX_scaleBits mATRIX_scaleX
                                                               else
                                                               inconsistent
                                                                 "fst (fromJust (mATRIX_scale (x :: MATRIX)))"
                                                                 ("Bit count incorrect: required "
                                                                    ++
                                                                    show
                                                                      (requiredBitsFB mATRIX_scaleX)
                                                                      ++
                                                                      " bits to store the value " ++
                                                                        show mATRIX_scaleX ++
                                                                          ", but only have available "
                                                                            ++
                                                                            show mATRIX_scaleBits)
                                                             if
                                                               requiredBitsFB mATRIX_scaleY <=
                                                                 mATRIX_scaleBits
                                                               then
                                                               putFB mATRIX_scaleBits mATRIX_scaleY
                                                               else
                                                               inconsistent
                                                                 "snd (fromJust (mATRIX_scale (x :: MATRIX)))"
                                                                 ("Bit count incorrect: required "
                                                                    ++
                                                                    show
                                                                      (requiredBitsFB mATRIX_scaleY)
                                                                      ++
                                                                      " bits to store the value " ++
                                                                        show mATRIX_scaleY ++
                                                                          ", but only have available "
                                                                            ++
                                                                            show mATRIX_scaleBits)
                                                             return ()
                  | otherwise ->
                    inconsistent "mATRIX_scale (x :: MATRIX)"
                      "Should have a Just iff mATRIX_hasScale is True"
           Nothing | mATRIX_hasScale ->
                     inconsistent "mATRIX_scale (x :: MATRIX)"
                       "Should have a Nothing iff mATRIX_hasScale is False"
                   | otherwise -> return ()
       let mATRIX_hasRotate = isJust mATRIX_rotate
       putFlag mATRIX_hasRotate
       case mATRIX_rotate of
           Just x | mATRIX_hasRotate ->
                    case x of
                        (mATRIX_rotateSkew0,
                         mATRIX_rotateSkew1) -> do let mATRIX_rotateBits
                                                         = requiredBitsFB mATRIX_rotateSkew0 `max`
                                                             requiredBitsFB mATRIX_rotateSkew1
                                                   if requiredBitsUB mATRIX_rotateBits <= 5 then
                                                     putUB 5 mATRIX_rotateBits else
                                                     inconsistent
                                                       "fromJust (mATRIX_rotate (x :: MATRIX))"
                                                       ("Bit count incorrect: required " ++
                                                          show (requiredBitsUB mATRIX_rotateBits) ++
                                                            " bits to store the value " ++
                                                              show mATRIX_rotateBits ++
                                                                ", but only have available " ++
                                                                  show 5)
                                                   if
                                                     requiredBitsFB mATRIX_rotateSkew0 <=
                                                       mATRIX_rotateBits
                                                     then putFB mATRIX_rotateBits mATRIX_rotateSkew0
                                                     else
                                                     inconsistent
                                                       "fst (fromJust (mATRIX_rotate (x :: MATRIX)))"
                                                       ("Bit count incorrect: required " ++
                                                          show (requiredBitsFB mATRIX_rotateSkew0)
                                                            ++
                                                            " bits to store the value " ++
                                                              show mATRIX_rotateSkew0 ++
                                                                ", but only have available " ++
                                                                  show mATRIX_rotateBits)
                                                   if
                                                     requiredBitsFB mATRIX_rotateSkew1 <=
                                                       mATRIX_rotateBits
                                                     then putFB mATRIX_rotateBits mATRIX_rotateSkew1
                                                     else
                                                     inconsistent
                                                       "snd (fromJust (mATRIX_rotate (x :: MATRIX)))"
                                                       ("Bit count incorrect: required " ++
                                                          show (requiredBitsFB mATRIX_rotateSkew1)
                                                            ++
                                                            " bits to store the value " ++
                                                              show mATRIX_rotateSkew1 ++
                                                                ", but only have available " ++
                                                                  show mATRIX_rotateBits)
                                                   return ()
                  | otherwise ->
                    inconsistent "mATRIX_rotate (x :: MATRIX)"
                      "Should have a Just iff mATRIX_hasRotate is True"
           Nothing | mATRIX_hasRotate ->
                     inconsistent "mATRIX_rotate (x :: MATRIX)"
                       "Should have a Nothing iff mATRIX_hasRotate is False"
                   | otherwise -> return ()
       let mATRIX_translateBits
             = requiredBitsSB mATRIX_translateX `max`
                 requiredBitsSB mATRIX_translateY
       if requiredBitsUB mATRIX_translateBits <= 5 then
         putUB 5 mATRIX_translateBits else
         inconsistent "x :: MATRIX"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB mATRIX_translateBits) ++
                " bits to store the value " ++
                  show mATRIX_translateBits ++
                    ", but only have available " ++ show 5)
       if requiredBitsSB mATRIX_translateX <= mATRIX_translateBits then
         putSB mATRIX_translateBits mATRIX_translateX else
         inconsistent "mATRIX_translateX (x :: MATRIX)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB mATRIX_translateX) ++
                " bits to store the value " ++
                  show mATRIX_translateX ++
                    ", but only have available " ++ show mATRIX_translateBits)
       if requiredBitsSB mATRIX_translateY <= mATRIX_translateBits then
         putSB mATRIX_translateBits mATRIX_translateY else
         inconsistent "mATRIX_translateY (x :: MATRIX)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB mATRIX_translateY) ++
                " bits to store the value " ++
                  show mATRIX_translateY ++
                    ", but only have available " ++ show mATRIX_translateBits)
       let mATRIX_padding = reservedDefault
       const flushBits (mATRIX_padding :: ())
       return ()

\end{code}

\begin{code}

requiredBitsSB3 :: Integral a => (SB, SB, SB) -> a
requiredBitsSB3 (a, b, c) = requiredBitsSB a `max` requiredBitsSB b `max` requiredBitsSB c

\end{code}

p22: Color transform record
\begin{code}
 
data CXFORM = CXFORM{cXFORM_multTerm :: Maybe (SB, SB, SB),
                     cXFORM_addTerm :: Maybe (SB, SB, SB)}
            deriving (Eq, Show, Typeable, Data)
getCXFORM
  = do cXFORM_hasAddTerms <- getFlag
       cXFORM_hasMultTerms <- getFlag
       cXFORM_nbits <- getUB 4
       cXFORM_multTerm <- maybeHas cXFORM_hasMultTerms
                            (do cXFORM_redMultTerm <- getSB cXFORM_nbits
                                cXFORM_greenMultTerm <- getSB cXFORM_nbits
                                cXFORM_blueMultTerm <- getSB cXFORM_nbits
                                return
                                  (cXFORM_redMultTerm, cXFORM_greenMultTerm, cXFORM_blueMultTerm))
       cXFORM_addTerm <- maybeHas cXFORM_hasAddTerms
                           (do cXFORM_redAddTerm <- getSB cXFORM_nbits
                               cXFORM_greenAddTerm <- getSB cXFORM_nbits
                               cXFORM_blueAddTerm <- getSB cXFORM_nbits
                               return
                                 (cXFORM_redAddTerm, cXFORM_greenAddTerm, cXFORM_blueAddTerm))
       discardReserved "_reserved (x :: ?)" byteAlign
       return (CXFORM{..})
putCXFORM CXFORM{..}
  = do let cXFORM_hasAddTerms = isJust cXFORM_addTerm
       putFlag cXFORM_hasAddTerms
       let cXFORM_hasMultTerms = isJust cXFORM_multTerm
       putFlag cXFORM_hasMultTerms
       let cXFORM_nbits
             = maybe 0 requiredBitsSB3 cXFORM_multTerm `max`
                 maybe 0 requiredBitsSB3 cXFORM_addTerm
       if requiredBitsUB cXFORM_nbits <= 4 then putUB 4 cXFORM_nbits else
         inconsistent "x :: CXFORM"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB cXFORM_nbits) ++
                " bits to store the value " ++
                  show cXFORM_nbits ++ ", but only have available " ++ show 4)
       case cXFORM_multTerm of
           Just x | cXFORM_hasMultTerms ->
                    case x of
                        (cXFORM_redMultTerm, cXFORM_greenMultTerm,
                         cXFORM_blueMultTerm) -> do if
                                                      requiredBitsSB cXFORM_redMultTerm <=
                                                        cXFORM_nbits
                                                      then putSB cXFORM_nbits cXFORM_redMultTerm
                                                      else
                                                      inconsistent
                                                        "fst (fromJust (cXFORM_multTerm (x :: CXFORM)))"
                                                        ("Bit count incorrect: required " ++
                                                           show (requiredBitsSB cXFORM_redMultTerm)
                                                             ++
                                                             " bits to store the value " ++
                                                               show cXFORM_redMultTerm ++
                                                                 ", but only have available " ++
                                                                   show cXFORM_nbits)
                                                    if
                                                      requiredBitsSB cXFORM_greenMultTerm <=
                                                        cXFORM_nbits
                                                      then putSB cXFORM_nbits cXFORM_greenMultTerm
                                                      else
                                                      inconsistent
                                                        "snd (fromJust (cXFORM_multTerm (x :: CXFORM)))"
                                                        ("Bit count incorrect: required " ++
                                                           show
                                                             (requiredBitsSB cXFORM_greenMultTerm)
                                                             ++
                                                             " bits to store the value " ++
                                                               show cXFORM_greenMultTerm ++
                                                                 ", but only have available " ++
                                                                   show cXFORM_nbits)
                                                    if
                                                      requiredBitsSB cXFORM_blueMultTerm <=
                                                        cXFORM_nbits
                                                      then putSB cXFORM_nbits cXFORM_blueMultTerm
                                                      else
                                                      inconsistent
                                                        "thd (fromJust (cXFORM_multTerm (x :: CXFORM)))"
                                                        ("Bit count incorrect: required " ++
                                                           show (requiredBitsSB cXFORM_blueMultTerm)
                                                             ++
                                                             " bits to store the value " ++
                                                               show cXFORM_blueMultTerm ++
                                                                 ", but only have available " ++
                                                                   show cXFORM_nbits)
                                                    return ()
                  | otherwise ->
                    inconsistent "cXFORM_multTerm (x :: CXFORM)"
                      "Should have a Just iff cXFORM_hasMultTerms is True"
           Nothing | cXFORM_hasMultTerms ->
                     inconsistent "cXFORM_multTerm (x :: CXFORM)"
                       "Should have a Nothing iff cXFORM_hasMultTerms is False"
                   | otherwise -> return ()
       case cXFORM_addTerm of
           Just x | cXFORM_hasAddTerms ->
                    case x of
                        (cXFORM_redAddTerm, cXFORM_greenAddTerm,
                         cXFORM_blueAddTerm) -> do if
                                                     requiredBitsSB cXFORM_redAddTerm <=
                                                       cXFORM_nbits
                                                     then putSB cXFORM_nbits cXFORM_redAddTerm else
                                                     inconsistent
                                                       "fst (fromJust (cXFORM_addTerm (x :: CXFORM)))"
                                                       ("Bit count incorrect: required " ++
                                                          show (requiredBitsSB cXFORM_redAddTerm) ++
                                                            " bits to store the value " ++
                                                              show cXFORM_redAddTerm ++
                                                                ", but only have available " ++
                                                                  show cXFORM_nbits)
                                                   if
                                                     requiredBitsSB cXFORM_greenAddTerm <=
                                                       cXFORM_nbits
                                                     then putSB cXFORM_nbits cXFORM_greenAddTerm
                                                     else
                                                     inconsistent
                                                       "snd (fromJust (cXFORM_addTerm (x :: CXFORM)))"
                                                       ("Bit count incorrect: required " ++
                                                          show (requiredBitsSB cXFORM_greenAddTerm)
                                                            ++
                                                            " bits to store the value " ++
                                                              show cXFORM_greenAddTerm ++
                                                                ", but only have available " ++
                                                                  show cXFORM_nbits)
                                                   if
                                                     requiredBitsSB cXFORM_blueAddTerm <=
                                                       cXFORM_nbits
                                                     then putSB cXFORM_nbits cXFORM_blueAddTerm else
                                                     inconsistent
                                                       "thd (fromJust (cXFORM_addTerm (x :: CXFORM)))"
                                                       ("Bit count incorrect: required " ++
                                                          show (requiredBitsSB cXFORM_blueAddTerm)
                                                            ++
                                                            " bits to store the value " ++
                                                              show cXFORM_blueAddTerm ++
                                                                ", but only have available " ++
                                                                  show cXFORM_nbits)
                                                   return ()
                  | otherwise ->
                    inconsistent "cXFORM_addTerm (x :: CXFORM)"
                      "Should have a Just iff cXFORM_hasAddTerms is True"
           Nothing | cXFORM_hasAddTerms ->
                     inconsistent "cXFORM_addTerm (x :: CXFORM)"
                       "Should have a Nothing iff cXFORM_hasAddTerms is False"
                   | otherwise -> return ()
       let cXFORM_padding = reservedDefault
       const flushBits (cXFORM_padding :: ())
       return ()

\end{code}

\begin{code}

requiredBitsSB4 :: Integral a => (SB, SB, SB, SB) -> a
requiredBitsSB4 (a, b, c, d) = requiredBitsSB a `max` requiredBitsSB b `max` requiredBitsSB c `max` requiredBitsSB d

\end{code}

p23: Color transform with alpha record

\begin{code}
 
data CXFORMWITHALPHA = CXFORMWITHALPHA{cXFORMWITHALPHA_multTerm ::
                                       Maybe (SB, SB, SB, SB),
                                       cXFORMWITHALPHA_addTerm :: Maybe (SB, SB, SB, SB)}
                     deriving (Eq, Show, Typeable, Data)
getCXFORMWITHALPHA
  = do cXFORMWITHALPHA_hasAddTerms <- getFlag
       cXFORMWITHALPHA_hasMultTerms <- getFlag
       cXFORMWITHALPHA_nbits <- getUB 4
       cXFORMWITHALPHA_multTerm <- maybeHas cXFORMWITHALPHA_hasMultTerms
                                     (do cXFORMWITHALPHA_redMultTerm <- getSB cXFORMWITHALPHA_nbits
                                         cXFORMWITHALPHA_greenMultTerm <- getSB
                                                                            cXFORMWITHALPHA_nbits
                                         cXFORMWITHALPHA_blueMultTerm <- getSB cXFORMWITHALPHA_nbits
                                         cXFORMWITHALPHA_alphaMultTerm <- getSB
                                                                            cXFORMWITHALPHA_nbits
                                         return
                                           (cXFORMWITHALPHA_redMultTerm,
                                            cXFORMWITHALPHA_greenMultTerm,
                                            cXFORMWITHALPHA_blueMultTerm,
                                            cXFORMWITHALPHA_alphaMultTerm))
       cXFORMWITHALPHA_addTerm <- maybeHas cXFORMWITHALPHA_hasAddTerms
                                    (do cXFORMWITHALPHA_redAddTerm <- getSB cXFORMWITHALPHA_nbits
                                        cXFORMWITHALPHA_greenAddTerm <- getSB cXFORMWITHALPHA_nbits
                                        cXFORMWITHALPHA_blueAddTerm <- getSB cXFORMWITHALPHA_nbits
                                        cXFORMWITHALPHA_alphaAddTerm <- getSB cXFORMWITHALPHA_nbits
                                        return
                                          (cXFORMWITHALPHA_redAddTerm, cXFORMWITHALPHA_greenAddTerm,
                                           cXFORMWITHALPHA_blueAddTerm,
                                           cXFORMWITHALPHA_alphaAddTerm))
       discardReserved "_reserved (x :: ?)" byteAlign
       return (CXFORMWITHALPHA{..})
putCXFORMWITHALPHA CXFORMWITHALPHA{..}
  = do let cXFORMWITHALPHA_hasAddTerms
             = isJust cXFORMWITHALPHA_addTerm
       putFlag cXFORMWITHALPHA_hasAddTerms
       let cXFORMWITHALPHA_hasMultTerms = isJust cXFORMWITHALPHA_multTerm
       putFlag cXFORMWITHALPHA_hasMultTerms
       let cXFORMWITHALPHA_nbits
             = maybe 0 requiredBitsSB4 cXFORMWITHALPHA_multTerm `max`
                 maybe 0 requiredBitsSB4 cXFORMWITHALPHA_addTerm
       if requiredBitsUB cXFORMWITHALPHA_nbits <= 4 then
         putUB 4 cXFORMWITHALPHA_nbits else
         inconsistent "x :: CXFORMWITHALPHA"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB cXFORMWITHALPHA_nbits) ++
                " bits to store the value " ++
                  show cXFORMWITHALPHA_nbits ++
                    ", but only have available " ++ show 4)
       case cXFORMWITHALPHA_multTerm of
           Just x | cXFORMWITHALPHA_hasMultTerms ->
                    case x of
                        (cXFORMWITHALPHA_redMultTerm, cXFORMWITHALPHA_greenMultTerm,
                         cXFORMWITHALPHA_blueMultTerm,
                         cXFORMWITHALPHA_alphaMultTerm) -> do if
                                                                requiredBitsSB
                                                                  cXFORMWITHALPHA_redMultTerm
                                                                  <= cXFORMWITHALPHA_nbits
                                                                then
                                                                putSB cXFORMWITHALPHA_nbits
                                                                  cXFORMWITHALPHA_redMultTerm
                                                                else
                                                                inconsistent
                                                                  "fst (fromJust (cXFORMWITHALPHA_multTerm (x :: CXFORMWITHALPHA)))"
                                                                  ("Bit count incorrect: required "
                                                                     ++
                                                                     show
                                                                       (requiredBitsSB
                                                                          cXFORMWITHALPHA_redMultTerm)
                                                                       ++
                                                                       " bits to store the value "
                                                                         ++
                                                                         show
                                                                           cXFORMWITHALPHA_redMultTerm
                                                                           ++
                                                                           ", but only have available "
                                                                             ++
                                                                             show
                                                                               cXFORMWITHALPHA_nbits)
                                                              if
                                                                requiredBitsSB
                                                                  cXFORMWITHALPHA_greenMultTerm
                                                                  <= cXFORMWITHALPHA_nbits
                                                                then
                                                                putSB cXFORMWITHALPHA_nbits
                                                                  cXFORMWITHALPHA_greenMultTerm
                                                                else
                                                                inconsistent
                                                                  "snd (fromJust (cXFORMWITHALPHA_multTerm (x :: CXFORMWITHALPHA)))"
                                                                  ("Bit count incorrect: required "
                                                                     ++
                                                                     show
                                                                       (requiredBitsSB
                                                                          cXFORMWITHALPHA_greenMultTerm)
                                                                       ++
                                                                       " bits to store the value "
                                                                         ++
                                                                         show
                                                                           cXFORMWITHALPHA_greenMultTerm
                                                                           ++
                                                                           ", but only have available "
                                                                             ++
                                                                             show
                                                                               cXFORMWITHALPHA_nbits)
                                                              if
                                                                requiredBitsSB
                                                                  cXFORMWITHALPHA_blueMultTerm
                                                                  <= cXFORMWITHALPHA_nbits
                                                                then
                                                                putSB cXFORMWITHALPHA_nbits
                                                                  cXFORMWITHALPHA_blueMultTerm
                                                                else
                                                                inconsistent
                                                                  "thd (fromJust (cXFORMWITHALPHA_multTerm (x :: CXFORMWITHALPHA)))"
                                                                  ("Bit count incorrect: required "
                                                                     ++
                                                                     show
                                                                       (requiredBitsSB
                                                                          cXFORMWITHALPHA_blueMultTerm)
                                                                       ++
                                                                       " bits to store the value "
                                                                         ++
                                                                         show
                                                                           cXFORMWITHALPHA_blueMultTerm
                                                                           ++
                                                                           ", but only have available "
                                                                             ++
                                                                             show
                                                                               cXFORMWITHALPHA_nbits)
                                                              if
                                                                requiredBitsSB
                                                                  cXFORMWITHALPHA_alphaMultTerm
                                                                  <= cXFORMWITHALPHA_nbits
                                                                then
                                                                putSB cXFORMWITHALPHA_nbits
                                                                  cXFORMWITHALPHA_alphaMultTerm
                                                                else
                                                                inconsistent
                                                                  "frth (fromJust (cXFORMWITHALPHA_multTerm (x :: CXFORMWITHALPHA)))"
                                                                  ("Bit count incorrect: required "
                                                                     ++
                                                                     show
                                                                       (requiredBitsSB
                                                                          cXFORMWITHALPHA_alphaMultTerm)
                                                                       ++
                                                                       " bits to store the value "
                                                                         ++
                                                                         show
                                                                           cXFORMWITHALPHA_alphaMultTerm
                                                                           ++
                                                                           ", but only have available "
                                                                             ++
                                                                             show
                                                                               cXFORMWITHALPHA_nbits)
                                                              return ()
                  | otherwise ->
                    inconsistent "cXFORMWITHALPHA_multTerm (x :: CXFORMWITHALPHA)"
                      "Should have a Just iff cXFORMWITHALPHA_hasMultTerms is True"
           Nothing | cXFORMWITHALPHA_hasMultTerms ->
                     inconsistent "cXFORMWITHALPHA_multTerm (x :: CXFORMWITHALPHA)"
                       "Should have a Nothing iff cXFORMWITHALPHA_hasMultTerms is False"
                   | otherwise -> return ()
       case cXFORMWITHALPHA_addTerm of
           Just x | cXFORMWITHALPHA_hasAddTerms ->
                    case x of
                        (cXFORMWITHALPHA_redAddTerm, cXFORMWITHALPHA_greenAddTerm,
                         cXFORMWITHALPHA_blueAddTerm, cXFORMWITHALPHA_alphaAddTerm) -> do if
                                                                                            requiredBitsSB
                                                                                              cXFORMWITHALPHA_redAddTerm
                                                                                              <=
                                                                                              cXFORMWITHALPHA_nbits
                                                                                            then
                                                                                            putSB
                                                                                              cXFORMWITHALPHA_nbits
                                                                                              cXFORMWITHALPHA_redAddTerm
                                                                                            else
                                                                                            inconsistent
                                                                                              "fst (fromJust (cXFORMWITHALPHA_addTerm (x :: CXFORMWITHALPHA)))"
                                                                                              ("Bit count incorrect: required "
                                                                                                 ++
                                                                                                 show
                                                                                                   (requiredBitsSB
                                                                                                      cXFORMWITHALPHA_redAddTerm)
                                                                                                   ++
                                                                                                   " bits to store the value "
                                                                                                     ++
                                                                                                     show
                                                                                                       cXFORMWITHALPHA_redAddTerm
                                                                                                       ++
                                                                                                       ", but only have available "
                                                                                                         ++
                                                                                                         show
                                                                                                           cXFORMWITHALPHA_nbits)
                                                                                          if
                                                                                            requiredBitsSB
                                                                                              cXFORMWITHALPHA_greenAddTerm
                                                                                              <=
                                                                                              cXFORMWITHALPHA_nbits
                                                                                            then
                                                                                            putSB
                                                                                              cXFORMWITHALPHA_nbits
                                                                                              cXFORMWITHALPHA_greenAddTerm
                                                                                            else
                                                                                            inconsistent
                                                                                              "snd (fromJust (cXFORMWITHALPHA_addTerm (x :: CXFORMWITHALPHA)))"
                                                                                              ("Bit count incorrect: required "
                                                                                                 ++
                                                                                                 show
                                                                                                   (requiredBitsSB
                                                                                                      cXFORMWITHALPHA_greenAddTerm)
                                                                                                   ++
                                                                                                   " bits to store the value "
                                                                                                     ++
                                                                                                     show
                                                                                                       cXFORMWITHALPHA_greenAddTerm
                                                                                                       ++
                                                                                                       ", but only have available "
                                                                                                         ++
                                                                                                         show
                                                                                                           cXFORMWITHALPHA_nbits)
                                                                                          if
                                                                                            requiredBitsSB
                                                                                              cXFORMWITHALPHA_blueAddTerm
                                                                                              <=
                                                                                              cXFORMWITHALPHA_nbits
                                                                                            then
                                                                                            putSB
                                                                                              cXFORMWITHALPHA_nbits
                                                                                              cXFORMWITHALPHA_blueAddTerm
                                                                                            else
                                                                                            inconsistent
                                                                                              "thd (fromJust (cXFORMWITHALPHA_addTerm (x :: CXFORMWITHALPHA)))"
                                                                                              ("Bit count incorrect: required "
                                                                                                 ++
                                                                                                 show
                                                                                                   (requiredBitsSB
                                                                                                      cXFORMWITHALPHA_blueAddTerm)
                                                                                                   ++
                                                                                                   " bits to store the value "
                                                                                                     ++
                                                                                                     show
                                                                                                       cXFORMWITHALPHA_blueAddTerm
                                                                                                       ++
                                                                                                       ", but only have available "
                                                                                                         ++
                                                                                                         show
                                                                                                           cXFORMWITHALPHA_nbits)
                                                                                          if
                                                                                            requiredBitsSB
                                                                                              cXFORMWITHALPHA_alphaAddTerm
                                                                                              <=
                                                                                              cXFORMWITHALPHA_nbits
                                                                                            then
                                                                                            putSB
                                                                                              cXFORMWITHALPHA_nbits
                                                                                              cXFORMWITHALPHA_alphaAddTerm
                                                                                            else
                                                                                            inconsistent
                                                                                              "frth (fromJust (cXFORMWITHALPHA_addTerm (x :: CXFORMWITHALPHA)))"
                                                                                              ("Bit count incorrect: required "
                                                                                                 ++
                                                                                                 show
                                                                                                   (requiredBitsSB
                                                                                                      cXFORMWITHALPHA_alphaAddTerm)
                                                                                                   ++
                                                                                                   " bits to store the value "
                                                                                                     ++
                                                                                                     show
                                                                                                       cXFORMWITHALPHA_alphaAddTerm
                                                                                                       ++
                                                                                                       ", but only have available "
                                                                                                         ++
                                                                                                         show
                                                                                                           cXFORMWITHALPHA_nbits)
                                                                                          return ()
                  | otherwise ->
                    inconsistent "cXFORMWITHALPHA_addTerm (x :: CXFORMWITHALPHA)"
                      "Should have a Just iff cXFORMWITHALPHA_hasAddTerms is True"
           Nothing | cXFORMWITHALPHA_hasAddTerms ->
                     inconsistent "cXFORMWITHALPHA_addTerm (x :: CXFORMWITHALPHA)"
                       "Should have a Nothing iff cXFORMWITHALPHA_hasAddTerms is False"
                   | otherwise -> return ()
       let cXFORMWITHALPHA_padding = reservedDefault
       const flushBits (cXFORMWITHALPHA_padding :: ())
       return ()

\end{code}


Chapter 2: SWF Structure Summary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p25: The SWF header
\begin{code}

data Swf = Swf { compressed :: Bool, version :: UI8, fileLength :: UI32 {- after decompression -}, frameSize :: RECT {- Twips -}, frameRate :: FIXED8, frameCount :: UI16, tags :: [Tag] }
         deriving (Eq, Show, Typeable, Data)

getSwf :: ByteString -> Swf
getSwf bs = runSwfGet emptySwfEnv bs $ do
    signature_1 <- getUI8
    compressed <- case lookup (fromIntegral signature_1) [(ord 'F', False), (ord 'C', True)] of
        Just c  -> return c
        Nothing -> fail "SWF signature byte 1 unrecognised"
    discardKnown "_signature2 (x :: Swf)" "The 2nd SWF signature byte must be W" (fromIntegral $ ord 'W') getUI8
    discardKnown "_signature3 (x :: Swf)" "The 3rd SWF signature byte must be F" (fromIntegral $ ord 'S') getUI8
    version <- getUI8
    fileLength <- getUI32
    
    modifySwfGet (\e -> e { swfVersion = version }) $ (if compressed then decompressRemainder (fromIntegral fileLength) else id) $ do
        frameSize <- getRECT
        -- TODO: assert XMin/YMin are 0
        frameRate <- getFIXED8
        frameCount <- getUI16
        tags <- getToEnd getTag
        return Swf {..}

putSwf :: Swf -> ByteString
putSwf swf = runSwfPut emptySwfEnv $ do
    putUI8 $ fromIntegral (if compressed swf then ord 'C' else ord 'F')
    putUI8 $ fromIntegral (ord 'W')
    putUI8 $ fromIntegral (ord 'S')
    putUI8 $ version swf
    putUI32 $ fileLength swf
    
    modifySwfPutM (\e -> e { swfVersion = version swf }) $ (if compressed swf then compressRemainder (fromIntegral $ fileLength swf) else id) $ do
        putRECT $ frameSize swf
        putFIXED8 $ frameRate swf
        putUI16 $ frameCount swf
        mapM_ putTag $ tags swf

\end{code}

p27: Tag format
\begin{code}

data RECORDHEADER = RECORDHEADER { rECORDHEADER_tagType :: UI16, rECORDHEADER_tagLength :: UI32 } -- NB: spec claims tagLength is SI32, but that's too stupid to be true.
                  deriving (Eq, Show, Typeable, Data)

getRECORDHEADER :: SwfGet RECORDHEADER
getRECORDHEADER = do
    tagCodeAndLength <- getUI16
    let tagLength = tagCodeAndLength .&. 0x3F
        rECORDHEADER_tagType = (tagCodeAndLength `shiftR` 6) .&. 0x3FF
    rECORDHEADER_tagLength <- if tagLength == 0x3F then getUI32 else return (fromIntegral tagLength)
    return $ RECORDHEADER {..}

putRECORDHEADER :: RECORDHEADER -> SwfPut
putRECORDHEADER (RECORDHEADER{..}) = do
    putUI16 ((rECORDHEADER_tagType `shiftL` 6) .|. (fromIntegral $ rECORDHEADER_tagLength `min` 0x3F))
    when (rECORDHEADER_tagLength >= 0x3F) $ putUI32 rECORDHEADER_tagLength

\end{code}

Chapter 3: The Display List
~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}

data Tag = UnknownTag { unknownTag_tagType :: UI16, unknownTag_data :: ByteString }
         | DefineFont { defineFont_fontID :: UI16, defineFont_glyphShapeTable :: [SHAPE] }
         |  PlaceObject{placeObject_characterId :: UI16,
              placeObject_depth :: UI16, placeObject_matrix :: MATRIX,
              placeObject_colorTransform :: Maybe CXFORM}
         |  PlaceObject2{placeObject2_placeFlagMove :: Bool,
               placeObject2_depth :: UI16, placeObject2_characterId :: Maybe UI16,
               placeObject2_matrix :: Maybe MATRIX,
               placeObject2_colorTransform :: Maybe CXFORMWITHALPHA,
               placeObject2_ratio :: Maybe UI16,
               placeObject2_name :: Maybe STRING,
               placeObject2_clipDepth :: Maybe UI16,
               placeObject2_clipActions :: Maybe CLIPACTIONS}
         |  PlaceObject3{placeObject3_placeFlagMove :: Bool,
               placeObject3_placeFlagHasImage :: Bool,
               placeObject3_placeFlagHasClassName :: Bool,
               placeObject3_depth :: UI16, placeObject3_className :: Maybe STRING,
               placeObject3_characterId :: Maybe UI16,
               placeObject3_matrix :: Maybe MATRIX,
               placeObject3_colorTransform :: Maybe CXFORMWITHALPHA,
               placeObject3_ratio :: Maybe UI16,
               placeObject3_name :: Maybe STRING,
               placeObject3_clipDepth :: Maybe UI16,
               placeObject3_surfaceFilterList :: Maybe FILTERLIST,
               placeObject3_blendMode :: Maybe BlendMode,
               placeObject3_bitmapCache :: Maybe UI8,
               placeObject3_clipActions :: Maybe CLIPACTIONS}
         |  RemoveObject{removeObject_characterId :: UI16,
               removeObject_depth :: UI16}
         |  RemoveObject2{removeObject2_depth :: UI16}
         |  ShowFrame{}
         |  SetBackgroundColor{setBackgroundColor_backgroundColor :: RGB}
         |  FrameLabel{frameLabel_name :: STRING,
             frameLabel_namedAnchorFlag :: Maybe UI8}
         |  Protect{}
         |  End{}
         |  ExportAssets{exportAssets_count :: UI16, exportAssets_tag1 :: UI16,
               exportAssets_name1 :: STRING, exportAssets_tagN :: UI16,
               exportAssets_nameN :: STRING}
         |  ImportAssets{importAssets_uRL :: STRING,
               importAssets_count :: UI16, importAssets_tag1 :: UI16,
               importAssets_name1 :: STRING, importAssets_tagN :: UI16,
               importAssets_nameN :: STRING}
         |  EnableDebugger{enableDebugger_password :: STRING}
         |  EnableDebugger2{enableDebugger2_password :: STRING}
         |  ScriptLimits{scriptLimits_maxRecursionDepth :: UI16,
               scriptLimits_scriptTimeoutSeconds :: UI16}
         |  SetTabIndex{setTabIndex_depth :: UI16,
              setTabIndex_tabIndex :: UI16}
         |  FileAttributes{fileAttributes_useDirectBlit :: Bool,
                 fileAttributes_useGPU :: Bool, fileAttributes_hasMetadata :: Bool,
                 fileAttributes_actionScript3 :: Bool,
                 fileAttributes_useNetwork :: Bool}
         |  ImportAssets2{importAssets2_uRL :: STRING,
                importAssets2_count :: UI16, importAssets2_tag1 :: UI16,
                importAssets2_name1 :: STRING, importAssets2_tagN :: UI16,
                importAssets2_nameN :: STRING}
         |  SymbolClass{symbolClass_tagsNames :: [(UI16, STRING)]}
         |  Metadata{metadata_metadata :: STRING}
         |  DefineScalingGrid{defineScalingGrid_characterId :: UI16,
                    defineScalingGrid_splitter :: RECT}
         |  DefineSceneAndFrameLabelData{defineSceneAndFrameLabelData_offsetNames
                               :: [(EncodedU32, STRING)],
                               defineSceneAndFrameLabelData_frameNumLabels ::
                               [(EncodedU32, STRING)]}
         |  DoAction{doAction_actions :: ACTIONRECORDS}
         |  DoInitAction{doInitAction_spriteID :: UI16,
               doInitAction_actions :: ACTIONRECORDS}
         |  DoABC{doABC_flags :: UI32, doABC_name :: STRING,
        doABC_aBCData :: ByteString}
         |  DefineShape{defineShape_shapeId :: UI16,
              defineShape_shapeBounds :: RECT,
              defineShape_shapes :: SHAPEWITHSTYLE}
         |  DefineShape2{defineShape2_shapeId :: UI16,
               defineShape2_shapeBounds :: RECT,
               defineShape2_shapes :: SHAPEWITHSTYLE}
         |  DefineShape3{defineShape3_shapeId :: UI16,
               defineShape3_shapeBounds :: RECT,
               defineShape3_shapes :: SHAPEWITHSTYLE}
         |  DefineShape4{defineShape4_shapeId :: UI16,
               defineShape4_shapeBounds :: RECT, defineShape4_edgeBounds :: RECT,
               defineShape4_usesFillWindingRule :: Bool,
               defineShape4_usesNonScalingStrokes :: Bool,
               defineShape4_usesScalingStrokes :: Bool,
               defineShape4_shapes :: SHAPEWITHSTYLE}
         |  DefineBits{defineBits_characterID :: UI16,
             defineBits_jPEGData :: ByteString}
         |  JPEGTables{jPEGTables_jPEGData :: ByteString}
         |  DefineBitsJPEG2{defineBitsJPEG2_characterID :: UI16,
                  defineBitsJPEG2_imageData :: ByteString}
         |  DefineBitsJPEG3{defineBitsJPEG3_characterID :: UI16,
                  defineBitsJPEG3_imageData :: [UI8],
                  defineBitsJPEG3_bitmapAlphaData :: ByteString}
         |  DefineBitsLossless{defineBitsLossless_characterID :: UI16,
                     defineBitsLossless_bitmapFormat :: UI8,
                     defineBitsLossless_bitmapWidth :: UI16,
                     defineBitsLossless_bitmapHeight :: UI16,
                     defineBitsLossless_bitmapColorTableSize :: Maybe UI8,
                     defineBitsLossless_zlibBitmapData :: ByteString}
         |  DefineBitsLossless2{defineBitsLossless2_characterID :: UI16,
                      defineBitsLossless2_bitmapFormat :: UI8,
                      defineBitsLossless2_bitmapWidth :: UI16,
                      defineBitsLossless2_bitmapHeight :: UI16,
                      defineBitsLossless2_bitmapColorTableSize :: Maybe UI8,
                      defineBitsLossless2_zlibBitmapData :: ByteString}
         |  DefineBitsJPEG4{defineBitsJPEG4_characterID :: UI16,
                  defineBitsJPEG4_deblockParam :: UI16,
                  defineBitsJPEG4_imageData :: [UI8],
                  defineBitsJPEG4_bitmapAlphaData :: ByteString}
         |  DefineMorphShape{defineMorphShape_characterId :: UI16,
                   defineMorphShape_startBounds :: RECT,
                   defineMorphShape_endBounds :: RECT,
                   defineMorphShape_offset :: UI32,
                   defineMorphShape_morphFillStyles :: MORPHFILLSTYLEARRAY,
                   defineMorphShape_morphLineStyles :: MORPHLINESTYLEARRAY,
                   defineMorphShape_startEdges :: SHAPE,
                   defineMorphShape_endEdges :: SHAPE}
         |  DefineMorphShape2{defineMorphShape2_characterId :: UI16,
                    defineMorphShape2_startBounds :: RECT,
                    defineMorphShape2_endBounds :: RECT,
                    defineMorphShape2_startEdgeBounds :: RECT,
                    defineMorphShape2_endEdgeBounds :: RECT,
                    defineMorphShape2_usesNonScalingStrokes :: Bool,
                    defineMorphShape2_usesScalingStrokes :: Bool,
                    defineMorphShape2_offset :: UI32,
                    defineMorphShape2_morphFillStyles :: MORPHFILLSTYLEARRAY,
                    defineMorphShape2_morphLineStyles :: MORPHLINESTYLEARRAY,
                    defineMorphShape2_startEdges :: SHAPE,
                    defineMorphShape2_endEdges :: SHAPE}
         |  DefineFontInfo{defineFontInfo_fontID :: UI16,
                 defineFontInfo_fontName :: [UI8],
                 defineFontInfo_fontFlagsSmallText :: Bool,
                 defineFontInfo_fontFlagsShiftJIS :: Bool,
                 defineFontInfo_fontFlagsANSI :: Bool,
                 defineFontInfo_fontFlagsItalic :: Bool,
                 defineFontInfo_fontFlagsBold :: Bool,
                 defineFontInfo_codeTable :: Either [UI16] [UI8]}
         |  DefineFontInfo2{defineFontInfo2_fontID :: UI16,
                  defineFontInfo2_fontName :: [UI8],
                  defineFontInfo2_fontFlagsSmallText :: Bool,
                  defineFontInfo2_fontFlagsShiftJIS :: Bool,
                  defineFontInfo2_fontFlagsANSI :: Bool,
                  defineFontInfo2_fontFlagsItalic :: Bool,
                  defineFontInfo2_fontFlagsBold :: Bool,
                  defineFontInfo2_fontFlagsWideCodes :: Bool,
                  defineFontInfo2_languageCode :: LANGCODE,
                  defineFontInfo2_codeTable :: [UI16]}
         |  DefineFont2{defineFont2_fontID :: UI16,
              defineFont2_fontFlagsShiftJIS :: Bool,
              defineFont2_fontFlagsSmallText :: Bool,
              defineFont2_fontFlagsANSI :: Bool,
              defineFont2_fontFlagsItalic :: Bool,
              defineFont2_fontFlagsBold :: Bool,
              defineFont2_languageCode :: LANGCODE,
              defineFont2_fontName :: [UI8],
              defineFont2_offsetTable :: Either [UI32] [UI16],
              defineFont2_codeTableOffset :: Either UI32 UI16,
              defineFont2_glyphShapeTable :: [SHAPE],
              defineFont2_codeTable :: Either [UI16] [UI8],
              defineFont2_fontLayout ::
              Maybe (SI16, SI16, SI16, [SI16], [RECT], UI16, [KERNINGRECORD])}
         |  DefineFont3{defineFont3_fontID :: UI16,
              defineFont3_fontFlagsShiftJIS :: Bool,
              defineFont3_fontFlagsSmallText :: Bool,
              defineFont3_fontFlagsANSI :: Bool,
              defineFont3_fontFlagsWideCodes :: Bool,
              defineFont3_fontFlagsItalic :: Bool,
              defineFont3_fontFlagsBold :: Bool,
              defineFont3_languageCode :: LANGCODE,
              defineFont3_fontName :: [UI8],
              defineFont3_offsetTable :: Either [UI32] [UI16],
              defineFont3_codeTableOffset :: Either UI32 UI16,
              defineFont3_glyphShapeTable :: [SHAPE],
              defineFont3_codeTable :: [UI16],
              defineFont3_fontLayout ::
              Maybe (SI16, SI16, SI16, [SI16], [RECT], UI16, [KERNINGRECORD])}
         |  DefineFontAlignZones{defineFontAlignZones_fontID :: UI16,
                       defineFontAlignZones_cSMTableHint :: UB,
                       defineFontAlignZones_zoneTable :: [ZONERECORD]}
         |  DefineFontName{defineFontName_fontID :: UI16,
                 defineFontName_fontName :: STRING,
                 defineFontName_fontCopyright :: STRING}
         |  DefineText{defineText_characterID :: UI16,
             defineText_textBounds :: RECT, defineText_textMatrix :: MATRIX,
             defineText_glyphBits :: UI8, defineText_advanceBits :: UI8,
             defineText_textRecords :: TEXTRECORDS}
         |  DefineText2{defineText2_characterID :: UI16,
              defineText2_textBounds :: RECT, defineText2_textMatrix :: MATRIX,
              defineText2_glyphBits :: UI8, defineText2_advanceBits :: UI8,
              defineText2_textRecords :: TEXTRECORDS}
         |  DefineEditText{defineEditText_characterID :: UI16,
                 defineEditText_bounds :: RECT, defineEditText_wordWrap :: Bool,
                 defineEditText_multiline :: Bool, defineEditText_password :: Bool,
                 defineEditText_readOnly :: Bool, defineEditText_autoSize :: Bool,
                 defineEditText_noSelect :: Bool, defineEditText_border :: Bool,
                 defineEditText_wasStatic :: Bool, defineEditText_hTML :: Bool,
                 defineEditText_useOutlines :: Bool,
                 defineEditText_fontID :: Maybe UI16,
                 defineEditText_fontClass :: Maybe STRING,
                 defineEditText_fontHeight :: Maybe UI16,
                 defineEditText_textColor :: Maybe RGBA,
                 defineEditText_maxLength :: Maybe UI16,
                 defineEditText_layout :: Maybe (UI8, UI16, UI16, UI16, SI16),
                 defineEditText_variableName :: STRING,
                 defineEditText_initialText :: Maybe STRING}
         |  CSMTextSettings{cSMTextSettings_textID :: UI16,
                  cSMTextSettings_useFlashType :: UB, cSMTextSettings_gridFit :: UB,
                  cSMTextSettings_thickness :: FLOAT,
                  cSMTextSettings_sharpness :: FLOAT}
         |  DefineFont4{defineFont4_fontID :: UI16,
              defineFont4_fontFlagsHasFontData :: Bool,
              defineFont4_fontFlagsItalic :: Bool,
              defineFont4_fontFlagsBold :: Bool, defineFont4_fontName :: STRING,
              defineFont4_fontData :: ByteString}
         |  DefineSound{defineSound_soundId :: UI16,
              defineSound_soundFormat :: UB, defineSound_soundRate :: UB,
              defineSound_soundSize :: Bool, defineSound_soundType :: Bool,
              defineSound_soundSampleCount :: UI32,
              defineSound_soundData :: ByteString}
         |  StartSound{startSound_soundId :: UI16,
             startSound_soundInfo :: SOUNDINFO}
         |  StartSound2{startSound2_soundClassName :: STRING,
              startSound2_soundInfo :: SOUNDINFO}
         |  SoundStreamHead{soundStreamHead_playbackSoundRate :: UB,
                  soundStreamHead_playbackSoundSize :: Bool,
                  soundStreamHead_playbackSoundType :: Bool,
                  soundStreamHead_streamSoundCompression :: UB,
                  soundStreamHead_streamSoundRate :: UB,
                  soundStreamHead_streamSoundSize :: Bool,
                  soundStreamHead_streamSoundType :: Bool,
                  soundStreamHead_streamSoundSampleCount :: UI16,
                  soundStreamHead_latencySeek :: Maybe SI16}
         |  SoundStreamHead2{soundStreamHead2_playbackSoundRate :: UB,
                   soundStreamHead2_playbackSoundSize :: Bool,
                   soundStreamHead2_playbackSoundType :: Bool,
                   soundStreamHead2_streamSoundCompression :: UB,
                   soundStreamHead2_streamSoundRate :: UB,
                   soundStreamHead2_streamSoundSize :: Bool,
                   soundStreamHead2_streamSoundType :: Bool,
                   soundStreamHead2_streamSoundSampleCount :: UI16,
                   soundStreamHead2_latencySeek :: Maybe SI16}
         |  SoundStreamBlock{soundStreamBlock_streamSoundData :: ByteString}
         |  DefineButton{defineButton_buttonId :: UI16,
               defineButton_characters :: BUTTONRECORDS,
               defineButton_actions :: ACTIONRECORDS}
         |  DefineButton2{defineButton2_buttonId :: UI16,
                defineButton2_trackAsMenu :: Bool,
                defineButton2_actionOffset :: UI16,
                defineButton2_characters :: BUTTONRECORDS,
                defineButton2_characterEndFlag :: UI8,
                defineButton2_actions :: BUTTONCONDACTIONS}
         |  DefineButtonCxform{defineButtonCxform_buttonId :: UI16,
                     defineButtonCxform_buttonColorTransform :: CXFORM}
         |  DefineButtonSound{defineButtonSound_buttonId :: UI16,
                    defineButtonSound_buttonSoundChar0 :: UI16,
                    defineButtonSound_buttonSoundInfo0 :: Maybe SOUNDINFO,
                    defineButtonSound_buttonSoundChar1 :: UI16,
                    defineButtonSound_buttonSoundInfo1 :: Maybe SOUNDINFO,
                    defineButtonSound_buttonSoundChar2 :: UI16,
                    defineButtonSound_buttonSoundInfo2 :: Maybe SOUNDINFO,
                    defineButtonSound_buttonSoundChar3 :: UI16,
                    defineButtonSound_buttonSoundInfo3 :: Maybe SOUNDINFO}
         |  DefineSprite{defineSprite_spriteID :: UI16,
               defineSprite_frameCount :: UI16, defineSprite_controlTags :: [Tag]}
         |  DefineVideoStream{defineVideoStream_characterID :: UI16,
                    defineVideoStream_numFrames :: UI16,
                    defineVideoStream_width :: UI16, defineVideoStream_height :: UI16,
                    defineVideoStream_videoFlagsDeblocking :: UB,
                    defineVideoStream_videoFlagsSmoothing :: Bool,
                    defineVideoStream_codecID :: UI8}
         |  StreamID{streamID_streamID :: UI16, streamID_frameNum :: UI16,
           streamID_videoData :: ByteString}
         |  DefineBinaryData{defineBinaryData_tag :: UI16,
                   defineBinaryData_data :: ByteString}
         deriving (Eq, Show, Typeable, Data)

getTag = do
    RECORDHEADER {..} <- getRECORDHEADER

    let mb_getter = case rECORDHEADER_tagType of
          10 -> Just getDefineFont
          _  -> generatedTagGetters rECORDHEADER_tagType

    nestSwfGet (fromIntegral rECORDHEADER_tagLength) $ case mb_getter of
        Just getter -> getter
        Nothing -> do
            let unknownTag_tagType = rECORDHEADER_tagType
            unknownTag_data <- getRemainingLazyByteString
            return $ UnknownTag {..}

putTag tag = do
    (rECORDHEADER_tagLength, put) <- nestSwfPut $ case tag of
        UnknownTag {..} -> putLazyByteString unknownTag_data
        DefineFont {}   -> putDefineFont tag
        _               -> generatedTagPutters tag
  
    let rECORDHEADER_tagType = tagType tag
    putRECORDHEADER $ RECORDHEADER {..}
    put

tagType (UnknownTag {..}) = unknownTag_tagType
tagType (DefineFont {})   = 10
tagType tag               = generatedTagTypes tag

\end{code}

\begin{code}
generatedTagGetters tag
  = case tag of
        4 -> Just getPlaceObject
        26 -> Just getPlaceObject2
        70 -> Just getPlaceObject3
        5 -> Just getRemoveObject
        28 -> Just getRemoveObject2
        1 -> Just getShowFrame
        9 -> Just getSetBackgroundColor
        43 -> Just getFrameLabel
        24 -> Just getProtect
        0 -> Just getEnd
        56 -> Just getExportAssets
        57 -> Just getImportAssets
        58 -> Just getEnableDebugger
        64 -> Just getEnableDebugger2
        65 -> Just getScriptLimits
        66 -> Just getSetTabIndex
        69 -> Just getFileAttributes
        71 -> Just getImportAssets2
        76 -> Just getSymbolClass
        77 -> Just getMetadata
        78 -> Just getDefineScalingGrid
        86 -> Just getDefineSceneAndFrameLabelData
        12 -> Just getDoAction
        59 -> Just getDoInitAction
        82 -> Just getDoABC
        2 -> Just getDefineShape
        22 -> Just getDefineShape2
        32 -> Just getDefineShape3
        83 -> Just getDefineShape4
        6 -> Just getDefineBits
        8 -> Just getJPEGTables
        21 -> Just getDefineBitsJPEG2
        35 -> Just getDefineBitsJPEG3
        20 -> Just getDefineBitsLossless
        36 -> Just getDefineBitsLossless2
        90 -> Just getDefineBitsJPEG4
        46 -> Just getDefineMorphShape
        84 -> Just getDefineMorphShape2
        13 -> Just getDefineFontInfo
        62 -> Just getDefineFontInfo2
        48 -> Just getDefineFont2
        75 -> Just getDefineFont3
        73 -> Just getDefineFontAlignZones
        88 -> Just getDefineFontName
        11 -> Just getDefineText
        33 -> Just getDefineText2
        37 -> Just getDefineEditText
        74 -> Just getCSMTextSettings
        91 -> Just getDefineFont4
        14 -> Just getDefineSound
        15 -> Just getStartSound
        89 -> Just getStartSound2
        18 -> Just getSoundStreamHead
        45 -> Just getSoundStreamHead2
        19 -> Just getSoundStreamBlock
        7 -> Just getDefineButton
        34 -> Just getDefineButton2
        23 -> Just getDefineButtonCxform
        17 -> Just getDefineButtonSound
        39 -> Just getDefineSprite
        60 -> Just getDefineVideoStream
        61 -> Just getStreamID
        87 -> Just getDefineBinaryData
        _ -> Nothing
generatedTagPutters tag
  = case tag of
        PlaceObject{..} -> putPlaceObject tag
        PlaceObject2{..} -> putPlaceObject2 tag
        PlaceObject3{..} -> putPlaceObject3 tag
        RemoveObject{..} -> putRemoveObject tag
        RemoveObject2{..} -> putRemoveObject2 tag
        ShowFrame{..} -> putShowFrame tag
        SetBackgroundColor{..} -> putSetBackgroundColor tag
        FrameLabel{..} -> putFrameLabel tag
        Protect{..} -> putProtect tag
        End{..} -> putEnd tag
        ExportAssets{..} -> putExportAssets tag
        ImportAssets{..} -> putImportAssets tag
        EnableDebugger{..} -> putEnableDebugger tag
        EnableDebugger2{..} -> putEnableDebugger2 tag
        ScriptLimits{..} -> putScriptLimits tag
        SetTabIndex{..} -> putSetTabIndex tag
        FileAttributes{..} -> putFileAttributes tag
        ImportAssets2{..} -> putImportAssets2 tag
        SymbolClass{..} -> putSymbolClass tag
        Metadata{..} -> putMetadata tag
        DefineScalingGrid{..} -> putDefineScalingGrid tag
        DefineSceneAndFrameLabelData{..} -> putDefineSceneAndFrameLabelData
                                              tag
        DoAction{..} -> putDoAction tag
        DoInitAction{..} -> putDoInitAction tag
        DoABC{..} -> putDoABC tag
        DefineShape{..} -> putDefineShape tag
        DefineShape2{..} -> putDefineShape2 tag
        DefineShape3{..} -> putDefineShape3 tag
        DefineShape4{..} -> putDefineShape4 tag
        DefineBits{..} -> putDefineBits tag
        JPEGTables{..} -> putJPEGTables tag
        DefineBitsJPEG2{..} -> putDefineBitsJPEG2 tag
        DefineBitsJPEG3{..} -> putDefineBitsJPEG3 tag
        DefineBitsLossless{..} -> putDefineBitsLossless tag
        DefineBitsLossless2{..} -> putDefineBitsLossless2 tag
        DefineBitsJPEG4{..} -> putDefineBitsJPEG4 tag
        DefineMorphShape{..} -> putDefineMorphShape tag
        DefineMorphShape2{..} -> putDefineMorphShape2 tag
        DefineFontInfo{..} -> putDefineFontInfo tag
        DefineFontInfo2{..} -> putDefineFontInfo2 tag
        DefineFont2{..} -> putDefineFont2 tag
        DefineFont3{..} -> putDefineFont3 tag
        DefineFontAlignZones{..} -> putDefineFontAlignZones tag
        DefineFontName{..} -> putDefineFontName tag
        DefineText{..} -> putDefineText tag
        DefineText2{..} -> putDefineText2 tag
        DefineEditText{..} -> putDefineEditText tag
        CSMTextSettings{..} -> putCSMTextSettings tag
        DefineFont4{..} -> putDefineFont4 tag
        DefineSound{..} -> putDefineSound tag
        StartSound{..} -> putStartSound tag
        StartSound2{..} -> putStartSound2 tag
        SoundStreamHead{..} -> putSoundStreamHead tag
        SoundStreamHead2{..} -> putSoundStreamHead2 tag
        SoundStreamBlock{..} -> putSoundStreamBlock tag
        DefineButton{..} -> putDefineButton tag
        DefineButton2{..} -> putDefineButton2 tag
        DefineButtonCxform{..} -> putDefineButtonCxform tag
        DefineButtonSound{..} -> putDefineButtonSound tag
        DefineSprite{..} -> putDefineSprite tag
        DefineVideoStream{..} -> putDefineVideoStream tag
        StreamID{..} -> putStreamID tag
        DefineBinaryData{..} -> putDefineBinaryData tag
generatedTagTypes tag
  = case tag of
        PlaceObject{..} -> 4
        PlaceObject2{..} -> 26
        PlaceObject3{..} -> 70
        RemoveObject{..} -> 5
        RemoveObject2{..} -> 28
        ShowFrame{..} -> 1
        SetBackgroundColor{..} -> 9
        FrameLabel{..} -> 43
        Protect{..} -> 24
        End{..} -> 0
        ExportAssets{..} -> 56
        ImportAssets{..} -> 57
        EnableDebugger{..} -> 58
        EnableDebugger2{..} -> 64
        ScriptLimits{..} -> 65
        SetTabIndex{..} -> 66
        FileAttributes{..} -> 69
        ImportAssets2{..} -> 71
        SymbolClass{..} -> 76
        Metadata{..} -> 77
        DefineScalingGrid{..} -> 78
        DefineSceneAndFrameLabelData{..} -> 86
        DoAction{..} -> 12
        DoInitAction{..} -> 59
        DoABC{..} -> 82
        DefineShape{..} -> 2
        DefineShape2{..} -> 22
        DefineShape3{..} -> 32
        DefineShape4{..} -> 83
        DefineBits{..} -> 6
        JPEGTables{..} -> 8
        DefineBitsJPEG2{..} -> 21
        DefineBitsJPEG3{..} -> 35
        DefineBitsLossless{..} -> 20
        DefineBitsLossless2{..} -> 36
        DefineBitsJPEG4{..} -> 90
        DefineMorphShape{..} -> 46
        DefineMorphShape2{..} -> 84
        DefineFontInfo{..} -> 13
        DefineFontInfo2{..} -> 62
        DefineFont2{..} -> 48
        DefineFont3{..} -> 75
        DefineFontAlignZones{..} -> 73
        DefineFontName{..} -> 88
        DefineText{..} -> 11
        DefineText2{..} -> 33
        DefineEditText{..} -> 37
        CSMTextSettings{..} -> 74
        DefineFont4{..} -> 91
        DefineSound{..} -> 14
        StartSound{..} -> 15
        StartSound2{..} -> 89
        SoundStreamHead{..} -> 18
        SoundStreamHead2{..} -> 45
        SoundStreamBlock{..} -> 19
        DefineButton{..} -> 7
        DefineButton2{..} -> 34
        DefineButtonCxform{..} -> 23
        DefineButtonSound{..} -> 17
        DefineSprite{..} -> 39
        DefineVideoStream{..} -> 60
        StreamID{..} -> 61
        DefineBinaryData{..} -> 87

\end{code}

p34: PlaceObject
\begin{code}
getPlaceObject
  = do placeObject_characterId <- getUI16
       placeObject_depth <- getUI16
       placeObject_matrix <- getMATRIX
       placeObject_colorTransform <- maybeHasM (fmap not isEmpty)
                                       getCXFORM
       return (PlaceObject{..})
putPlaceObject PlaceObject{..}
  = do putUI16 placeObject_characterId
       putUI16 placeObject_depth
       putMATRIX placeObject_matrix
       case placeObject_colorTransform of
           Just x -> putCXFORM x
           Nothing -> return ()
       return ()

\end{code}

p35: PlaceObject2
\begin{code}
getPlaceObject2
  = do placeObject2_placeFlagHasClipActions <- getFlag
       placeObject2_placeFlagHasClipDepth <- getFlag
       placeObject2_placeFlagHasName <- getFlag
       placeObject2_placeFlagHasRatio <- getFlag
       placeObject2_placeFlagHasColorTransform <- getFlag
       placeObject2_placeFlagHasMatrix <- getFlag
       placeObject2_placeFlagHasCharacter <- getFlag
       placeObject2_placeFlagMove <- getFlag
       placeObject2_depth <- getUI16
       placeObject2_characterId <- maybeHas
                                     placeObject2_placeFlagHasCharacter
                                     getUI16
       placeObject2_matrix <- maybeHas placeObject2_placeFlagHasMatrix
                                getMATRIX
       placeObject2_colorTransform <- maybeHas
                                        placeObject2_placeFlagHasColorTransform
                                        getCXFORMWITHALPHA
       placeObject2_ratio <- maybeHas placeObject2_placeFlagHasRatio
                               getUI16
       placeObject2_name <- maybeHas placeObject2_placeFlagHasName
                              getSTRING
       placeObject2_clipDepth <- maybeHas
                                   placeObject2_placeFlagHasClipDepth
                                   getUI16
       placeObject2_clipActions <- maybeHas
                                     placeObject2_placeFlagHasClipActions
                                     getCLIPACTIONS
       return (PlaceObject2{..})
putPlaceObject2 PlaceObject2{..}
  = do let placeObject2_placeFlagHasClipActions
             = isJust placeObject2_clipActions
       putFlag placeObject2_placeFlagHasClipActions
       let placeObject2_placeFlagHasClipDepth
             = isJust placeObject2_clipDepth
       putFlag placeObject2_placeFlagHasClipDepth
       let placeObject2_placeFlagHasName = isJust placeObject2_name
       putFlag placeObject2_placeFlagHasName
       let placeObject2_placeFlagHasRatio = isJust placeObject2_ratio
       putFlag placeObject2_placeFlagHasRatio
       let placeObject2_placeFlagHasColorTransform
             = isJust placeObject2_colorTransform
       putFlag placeObject2_placeFlagHasColorTransform
       let placeObject2_placeFlagHasMatrix = isJust placeObject2_matrix
       putFlag placeObject2_placeFlagHasMatrix
       let placeObject2_placeFlagHasCharacter
             = isJust placeObject2_characterId
       putFlag placeObject2_placeFlagHasCharacter
       putFlag placeObject2_placeFlagMove
       putUI16 placeObject2_depth
       case placeObject2_characterId of
           Just x | placeObject2_placeFlagHasCharacter -> putUI16 x
                  | otherwise ->
                    inconsistent "placeObject2_characterId (x :: PlaceObject2)"
                      "Should have a Just iff placeObject2_placeFlagHasCharacter is True"
           Nothing | placeObject2_placeFlagHasCharacter ->
                     inconsistent "placeObject2_characterId (x :: PlaceObject2)"
                       "Should have a Nothing iff placeObject2_placeFlagHasCharacter is False"
                   | otherwise -> return ()
       case placeObject2_matrix of
           Just x | placeObject2_placeFlagHasMatrix -> putMATRIX x
                  | otherwise ->
                    inconsistent "placeObject2_matrix (x :: PlaceObject2)"
                      "Should have a Just iff placeObject2_placeFlagHasMatrix is True"
           Nothing | placeObject2_placeFlagHasMatrix ->
                     inconsistent "placeObject2_matrix (x :: PlaceObject2)"
                       "Should have a Nothing iff placeObject2_placeFlagHasMatrix is False"
                   | otherwise -> return ()
       case placeObject2_colorTransform of
           Just x | placeObject2_placeFlagHasColorTransform ->
                    putCXFORMWITHALPHA x
                  | otherwise ->
                    inconsistent "placeObject2_colorTransform (x :: PlaceObject2)"
                      "Should have a Just iff placeObject2_placeFlagHasColorTransform is True"
           Nothing | placeObject2_placeFlagHasColorTransform ->
                     inconsistent "placeObject2_colorTransform (x :: PlaceObject2)"
                       "Should have a Nothing iff placeObject2_placeFlagHasColorTransform is False"
                   | otherwise -> return ()
       case placeObject2_ratio of
           Just x | placeObject2_placeFlagHasRatio -> putUI16 x
                  | otherwise ->
                    inconsistent "placeObject2_ratio (x :: PlaceObject2)"
                      "Should have a Just iff placeObject2_placeFlagHasRatio is True"
           Nothing | placeObject2_placeFlagHasRatio ->
                     inconsistent "placeObject2_ratio (x :: PlaceObject2)"
                       "Should have a Nothing iff placeObject2_placeFlagHasRatio is False"
                   | otherwise -> return ()
       case placeObject2_name of
           Just x | placeObject2_placeFlagHasName -> putSTRING x
                  | otherwise ->
                    inconsistent "placeObject2_name (x :: PlaceObject2)"
                      "Should have a Just iff placeObject2_placeFlagHasName is True"
           Nothing | placeObject2_placeFlagHasName ->
                     inconsistent "placeObject2_name (x :: PlaceObject2)"
                       "Should have a Nothing iff placeObject2_placeFlagHasName is False"
                   | otherwise -> return ()
       case placeObject2_clipDepth of
           Just x | placeObject2_placeFlagHasClipDepth -> putUI16 x
                  | otherwise ->
                    inconsistent "placeObject2_clipDepth (x :: PlaceObject2)"
                      "Should have a Just iff placeObject2_placeFlagHasClipDepth is True"
           Nothing | placeObject2_placeFlagHasClipDepth ->
                     inconsistent "placeObject2_clipDepth (x :: PlaceObject2)"
                       "Should have a Nothing iff placeObject2_placeFlagHasClipDepth is False"
                   | otherwise -> return ()
       case placeObject2_clipActions of
           Just x | placeObject2_placeFlagHasClipActions -> putCLIPACTIONS x
                  | otherwise ->
                    inconsistent "placeObject2_clipActions (x :: PlaceObject2)"
                      "Should have a Just iff placeObject2_placeFlagHasClipActions is True"
           Nothing | placeObject2_placeFlagHasClipActions ->
                     inconsistent "placeObject2_clipActions (x :: PlaceObject2)"
                       "Should have a Nothing iff placeObject2_placeFlagHasClipActions is False"
                   | otherwise -> return ()
       return ()

\end{code}

\begin{code}
 
data CLIPACTIONS = CLIPACTIONS{cLIPACTIONS_allEventFlags ::
                               CLIPEVENTFLAGS,
                               cLIPACTIONS_clipActionRecords :: CLIPACTIONRECORDS}
                 deriving (Eq, Show, Typeable, Data)
getCLIPACTIONS
  = do discardReserved "_reserved (x :: ?)" getUI16
       cLIPACTIONS_allEventFlags <- getCLIPEVENTFLAGS
       cLIPACTIONS_clipActionRecords <- getCLIPACTIONRECORDS
       return (CLIPACTIONS{..})
putCLIPACTIONS CLIPACTIONS{..}
  = do let cLIPACTIONS_reserved = reservedDefault
       putUI16 cLIPACTIONS_reserved
       putCLIPEVENTFLAGS cLIPACTIONS_allEventFlags
       putCLIPACTIONRECORDS cLIPACTIONS_clipActionRecords
       return ()

\end{code}

\begin{code}

type CLIPACTIONRECORDS = [CLIPACTIONRECORD]

getCLIPACTIONRECORDS = do
    look <- lookAhead getCLIPEVENTFLAGS
    if (null look)
     then getCLIPEVENTFLAGS >> return []
     else do
       x <- getCLIPACTIONRECORD
       fmap (x:) getCLIPACTIONRECORDS

putCLIPACTIONRECORDS rs = do
    mapM_ putCLIPACTIONRECORD rs
    putCLIPEVENTFLAGS []

data CLIPACTIONRECORD = CLIPACTIONRECORD { cLIPACTIONRECORD_eventFlags :: CLIPEVENTFLAGS, cLIPACTIONRECORD_keyCode :: Maybe UI8, cLIPACTIONRECORD_actions :: [ACTIONRECORD] }
                      deriving (Eq, Show, Typeable, Data)

getCLIPACTIONRECORD = do
    cLIPACTIONRECORD_eventFlags <- getCLIPEVENTFLAGS
    actionRecordSize <- getUI32
    (cLIPACTIONRECORD_keyCode, cLIPACTIONRECORD_actions) <- nestSwfGet (fromIntegral actionRecordSize) $ do
        keyCode <- maybeHas (ClipEventKeyPress `elem` cLIPACTIONRECORD_eventFlags) getUI8
        actions <- getACTIONRECORDS
        return (keyCode, actions)
    return $ CLIPACTIONRECORD {..}
  where
    getACTIONRECORDS = condM isEmpty (return []) $ do
        action <- getACTIONRECORD
        fmap (action:) getACTIONRECORDS

putCLIPACTIONRECORD (CLIPACTIONRECORD {..}) = do
    putCLIPEVENTFLAGS cLIPACTIONRECORD_eventFlags
    (len, putrest) <- nestSwfPut $ do
        when (consistentWith (inconsistent "cLIPACTIONRECORD_keyCode (x :: CLIPACTIONRECORD)" "Can have a cLIPACTIONRECORD_keyCode iff the ClipEventKeyPress flag is set")
                             (ClipEventKeyPress `elem` cLIPACTIONRECORD_eventFlags)
                             (isJust cLIPACTIONRECORD_keyCode)) $ do
            putUI8 (fromJust cLIPACTIONRECORD_keyCode)
        putACTIONRECORDS cLIPACTIONRECORD_actions
    putUI32 len
    putrest

\end{code}

p38: PlaceObject3
\begin{code}
getPlaceObject3
  = do placeObject3_placeFlagHasClipActions <- getFlag
       placeObject3_placeFlagHasClipDepth <- getFlag
       placeObject3_placeFlagHasName <- getFlag
       placeObject3_placeFlagHasRatio <- getFlag
       placeObject3_placeFlagHasColorTransform <- getFlag
       placeObject3_placeFlagHasMatrix <- getFlag
       placeObject3_placeFlagHasCharacter <- getFlag
       placeObject3_placeFlagMove <- getFlag
       discardReserved "_reserved (x :: ?)" (getUB 3)
       placeObject3_placeFlagHasImage <- getFlag
       placeObject3_placeFlagHasClassName <- getFlag
       placeObject3_placeFlagHasCacheAsBitmap <- getFlag
       placeObject3_placeFlagHasBlendMode <- getFlag
       placeObject3_placeFlagHasFilterList <- getFlag
       placeObject3_depth <- getUI16
       placeObject3_className <- maybeHas
                                   (placeObject3_placeFlagHasClassName ||
                                      placeObject3_placeFlagHasImage &&
                                        placeObject3_placeFlagHasCharacter)
                                   getSTRING
       placeObject3_characterId <- maybeHas
                                     placeObject3_placeFlagHasCharacter
                                     getUI16
       placeObject3_matrix <- maybeHas placeObject3_placeFlagHasMatrix
                                getMATRIX
       placeObject3_colorTransform <- maybeHas
                                        placeObject3_placeFlagHasColorTransform
                                        getCXFORMWITHALPHA
       placeObject3_ratio <- maybeHas placeObject3_placeFlagHasRatio
                               getUI16
       placeObject3_name <- maybeHas placeObject3_placeFlagHasName
                              getSTRING
       placeObject3_clipDepth <- maybeHas
                                   placeObject3_placeFlagHasClipDepth
                                   getUI16
       placeObject3_surfaceFilterList <- maybeHas
                                           placeObject3_placeFlagHasFilterList
                                           getFILTERLIST
       placeObject3_blendMode <- maybeHas
                                   placeObject3_placeFlagHasBlendMode
                                   getBlendMode
       placeObject3_bitmapCache <- maybeHas
                                     placeObject3_placeFlagHasCacheAsBitmap
                                     getUI8
       placeObject3_clipActions <- maybeHas
                                     placeObject3_placeFlagHasClipActions
                                     getCLIPACTIONS
       return (PlaceObject3{..})
putPlaceObject3 PlaceObject3{..}
  = do let placeObject3_placeFlagHasClipActions
             = isJust placeObject3_clipActions
       putFlag placeObject3_placeFlagHasClipActions
       let placeObject3_placeFlagHasClipDepth
             = isJust placeObject3_clipDepth
       putFlag placeObject3_placeFlagHasClipDepth
       let placeObject3_placeFlagHasName = isJust placeObject3_name
       putFlag placeObject3_placeFlagHasName
       let placeObject3_placeFlagHasRatio = isJust placeObject3_ratio
       putFlag placeObject3_placeFlagHasRatio
       let placeObject3_placeFlagHasColorTransform
             = isJust placeObject3_colorTransform
       putFlag placeObject3_placeFlagHasColorTransform
       let placeObject3_placeFlagHasMatrix = isJust placeObject3_matrix
       putFlag placeObject3_placeFlagHasMatrix
       let placeObject3_placeFlagHasCharacter
             = isJust placeObject3_characterId
       putFlag placeObject3_placeFlagHasCharacter
       putFlag placeObject3_placeFlagMove
       let placeObject3_reserved = reservedDefault
       if requiredBitsUB placeObject3_reserved <= 3 then
         putUB 3 placeObject3_reserved else
         inconsistent "x :: PlaceObject3"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB placeObject3_reserved) ++
                " bits to store the value " ++
                  show placeObject3_reserved ++
                    ", but only have available " ++ show 3)
       putFlag placeObject3_placeFlagHasImage
       putFlag placeObject3_placeFlagHasClassName
       let placeObject3_placeFlagHasCacheAsBitmap
             = isJust placeObject3_bitmapCache
       putFlag placeObject3_placeFlagHasCacheAsBitmap
       let placeObject3_placeFlagHasBlendMode
             = isJust placeObject3_blendMode
       putFlag placeObject3_placeFlagHasBlendMode
       let placeObject3_placeFlagHasFilterList
             = isJust placeObject3_surfaceFilterList
       putFlag placeObject3_placeFlagHasFilterList
       putUI16 placeObject3_depth
       case placeObject3_className of
           Just x |
                    placeObject3_placeFlagHasClassName ||
                      placeObject3_placeFlagHasImage &&
                        placeObject3_placeFlagHasCharacter
                    -> putSTRING x
                  | otherwise ->
                    inconsistent "placeObject3_className (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasClassName || placeObject3_placeFlagHasImage && placeObject3_placeFlagHasCharacter is True"
           Nothing |
                     placeObject3_placeFlagHasClassName ||
                       placeObject3_placeFlagHasImage &&
                         placeObject3_placeFlagHasCharacter
                     ->
                     inconsistent "placeObject3_className (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasClassName || placeObject3_placeFlagHasImage && placeObject3_placeFlagHasCharacter is False"
                   | otherwise -> return ()
       case placeObject3_characterId of
           Just x | placeObject3_placeFlagHasCharacter -> putUI16 x
                  | otherwise ->
                    inconsistent "placeObject3_characterId (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasCharacter is True"
           Nothing | placeObject3_placeFlagHasCharacter ->
                     inconsistent "placeObject3_characterId (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasCharacter is False"
                   | otherwise -> return ()
       case placeObject3_matrix of
           Just x | placeObject3_placeFlagHasMatrix -> putMATRIX x
                  | otherwise ->
                    inconsistent "placeObject3_matrix (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasMatrix is True"
           Nothing | placeObject3_placeFlagHasMatrix ->
                     inconsistent "placeObject3_matrix (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasMatrix is False"
                   | otherwise -> return ()
       case placeObject3_colorTransform of
           Just x | placeObject3_placeFlagHasColorTransform ->
                    putCXFORMWITHALPHA x
                  | otherwise ->
                    inconsistent "placeObject3_colorTransform (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasColorTransform is True"
           Nothing | placeObject3_placeFlagHasColorTransform ->
                     inconsistent "placeObject3_colorTransform (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasColorTransform is False"
                   | otherwise -> return ()
       case placeObject3_ratio of
           Just x | placeObject3_placeFlagHasRatio -> putUI16 x
                  | otherwise ->
                    inconsistent "placeObject3_ratio (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasRatio is True"
           Nothing | placeObject3_placeFlagHasRatio ->
                     inconsistent "placeObject3_ratio (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasRatio is False"
                   | otherwise -> return ()
       case placeObject3_name of
           Just x | placeObject3_placeFlagHasName -> putSTRING x
                  | otherwise ->
                    inconsistent "placeObject3_name (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasName is True"
           Nothing | placeObject3_placeFlagHasName ->
                     inconsistent "placeObject3_name (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasName is False"
                   | otherwise -> return ()
       case placeObject3_clipDepth of
           Just x | placeObject3_placeFlagHasClipDepth -> putUI16 x
                  | otherwise ->
                    inconsistent "placeObject3_clipDepth (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasClipDepth is True"
           Nothing | placeObject3_placeFlagHasClipDepth ->
                     inconsistent "placeObject3_clipDepth (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasClipDepth is False"
                   | otherwise -> return ()
       case placeObject3_surfaceFilterList of
           Just x | placeObject3_placeFlagHasFilterList -> putFILTERLIST x
                  | otherwise ->
                    inconsistent "placeObject3_surfaceFilterList (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasFilterList is True"
           Nothing | placeObject3_placeFlagHasFilterList ->
                     inconsistent "placeObject3_surfaceFilterList (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasFilterList is False"
                   | otherwise -> return ()
       case placeObject3_blendMode of
           Just x | placeObject3_placeFlagHasBlendMode -> putBlendMode x
                  | otherwise ->
                    inconsistent "placeObject3_blendMode (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasBlendMode is True"
           Nothing | placeObject3_placeFlagHasBlendMode ->
                     inconsistent "placeObject3_blendMode (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasBlendMode is False"
                   | otherwise -> return ()
       case placeObject3_bitmapCache of
           Just x | placeObject3_placeFlagHasCacheAsBitmap -> putUI8 x
                  | otherwise ->
                    inconsistent "placeObject3_bitmapCache (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasCacheAsBitmap is True"
           Nothing | placeObject3_placeFlagHasCacheAsBitmap ->
                     inconsistent "placeObject3_bitmapCache (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasCacheAsBitmap is False"
                   | otherwise -> return ()
       case placeObject3_clipActions of
           Just x | placeObject3_placeFlagHasClipActions -> putCLIPACTIONS x
                  | otherwise ->
                    inconsistent "placeObject3_clipActions (x :: PlaceObject3)"
                      "Should have a Just iff placeObject3_placeFlagHasClipActions is True"
           Nothing | placeObject3_placeFlagHasClipActions ->
                     inconsistent "placeObject3_clipActions (x :: PlaceObject3)"
                       "Should have a Nothing iff placeObject3_placeFlagHasClipActions is False"
                   | otherwise -> return ()
       return ()

\end{code}

\begin{code}

data BlendMode = Normal0 | Normal1
               | Layer | Multiply | Screen
               | Lighten | Darken
               | Difference | Add | Subtract
               | Invert | Alpha
               | Erase | Overlay
               | Hardlight
               | UnknownBlendMode UI8
               deriving (Eq, Show, Typeable, Data)

(getBlendMode, putBlendMode) = (getter, putter)
  where
    table = [0..] `zip` [Normal0, Normal1,
                         Layer, Multiply, Screen,
                         Lighten, Darken,
                         Difference, Add, Subtract,
                         Invert, Alpha,
                         Erase, Overlay, 
                         Hardlight]
    
    inverse_table = map (uncurry $ flip (,)) table
    
    getter = do
        i <- getUI8
        return $ case lookup i table of
          Just bm -> bm
          Nothing -> UnknownBlendMode i

    putter (UnknownBlendMode x) = putUI8 x
    putter bm                   = putUI8 (fromJust $ lookup bm inverse_table)

type FILTERLIST = [FILTER]

getFILTERLIST = do
    numberOfFilters <- getUI8
    genericReplicateM numberOfFilters getFILTER

putFILTERLIST filters = do
    putUI8 (genericLength filters)
    mapM_ putFILTER filters

data FILTER = DropShadowFilter DROPSHADOWFILTER
            | BlurFilter BLURFILTER
            | GlowFilter GLOWFILTER
            | BevelFilter BEVELFILTER
            | GradientGlowFilter GRADIENTGLOWFILTER
            | ConvolutionFilter CONVOLUTIONFILTER
            | ColorMatrixFilter COLORMATRIXFILTER
            | GradientBevelFilter GRADIENTBEVELFILTER
            deriving (Eq, Show, Typeable, Data)

getFILTER = do
    filterID <- getUI8
    case filterID of
        0 -> fmap DropShadowFilter getDROPSHADOWFILTER
        1 -> fmap BlurFilter getBLURFILTER
        2 -> fmap GlowFilter getGLOWFILTER
        3 -> fmap BevelFilter getBEVELFILTER
        4 -> fmap GradientGlowFilter getGRADIENTGLOWFILTER
        5 -> fmap ConvolutionFilter getCONVOLUTIONFILTER
        6 -> fmap ColorMatrixFilter getCOLORMATRIXFILTER
        7 -> fmap GradientBevelFilter getGRADIENTBEVELFILTER

putFILTER f = case f of
    DropShadowFilter x    -> putUI8 0 >> putDROPSHADOWFILTER x
    BlurFilter x          -> putUI8 1 >> putBLURFILTER x
    GlowFilter x          -> putUI8 2 >> putGLOWFILTER x
    BevelFilter x         -> putUI8 3 >> putBEVELFILTER x
    GradientGlowFilter x  -> putUI8 4 >> putGRADIENTGLOWFILTER x
    ConvolutionFilter x   -> putUI8 5 >> putCONVOLUTIONFILTER x
    ColorMatrixFilter x   -> putUI8 6 >> putCOLORMATRIXFILTER x
    GradientBevelFilter x -> putUI8 7 >> putGRADIENTBEVELFILTER x

\end{code}

p42: Color Matrix filter
\begin{code}
 
data COLORMATRIXFILTER = COLORMATRIXFILTER{cOLORMATRIXFILTER_matrix
                                           :: [FLOAT]}
                       deriving (Eq, Show, Typeable, Data)
getCOLORMATRIXFILTER
  = do cOLORMATRIXFILTER_matrix <- genericReplicateM 20 getFLOAT
       return (COLORMATRIXFILTER{..})
putCOLORMATRIXFILTER COLORMATRIXFILTER{..}
  = do if genericLength cOLORMATRIXFILTER_matrix /= (20) then
         inconsistent "cOLORMATRIXFILTER_matrix (x :: COLORMATRIXFILTER)"
           ("Mismatch with the required length: 20" ++
              show (genericLength cOLORMATRIXFILTER_matrix) ++ " /= " ++ show 20)
         else mapM_ (\ x -> putFLOAT x) cOLORMATRIXFILTER_matrix
       return ()

\end{code}

p43: Convolution filter
\begin{code}
 
data CONVOLUTIONFILTER = CONVOLUTIONFILTER{cONVOLUTIONFILTER_matrixX
                                           :: UI8,
                                           cONVOLUTIONFILTER_matrixY :: UI8,
                                           cONVOLUTIONFILTER_divisor :: FLOAT,
                                           cONVOLUTIONFILTER_bias :: FLOAT,
                                           cONVOLUTIONFILTER_matrix :: [FLOAT],
                                           cONVOLUTIONFILTER_defaultColor :: RGBA,
                                           cONVOLUTIONFILTER_clamp :: Bool,
                                           cONVOLUTIONFILTER_preserveAlpha :: Bool}
                       deriving (Eq, Show, Typeable, Data)
getCONVOLUTIONFILTER
  = do cONVOLUTIONFILTER_matrixX <- getUI8
       cONVOLUTIONFILTER_matrixY <- getUI8
       cONVOLUTIONFILTER_divisor <- getFLOAT
       cONVOLUTIONFILTER_bias <- getFLOAT
       cONVOLUTIONFILTER_matrix <- genericReplicateM
                                     (cONVOLUTIONFILTER_matrixX * cONVOLUTIONFILTER_matrixY)
                                     getFLOAT
       cONVOLUTIONFILTER_defaultColor <- getRGBA
       discardReserved "_reserved (x :: ?)" (getUB 6)
       cONVOLUTIONFILTER_clamp <- getFlag
       cONVOLUTIONFILTER_preserveAlpha <- getFlag
       return (CONVOLUTIONFILTER{..})
putCONVOLUTIONFILTER CONVOLUTIONFILTER{..}
  = do putUI8 cONVOLUTIONFILTER_matrixX
       putUI8 cONVOLUTIONFILTER_matrixY
       putFLOAT cONVOLUTIONFILTER_divisor
       putFLOAT cONVOLUTIONFILTER_bias
       if
         genericLength cONVOLUTIONFILTER_matrix /=
           (cONVOLUTIONFILTER_matrixX * cONVOLUTIONFILTER_matrixY)
         then
         inconsistent "cONVOLUTIONFILTER_matrix (x :: CONVOLUTIONFILTER)"
           ("Mismatch with the required length: cONVOLUTIONFILTER_matrixX * cONVOLUTIONFILTER_matrixY"
              ++
              show (genericLength cONVOLUTIONFILTER_matrix) ++
                " /= " ++
                  show (cONVOLUTIONFILTER_matrixX * cONVOLUTIONFILTER_matrixY))
         else mapM_ (\ x -> putFLOAT x) cONVOLUTIONFILTER_matrix
       putRGBA cONVOLUTIONFILTER_defaultColor
       let cONVOLUTIONFILTER_reserved = reservedDefault
       if requiredBitsUB cONVOLUTIONFILTER_reserved <= 6 then
         putUB 6 cONVOLUTIONFILTER_reserved else
         inconsistent "x :: CONVOLUTIONFILTER"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB cONVOLUTIONFILTER_reserved) ++
                " bits to store the value " ++
                  show cONVOLUTIONFILTER_reserved ++
                    ", but only have available " ++ show 6)
       putFlag cONVOLUTIONFILTER_clamp
       putFlag cONVOLUTIONFILTER_preserveAlpha
       return ()

\end{code}

p44: Blur filter
\begin{code}
 
data BLURFILTER = BLURFILTER{bLURFILTER_blurX :: FIXED,
                             bLURFILTER_blurY :: FIXED, bLURFILTER_passes :: UB}
                deriving (Eq, Show, Typeable, Data)
getBLURFILTER
  = do bLURFILTER_blurX <- getFIXED
       bLURFILTER_blurY <- getFIXED
       bLURFILTER_passes <- getUB 5
       discardReserved "_reserved (x :: ?)" (getUB 3)
       return (BLURFILTER{..})
putBLURFILTER BLURFILTER{..}
  = do putFIXED bLURFILTER_blurX
       putFIXED bLURFILTER_blurY
       if requiredBitsUB bLURFILTER_passes <= 5 then
         putUB 5 bLURFILTER_passes else
         inconsistent "bLURFILTER_passes (x :: BLURFILTER)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB bLURFILTER_passes) ++
                " bits to store the value " ++
                  show bLURFILTER_passes ++ ", but only have available " ++ show 5)
       let bLURFILTER_reserved = reservedDefault
       if requiredBitsUB bLURFILTER_reserved <= 3 then
         putUB 3 bLURFILTER_reserved else
         inconsistent "x :: BLURFILTER"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB bLURFILTER_reserved) ++
                " bits to store the value " ++
                  show bLURFILTER_reserved ++ ", but only have available " ++ show 3)
       return ()

\end{code}

p45: Drop Shadow filter
\begin{code}
 
data DROPSHADOWFILTER = DROPSHADOWFILTER{dROPSHADOWFILTER_dropShadowColor
                                         :: RGBA,
                                         dROPSHADOWFILTER_blurX :: FIXED,
                                         dROPSHADOWFILTER_blurY :: FIXED,
                                         dROPSHADOWFILTER_angle :: FIXED,
                                         dROPSHADOWFILTER_distance :: FIXED,
                                         dROPSHADOWFILTER_strength :: FIXED8,
                                         dROPSHADOWFILTER_innerShadow :: Bool,
                                         dROPSHADOWFILTER_knockout :: Bool,
                                         dROPSHADOWFILTER_compositeSource :: Bool,
                                         dROPSHADOWFILTER_passes :: UB}
                      deriving (Eq, Show, Typeable, Data)
getDROPSHADOWFILTER
  = do dROPSHADOWFILTER_dropShadowColor <- getRGBA
       dROPSHADOWFILTER_blurX <- getFIXED
       dROPSHADOWFILTER_blurY <- getFIXED
       dROPSHADOWFILTER_angle <- getFIXED
       dROPSHADOWFILTER_distance <- getFIXED
       dROPSHADOWFILTER_strength <- getFIXED8
       dROPSHADOWFILTER_innerShadow <- getFlag
       dROPSHADOWFILTER_knockout <- getFlag
       dROPSHADOWFILTER_compositeSource <- getFlag
       dROPSHADOWFILTER_passes <- getUB 5
       return (DROPSHADOWFILTER{..})
putDROPSHADOWFILTER DROPSHADOWFILTER{..}
  = do putRGBA dROPSHADOWFILTER_dropShadowColor
       putFIXED dROPSHADOWFILTER_blurX
       putFIXED dROPSHADOWFILTER_blurY
       putFIXED dROPSHADOWFILTER_angle
       putFIXED dROPSHADOWFILTER_distance
       putFIXED8 dROPSHADOWFILTER_strength
       putFlag dROPSHADOWFILTER_innerShadow
       putFlag dROPSHADOWFILTER_knockout
       putFlag dROPSHADOWFILTER_compositeSource
       if requiredBitsUB dROPSHADOWFILTER_passes <= 5 then
         putUB 5 dROPSHADOWFILTER_passes else
         inconsistent "dROPSHADOWFILTER_passes (x :: DROPSHADOWFILTER)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB dROPSHADOWFILTER_passes) ++
                " bits to store the value " ++
                  show dROPSHADOWFILTER_passes ++
                    ", but only have available " ++ show 5)
       return ()

\end{code}

p46: Glow filter
\begin{code}
 
data GLOWFILTER = GLOWFILTER{gLOWFILTER_glowColor :: RGBA,
                             gLOWFILTER_blurX :: FIXED, gLOWFILTER_blurY :: FIXED,
                             gLOWFILTER_strength :: FIXED8, gLOWFILTER_innerGlow :: Bool,
                             gLOWFILTER_knockout :: Bool, gLOWFILTER_compositeSource :: Bool,
                             gLOWFILTER_passes :: UB}
                deriving (Eq, Show, Typeable, Data)
getGLOWFILTER
  = do gLOWFILTER_glowColor <- getRGBA
       gLOWFILTER_blurX <- getFIXED
       gLOWFILTER_blurY <- getFIXED
       gLOWFILTER_strength <- getFIXED8
       gLOWFILTER_innerGlow <- getFlag
       gLOWFILTER_knockout <- getFlag
       gLOWFILTER_compositeSource <- getFlag
       gLOWFILTER_passes <- getUB 5
       return (GLOWFILTER{..})
putGLOWFILTER GLOWFILTER{..}
  = do putRGBA gLOWFILTER_glowColor
       putFIXED gLOWFILTER_blurX
       putFIXED gLOWFILTER_blurY
       putFIXED8 gLOWFILTER_strength
       putFlag gLOWFILTER_innerGlow
       putFlag gLOWFILTER_knockout
       putFlag gLOWFILTER_compositeSource
       if requiredBitsUB gLOWFILTER_passes <= 5 then
         putUB 5 gLOWFILTER_passes else
         inconsistent "gLOWFILTER_passes (x :: GLOWFILTER)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB gLOWFILTER_passes) ++
                " bits to store the value " ++
                  show gLOWFILTER_passes ++ ", but only have available " ++ show 5)
       return ()

\end{code}

p48: Bevel filter
\begin{code}
 
data BEVELFILTER = BEVELFILTER{bEVELFILTER_shadowColor :: RGBA,
                               bEVELFILTER_highlightColor :: RGBA, bEVELFILTER_blurX :: FIXED,
                               bEVELFILTER_blurY :: FIXED, bEVELFILTER_angle :: FIXED,
                               bEVELFILTER_distance :: FIXED, bEVELFILTER_strength :: FIXED8,
                               bEVELFILTER_innerShadow :: Bool, bEVELFILTER_knockout :: Bool,
                               bEVELFILTER_compositeSource :: Bool, bEVELFILTER_onTop :: Bool,
                               bEVELFILTER_passes :: UB}
                 deriving (Eq, Show, Typeable, Data)
getBEVELFILTER
  = do bEVELFILTER_shadowColor <- getRGBA
       bEVELFILTER_highlightColor <- getRGBA
       bEVELFILTER_blurX <- getFIXED
       bEVELFILTER_blurY <- getFIXED
       bEVELFILTER_angle <- getFIXED
       bEVELFILTER_distance <- getFIXED
       bEVELFILTER_strength <- getFIXED8
       bEVELFILTER_innerShadow <- getFlag
       bEVELFILTER_knockout <- getFlag
       bEVELFILTER_compositeSource <- getFlag
       bEVELFILTER_onTop <- getFlag
       bEVELFILTER_passes <- getUB 4
       return (BEVELFILTER{..})
putBEVELFILTER BEVELFILTER{..}
  = do putRGBA bEVELFILTER_shadowColor
       putRGBA bEVELFILTER_highlightColor
       putFIXED bEVELFILTER_blurX
       putFIXED bEVELFILTER_blurY
       putFIXED bEVELFILTER_angle
       putFIXED bEVELFILTER_distance
       putFIXED8 bEVELFILTER_strength
       putFlag bEVELFILTER_innerShadow
       putFlag bEVELFILTER_knockout
       putFlag bEVELFILTER_compositeSource
       putFlag bEVELFILTER_onTop
       if requiredBitsUB bEVELFILTER_passes <= 4 then
         putUB 4 bEVELFILTER_passes else
         inconsistent "bEVELFILTER_passes (x :: BEVELFILTER)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB bEVELFILTER_passes) ++
                " bits to store the value " ++
                  show bEVELFILTER_passes ++ ", but only have available " ++ show 4)
       return ()

\end{code}

p48: Gradient Glow and Gradient Bevel filters
\begin{code}
 
data GRADIENTGLOWFILTER = GRADIENTGLOWFILTER{gRADIENTGLOWFILTER_gradientColors
                                             :: [RGBA],
                                             gRADIENTGLOWFILTER_gradientRatio :: [UI8],
                                             gRADIENTGLOWFILTER_blurX :: FIXED,
                                             gRADIENTGLOWFILTER_blurY :: FIXED,
                                             gRADIENTGLOWFILTER_angle :: FIXED,
                                             gRADIENTGLOWFILTER_distance :: FIXED,
                                             gRADIENTGLOWFILTER_strength :: FIXED8,
                                             gRADIENTGLOWFILTER_innerShadow :: Bool,
                                             gRADIENTGLOWFILTER_knockout :: Bool,
                                             gRADIENTGLOWFILTER_compositeSource :: Bool,
                                             gRADIENTGLOWFILTER_onTop :: Bool,
                                             gRADIENTGLOWFILTER_passes :: UB}
                        deriving (Eq, Show, Typeable, Data)
getGRADIENTGLOWFILTER
  = do gRADIENTGLOWFILTER_numColors <- getUI8
       gRADIENTGLOWFILTER_gradientColors <- genericReplicateM
                                              gRADIENTGLOWFILTER_numColors
                                              getRGBA
       gRADIENTGLOWFILTER_gradientRatio <- genericReplicateM
                                             gRADIENTGLOWFILTER_numColors
                                             getUI8
       gRADIENTGLOWFILTER_blurX <- getFIXED
       gRADIENTGLOWFILTER_blurY <- getFIXED
       gRADIENTGLOWFILTER_angle <- getFIXED
       gRADIENTGLOWFILTER_distance <- getFIXED
       gRADIENTGLOWFILTER_strength <- getFIXED8
       gRADIENTGLOWFILTER_innerShadow <- getFlag
       gRADIENTGLOWFILTER_knockout <- getFlag
       gRADIENTGLOWFILTER_compositeSource <- getFlag
       gRADIENTGLOWFILTER_onTop <- getFlag
       gRADIENTGLOWFILTER_passes <- getUB 4
       return (GRADIENTGLOWFILTER{..})
putGRADIENTGLOWFILTER GRADIENTGLOWFILTER{..}
  = do let gRADIENTGLOWFILTER_numColors
             = genericLength gRADIENTGLOWFILTER_gradientColors
       putUI8 gRADIENTGLOWFILTER_numColors
       if
         genericLength gRADIENTGLOWFILTER_gradientColors /=
           (gRADIENTGLOWFILTER_numColors)
         then
         inconsistent
           "gRADIENTGLOWFILTER_gradientColors (x :: GRADIENTGLOWFILTER)"
           ("Mismatch with the required length: gRADIENTGLOWFILTER_numColors"
              ++
              show (genericLength gRADIENTGLOWFILTER_gradientColors) ++
                " /= " ++ show gRADIENTGLOWFILTER_numColors)
         else mapM_ (\ x -> putRGBA x) gRADIENTGLOWFILTER_gradientColors
       if
         genericLength gRADIENTGLOWFILTER_gradientRatio /=
           (gRADIENTGLOWFILTER_numColors)
         then
         inconsistent
           "gRADIENTGLOWFILTER_gradientRatio (x :: GRADIENTGLOWFILTER)"
           ("Mismatch with the required length: gRADIENTGLOWFILTER_numColors"
              ++
              show (genericLength gRADIENTGLOWFILTER_gradientRatio) ++
                " /= " ++ show gRADIENTGLOWFILTER_numColors)
         else mapM_ (\ x -> putUI8 x) gRADIENTGLOWFILTER_gradientRatio
       putFIXED gRADIENTGLOWFILTER_blurX
       putFIXED gRADIENTGLOWFILTER_blurY
       putFIXED gRADIENTGLOWFILTER_angle
       putFIXED gRADIENTGLOWFILTER_distance
       putFIXED8 gRADIENTGLOWFILTER_strength
       putFlag gRADIENTGLOWFILTER_innerShadow
       putFlag gRADIENTGLOWFILTER_knockout
       putFlag gRADIENTGLOWFILTER_compositeSource
       putFlag gRADIENTGLOWFILTER_onTop
       if requiredBitsUB gRADIENTGLOWFILTER_passes <= 4 then
         putUB 4 gRADIENTGLOWFILTER_passes else
         inconsistent "gRADIENTGLOWFILTER_passes (x :: GRADIENTGLOWFILTER)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB gRADIENTGLOWFILTER_passes) ++
                " bits to store the value " ++
                  show gRADIENTGLOWFILTER_passes ++
                    ", but only have available " ++ show 4)
       return ()

\end{code}

\begin{code}
 
data GRADIENTBEVELFILTER = GRADIENTBEVELFILTER{gRADIENTBEVELFILTER_gradientColors
                                               :: [RGBA],
                                               gRADIENTBEVELFILTER_gradientRatio :: [UI8],
                                               gRADIENTBEVELFILTER_blurX :: FIXED,
                                               gRADIENTBEVELFILTER_blurY :: FIXED,
                                               gRADIENTBEVELFILTER_angle :: FIXED,
                                               gRADIENTBEVELFILTER_distance :: FIXED,
                                               gRADIENTBEVELFILTER_strength :: FIXED8,
                                               gRADIENTBEVELFILTER_innerShadow :: Bool,
                                               gRADIENTBEVELFILTER_knockout :: Bool,
                                               gRADIENTBEVELFILTER_compositeSource :: Bool,
                                               gRADIENTBEVELFILTER_onTop :: Bool,
                                               gRADIENTBEVELFILTER_passes :: UB}
                         deriving (Eq, Show, Typeable, Data)
getGRADIENTBEVELFILTER
  = do gRADIENTBEVELFILTER_numColors <- getUI8
       gRADIENTBEVELFILTER_gradientColors <- genericReplicateM
                                               gRADIENTBEVELFILTER_numColors
                                               getRGBA
       gRADIENTBEVELFILTER_gradientRatio <- genericReplicateM
                                              gRADIENTBEVELFILTER_numColors
                                              getUI8
       gRADIENTBEVELFILTER_blurX <- getFIXED
       gRADIENTBEVELFILTER_blurY <- getFIXED
       gRADIENTBEVELFILTER_angle <- getFIXED
       gRADIENTBEVELFILTER_distance <- getFIXED
       gRADIENTBEVELFILTER_strength <- getFIXED8
       gRADIENTBEVELFILTER_innerShadow <- getFlag
       gRADIENTBEVELFILTER_knockout <- getFlag
       gRADIENTBEVELFILTER_compositeSource <- getFlag
       gRADIENTBEVELFILTER_onTop <- getFlag
       gRADIENTBEVELFILTER_passes <- getUB 4
       return (GRADIENTBEVELFILTER{..})
putGRADIENTBEVELFILTER GRADIENTBEVELFILTER{..}
  = do let gRADIENTBEVELFILTER_numColors
             = genericLength gRADIENTBEVELFILTER_gradientColors
       putUI8 gRADIENTBEVELFILTER_numColors
       if
         genericLength gRADIENTBEVELFILTER_gradientColors /=
           (gRADIENTBEVELFILTER_numColors)
         then
         inconsistent
           "gRADIENTBEVELFILTER_gradientColors (x :: GRADIENTBEVELFILTER)"
           ("Mismatch with the required length: gRADIENTBEVELFILTER_numColors"
              ++
              show (genericLength gRADIENTBEVELFILTER_gradientColors) ++
                " /= " ++ show gRADIENTBEVELFILTER_numColors)
         else mapM_ (\ x -> putRGBA x) gRADIENTBEVELFILTER_gradientColors
       if
         genericLength gRADIENTBEVELFILTER_gradientRatio /=
           (gRADIENTBEVELFILTER_numColors)
         then
         inconsistent
           "gRADIENTBEVELFILTER_gradientRatio (x :: GRADIENTBEVELFILTER)"
           ("Mismatch with the required length: gRADIENTBEVELFILTER_numColors"
              ++
              show (genericLength gRADIENTBEVELFILTER_gradientRatio) ++
                " /= " ++ show gRADIENTBEVELFILTER_numColors)
         else mapM_ (\ x -> putUI8 x) gRADIENTBEVELFILTER_gradientRatio
       putFIXED gRADIENTBEVELFILTER_blurX
       putFIXED gRADIENTBEVELFILTER_blurY
       putFIXED gRADIENTBEVELFILTER_angle
       putFIXED gRADIENTBEVELFILTER_distance
       putFIXED8 gRADIENTBEVELFILTER_strength
       putFlag gRADIENTBEVELFILTER_innerShadow
       putFlag gRADIENTBEVELFILTER_knockout
       putFlag gRADIENTBEVELFILTER_compositeSource
       putFlag gRADIENTBEVELFILTER_onTop
       if requiredBitsUB gRADIENTBEVELFILTER_passes <= 4 then
         putUB 4 gRADIENTBEVELFILTER_passes else
         inconsistent
           "gRADIENTBEVELFILTER_passes (x :: GRADIENTBEVELFILTER)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB gRADIENTBEVELFILTER_passes) ++
                " bits to store the value " ++
                  show gRADIENTBEVELFILTER_passes ++
                    ", but only have available " ++ show 4)
       return ()

\end{code}

p50: CLIPEVENTFLAGS
\begin{code}

data CLIPEVENTFLAG = ClipEventKeyUp | ClipEventKeyDown
                   | ClipEventMouseUp | ClipEventMouseDown | ClipEventMouseMove
                   | ClipEventUnload | ClipEventEnterFrame | ClipEventLoad
                   | ClipEventDragOver | ClipEventRollOut | ClipEventRollOver
                   | ClipEventReleaseOutside | ClipEventRelease | ClipEventPress
                   | ClipEventInitialize | ClipEventData
                   | ClipEventConstruct | ClipEventKeyPress | ClipEventDragOut
                   deriving (Eq, Show, Data, Typeable)

type CLIPEVENTFLAGS = [CLIPEVENTFLAG]

(getCLIPEVENTFLAGS, putCLIPEVENTFLAGS) = (getter, putter)
  where
    initial_flags = [ClipEventKeyUp, ClipEventKeyDown,
                     ClipEventMouseUp, ClipEventMouseDown, ClipEventMouseMove,
                     ClipEventUnload, ClipEventEnterFrame, ClipEventLoad,
                     ClipEventDragOver, ClipEventRollOut, ClipEventRollOver,
                     ClipEventReleaseOutside, ClipEventRelease, ClipEventPress,
                     ClipEventInitialize, ClipEventData]
    
    swf6_flags = [ClipEventConstruct, ClipEventKeyPress, ClipEventDragOut]
    
    getter = do
      let f cefs cef = getFlag >>= \b -> return $ if b then cef:cefs else cefs
      cefs <- foldM f [] initial_flags
  
      version <- fmap swfVersion getSwfGet
      if (version <= 5)
       then return cefs
       else do
          discardReserved "x :: CLIPEVENTFLAGS" $ getUB 5
          cefs <- foldM f cefs swf6_flags
          discardReserved "x :: CLIPEVENTFLAGS" $ getUB 8
          return cefs

    putter flags = do
      let f cef = putFlag (cef `elem` flags)
      mapM_ f initial_flags
      
      version <- fmap swfVersion getSwfPutM
      when (version > 5) $ do
          putUB 5 reservedDefault
          mapM_ f swf6_flags
          putUB 8 reservedDefault

\end{code}

p52: RemoveObject
\begin{code}
getRemoveObject
  = do removeObject_characterId <- getUI16
       removeObject_depth <- getUI16
       return (RemoveObject{..})
putRemoveObject RemoveObject{..}
  = do putUI16 removeObject_characterId
       putUI16 removeObject_depth
       return ()

\end{code}

p52: RemoveObject2
\begin{code}
getRemoveObject2
  = do removeObject2_depth <- getUI16
       return (RemoveObject2{..})
putRemoveObject2 RemoveObject2{..}
  = do putUI16 removeObject2_depth
       return ()

\end{code}

p52: ShowFrame
\begin{code}
getShowFrame = do return (ShowFrame{..})
putShowFrame ShowFrame{..} = do return ()

\end{code}


Chapter 4: Control Tags
~~~~~~~~~~~~~~~~~~~~~~~

p53: SetBackgroundColor
\begin{code}
getSetBackgroundColor
  = do setBackgroundColor_backgroundColor <- getRGB
       return (SetBackgroundColor{..})
putSetBackgroundColor SetBackgroundColor{..}
  = do putRGB setBackgroundColor_backgroundColor
       return ()

\end{code}

p53: FrameLabel
\begin{code}
getFrameLabel
  = do frameLabel_name <- getSTRING
       frameLabel_namedAnchorFlag <- maybeHasM (fmap not isEmpty) getUI8
       return (FrameLabel{..})
putFrameLabel FrameLabel{..}
  = do putSTRING frameLabel_name
       case frameLabel_namedAnchorFlag of
           Just x -> putUI8 x
           Nothing -> return ()
       return ()

\end{code}

p54: Protect
\begin{code}
getProtect = do return (Protect{..})
putProtect Protect{..} = do return ()

\end{code}

p55: End
\begin{code}
getEnd = do return (End{..})
putEnd End{..} = do return ()

\end{code}

p55: ExportAssets
\begin{code}
getExportAssets
  = do exportAssets_count <- getUI16
       exportAssets_tag1 <- getUI16
       exportAssets_name1 <- getSTRING
       exportAssets_tagN <- getUI16
       exportAssets_nameN <- getSTRING
       return (ExportAssets{..})
putExportAssets ExportAssets{..}
  = do putUI16 exportAssets_count
       putUI16 exportAssets_tag1
       putSTRING exportAssets_name1
       putUI16 exportAssets_tagN
       putSTRING exportAssets_nameN
       return ()

\end{code}

p56: ImportAssets
\begin{code}
getImportAssets
  = do importAssets_uRL <- getSTRING
       importAssets_count <- getUI16
       importAssets_tag1 <- getUI16
       importAssets_name1 <- getSTRING
       importAssets_tagN <- getUI16
       importAssets_nameN <- getSTRING
       return (ImportAssets{..})
putImportAssets ImportAssets{..}
  = do putSTRING importAssets_uRL
       putUI16 importAssets_count
       putUI16 importAssets_tag1
       putSTRING importAssets_name1
       putUI16 importAssets_tagN
       putSTRING importAssets_nameN
       return ()

\end{code}

p57: EnableDebugger
\begin{code}
getEnableDebugger
  = do enableDebugger_password <- getSTRING
       return (EnableDebugger{..})
putEnableDebugger EnableDebugger{..}
  = do putSTRING enableDebugger_password
       return ()

\end{code}

p57: EnableDebugger2
\begin{code}
getEnableDebugger2
  = do discardReserved "_reserved (x :: ?)" getUI16
       enableDebugger2_password <- getSTRING
       return (EnableDebugger2{..})
putEnableDebugger2 EnableDebugger2{..}
  = do let enableDebugger2_reserved = reservedDefault
       putUI16 enableDebugger2_reserved
       putSTRING enableDebugger2_password
       return ()

\end{code}

p58: ScriptLimits
\begin{code}
getScriptLimits
  = do scriptLimits_maxRecursionDepth <- getUI16
       scriptLimits_scriptTimeoutSeconds <- getUI16
       return (ScriptLimits{..})
putScriptLimits ScriptLimits{..}
  = do putUI16 scriptLimits_maxRecursionDepth
       putUI16 scriptLimits_scriptTimeoutSeconds
       return ()

\end{code}

p58: SetTabIndex
\begin{code}
getSetTabIndex
  = do setTabIndex_depth <- getUI16
       setTabIndex_tabIndex <- getUI16
       return (SetTabIndex{..})
putSetTabIndex SetTabIndex{..}
  = do putUI16 setTabIndex_depth
       putUI16 setTabIndex_tabIndex
       return ()

\end{code}

p59: FileAttributes
\begin{code}
getFileAttributes
  = do discardReserved "_reserved (x :: ?)" getFlag
       fileAttributes_useDirectBlit <- getFlag
       fileAttributes_useGPU <- getFlag
       fileAttributes_hasMetadata <- getFlag
       fileAttributes_actionScript3 <- getFlag
       discardReserved "_reserved (x :: ?)" (getUB 2)
       fileAttributes_useNetwork <- getFlag
       discardReserved "_reserved (x :: ?)" (getUB 24)
       return (FileAttributes{..})
putFileAttributes FileAttributes{..}
  = do let fileAttributes_reserved = reservedDefault
       putFlag fileAttributes_reserved
       putFlag fileAttributes_useDirectBlit
       putFlag fileAttributes_useGPU
       putFlag fileAttributes_hasMetadata
       putFlag fileAttributes_actionScript3
       let fileAttributes_reserved = reservedDefault
       if requiredBitsUB fileAttributes_reserved <= 2 then
         putUB 2 fileAttributes_reserved else
         inconsistent "x :: FileAttributes"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB fileAttributes_reserved) ++
                " bits to store the value " ++
                  show fileAttributes_reserved ++
                    ", but only have available " ++ show 2)
       putFlag fileAttributes_useNetwork
       let fileAttributes_reserved = reservedDefault
       if requiredBitsUB fileAttributes_reserved <= 24 then
         putUB 24 fileAttributes_reserved else
         inconsistent "x :: FileAttributes"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB fileAttributes_reserved) ++
                " bits to store the value " ++
                  show fileAttributes_reserved ++
                    ", but only have available " ++ show 24)
       return ()

\end{code}

p60: ImportAssets2
\begin{code}
getImportAssets2
  = do importAssets2_uRL <- getSTRING
       discardReserved "_reserved (x :: ?)" getUI8
       discardReserved "_reserved (x :: ?)" getUI8
       importAssets2_count <- getUI16
       importAssets2_tag1 <- getUI16
       importAssets2_name1 <- getSTRING
       importAssets2_tagN <- getUI16
       importAssets2_nameN <- getSTRING
       return (ImportAssets2{..})
putImportAssets2 ImportAssets2{..}
  = do putSTRING importAssets2_uRL
       let importAssets2_reserved = reservedDefault
       putUI8 importAssets2_reserved
       let importAssets2_reserved = reservedDefault
       putUI8 importAssets2_reserved
       putUI16 importAssets2_count
       putUI16 importAssets2_tag1
       putSTRING importAssets2_name1
       putUI16 importAssets2_tagN
       putSTRING importAssets2_nameN
       return ()

\end{code}

p62: SymbolClass
\begin{code}
getSymbolClass
  = do symbolClass_numSymbols <- getUI16
       symbolClass_tagsNames <- genericReplicateM symbolClass_numSymbols
                                  (liftM2 (,) getUI16 getSTRING)
       return (SymbolClass{..})
putSymbolClass SymbolClass{..}
  = do let symbolClass_numSymbols
             = genericLength symbolClass_tagsNames
       putUI16 symbolClass_numSymbols
       if genericLength symbolClass_tagsNames /= (symbolClass_numSymbols)
         then
         inconsistent "symbolClass_tagsNames (x :: SymbolClass)"
           ("Mismatch with the required length: symbolClass_numSymbols" ++
              show (genericLength symbolClass_tagsNames) ++
                " /= " ++ show symbolClass_numSymbols)
         else
         mapM_
           (\ x ->
              case x of
                  (x1, x2) -> do putUI16 x1
                                 putSTRING x2)
           symbolClass_tagsNames
       return ()

\end{code}

p64: Metadata
\begin{code}
getMetadata
  = do metadata_metadata <- getSTRING
       return (Metadata{..})
putMetadata Metadata{..}
  = do putSTRING metadata_metadata
       return ()

\end{code}

p65: DefineScalingGrid
\begin{code}
getDefineScalingGrid
  = do defineScalingGrid_characterId <- getUI16
       defineScalingGrid_splitter <- getRECT
       return (DefineScalingGrid{..})
putDefineScalingGrid DefineScalingGrid{..}
  = do putUI16 defineScalingGrid_characterId
       putRECT defineScalingGrid_splitter
       return ()

\end{code}

p66: DefineSceneAndFrameLabelData
\begin{code}
getDefineSceneAndFrameLabelData
  = do defineSceneAndFrameLabelData_sceneCount <- getEncodedU32
       defineSceneAndFrameLabelData_offsetNames <- genericReplicateM
                                                     defineSceneAndFrameLabelData_sceneCount
                                                     (liftM2 (,) getEncodedU32 getSTRING)
       defineSceneAndFrameLabelData_frameLabelCount <- getEncodedU32
       defineSceneAndFrameLabelData_frameNumLabels <- genericReplicateM
                                                        defineSceneAndFrameLabelData_frameLabelCount
                                                        (liftM2 (,) getEncodedU32 getSTRING)
       return (DefineSceneAndFrameLabelData{..})
putDefineSceneAndFrameLabelData DefineSceneAndFrameLabelData{..}
  = do let defineSceneAndFrameLabelData_sceneCount
             = genericLength defineSceneAndFrameLabelData_offsetNames
       putEncodedU32 defineSceneAndFrameLabelData_sceneCount
       if
         genericLength defineSceneAndFrameLabelData_offsetNames /=
           (defineSceneAndFrameLabelData_sceneCount)
         then
         inconsistent
           "defineSceneAndFrameLabelData_offsetNames (x :: DefineSceneAndFrameLabelData)"
           ("Mismatch with the required length: defineSceneAndFrameLabelData_sceneCount"
              ++
              show (genericLength defineSceneAndFrameLabelData_offsetNames) ++
                " /= " ++ show defineSceneAndFrameLabelData_sceneCount)
         else
         mapM_
           (\ x ->
              case x of
                  (x1, x2) -> do putEncodedU32 x1
                                 putSTRING x2)
           defineSceneAndFrameLabelData_offsetNames
       let defineSceneAndFrameLabelData_frameLabelCount
             = genericLength defineSceneAndFrameLabelData_frameNumLabels
       putEncodedU32 defineSceneAndFrameLabelData_frameLabelCount
       if
         genericLength defineSceneAndFrameLabelData_frameNumLabels /=
           (defineSceneAndFrameLabelData_frameLabelCount)
         then
         inconsistent
           "defineSceneAndFrameLabelData_frameNumLabels (x :: DefineSceneAndFrameLabelData)"
           ("Mismatch with the required length: defineSceneAndFrameLabelData_frameLabelCount"
              ++
              show (genericLength defineSceneAndFrameLabelData_frameNumLabels) ++
                " /= " ++ show defineSceneAndFrameLabelData_frameLabelCount)
         else
         mapM_
           (\ x ->
              case x of
                  (x1, x2) -> do putEncodedU32 x1
                                 putSTRING x2)
           defineSceneAndFrameLabelData_frameNumLabels
       return ()

\end{code}


Chapter 5: Actions
~~~~~~~~~~~~~~~~~~

p68: DoAction
\begin{code}
getDoAction
  = do doAction_actions <- getACTIONRECORDS
       return (DoAction{..})
putDoAction DoAction{..}
  = do putACTIONRECORDS doAction_actions
       return ()

\end{code}

\begin{code}

type ACTIONRECORDS = [ACTIONRECORD]

getACTIONRECORDS = do
    look <- lookAhead getUI8
    if look == 0
     then getUI8 >> return []
     else do
        actionRecord <- getACTIONRECORD
        fmap (actionRecord:) getACTIONRECORDS

putACTIONRECORDS rs = do
    mapM_ putACTIONRECORD rs
    putUI8 0

\end{code}

p68: ACTIONRECORD
\begin{code}

data ACTIONRECORDHEADER = ACTIONRECORDHEADER { aCTIONRECORDHEADER_actionCode :: UI8, aCTIONRECORDHEADER_actionLength :: Maybe UI16 }
                        deriving (Eq, Show, Typeable, Data)

getACTIONRECORDHEADER = do
    aCTIONRECORDHEADER_actionCode <- getUI8
    aCTIONRECORDHEADER_actionLength <- maybeHas ((aCTIONRECORDHEADER_actionCode .&. 0x80) /= 0) getUI16
    return $ ACTIONRECORDHEADER {..}

putACTIONRECORDHEADER (ACTIONRECORDHEADER {..}) = do
    putUI8 aCTIONRECORDHEADER_actionCode
    when (consistentWith (inconsistent "aCTIONRECORDHEADER_actionLength (x :: ACTIONRECORDHEADER)" "You can have aCTIONRECORDHEADER_actionLength iff the top bit of aCTIONRECORDHEADER_actionCode is set")
                         ((aCTIONRECORDHEADER_actionCode .&. 0x80) /= 0)
                         (isJust aCTIONRECORDHEADER_actionLength)) $ do
        putUI16 (fromJust aCTIONRECORDHEADER_actionLength)

data ACTIONRECORD = UnknownAction { unknownAction_actionCode :: UI8, unknownAction_data :: Maybe ByteString }
                  | ActionPush { actionPush_actionPushLiteral :: ActionPushLiteral }
         |  ActionGotoFrame{actionGotoFrame_frame :: UI16}
         |  ActionGetURL{actionGetURL_urlString :: STRING,
               actionGetURL_targetString :: STRING}
         |  ActionNextFrame{}
         |  ActionPreviousFrame{}
         |  ActionPlay{}
         |  ActionStop{}
         |  ActionToggleQuality{}
         |  ActionStopSounds{}
         |  ActionWaitForFrame{actionWaitForFrame_frame :: UI16,
                     actionWaitForFrame_skipCount :: UI8}
         |  ActionSetTarget{actionSetTarget_targetName :: STRING}
         |  ActionGoToLabel{actionGoToLabel_label :: STRING}
         |  ActionPop{}
         |  ActionAdd{}
         |  ActionSubtract{}
         |  ActionMultiply{}
         |  ActionDivide{}
         |  ActionEquals{}
         |  ActionLess{}
         |  ActionAnd{}
         |  ActionOr{}
         |  ActionNot{}
         |  ActionStringEquals{}
         |  ActionStringLength{}
         |  ActionStringAdd{}
         |  ActionStringExtract{}
         |  ActionStringLess{}
         |  ActionMBStringLength{}
         |  ActionMBStringExtract{}
         |  ActionToInteger{}
         |  ActionCharToAscii{}
         |  ActionAsciiToChar{}
         |  ActionMBCharToAscii{}
         |  ActionMBAsciiToChar{}
         |  ActionJump{actionJump_branchOffset :: SI16}
         |  ActionIf{actionIf_branchOffset :: SI16}
         |  ActionCall{}
         |  ActionGetVariable{}
         |  ActionSetVariable{}
         |  ActionGetURL2{actionGetURL2_sendVarsMethod :: UB,
                actionGetURL2_loadTargetFlag :: Bool,
                actionGetURL2_loadVariablesFlag :: Bool}
         |  ActionGotoFrame2{actionGotoFrame2_playFlag :: Bool,
                   actionGotoFrame2_sceneBias :: Maybe UI16}
         |  ActionSetTarget2{}
         |  ActionGetProperty{}
         |  ActionSetProperty{}
         |  ActionCloneSprite{}
         |  ActionRemoveSprite{}
         |  ActionStartDrag{}
         |  ActionEndDrag{}
         |  ActionWaitForFrame2{actionWaitForFrame2_skipCount :: UI8}
         |  ActionTrace{}
         |  ActionGetTime{}
         |  ActionRandomNumber{}
         |  ActionCallFunction{}
         |  ActionCallMethod{}
         |  ActionConstantPool{actionConstantPool_constantPool :: [STRING]}
         |  ActionDefineFunction{actionDefineFunction_functionName :: STRING,
                       actionDefineFunction_params :: [STRING],
                       actionDefineFunction_codeSize :: UI16}
         |  ActionDefineLocal{}
         |  ActionDefineLocal2{}
         |  ActionDelete{}
         |  ActionDelete2{}
         |  ActionEnumerate{}
         |  ActionEquals2{}
         |  ActionGetMember{}
         |  ActionInitArray{}
         |  ActionInitObject{}
         |  ActionNewMethod{}
         |  ActionNewObject{}
         |  ActionSetMember{}
         |  ActionTargetPath{}
         |  ActionWith{actionWith_size :: UI16}
         |  ActionToNumber{}
         |  ActionToString{}
         |  ActionTypeOf{}
         |  ActionAdd2{}
         |  ActionLess2{}
         |  ActionModulo{}
         |  ActionBitAnd{}
         |  ActionBitLShift{}
         |  ActionBitOr{}
         |  ActionBitRShift{}
         |  ActionBitURShift{}
         |  ActionBitXor{}
         |  ActionDecrement{}
         |  ActionIncrement{}
         |  ActionPushDuplicate{}
         |  ActionReturn{}
         |  ActionStackSwap{}
         |  ActionStoreRegister{actionStoreRegister_registerNumber :: UI8}
         |  ActionInstanceOf{}
         |  ActionEnumerate2{}
         |  ActionStrictEquals{}
         |  ActionGreater{}
         |  ActionStringGreater{}
         |  ActionDefineFunction2{actionDefineFunction2_functionName :: STRING,
                        actionDefineFunction2_registerCount :: UI8,
                        actionDefineFunction2_preloadParentFlag :: Bool,
                        actionDefineFunction2_preloadRootFlag :: Bool,
                        actionDefineFunction2_suppressSuperFlag :: Bool,
                        actionDefineFunction2_preloadSuperFlag :: Bool,
                        actionDefineFunction2_suppressArgumentsFlag :: Bool,
                        actionDefineFunction2_preloadArgumentsFlag :: Bool,
                        actionDefineFunction2_suppressThisFlag :: Bool,
                        actionDefineFunction2_preloadThisFlag :: Bool,
                        actionDefineFunction2_preloadGlobalFlag :: Bool,
                        actionDefineFunction2_parameters :: [REGISTERPARAM],
                        actionDefineFunction2_codeSize :: UI16}
         |  ActionExtends{}
         |  ActionCastOp{}
         |  ActionImplementsOp{}
         |  ActionTry{actionTry_finallyBlockFlag :: Bool,
            actionTry_catchBlockFlag :: Bool,
            actionTry_catchName :: Maybe STRING,
            actionTry_catchRegister :: Maybe UI8, actionTry_tryBody :: [UI8],
            actionTry_catchBody :: [UI8], actionTry_finallyBody :: [UI8]}
         |  ActionThrow{}
            deriving (Eq, Show, Typeable, Data)

getACTIONRECORD = do
    ACTIONRECORDHEADER {..} <- getACTIONRECORDHEADER
    
    let mb_getter = case aCTIONRECORDHEADER_actionCode of
                      0x96 -> Just getActionPush
                      _    -> generatedActionGetters aCTIONRECORDHEADER_actionCode
    
    -- Only some tags (with code >= 0x80) have a payload:
    nestSwfGet (maybe 0 fromIntegral aCTIONRECORDHEADER_actionLength) $ case mb_getter of
        Just getter -> getter
        Nothing -> do
            dat <- getRemainingLazyByteString
            return $ UnknownAction { unknownAction_actionCode = aCTIONRECORDHEADER_actionCode, unknownAction_data = fmap (const dat) aCTIONRECORDHEADER_actionLength }

putACTIONRECORD actionrecord = do
    (len, put) <- nestSwfPut $ case actionrecord of
        UnknownAction {..} -> when (consistentWith (inconsistent "unknownAction_data (x :: Tag)" "You can have unknownAction_data iff the top bit of unknownAction_actionCode is set")
                                                   (isJust unknownAction_data)
                                                   (unknownAction_actionCode >= 0x80)) $
                                putLazyByteString $ fromJust unknownAction_data
        ActionPush {}      -> putActionPush actionrecord
        _                  -> generatedActionPutters actionrecord

    let code = generatedActionTypes actionrecord
    putACTIONRECORDHEADER $ ACTIONRECORDHEADER {
        aCTIONRECORDHEADER_actionCode = code,
        aCTIONRECORDHEADER_actionLength = if code < 0x80 then Nothing else Just len
      }
    put

\end{code}

\begin{code}
generatedActionGetters action
  = case action of
        129 -> Just getActionGotoFrame
        131 -> Just getActionGetURL
        4 -> Just getActionNextFrame
        5 -> Just getActionPreviousFrame
        6 -> Just getActionPlay
        7 -> Just getActionStop
        8 -> Just getActionToggleQuality
        9 -> Just getActionStopSounds
        138 -> Just getActionWaitForFrame
        139 -> Just getActionSetTarget
        140 -> Just getActionGoToLabel
        23 -> Just getActionPop
        10 -> Just getActionAdd
        11 -> Just getActionSubtract
        12 -> Just getActionMultiply
        13 -> Just getActionDivide
        14 -> Just getActionEquals
        15 -> Just getActionLess
        16 -> Just getActionAnd
        17 -> Just getActionOr
        18 -> Just getActionNot
        19 -> Just getActionStringEquals
        20 -> Just getActionStringLength
        33 -> Just getActionStringAdd
        21 -> Just getActionStringExtract
        41 -> Just getActionStringLess
        49 -> Just getActionMBStringLength
        53 -> Just getActionMBStringExtract
        24 -> Just getActionToInteger
        50 -> Just getActionCharToAscii
        51 -> Just getActionAsciiToChar
        54 -> Just getActionMBCharToAscii
        55 -> Just getActionMBAsciiToChar
        153 -> Just getActionJump
        157 -> Just getActionIf
        158 -> Just getActionCall
        28 -> Just getActionGetVariable
        29 -> Just getActionSetVariable
        154 -> Just getActionGetURL2
        159 -> Just getActionGotoFrame2
        32 -> Just getActionSetTarget2
        34 -> Just getActionGetProperty
        35 -> Just getActionSetProperty
        36 -> Just getActionCloneSprite
        37 -> Just getActionRemoveSprite
        39 -> Just getActionStartDrag
        40 -> Just getActionEndDrag
        141 -> Just getActionWaitForFrame2
        38 -> Just getActionTrace
        52 -> Just getActionGetTime
        48 -> Just getActionRandomNumber
        61 -> Just getActionCallFunction
        82 -> Just getActionCallMethod
        136 -> Just getActionConstantPool
        155 -> Just getActionDefineFunction
        60 -> Just getActionDefineLocal
        65 -> Just getActionDefineLocal2
        58 -> Just getActionDelete
        59 -> Just getActionDelete2
        70 -> Just getActionEnumerate
        73 -> Just getActionEquals2
        78 -> Just getActionGetMember
        66 -> Just getActionInitArray
        67 -> Just getActionInitObject
        83 -> Just getActionNewMethod
        64 -> Just getActionNewObject
        79 -> Just getActionSetMember
        69 -> Just getActionTargetPath
        148 -> Just getActionWith
        74 -> Just getActionToNumber
        75 -> Just getActionToString
        68 -> Just getActionTypeOf
        71 -> Just getActionAdd2
        72 -> Just getActionLess2
        63 -> Just getActionModulo
        96 -> Just getActionBitAnd
        99 -> Just getActionBitLShift
        97 -> Just getActionBitOr
        100 -> Just getActionBitRShift
        101 -> Just getActionBitURShift
        98 -> Just getActionBitXor
        81 -> Just getActionDecrement
        80 -> Just getActionIncrement
        76 -> Just getActionPushDuplicate
        62 -> Just getActionReturn
        77 -> Just getActionStackSwap
        135 -> Just getActionStoreRegister
        84 -> Just getActionInstanceOf
        85 -> Just getActionEnumerate2
        102 -> Just getActionStrictEquals
        103 -> Just getActionGreater
        104 -> Just getActionStringGreater
        142 -> Just getActionDefineFunction2
        105 -> Just getActionExtends
        43 -> Just getActionCastOp
        44 -> Just getActionImplementsOp
        143 -> Just getActionTry
        42 -> Just getActionThrow
        _ -> Nothing
generatedActionPutters action
  = case action of
        ActionGotoFrame{..} -> putActionGotoFrame action
        ActionGetURL{..} -> putActionGetURL action
        ActionNextFrame{..} -> putActionNextFrame action
        ActionPreviousFrame{..} -> putActionPreviousFrame action
        ActionPlay{..} -> putActionPlay action
        ActionStop{..} -> putActionStop action
        ActionToggleQuality{..} -> putActionToggleQuality action
        ActionStopSounds{..} -> putActionStopSounds action
        ActionWaitForFrame{..} -> putActionWaitForFrame action
        ActionSetTarget{..} -> putActionSetTarget action
        ActionGoToLabel{..} -> putActionGoToLabel action
        ActionPop{..} -> putActionPop action
        ActionAdd{..} -> putActionAdd action
        ActionSubtract{..} -> putActionSubtract action
        ActionMultiply{..} -> putActionMultiply action
        ActionDivide{..} -> putActionDivide action
        ActionEquals{..} -> putActionEquals action
        ActionLess{..} -> putActionLess action
        ActionAnd{..} -> putActionAnd action
        ActionOr{..} -> putActionOr action
        ActionNot{..} -> putActionNot action
        ActionStringEquals{..} -> putActionStringEquals action
        ActionStringLength{..} -> putActionStringLength action
        ActionStringAdd{..} -> putActionStringAdd action
        ActionStringExtract{..} -> putActionStringExtract action
        ActionStringLess{..} -> putActionStringLess action
        ActionMBStringLength{..} -> putActionMBStringLength action
        ActionMBStringExtract{..} -> putActionMBStringExtract action
        ActionToInteger{..} -> putActionToInteger action
        ActionCharToAscii{..} -> putActionCharToAscii action
        ActionAsciiToChar{..} -> putActionAsciiToChar action
        ActionMBCharToAscii{..} -> putActionMBCharToAscii action
        ActionMBAsciiToChar{..} -> putActionMBAsciiToChar action
        ActionJump{..} -> putActionJump action
        ActionIf{..} -> putActionIf action
        ActionCall{..} -> putActionCall action
        ActionGetVariable{..} -> putActionGetVariable action
        ActionSetVariable{..} -> putActionSetVariable action
        ActionGetURL2{..} -> putActionGetURL2 action
        ActionGotoFrame2{..} -> putActionGotoFrame2 action
        ActionSetTarget2{..} -> putActionSetTarget2 action
        ActionGetProperty{..} -> putActionGetProperty action
        ActionSetProperty{..} -> putActionSetProperty action
        ActionCloneSprite{..} -> putActionCloneSprite action
        ActionRemoveSprite{..} -> putActionRemoveSprite action
        ActionStartDrag{..} -> putActionStartDrag action
        ActionEndDrag{..} -> putActionEndDrag action
        ActionWaitForFrame2{..} -> putActionWaitForFrame2 action
        ActionTrace{..} -> putActionTrace action
        ActionGetTime{..} -> putActionGetTime action
        ActionRandomNumber{..} -> putActionRandomNumber action
        ActionCallFunction{..} -> putActionCallFunction action
        ActionCallMethod{..} -> putActionCallMethod action
        ActionConstantPool{..} -> putActionConstantPool action
        ActionDefineFunction{..} -> putActionDefineFunction action
        ActionDefineLocal{..} -> putActionDefineLocal action
        ActionDefineLocal2{..} -> putActionDefineLocal2 action
        ActionDelete{..} -> putActionDelete action
        ActionDelete2{..} -> putActionDelete2 action
        ActionEnumerate{..} -> putActionEnumerate action
        ActionEquals2{..} -> putActionEquals2 action
        ActionGetMember{..} -> putActionGetMember action
        ActionInitArray{..} -> putActionInitArray action
        ActionInitObject{..} -> putActionInitObject action
        ActionNewMethod{..} -> putActionNewMethod action
        ActionNewObject{..} -> putActionNewObject action
        ActionSetMember{..} -> putActionSetMember action
        ActionTargetPath{..} -> putActionTargetPath action
        ActionWith{..} -> putActionWith action
        ActionToNumber{..} -> putActionToNumber action
        ActionToString{..} -> putActionToString action
        ActionTypeOf{..} -> putActionTypeOf action
        ActionAdd2{..} -> putActionAdd2 action
        ActionLess2{..} -> putActionLess2 action
        ActionModulo{..} -> putActionModulo action
        ActionBitAnd{..} -> putActionBitAnd action
        ActionBitLShift{..} -> putActionBitLShift action
        ActionBitOr{..} -> putActionBitOr action
        ActionBitRShift{..} -> putActionBitRShift action
        ActionBitURShift{..} -> putActionBitURShift action
        ActionBitXor{..} -> putActionBitXor action
        ActionDecrement{..} -> putActionDecrement action
        ActionIncrement{..} -> putActionIncrement action
        ActionPushDuplicate{..} -> putActionPushDuplicate action
        ActionReturn{..} -> putActionReturn action
        ActionStackSwap{..} -> putActionStackSwap action
        ActionStoreRegister{..} -> putActionStoreRegister action
        ActionInstanceOf{..} -> putActionInstanceOf action
        ActionEnumerate2{..} -> putActionEnumerate2 action
        ActionStrictEquals{..} -> putActionStrictEquals action
        ActionGreater{..} -> putActionGreater action
        ActionStringGreater{..} -> putActionStringGreater action
        ActionDefineFunction2{..} -> putActionDefineFunction2 action
        ActionExtends{..} -> putActionExtends action
        ActionCastOp{..} -> putActionCastOp action
        ActionImplementsOp{..} -> putActionImplementsOp action
        ActionTry{..} -> putActionTry action
        ActionThrow{..} -> putActionThrow action
generatedActionTypes action
  = case action of
        ActionGotoFrame{..} -> 129
        ActionGetURL{..} -> 131
        ActionNextFrame{..} -> 4
        ActionPreviousFrame{..} -> 5
        ActionPlay{..} -> 6
        ActionStop{..} -> 7
        ActionToggleQuality{..} -> 8
        ActionStopSounds{..} -> 9
        ActionWaitForFrame{..} -> 138
        ActionSetTarget{..} -> 139
        ActionGoToLabel{..} -> 140
        ActionPop{..} -> 23
        ActionAdd{..} -> 10
        ActionSubtract{..} -> 11
        ActionMultiply{..} -> 12
        ActionDivide{..} -> 13
        ActionEquals{..} -> 14
        ActionLess{..} -> 15
        ActionAnd{..} -> 16
        ActionOr{..} -> 17
        ActionNot{..} -> 18
        ActionStringEquals{..} -> 19
        ActionStringLength{..} -> 20
        ActionStringAdd{..} -> 33
        ActionStringExtract{..} -> 21
        ActionStringLess{..} -> 41
        ActionMBStringLength{..} -> 49
        ActionMBStringExtract{..} -> 53
        ActionToInteger{..} -> 24
        ActionCharToAscii{..} -> 50
        ActionAsciiToChar{..} -> 51
        ActionMBCharToAscii{..} -> 54
        ActionMBAsciiToChar{..} -> 55
        ActionJump{..} -> 153
        ActionIf{..} -> 157
        ActionCall{..} -> 158
        ActionGetVariable{..} -> 28
        ActionSetVariable{..} -> 29
        ActionGetURL2{..} -> 154
        ActionGotoFrame2{..} -> 159
        ActionSetTarget2{..} -> 32
        ActionGetProperty{..} -> 34
        ActionSetProperty{..} -> 35
        ActionCloneSprite{..} -> 36
        ActionRemoveSprite{..} -> 37
        ActionStartDrag{..} -> 39
        ActionEndDrag{..} -> 40
        ActionWaitForFrame2{..} -> 141
        ActionTrace{..} -> 38
        ActionGetTime{..} -> 52
        ActionRandomNumber{..} -> 48
        ActionCallFunction{..} -> 61
        ActionCallMethod{..} -> 82
        ActionConstantPool{..} -> 136
        ActionDefineFunction{..} -> 155
        ActionDefineLocal{..} -> 60
        ActionDefineLocal2{..} -> 65
        ActionDelete{..} -> 58
        ActionDelete2{..} -> 59
        ActionEnumerate{..} -> 70
        ActionEquals2{..} -> 73
        ActionGetMember{..} -> 78
        ActionInitArray{..} -> 66
        ActionInitObject{..} -> 67
        ActionNewMethod{..} -> 83
        ActionNewObject{..} -> 64
        ActionSetMember{..} -> 79
        ActionTargetPath{..} -> 69
        ActionWith{..} -> 148
        ActionToNumber{..} -> 74
        ActionToString{..} -> 75
        ActionTypeOf{..} -> 68
        ActionAdd2{..} -> 71
        ActionLess2{..} -> 72
        ActionModulo{..} -> 63
        ActionBitAnd{..} -> 96
        ActionBitLShift{..} -> 99
        ActionBitOr{..} -> 97
        ActionBitRShift{..} -> 100
        ActionBitURShift{..} -> 101
        ActionBitXor{..} -> 98
        ActionDecrement{..} -> 81
        ActionIncrement{..} -> 80
        ActionPushDuplicate{..} -> 76
        ActionReturn{..} -> 62
        ActionStackSwap{..} -> 77
        ActionStoreRegister{..} -> 135
        ActionInstanceOf{..} -> 84
        ActionEnumerate2{..} -> 85
        ActionStrictEquals{..} -> 102
        ActionGreater{..} -> 103
        ActionStringGreater{..} -> 104
        ActionDefineFunction2{..} -> 142
        ActionExtends{..} -> 105
        ActionCastOp{..} -> 43
        ActionImplementsOp{..} -> 44
        ActionTry{..} -> 143
        ActionThrow{..} -> 42

\end{code}

p69: ActionGotoFrame
\begin{code}
getActionGotoFrame
  = do actionGotoFrame_frame <- getUI16
       return (ActionGotoFrame{..})
putActionGotoFrame ActionGotoFrame{..}
  = do putUI16 actionGotoFrame_frame
       return ()

\end{code}

p69: ActionGetURL
\begin{code}
getActionGetURL
  = do actionGetURL_urlString <- getSTRING
       actionGetURL_targetString <- getSTRING
       return (ActionGetURL{..})
putActionGetURL ActionGetURL{..}
  = do putSTRING actionGetURL_urlString
       putSTRING actionGetURL_targetString
       return ()

\end{code}

p69: ActionNextFrame
\begin{code}
getActionNextFrame = do return (ActionNextFrame{..})
putActionNextFrame ActionNextFrame{..} = do return ()

\end{code}

p70: ActionPreviousFrame
\begin{code}
getActionPreviousFrame = do return (ActionPreviousFrame{..})
putActionPreviousFrame ActionPreviousFrame{..} = do return ()

\end{code}

p70: ActionPlay
\begin{code}
getActionPlay = do return (ActionPlay{..})
putActionPlay ActionPlay{..} = do return ()

\end{code}

p70: ActionStop
\begin{code}
getActionStop = do return (ActionStop{..})
putActionStop ActionStop{..} = do return ()

\end{code}

p70: ActionToggleQuality
\begin{code}
getActionToggleQuality = do return (ActionToggleQuality{..})
putActionToggleQuality ActionToggleQuality{..} = do return ()

\end{code}

p70: ActionStopSounds
\begin{code}
getActionStopSounds = do return (ActionStopSounds{..})
putActionStopSounds ActionStopSounds{..} = do return ()

\end{code}

p71: ActionWaitForFrame
\begin{code}
getActionWaitForFrame
  = do actionWaitForFrame_frame <- getUI16
       actionWaitForFrame_skipCount <- getUI8
       return (ActionWaitForFrame{..})
putActionWaitForFrame ActionWaitForFrame{..}
  = do putUI16 actionWaitForFrame_frame
       putUI8 actionWaitForFrame_skipCount
       return ()

\end{code}

p71: ActionSetTarget
\begin{code}
getActionSetTarget
  = do actionSetTarget_targetName <- getSTRING
       return (ActionSetTarget{..})
putActionSetTarget ActionSetTarget{..}
  = do putSTRING actionSetTarget_targetName
       return ()

\end{code}

p71: ActionGoToLabel
\begin{code}
getActionGoToLabel
  = do actionGoToLabel_label <- getSTRING
       return (ActionGoToLabel{..})
putActionGoToLabel ActionGoToLabel{..}
  = do putSTRING actionGoToLabel_label
       return ()

\end{code}

p74: ActionPush
\begin{code}

data ActionPushLiteral
  = ActionPushString STRING
  | ActionPushFloat FLOAT
  | ActionPushRegisterNumber UI8
  | ActionPushBoolean UI8
  | ActionPushDouble DOUBLE
  | ActionPushInteger UI32
  | ActionPushConstant8 UI8
  | ActionPushConstant16 UI16
  deriving (Eq, Show, Typeable, Data)

getActionPush = do
    typ <- getUI8
    fmap ActionPush $ case typ of
        0 -> fmap ActionPushString getSTRING
        1 -> fmap ActionPushFloat getFLOAT
        4 -> fmap ActionPushRegisterNumber getUI8
        5 -> fmap ActionPushBoolean getUI8
        6 -> fmap ActionPushDouble getDOUBLE
        7 -> fmap ActionPushInteger getUI32
        8 -> fmap ActionPushConstant8 getUI8
        9 -> fmap ActionPushConstant16 getUI16

putActionPush (ActionPush lit) = case lit of
    ActionPushString x         -> putUI8 0 >> putSTRING x
    ActionPushFloat x          -> putUI8 1 >> putFLOAT x
    ActionPushRegisterNumber x -> putUI8 4 >> putUI8 x
    ActionPushBoolean x        -> putUI8 5 >> putUI8 x
    ActionPushDouble x         -> putUI8 6 >> putDOUBLE x
    ActionPushInteger x        -> putUI8 7 >> putUI32 x
    ActionPushConstant8 x      -> putUI8 8 >> putUI8 x
    ActionPushConstant16 x     -> putUI8 9 >> putUI16 x

\end{code}

p75: ActionPop
\begin{code}
getActionPop = do return (ActionPop{..})
putActionPop ActionPop{..} = do return ()

\end{code}

p76: ActionAdd
\begin{code}
getActionAdd = do return (ActionAdd{..})
putActionAdd ActionAdd{..} = do return ()

\end{code}

p76: ActionSubtract
\begin{code}
getActionSubtract = do return (ActionSubtract{..})
putActionSubtract ActionSubtract{..} = do return ()

\end{code}

p76: ActionMultiply
\begin{code}
getActionMultiply = do return (ActionMultiply{..})
putActionMultiply ActionMultiply{..} = do return ()

\end{code}

p77: ActionDivide
\begin{code}
getActionDivide = do return (ActionDivide{..})
putActionDivide ActionDivide{..} = do return ()

\end{code}

p77: ActionEquals
\begin{code}
getActionEquals = do return (ActionEquals{..})
putActionEquals ActionEquals{..} = do return ()

\end{code}

p78: ActionLess
\begin{code}
getActionLess = do return (ActionLess{..})
putActionLess ActionLess{..} = do return ()

\end{code}

p78: ActionAnd
\begin{code}
getActionAnd = do return (ActionAnd{..})
putActionAnd ActionAnd{..} = do return ()

\end{code}

p79: ActionOr
\begin{code}
getActionOr = do return (ActionOr{..})
putActionOr ActionOr{..} = do return ()

\end{code}

p79: ActionNot
\begin{code}
getActionNot = do return (ActionNot{..})
putActionNot ActionNot{..} = do return ()

\end{code}

p80: ActionStringEquals
\begin{code}
getActionStringEquals = do return (ActionStringEquals{..})
putActionStringEquals ActionStringEquals{..} = do return ()

\end{code}

p80: ActionStringLength
\begin{code}
getActionStringLength = do return (ActionStringLength{..})
putActionStringLength ActionStringLength{..} = do return ()

\end{code}

p80: ActionStringAdd
\begin{code}
getActionStringAdd = do return (ActionStringAdd{..})
putActionStringAdd ActionStringAdd{..} = do return ()

\end{code}

p81: ActionStringExtract
\begin{code}
getActionStringExtract = do return (ActionStringExtract{..})
putActionStringExtract ActionStringExtract{..} = do return ()

\end{code}

p81: ActionStringLess
\begin{code}
getActionStringLess = do return (ActionStringLess{..})
putActionStringLess ActionStringLess{..} = do return ()

\end{code}

p81: ActionMBStringLength
\begin{code}
getActionMBStringLength = do return (ActionMBStringLength{..})
putActionMBStringLength ActionMBStringLength{..} = do return ()

\end{code}

p82: ActionMBStringExtract
\begin{code}
getActionMBStringExtract = do return (ActionMBStringExtract{..})
putActionMBStringExtract ActionMBStringExtract{..} = do return ()

\end{code}

p82: ActionToInteger
\begin{code}
getActionToInteger = do return (ActionToInteger{..})
putActionToInteger ActionToInteger{..} = do return ()

\end{code}

p83: ActionCharToAscii
\begin{code}
getActionCharToAscii = do return (ActionCharToAscii{..})
putActionCharToAscii ActionCharToAscii{..} = do return ()

\end{code}

p83: ActionAsciiToChar
\begin{code}
getActionAsciiToChar = do return (ActionAsciiToChar{..})
putActionAsciiToChar ActionAsciiToChar{..} = do return ()

\end{code}

p83: ActionMBCharToAscii
\begin{code}
getActionMBCharToAscii = do return (ActionMBCharToAscii{..})
putActionMBCharToAscii ActionMBCharToAscii{..} = do return ()

\end{code}

p84: ActionMBAsciiToChar
\begin{code}
getActionMBAsciiToChar = do return (ActionMBAsciiToChar{..})
putActionMBAsciiToChar ActionMBAsciiToChar{..} = do return ()

\end{code}

p84: ActionJump
\begin{code}
getActionJump
  = do actionJump_branchOffset <- getSI16
       return (ActionJump{..})
putActionJump ActionJump{..}
  = do putSI16 actionJump_branchOffset
       return ()

\end{code}

p84: ActionIf
\begin{code}
getActionIf
  = do actionIf_branchOffset <- getSI16
       return (ActionIf{..})
putActionIf ActionIf{..}
  = do putSI16 actionIf_branchOffset
       return ()

\end{code}

p85: ActionCall
\begin{code}
getActionCall = do return (ActionCall{..})
putActionCall ActionCall{..} = do return ()

\end{code}

p86: ActionGetVariable
\begin{code}
getActionGetVariable = do return (ActionGetVariable{..})
putActionGetVariable ActionGetVariable{..} = do return ()

\end{code}

p86: ActionSetVariable
\begin{code}
getActionSetVariable = do return (ActionSetVariable{..})
putActionSetVariable ActionSetVariable{..} = do return ()

\end{code}

p87: ActionGetURL2
\begin{code}
getActionGetURL2
  = do actionGetURL2_sendVarsMethod <- getUB 2
       discardReserved "_reserved (x :: ?)" (getUB 4)
       actionGetURL2_loadTargetFlag <- getFlag
       actionGetURL2_loadVariablesFlag <- getFlag
       return (ActionGetURL2{..})
putActionGetURL2 ActionGetURL2{..}
  = do if requiredBitsUB actionGetURL2_sendVarsMethod <= 2 then
         putUB 2 actionGetURL2_sendVarsMethod else
         inconsistent "actionGetURL2_sendVarsMethod (x :: ActionGetURL2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB actionGetURL2_sendVarsMethod) ++
                " bits to store the value " ++
                  show actionGetURL2_sendVarsMethod ++
                    ", but only have available " ++ show 2)
       let actionGetURL2_reserved = reservedDefault
       if requiredBitsUB actionGetURL2_reserved <= 4 then
         putUB 4 actionGetURL2_reserved else
         inconsistent "x :: ActionGetURL2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB actionGetURL2_reserved) ++
                " bits to store the value " ++
                  show actionGetURL2_reserved ++
                    ", but only have available " ++ show 4)
       putFlag actionGetURL2_loadTargetFlag
       putFlag actionGetURL2_loadVariablesFlag
       return ()

\end{code}

p88: ActionGotoFrame2
\begin{code}
getActionGotoFrame2
  = do discardReserved "_reserved (x :: ?)" (getUB 6)
       actionGotoFrame2_sceneBiasFlag <- getFlag
       actionGotoFrame2_playFlag <- getFlag
       actionGotoFrame2_sceneBias <- maybeHas
                                       actionGotoFrame2_sceneBiasFlag
                                       getUI16
       return (ActionGotoFrame2{..})
putActionGotoFrame2 ActionGotoFrame2{..}
  = do let actionGotoFrame2_reserved = reservedDefault
       if requiredBitsUB actionGotoFrame2_reserved <= 6 then
         putUB 6 actionGotoFrame2_reserved else
         inconsistent "x :: ActionGotoFrame2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB actionGotoFrame2_reserved) ++
                " bits to store the value " ++
                  show actionGotoFrame2_reserved ++
                    ", but only have available " ++ show 6)
       let actionGotoFrame2_sceneBiasFlag
             = isJust actionGotoFrame2_sceneBias
       putFlag actionGotoFrame2_sceneBiasFlag
       putFlag actionGotoFrame2_playFlag
       case actionGotoFrame2_sceneBias of
           Just x | actionGotoFrame2_sceneBiasFlag -> putUI16 x
                  | otherwise ->
                    inconsistent "actionGotoFrame2_sceneBias (x :: ActionGotoFrame2)"
                      "Should have a Just iff actionGotoFrame2_sceneBiasFlag is True"
           Nothing | actionGotoFrame2_sceneBiasFlag ->
                     inconsistent "actionGotoFrame2_sceneBias (x :: ActionGotoFrame2)"
                       "Should have a Nothing iff actionGotoFrame2_sceneBiasFlag is False"
                   | otherwise -> return ()
       return ()

\end{code}

p89: ActionSetTarget2
\begin{code}
getActionSetTarget2 = do return (ActionSetTarget2{..})
putActionSetTarget2 ActionSetTarget2{..} = do return ()

\end{code}

p89: ActionGetProperty
\begin{code}
getActionGetProperty = do return (ActionGetProperty{..})
putActionGetProperty ActionGetProperty{..} = do return ()

\end{code}

p90: ActionSetProperty
\begin{code}
getActionSetProperty = do return (ActionSetProperty{..})
putActionSetProperty ActionSetProperty{..} = do return ()

\end{code}

p90: ActionCloneSprite
\begin{code}
getActionCloneSprite = do return (ActionCloneSprite{..})
putActionCloneSprite ActionCloneSprite{..} = do return ()

\end{code}

p91: ActionRemoveSprite
\begin{code}
getActionRemoveSprite = do return (ActionRemoveSprite{..})
putActionRemoveSprite ActionRemoveSprite{..} = do return ()

\end{code}

p91: ActionStartDrag
\begin{code}
getActionStartDrag = do return (ActionStartDrag{..})
putActionStartDrag ActionStartDrag{..} = do return ()

\end{code}

p92: ActionEndDrag
\begin{code}
getActionEndDrag = do return (ActionEndDrag{..})
putActionEndDrag ActionEndDrag{..} = do return ()

\end{code}

p92: ActionWaitForFrame2
\begin{code}
getActionWaitForFrame2
  = do actionWaitForFrame2_skipCount <- getUI8
       return (ActionWaitForFrame2{..})
putActionWaitForFrame2 ActionWaitForFrame2{..}
  = do putUI8 actionWaitForFrame2_skipCount
       return ()

\end{code}

p92: ActionTrace
\begin{code}
getActionTrace = do return (ActionTrace{..})
putActionTrace ActionTrace{..} = do return ()

\end{code}

p93: ActionGetTime
\begin{code}
getActionGetTime = do return (ActionGetTime{..})
putActionGetTime ActionGetTime{..} = do return ()

\end{code}

p93: ActionRandomNumber
\begin{code}
getActionRandomNumber = do return (ActionRandomNumber{..})
putActionRandomNumber ActionRandomNumber{..} = do return ()

\end{code}

p95: ActionCallFunction
\begin{code}
getActionCallFunction = do return (ActionCallFunction{..})
putActionCallFunction ActionCallFunction{..} = do return ()

\end{code}

p95: ActionCallMethod
\begin{code}
getActionCallMethod = do return (ActionCallMethod{..})
putActionCallMethod ActionCallMethod{..} = do return ()

\end{code}

p96: ActionConstantPool
\begin{code}
getActionConstantPool
  = do actionConstantPool_count <- getUI16
       actionConstantPool_constantPool <- genericReplicateM
                                            actionConstantPool_count
                                            getSTRING
       return (ActionConstantPool{..})
putActionConstantPool ActionConstantPool{..}
  = do let actionConstantPool_count
             = genericLength actionConstantPool_constantPool
       putUI16 actionConstantPool_count
       if
         genericLength actionConstantPool_constantPool /=
           (actionConstantPool_count)
         then
         inconsistent
           "actionConstantPool_constantPool (x :: ActionConstantPool)"
           ("Mismatch with the required length: actionConstantPool_count" ++
              show (genericLength actionConstantPool_constantPool) ++
                " /= " ++ show actionConstantPool_count)
         else mapM_ (\ x -> putSTRING x) actionConstantPool_constantPool
       return ()

\end{code}

p97: ActionDefineFunction
\begin{code}
getActionDefineFunction
  = do actionDefineFunction_functionName <- getSTRING
       actionDefineFunction_numParams <- getUI16
       actionDefineFunction_params <- genericReplicateM
                                        actionDefineFunction_numParams
                                        getSTRING
       actionDefineFunction_codeSize <- getUI16
       return (ActionDefineFunction{..})
putActionDefineFunction ActionDefineFunction{..}
  = do putSTRING actionDefineFunction_functionName
       let actionDefineFunction_numParams
             = genericLength actionDefineFunction_params
       putUI16 actionDefineFunction_numParams
       if
         genericLength actionDefineFunction_params /=
           (actionDefineFunction_numParams)
         then
         inconsistent
           "actionDefineFunction_params (x :: ActionDefineFunction)"
           ("Mismatch with the required length: actionDefineFunction_numParams"
              ++
              show (genericLength actionDefineFunction_params) ++
                " /= " ++ show actionDefineFunction_numParams)
         else mapM_ (\ x -> putSTRING x) actionDefineFunction_params
       putUI16 actionDefineFunction_codeSize
       return ()

\end{code}

p98: ActionDefineLocal
\begin{code}
getActionDefineLocal = do return (ActionDefineLocal{..})
putActionDefineLocal ActionDefineLocal{..} = do return ()

\end{code}

p98: ActionDefineLocal2
\begin{code}
getActionDefineLocal2 = do return (ActionDefineLocal2{..})
putActionDefineLocal2 ActionDefineLocal2{..} = do return ()

\end{code}

p98: ActionDelete
\begin{code}
getActionDelete = do return (ActionDelete{..})
putActionDelete ActionDelete{..} = do return ()

\end{code}

p99: ActionDelete2
\begin{code}
getActionDelete2 = do return (ActionDelete2{..})
putActionDelete2 ActionDelete2{..} = do return ()

\end{code}

p99: ActionEnumerate
\begin{code}
getActionEnumerate = do return (ActionEnumerate{..})
putActionEnumerate ActionEnumerate{..} = do return ()

\end{code}

p99: ActionEquals2
\begin{code}
getActionEquals2 = do return (ActionEquals2{..})
putActionEquals2 ActionEquals2{..} = do return ()

\end{code}

p100: ActionGetMember
\begin{code}
getActionGetMember = do return (ActionGetMember{..})
putActionGetMember ActionGetMember{..} = do return ()

\end{code}

p101: ActionInitArray
\begin{code}
getActionInitArray = do return (ActionInitArray{..})
putActionInitArray ActionInitArray{..} = do return ()

\end{code}

p101: ActionInitObject
\begin{code}
getActionInitObject = do return (ActionInitObject{..})
putActionInitObject ActionInitObject{..} = do return ()

\end{code}

p102: ActionNewMethod
\begin{code}
getActionNewMethod = do return (ActionNewMethod{..})
putActionNewMethod ActionNewMethod{..} = do return ()

\end{code}

p103: ActionNewObject
\begin{code}
getActionNewObject = do return (ActionNewObject{..})
putActionNewObject ActionNewObject{..} = do return ()

\end{code}

p103: ActionSetMember
\begin{code}
getActionSetMember = do return (ActionSetMember{..})
putActionSetMember ActionSetMember{..} = do return ()

\end{code}

p104: ActionTargetPath
\begin{code}
getActionTargetPath = do return (ActionTargetPath{..})
putActionTargetPath ActionTargetPath{..} = do return ()

\end{code}

p104: ActionWith
\begin{code}
getActionWith
  = do actionWith_size <- getUI16
       return (ActionWith{..})
putActionWith ActionWith{..}
  = do putUI16 actionWith_size
       return ()

\end{code}

p105: ActionToNumber
\begin{code}
getActionToNumber = do return (ActionToNumber{..})
putActionToNumber ActionToNumber{..} = do return ()

\end{code}

p105: ActionToString
\begin{code}
getActionToString = do return (ActionToString{..})
putActionToString ActionToString{..} = do return ()

\end{code}

p106: ActionTypeOf
\begin{code}
getActionTypeOf = do return (ActionTypeOf{..})
putActionTypeOf ActionTypeOf{..} = do return ()

\end{code}

p106: ActionAdd2
\begin{code}
getActionAdd2 = do return (ActionAdd2{..})
putActionAdd2 ActionAdd2{..} = do return ()

\end{code}

p107: ActionLess2
\begin{code}
getActionLess2 = do return (ActionLess2{..})
putActionLess2 ActionLess2{..} = do return ()

\end{code}

p107: ActionModulo
\begin{code}
getActionModulo = do return (ActionModulo{..})
putActionModulo ActionModulo{..} = do return ()

\end{code}

p107: ActionBitAnd
\begin{code}
getActionBitAnd = do return (ActionBitAnd{..})
putActionBitAnd ActionBitAnd{..} = do return ()

\end{code}

p108: ActionBitLShift
\begin{code}
getActionBitLShift = do return (ActionBitLShift{..})
putActionBitLShift ActionBitLShift{..} = do return ()

\end{code}

p108: ActionBitOr
\begin{code}
getActionBitOr = do return (ActionBitOr{..})
putActionBitOr ActionBitOr{..} = do return ()

\end{code}

p109: ActionBitRShift
\begin{code}
getActionBitRShift = do return (ActionBitRShift{..})
putActionBitRShift ActionBitRShift{..} = do return ()

\end{code}

p109: ActionBitURShift
\begin{code}
getActionBitURShift = do return (ActionBitURShift{..})
putActionBitURShift ActionBitURShift{..} = do return ()

\end{code}

p110: ActionBitXor
\begin{code}
getActionBitXor = do return (ActionBitXor{..})
putActionBitXor ActionBitXor{..} = do return ()

\end{code}

p110: ActionDecrement
\begin{code}
getActionDecrement = do return (ActionDecrement{..})
putActionDecrement ActionDecrement{..} = do return ()

\end{code}

p110: ActionIncrement
\begin{code}
getActionIncrement = do return (ActionIncrement{..})
putActionIncrement ActionIncrement{..} = do return ()

\end{code}

p111: ActionPushDuplicate
\begin{code}
getActionPushDuplicate = do return (ActionPushDuplicate{..})
putActionPushDuplicate ActionPushDuplicate{..} = do return ()

\end{code}

p111: ActionReturn
\begin{code}
getActionReturn = do return (ActionReturn{..})
putActionReturn ActionReturn{..} = do return ()

\end{code}

p111: ActionStackSwap
\begin{code}
getActionStackSwap = do return (ActionStackSwap{..})
putActionStackSwap ActionStackSwap{..} = do return ()

\end{code}

p111: ActionStoreRegister
\begin{code}
getActionStoreRegister
  = do actionStoreRegister_registerNumber <- getUI8
       return (ActionStoreRegister{..})
putActionStoreRegister ActionStoreRegister{..}
  = do putUI8 actionStoreRegister_registerNumber
       return ()

\end{code}

p112: DoInitAction
\begin{code}
getDoInitAction
  = do doInitAction_spriteID <- getUI16
       doInitAction_actions <- getACTIONRECORDS
       return (DoInitAction{..})
putDoInitAction DoInitAction{..}
  = do putUI16 doInitAction_spriteID
       putACTIONRECORDS doInitAction_actions
       return ()

\end{code}

p113: ActionInstanceOf
\begin{code}
getActionInstanceOf = do return (ActionInstanceOf{..})
putActionInstanceOf ActionInstanceOf{..} = do return ()

\end{code}

p113: ActionEnumerate2
\begin{code}
getActionEnumerate2 = do return (ActionEnumerate2{..})
putActionEnumerate2 ActionEnumerate2{..} = do return ()

\end{code}

p114: ActionStrictEquals
\begin{code}
getActionStrictEquals = do return (ActionStrictEquals{..})
putActionStrictEquals ActionStrictEquals{..} = do return ()

\end{code}

p114: ActionGreater
\begin{code}
getActionGreater = do return (ActionGreater{..})
putActionGreater ActionGreater{..} = do return ()

\end{code}

p115: ActionStringGreater
\begin{code}
getActionStringGreater = do return (ActionStringGreater{..})
putActionStringGreater ActionStringGreater{..} = do return ()

\end{code}

p116: ActionDefineFunction2
\begin{code}
getActionDefineFunction2
  = do actionDefineFunction2_functionName <- getSTRING
       actionDefineFunction2_numParams <- getUI16
       actionDefineFunction2_registerCount <- getUI8
       actionDefineFunction2_preloadParentFlag <- getFlag
       actionDefineFunction2_preloadRootFlag <- getFlag
       actionDefineFunction2_suppressSuperFlag <- getFlag
       actionDefineFunction2_preloadSuperFlag <- getFlag
       actionDefineFunction2_suppressArgumentsFlag <- getFlag
       actionDefineFunction2_preloadArgumentsFlag <- getFlag
       actionDefineFunction2_suppressThisFlag <- getFlag
       actionDefineFunction2_preloadThisFlag <- getFlag
       discardReserved "_reserved (x :: ?)" (getUB 7)
       actionDefineFunction2_preloadGlobalFlag <- getFlag
       actionDefineFunction2_parameters <- genericReplicateM
                                             actionDefineFunction2_numParams
                                             getREGISTERPARAM
       actionDefineFunction2_codeSize <- getUI16
       return (ActionDefineFunction2{..})
putActionDefineFunction2 ActionDefineFunction2{..}
  = do putSTRING actionDefineFunction2_functionName
       let actionDefineFunction2_numParams
             = genericLength actionDefineFunction2_parameters
       putUI16 actionDefineFunction2_numParams
       putUI8 actionDefineFunction2_registerCount
       putFlag actionDefineFunction2_preloadParentFlag
       putFlag actionDefineFunction2_preloadRootFlag
       putFlag actionDefineFunction2_suppressSuperFlag
       putFlag actionDefineFunction2_preloadSuperFlag
       putFlag actionDefineFunction2_suppressArgumentsFlag
       putFlag actionDefineFunction2_preloadArgumentsFlag
       putFlag actionDefineFunction2_suppressThisFlag
       putFlag actionDefineFunction2_preloadThisFlag
       let actionDefineFunction2_reserved = reservedDefault
       if requiredBitsUB actionDefineFunction2_reserved <= 7 then
         putUB 7 actionDefineFunction2_reserved else
         inconsistent "x :: ActionDefineFunction2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB actionDefineFunction2_reserved) ++
                " bits to store the value " ++
                  show actionDefineFunction2_reserved ++
                    ", but only have available " ++ show 7)
       putFlag actionDefineFunction2_preloadGlobalFlag
       if
         genericLength actionDefineFunction2_parameters /=
           (actionDefineFunction2_numParams)
         then
         inconsistent
           "actionDefineFunction2_parameters (x :: ActionDefineFunction2)"
           ("Mismatch with the required length: actionDefineFunction2_numParams"
              ++
              show (genericLength actionDefineFunction2_parameters) ++
                " /= " ++ show actionDefineFunction2_numParams)
         else
         mapM_ (\ x -> putREGISTERPARAM x) actionDefineFunction2_parameters
       putUI16 actionDefineFunction2_codeSize
       return ()

\end{code}

\begin{code}
 
data REGISTERPARAM = REGISTERPARAM{rEGISTERPARAM_register :: UI8,
                                   rEGISTERPARAM_paramName :: STRING}
                   deriving (Eq, Show, Typeable, Data)
getREGISTERPARAM
  = do rEGISTERPARAM_register <- getUI8
       rEGISTERPARAM_paramName <- getSTRING
       return (REGISTERPARAM{..})
putREGISTERPARAM REGISTERPARAM{..}
  = do putUI8 rEGISTERPARAM_register
       putSTRING rEGISTERPARAM_paramName
       return ()

\end{code}

p119: ActionExtends
\begin{code}
getActionExtends = do return (ActionExtends{..})
putActionExtends ActionExtends{..} = do return ()

\end{code}

p119: ActionCastOp
\begin{code}
getActionCastOp = do return (ActionCastOp{..})
putActionCastOp ActionCastOp{..} = do return ()

\end{code}

p120: ActionImplementsOp
\begin{code}
getActionImplementsOp = do return (ActionImplementsOp{..})
putActionImplementsOp ActionImplementsOp{..} = do return ()

\end{code}

p121: ActionTy
\begin{code}
getActionTry
  = do discardReserved "_reserved (x :: ?)" (getUB 5)
       actionTry_catchInRegisterFlag <- getFlag
       actionTry_finallyBlockFlag <- getFlag
       actionTry_catchBlockFlag <- getFlag
       actionTry_trySize <- getUI16
       actionTry_catchSize <- getUI16
       actionTry_finallySize <- getUI16
       actionTry_catchName <- maybeHas (not actionTry_catchInRegisterFlag)
                                getSTRING
       actionTry_catchRegister <- maybeHas actionTry_catchInRegisterFlag
                                    getUI8
       actionTry_tryBody <- genericReplicateM actionTry_trySize getUI8
       actionTry_catchBody <- genericReplicateM actionTry_catchSize getUI8
       actionTry_finallyBody <- genericReplicateM actionTry_finallySize
                                  getUI8
       return (ActionTry{..})
putActionTry ActionTry{..}
  = do let actionTry_reserved = reservedDefault
       if requiredBitsUB actionTry_reserved <= 5 then
         putUB 5 actionTry_reserved else
         inconsistent "x :: ActionTry"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB actionTry_reserved) ++
                " bits to store the value " ++
                  show actionTry_reserved ++ ", but only have available " ++ show 5)
       let actionTry_catchInRegisterFlag = isJust actionTry_catchRegister
       putFlag actionTry_catchInRegisterFlag
       putFlag actionTry_finallyBlockFlag
       putFlag actionTry_catchBlockFlag
       let actionTry_trySize = genericLength actionTry_tryBody
       putUI16 actionTry_trySize
       let actionTry_catchSize = genericLength actionTry_catchBody
       putUI16 actionTry_catchSize
       let actionTry_finallySize = genericLength actionTry_finallyBody
       putUI16 actionTry_finallySize
       case actionTry_catchName of
           Just x | not actionTry_catchInRegisterFlag -> putSTRING x
                  | otherwise ->
                    inconsistent "actionTry_catchName (x :: ActionTry)"
                      "Should have a Just iff not actionTry_catchInRegisterFlag is True"
           Nothing | not actionTry_catchInRegisterFlag ->
                     inconsistent "actionTry_catchName (x :: ActionTry)"
                       "Should have a Nothing iff not actionTry_catchInRegisterFlag is False"
                   | otherwise -> return ()
       case actionTry_catchRegister of
           Just x | actionTry_catchInRegisterFlag -> putUI8 x
                  | otherwise ->
                    inconsistent "actionTry_catchRegister (x :: ActionTry)"
                      "Should have a Just iff actionTry_catchInRegisterFlag is True"
           Nothing | actionTry_catchInRegisterFlag ->
                     inconsistent "actionTry_catchRegister (x :: ActionTry)"
                       "Should have a Nothing iff actionTry_catchInRegisterFlag is False"
                   | otherwise -> return ()
       if genericLength actionTry_tryBody /= (actionTry_trySize) then
         inconsistent "actionTry_tryBody (x :: ActionTry)"
           ("Mismatch with the required length: actionTry_trySize" ++
              show (genericLength actionTry_tryBody) ++
                " /= " ++ show actionTry_trySize)
         else mapM_ (\ x -> putUI8 x) actionTry_tryBody
       if genericLength actionTry_catchBody /= (actionTry_catchSize) then
         inconsistent "actionTry_catchBody (x :: ActionTry)"
           ("Mismatch with the required length: actionTry_catchSize" ++
              show (genericLength actionTry_catchBody) ++
                " /= " ++ show actionTry_catchSize)
         else mapM_ (\ x -> putUI8 x) actionTry_catchBody
       if genericLength actionTry_finallyBody /= (actionTry_finallySize)
         then
         inconsistent "actionTry_finallyBody (x :: ActionTry)"
           ("Mismatch with the required length: actionTry_finallySize" ++
              show (genericLength actionTry_finallyBody) ++
                " /= " ++ show actionTry_finallySize)
         else mapM_ (\ x -> putUI8 x) actionTry_finallyBody
       return ()

\end{code}

p122: ActionThrow
\begin{code}
getActionThrow = do return (ActionThrow{..})
putActionThrow ActionThrow{..} = do return ()

\end{code}

p123: DoABC
\begin{code}
getDoABC
  = do doABC_flags <- getUI32
       doABC_name <- getSTRING
       doABC_aBCData <- getRemainingLazyByteString
       return (DoABC{..})
putDoABC DoABC{..}
  = do putUI32 doABC_flags
       putSTRING doABC_name
       putLazyByteString doABC_aBCData
       return ()

\end{code}


Chapter 6: Shapes
~~~~~~~~~~~~~~~~~

p127: Fill styles
\begin{code}

type FILLSTYLEARRAY = [FILLSTYLE]

getFILLSTYLEARRAY shapeVer = do
    count <- getUI8
    count <- if count == 0xFF then getUI16 else return (fromIntegral count)
    genericReplicateM count (getFILLSTYLE shapeVer)

putFILLSTYLEARRAY shapeVer fs = do
    let count = length fs
    putUI8 (fromIntegral $ count `min` 0xFF)
    when (count >= 0xFF) $ putUI16 (fromIntegral count)
    mapM_ (putFILLSTYLE shapeVer) fs

data LinearRadial = Linear | Radial
                  deriving (Eq, Show, Typeable, Data)

data RepeatingClipped = Repeating | Clipped
                      deriving (Eq, Show, Typeable, Data)

data FILLSTYLE = SolidFill { fILLSTYLE_color :: Either RGB RGBA }
               | GradientFill { fILLSTYLE_linearRadial :: LinearRadial, fILLSTYLE_gradientMatrix :: MATRIX, fILLSTYLE_gradient :: GRADIENT }
               | FocalRadialGradientFill { fILLSTYLE_gradientMatrix :: MATRIX, fILLSTYLE_focalGradient :: FOCALGRADIENT }
               | BitmapFill { fILLSTYLE_repeatingClipped :: RepeatingClipped, fILLSTYLE_smoothed :: Bool, fILLSTYLE_bitmapId :: UI16, fILLSTYLE_bitmapMatrix :: MATRIX }
               deriving (Eq, Show, Typeable, Data)

getFILLSTYLE shapeVer = do
    fillStyleType <- getUI8
    case fillStyleType of
        0x00 -> fmap SolidFill $ if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
        0x10 -> liftM2 (GradientFill Linear) getMATRIX (getGRADIENT shapeVer)
        0x12 -> liftM2 (GradientFill Radial) getMATRIX (getGRADIENT shapeVer)
        0x13 -> liftM2 FocalRadialGradientFill getMATRIX (getFOCALGRADIENT shapeVer)
        0x40 -> liftM2 (BitmapFill Repeating True) getUI16 getMATRIX
        0x41 -> liftM2 (BitmapFill Clipped   True) getUI16 getMATRIX
        0x42 -> liftM2 (BitmapFill Repeating False) getUI16 getMATRIX
        0x43 -> liftM2 (BitmapFill Clipped   False) getUI16 getMATRIX

putFILLSTYLE shapeVer fs = case fs of
    SolidFill {..} -> do
        putUI8 0x00
        case fILLSTYLE_color of
            Left rgb | shapeVer <= 2  -> putRGB rgb
                     | otherwise      -> inconsistent "fILLSTYLE_color (x :: FILLSTYLE)" "Must use RGBA rather than RGB records for shape version > 2"
            Right rgba | shapeVer > 2 -> putRGBA rgba
                       | otherwise    -> inconsistent "fILLSTYLE_color (x :: FILLSTYLE)" "Must use RGB rather than RGBA records for shape version <= 2"
    GradientFill {..} -> do
        putUI8 (case fILLSTYLE_linearRadial of Linear -> 0x10; Radial -> 0x12)
        putMATRIX fILLSTYLE_gradientMatrix
        putGRADIENT shapeVer fILLSTYLE_gradient
    FocalRadialGradientFill {..} -> do
        putUI8 0x13
        putMATRIX fILLSTYLE_gradientMatrix
        putFOCALGRADIENT shapeVer fILLSTYLE_focalGradient
    BitmapFill {..} -> do
        putUI8 (case (fILLSTYLE_repeatingClipped, fILLSTYLE_smoothed) of (Repeating, True) -> 0x40; (Clipped, True) -> 0x41; (Repeating, False) -> 0x42; (Clipped, False) -> 0x43)
        putUI16 fILLSTYLE_bitmapId
        putMATRIX fILLSTYLE_bitmapMatrix

\end{code}

p130: Line styles
\begin{code}

type LINESTYLEARRAY = Either [LINESTYLE] [LINESTYLE2]

getLINESTYLEARRAY shapeVer = do
    count <- getUI8
    count <- if count == 0xFF then getUI16 else return (fromIntegral count)
    if shapeVer <= 3
     then fmap Left  $ genericReplicateM count (getLINESTYLE shapeVer)
     else fmap Right $ genericReplicateM count getLINESTYLE2

putLINESTYLEARRAY shapeVer ei_ls_ls2 = do
    let count = either length length ei_ls_ls2
    putUI8 (fromIntegral $ count `min` 0xFF)
    when (count >= 0xFF) $ putUI16 (fromIntegral count)
    case ei_ls_ls2 of
        Left ls   | shapeVer <= 3 -> mapM_ (putLINESTYLE shapeVer) ls
                  | otherwise     -> inconsistent "x :: LINESTYLEARRAY" "Must use LINESTYLE2 rather than LINESTYLE for shape version > 3"
        Right ls2 | shapeVer > 3  -> mapM_ putLINESTYLE2 ls2
                  | otherwise     -> inconsistent "x :: LINESTYLEARRAY" "Must use LINESTYLE rather than LINESTYLE2 for shape version <= 3"
    

data LINESTYLE = LINESTYLE { lINESTYLE_width :: UI16, lINESTYLE_color :: Either RGB RGBA }
               deriving (Eq, Show, Typeable, Data)

getLINESTYLE shapeVer = do
    lINESTYLE_width <- getUI16
    lINESTYLE_color <- if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
    return $ LINESTYLE {..}

putLINESTYLE shapeVer (LINESTYLE {..}) = do
    putUI16 lINESTYLE_width
    case lINESTYLE_color of
        Left rgb   | shapeVer <= 2 -> putRGB rgb
                   | otherwise     -> inconsistent "x :: LINESTYLE" "Must use RGBA rather than RGB for shape version > 2"
        Right rgba | shapeVer > 2  -> putRGBA rgba
                   | otherwise     -> inconsistent "x :: LINESTYLE" "Must use RGB rather than RGBA for shape version <= 2"

\end{code}

\begin{code}
 
data LINESTYLE2 = LINESTYLE2{lINESTYLE2_width :: UI16,
                             lINESTYLE2_startCapStyle :: UB, lINESTYLE2_joinStyle :: UB,
                             lINESTYLE2_noHScaleFlag :: Bool, lINESTYLE2_noVScaleFlag :: Bool,
                             lINESTYLE2_pixelHintingFlag :: Bool, lINESTYLE2_noClose :: Bool,
                             lINESTYLE2_endCapStyle :: UB,
                             lINESTYLE2_miterLimitFactor :: Maybe UI16,
                             lINESTYLE2_color :: Maybe RGBA,
                             lINESTYLE2_fillType :: Maybe FILLSTYLE}
                deriving (Eq, Show, Typeable, Data)
getLINESTYLE2
  = do lINESTYLE2_width <- getUI16
       lINESTYLE2_startCapStyle <- getUB 2
       lINESTYLE2_joinStyle <- getUB 2
       lINESTYLE2_hasFillFlag <- getFlag
       lINESTYLE2_noHScaleFlag <- getFlag
       lINESTYLE2_noVScaleFlag <- getFlag
       lINESTYLE2_pixelHintingFlag <- getFlag
       discardReserved "_reserved (x :: ?)" (getUB 5)
       lINESTYLE2_noClose <- getFlag
       lINESTYLE2_endCapStyle <- getUB 2
       lINESTYLE2_miterLimitFactor <- maybeHas (lINESTYLE2_joinStyle == 2)
                                        getUI16
       lINESTYLE2_color <- maybeHas (not lINESTYLE2_hasFillFlag) getRGBA
       lINESTYLE2_fillType <- maybeHas lINESTYLE2_hasFillFlag
                                (getFILLSTYLE 4)
       return (LINESTYLE2{..})
putLINESTYLE2 LINESTYLE2{..}
  = do putUI16 lINESTYLE2_width
       if requiredBitsUB lINESTYLE2_startCapStyle <= 2 then
         putUB 2 lINESTYLE2_startCapStyle else
         inconsistent "lINESTYLE2_startCapStyle (x :: LINESTYLE2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB lINESTYLE2_startCapStyle) ++
                " bits to store the value " ++
                  show lINESTYLE2_startCapStyle ++
                    ", but only have available " ++ show 2)
       if requiredBitsUB lINESTYLE2_joinStyle <= 2 then
         putUB 2 lINESTYLE2_joinStyle else
         inconsistent "lINESTYLE2_joinStyle (x :: LINESTYLE2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB lINESTYLE2_joinStyle) ++
                " bits to store the value " ++
                  show lINESTYLE2_joinStyle ++
                    ", but only have available " ++ show 2)
       let lINESTYLE2_hasFillFlag = isJust lINESTYLE2_fillType
       putFlag lINESTYLE2_hasFillFlag
       putFlag lINESTYLE2_noHScaleFlag
       putFlag lINESTYLE2_noVScaleFlag
       putFlag lINESTYLE2_pixelHintingFlag
       let lINESTYLE2_reserved = reservedDefault
       if requiredBitsUB lINESTYLE2_reserved <= 5 then
         putUB 5 lINESTYLE2_reserved else
         inconsistent "x :: LINESTYLE2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB lINESTYLE2_reserved) ++
                " bits to store the value " ++
                  show lINESTYLE2_reserved ++ ", but only have available " ++ show 5)
       putFlag lINESTYLE2_noClose
       if requiredBitsUB lINESTYLE2_endCapStyle <= 2 then
         putUB 2 lINESTYLE2_endCapStyle else
         inconsistent "lINESTYLE2_endCapStyle (x :: LINESTYLE2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB lINESTYLE2_endCapStyle) ++
                " bits to store the value " ++
                  show lINESTYLE2_endCapStyle ++
                    ", but only have available " ++ show 2)
       case lINESTYLE2_miterLimitFactor of
           Just x | lINESTYLE2_joinStyle == 2 -> putUI16 x
                  | otherwise ->
                    inconsistent "lINESTYLE2_miterLimitFactor (x :: LINESTYLE2)"
                      "Should have a Just iff lINESTYLE2_joinStyle == 2 is True"
           Nothing | lINESTYLE2_joinStyle == 2 ->
                     inconsistent "lINESTYLE2_miterLimitFactor (x :: LINESTYLE2)"
                       "Should have a Nothing iff lINESTYLE2_joinStyle == 2 is False"
                   | otherwise -> return ()
       case lINESTYLE2_color of
           Just x | not lINESTYLE2_hasFillFlag -> putRGBA x
                  | otherwise ->
                    inconsistent "lINESTYLE2_color (x :: LINESTYLE2)"
                      "Should have a Just iff not lINESTYLE2_hasFillFlag is True"
           Nothing | not lINESTYLE2_hasFillFlag ->
                     inconsistent "lINESTYLE2_color (x :: LINESTYLE2)"
                       "Should have a Nothing iff not lINESTYLE2_hasFillFlag is False"
                   | otherwise -> return ()
       case lINESTYLE2_fillType of
           Just x | lINESTYLE2_hasFillFlag -> putFILLSTYLE 4 x
                  | otherwise ->
                    inconsistent "lINESTYLE2_fillType (x :: LINESTYLE2)"
                      "Should have a Just iff lINESTYLE2_hasFillFlag is True"
           Nothing | lINESTYLE2_hasFillFlag ->
                     inconsistent "lINESTYLE2_fillType (x :: LINESTYLE2)"
                       "Should have a Nothing iff lINESTYLE2_hasFillFlag is False"
                   | otherwise -> return ()
       return ()

\end{code}

p133: Shape Structures
\begin{code}
 
data SHAPE = SHAPE{sHAPE_numFillBits :: UB,
                   sHAPE_numLineBits :: UB, sHAPE_shapeRecords :: SHAPERECORDS}
           deriving (Eq, Show, Typeable, Data)
getSHAPE sHAPE_shapeVer
  = do sHAPE_numFillBits <- getUB 4
       sHAPE_numLineBits <- getUB 4
       sHAPE_shapeRecords <- getSHAPERECORDS sHAPE_shapeVer
                               sHAPE_numFillBits
                               sHAPE_numLineBits
       return (SHAPE{..})
putSHAPE sHAPE_shapeVer SHAPE{..}
  = do if requiredBitsUB sHAPE_numFillBits <= 4 then
         putUB 4 sHAPE_numFillBits else
         inconsistent "sHAPE_numFillBits (x :: SHAPE)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB sHAPE_numFillBits) ++
                " bits to store the value " ++
                  show sHAPE_numFillBits ++ ", but only have available " ++ show 4)
       if requiredBitsUB sHAPE_numLineBits <= 4 then
         putUB 4 sHAPE_numLineBits else
         inconsistent "sHAPE_numLineBits (x :: SHAPE)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB sHAPE_numLineBits) ++
                " bits to store the value " ++
                  show sHAPE_numLineBits ++ ", but only have available " ++ show 4)
       putSHAPERECORDS sHAPE_shapeVer sHAPE_numFillBits sHAPE_numLineBits
         sHAPE_shapeRecords
       return ()

\end{code}

\begin{code}
 
data SHAPEWITHSTYLE = SHAPEWITHSTYLE{sHAPEWITHSTYLE_fillStyles ::
                                     FILLSTYLEARRAY,
                                     sHAPEWITHSTYLE_lineStyles :: LINESTYLEARRAY,
                                     sHAPEWITHSTYLE_numFillBits :: UB,
                                     sHAPEWITHSTYLE_numLineBits :: UB,
                                     sHAPEWITHSTYLE_shapeRecords :: SHAPERECORDS}
                    deriving (Eq, Show, Typeable, Data)
getSHAPEWITHSTYLE sHAPEWITHSTYLE_shapeVer
  = do sHAPEWITHSTYLE_fillStyles <- getFILLSTYLEARRAY
                                      sHAPEWITHSTYLE_shapeVer
       sHAPEWITHSTYLE_lineStyles <- getLINESTYLEARRAY
                                      sHAPEWITHSTYLE_shapeVer
       sHAPEWITHSTYLE_numFillBits <- getUB 4
       sHAPEWITHSTYLE_numLineBits <- getUB 4
       sHAPEWITHSTYLE_shapeRecords <- getSHAPERECORDS
                                        sHAPEWITHSTYLE_shapeVer
                                        sHAPEWITHSTYLE_numFillBits
                                        sHAPEWITHSTYLE_numLineBits
       return (SHAPEWITHSTYLE{..})
putSHAPEWITHSTYLE sHAPEWITHSTYLE_shapeVer SHAPEWITHSTYLE{..}
  = do putFILLSTYLEARRAY sHAPEWITHSTYLE_shapeVer
         sHAPEWITHSTYLE_fillStyles
       putLINESTYLEARRAY sHAPEWITHSTYLE_shapeVer sHAPEWITHSTYLE_lineStyles
       if requiredBitsUB sHAPEWITHSTYLE_numFillBits <= 4 then
         putUB 4 sHAPEWITHSTYLE_numFillBits else
         inconsistent "sHAPEWITHSTYLE_numFillBits (x :: SHAPEWITHSTYLE)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB sHAPEWITHSTYLE_numFillBits) ++
                " bits to store the value " ++
                  show sHAPEWITHSTYLE_numFillBits ++
                    ", but only have available " ++ show 4)
       if requiredBitsUB sHAPEWITHSTYLE_numLineBits <= 4 then
         putUB 4 sHAPEWITHSTYLE_numLineBits else
         inconsistent "sHAPEWITHSTYLE_numLineBits (x :: SHAPEWITHSTYLE)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB sHAPEWITHSTYLE_numLineBits) ++
                " bits to store the value " ++
                  show sHAPEWITHSTYLE_numLineBits ++
                    ", but only have available " ++ show 4)
       putSHAPERECORDS sHAPEWITHSTYLE_shapeVer sHAPEWITHSTYLE_numFillBits
         sHAPEWITHSTYLE_numLineBits
         sHAPEWITHSTYLE_shapeRecords
       return ()

\end{code}

\begin{code}

type SHAPERECORDS = [SHAPERECORD]

(getSHAPERECORDS, putSHAPERECORDS) = (getter, putter)
  where
    sTYLECHANGERECORD_fillBitsLineBits = fmap (thd4 &&& fth4) . sTYLECHANGERECORD_new
    
    getter shapeVer = go
      where
        go fillBits lineBits = do
          edgeRecord <- getFlag
          if edgeRecord
           then do
              straightEdge <- getFlag
              if straightEdge
               then do
                 x <- getSTRAIGHTEDGERECORD
                 fmap (x:) $ go fillBits lineBits
               else do
                 x <- getCURVEDEDGERECORD
                 fmap (x:) $ go fillBits lineBits
           else do
              look <- lookAhead (getUB 5)
              if look == 0
               then do
                 discardKnown "x :: SHAPERECORDS" "SHAPERECORDS array should be 0 terminated" 0 $ getUB 5
                 -- NB: align the SHAPERECORD array only at the end!
                 byteAlign
                 return []
               else do
                 x <- getSTYLECHANGERECORD shapeVer fillBits lineBits
                 let (fillBits', lineBits') = fromMaybe (fillBits, lineBits) $ sTYLECHANGERECORD_fillBitsLineBits x
                 fmap (x:) $ go fillBits' lineBits'
    
    putter shapeVer = go
      where
        go _ _ [] = do
          putFlag False
          putUB 5 0
          flushBits
        go fillBits lineBits (r:rs) = do
          (fillBits, lineBits) <- case r of
            STRAIGHTEDGERECORD {} -> putFlag True >> putFlag True >> putSTRAIGHTEDGERECORD r >> return (fillBits, lineBits)
            CURVEDEDGERECORD {}   -> putFlag True >> putFlag False >> putCURVEDEDGERECORD r >> return (fillBits, lineBits)
            STYLECHANGERECORD {}  -> putFlag False >> putSTYLECHANGERECORD shapeVer fillBits lineBits r >> return (fromMaybe (fillBits, lineBits) $ sTYLECHANGERECORD_fillBitsLineBits r)
          go fillBits lineBits rs

data SHAPERECORD
         =  STYLECHANGERECORD{sTYLECHANGERECORD_move :: Maybe (UB, SB, SB),
                    sTYLECHANGERECORD_fillStyle0 :: Maybe UB,
                    sTYLECHANGERECORD_fillStyle1 :: Maybe UB,
                    sTYLECHANGERECORD_lineStyle :: Maybe UB,
                    sTYLECHANGERECORD_new ::
                    Maybe (FILLSTYLEARRAY, LINESTYLEARRAY, UB, UB)}
         |  STRAIGHTEDGERECORD{sTRAIGHTEDGERECORD_numBits :: UB,
                     sTRAIGHTEDGERECORD_straightEdge :: StraightEdge}
         |  CURVEDEDGERECORD{cURVEDEDGERECORD_numBits :: UB,
                   cURVEDEDGERECORD_controlDeltaX :: SB,
                   cURVEDEDGERECORD_controlDeltaY :: SB,
                   cURVEDEDGERECORD_anchorDeltaX :: SB,
                   cURVEDEDGERECORD_anchorDeltaY :: SB}
                 deriving (Eq, Show, Typeable, Data)

\end{code}

NB: the various SHAPERECORDs are intentionally not padded to byte
align them, because they are packed together on disk. The entire
array will be aligned as a unit, however.

This appears to be the exact opposite of what the spec says happens!

\begin{code}
getSTYLECHANGERECORD sTYLECHANGERECORD_shapeVer
  sTYLECHANGERECORD_fillBits sTYLECHANGERECORD_lineBits
  = do sTYLECHANGERECORD_stateNewStyles <- getFlag
       sTYLECHANGERECORD_stateLineStyle <- getFlag
       sTYLECHANGERECORD_stateFillStyle1 <- getFlag
       sTYLECHANGERECORD_stateFillStyle0 <- getFlag
       sTYLECHANGERECORD_stateMoveTo <- getFlag
       sTYLECHANGERECORD_move <- maybeHas sTYLECHANGERECORD_stateMoveTo
                                   (do sTYLECHANGERECORD_moveBits <- getUB 5
                                       sTYLECHANGERECORD_moveDeltaX <- getSB
                                                                         sTYLECHANGERECORD_moveBits
                                       sTYLECHANGERECORD_moveDeltaY <- getSB
                                                                         sTYLECHANGERECORD_moveBits
                                       return
                                         (sTYLECHANGERECORD_moveBits, sTYLECHANGERECORD_moveDeltaX,
                                          sTYLECHANGERECORD_moveDeltaY))
       sTYLECHANGERECORD_fillStyle0 <- maybeHas
                                         sTYLECHANGERECORD_stateFillStyle0
                                         (getUB sTYLECHANGERECORD_fillBits)
       sTYLECHANGERECORD_fillStyle1 <- maybeHas
                                         sTYLECHANGERECORD_stateFillStyle1
                                         (getUB sTYLECHANGERECORD_fillBits)
       sTYLECHANGERECORD_lineStyle <- maybeHas
                                        sTYLECHANGERECORD_stateLineStyle
                                        (getUB sTYLECHANGERECORD_lineBits)
       sTYLECHANGERECORD_new <- maybeHas sTYLECHANGERECORD_stateNewStyles
                                  (do sTYLECHANGERECORD_newFillStyles <- getFILLSTYLEARRAY
                                                                           sTYLECHANGERECORD_shapeVer
                                      sTYLECHANGERECORD_newLineStyles <- getLINESTYLEARRAY
                                                                           sTYLECHANGERECORD_shapeVer
                                      sTYLECHANGERECORD_newNumFillBits <- getUB 4
                                      sTYLECHANGERECORD_newNumLineBits <- getUB 4
                                      return
                                        (sTYLECHANGERECORD_newFillStyles,
                                         sTYLECHANGERECORD_newLineStyles,
                                         sTYLECHANGERECORD_newNumFillBits,
                                         sTYLECHANGERECORD_newNumLineBits))
       return (STYLECHANGERECORD{..})
putSTYLECHANGERECORD sTYLECHANGERECORD_shapeVer
  sTYLECHANGERECORD_fillBits sTYLECHANGERECORD_lineBits
  STYLECHANGERECORD{..}
  = do let sTYLECHANGERECORD_stateNewStyles
             = isJust sTYLECHANGERECORD_new
       putFlag sTYLECHANGERECORD_stateNewStyles
       let sTYLECHANGERECORD_stateLineStyle
             = isJust sTYLECHANGERECORD_lineStyle
       putFlag sTYLECHANGERECORD_stateLineStyle
       let sTYLECHANGERECORD_stateFillStyle1
             = isJust sTYLECHANGERECORD_fillStyle1
       putFlag sTYLECHANGERECORD_stateFillStyle1
       let sTYLECHANGERECORD_stateFillStyle0
             = isJust sTYLECHANGERECORD_fillStyle0
       putFlag sTYLECHANGERECORD_stateFillStyle0
       let sTYLECHANGERECORD_stateMoveTo = isJust sTYLECHANGERECORD_move
       putFlag sTYLECHANGERECORD_stateMoveTo
       case sTYLECHANGERECORD_move of
           Just x | sTYLECHANGERECORD_stateMoveTo ->
                    case x of
                        (sTYLECHANGERECORD_moveBits, sTYLECHANGERECORD_moveDeltaX,
                         sTYLECHANGERECORD_moveDeltaY) -> do if
                                                               requiredBitsUB
                                                                 sTYLECHANGERECORD_moveBits
                                                                 <= 5
                                                               then
                                                               putUB 5 sTYLECHANGERECORD_moveBits
                                                               else
                                                               inconsistent
                                                                 "fst (fromJust (sTYLECHANGERECORD_move (x :: STYLECHANGERECORD)))"
                                                                 ("Bit count incorrect: required "
                                                                    ++
                                                                    show
                                                                      (requiredBitsUB
                                                                         sTYLECHANGERECORD_moveBits)
                                                                      ++
                                                                      " bits to store the value " ++
                                                                        show
                                                                          sTYLECHANGERECORD_moveBits
                                                                          ++
                                                                          ", but only have available "
                                                                            ++ show 5)
                                                             if
                                                               requiredBitsSB
                                                                 sTYLECHANGERECORD_moveDeltaX
                                                                 <= sTYLECHANGERECORD_moveBits
                                                               then
                                                               putSB sTYLECHANGERECORD_moveBits
                                                                 sTYLECHANGERECORD_moveDeltaX
                                                               else
                                                               inconsistent
                                                                 "snd (fromJust (sTYLECHANGERECORD_move (x :: STYLECHANGERECORD)))"
                                                                 ("Bit count incorrect: required "
                                                                    ++
                                                                    show
                                                                      (requiredBitsSB
                                                                         sTYLECHANGERECORD_moveDeltaX)
                                                                      ++
                                                                      " bits to store the value " ++
                                                                        show
                                                                          sTYLECHANGERECORD_moveDeltaX
                                                                          ++
                                                                          ", but only have available "
                                                                            ++
                                                                            show
                                                                              sTYLECHANGERECORD_moveBits)
                                                             if
                                                               requiredBitsSB
                                                                 sTYLECHANGERECORD_moveDeltaY
                                                                 <= sTYLECHANGERECORD_moveBits
                                                               then
                                                               putSB sTYLECHANGERECORD_moveBits
                                                                 sTYLECHANGERECORD_moveDeltaY
                                                               else
                                                               inconsistent
                                                                 "thd (fromJust (sTYLECHANGERECORD_move (x :: STYLECHANGERECORD)))"
                                                                 ("Bit count incorrect: required "
                                                                    ++
                                                                    show
                                                                      (requiredBitsSB
                                                                         sTYLECHANGERECORD_moveDeltaY)
                                                                      ++
                                                                      " bits to store the value " ++
                                                                        show
                                                                          sTYLECHANGERECORD_moveDeltaY
                                                                          ++
                                                                          ", but only have available "
                                                                            ++
                                                                            show
                                                                              sTYLECHANGERECORD_moveBits)
                                                             return ()
                  | otherwise ->
                    inconsistent "sTYLECHANGERECORD_move (x :: STYLECHANGERECORD)"
                      "Should have a Just iff sTYLECHANGERECORD_stateMoveTo is True"
           Nothing | sTYLECHANGERECORD_stateMoveTo ->
                     inconsistent "sTYLECHANGERECORD_move (x :: STYLECHANGERECORD)"
                       "Should have a Nothing iff sTYLECHANGERECORD_stateMoveTo is False"
                   | otherwise -> return ()
       case sTYLECHANGERECORD_fillStyle0 of
           Just x | sTYLECHANGERECORD_stateFillStyle0 ->
                    if requiredBitsUB x <= sTYLECHANGERECORD_fillBits then
                      putUB sTYLECHANGERECORD_fillBits x else
                      inconsistent
                        "fromJust (sTYLECHANGERECORD_fillStyle0 (x :: STYLECHANGERECORD))"
                        ("Bit count incorrect: required " ++
                           show (requiredBitsUB x) ++
                             " bits to store the value " ++
                               show x ++
                                 ", but only have available " ++ show sTYLECHANGERECORD_fillBits)
                  | otherwise ->
                    inconsistent
                      "sTYLECHANGERECORD_fillStyle0 (x :: STYLECHANGERECORD)"
                      "Should have a Just iff sTYLECHANGERECORD_stateFillStyle0 is True"
           Nothing | sTYLECHANGERECORD_stateFillStyle0 ->
                     inconsistent
                       "sTYLECHANGERECORD_fillStyle0 (x :: STYLECHANGERECORD)"
                       "Should have a Nothing iff sTYLECHANGERECORD_stateFillStyle0 is False"
                   | otherwise -> return ()
       case sTYLECHANGERECORD_fillStyle1 of
           Just x | sTYLECHANGERECORD_stateFillStyle1 ->
                    if requiredBitsUB x <= sTYLECHANGERECORD_fillBits then
                      putUB sTYLECHANGERECORD_fillBits x else
                      inconsistent
                        "fromJust (sTYLECHANGERECORD_fillStyle1 (x :: STYLECHANGERECORD))"
                        ("Bit count incorrect: required " ++
                           show (requiredBitsUB x) ++
                             " bits to store the value " ++
                               show x ++
                                 ", but only have available " ++ show sTYLECHANGERECORD_fillBits)
                  | otherwise ->
                    inconsistent
                      "sTYLECHANGERECORD_fillStyle1 (x :: STYLECHANGERECORD)"
                      "Should have a Just iff sTYLECHANGERECORD_stateFillStyle1 is True"
           Nothing | sTYLECHANGERECORD_stateFillStyle1 ->
                     inconsistent
                       "sTYLECHANGERECORD_fillStyle1 (x :: STYLECHANGERECORD)"
                       "Should have a Nothing iff sTYLECHANGERECORD_stateFillStyle1 is False"
                   | otherwise -> return ()
       case sTYLECHANGERECORD_lineStyle of
           Just x | sTYLECHANGERECORD_stateLineStyle ->
                    if requiredBitsUB x <= sTYLECHANGERECORD_lineBits then
                      putUB sTYLECHANGERECORD_lineBits x else
                      inconsistent
                        "fromJust (sTYLECHANGERECORD_lineStyle (x :: STYLECHANGERECORD))"
                        ("Bit count incorrect: required " ++
                           show (requiredBitsUB x) ++
                             " bits to store the value " ++
                               show x ++
                                 ", but only have available " ++ show sTYLECHANGERECORD_lineBits)
                  | otherwise ->
                    inconsistent "sTYLECHANGERECORD_lineStyle (x :: STYLECHANGERECORD)"
                      "Should have a Just iff sTYLECHANGERECORD_stateLineStyle is True"
           Nothing | sTYLECHANGERECORD_stateLineStyle ->
                     inconsistent "sTYLECHANGERECORD_lineStyle (x :: STYLECHANGERECORD)"
                       "Should have a Nothing iff sTYLECHANGERECORD_stateLineStyle is False"
                   | otherwise -> return ()
       case sTYLECHANGERECORD_new of
           Just x | sTYLECHANGERECORD_stateNewStyles ->
                    case x of
                        (sTYLECHANGERECORD_newFillStyles, sTYLECHANGERECORD_newLineStyles,
                         sTYLECHANGERECORD_newNumFillBits,
                         sTYLECHANGERECORD_newNumLineBits) -> do putFILLSTYLEARRAY
                                                                   sTYLECHANGERECORD_shapeVer
                                                                   sTYLECHANGERECORD_newFillStyles
                                                                 putLINESTYLEARRAY
                                                                   sTYLECHANGERECORD_shapeVer
                                                                   sTYLECHANGERECORD_newLineStyles
                                                                 if
                                                                   requiredBitsUB
                                                                     sTYLECHANGERECORD_newNumFillBits
                                                                     <= 4
                                                                   then
                                                                   putUB 4
                                                                     sTYLECHANGERECORD_newNumFillBits
                                                                   else
                                                                   inconsistent
                                                                     "thd (fromJust (sTYLECHANGERECORD_new (x :: STYLECHANGERECORD)))"
                                                                     ("Bit count incorrect: required "
                                                                        ++
                                                                        show
                                                                          (requiredBitsUB
                                                                             sTYLECHANGERECORD_newNumFillBits)
                                                                          ++
                                                                          " bits to store the value "
                                                                            ++
                                                                            show
                                                                              sTYLECHANGERECORD_newNumFillBits
                                                                              ++
                                                                              ", but only have available "
                                                                                ++ show 4)
                                                                 if
                                                                   requiredBitsUB
                                                                     sTYLECHANGERECORD_newNumLineBits
                                                                     <= 4
                                                                   then
                                                                   putUB 4
                                                                     sTYLECHANGERECORD_newNumLineBits
                                                                   else
                                                                   inconsistent
                                                                     "frth (fromJust (sTYLECHANGERECORD_new (x :: STYLECHANGERECORD)))"
                                                                     ("Bit count incorrect: required "
                                                                        ++
                                                                        show
                                                                          (requiredBitsUB
                                                                             sTYLECHANGERECORD_newNumLineBits)
                                                                          ++
                                                                          " bits to store the value "
                                                                            ++
                                                                            show
                                                                              sTYLECHANGERECORD_newNumLineBits
                                                                              ++
                                                                              ", but only have available "
                                                                                ++ show 4)
                                                                 return ()
                  | otherwise ->
                    inconsistent "sTYLECHANGERECORD_new (x :: STYLECHANGERECORD)"
                      "Should have a Just iff sTYLECHANGERECORD_stateNewStyles is True"
           Nothing | sTYLECHANGERECORD_stateNewStyles ->
                     inconsistent "sTYLECHANGERECORD_new (x :: STYLECHANGERECORD)"
                       "Should have a Nothing iff sTYLECHANGERECORD_stateNewStyles is False"
                   | otherwise -> return ()
       return ()

\end{code}

\begin{code}
getSTRAIGHTEDGERECORD
  = do sTRAIGHTEDGERECORD_numBits <- getUB 4
       sTRAIGHTEDGERECORD_straightEdge <- getStraightEdge
                                            sTRAIGHTEDGERECORD_numBits
       return (STRAIGHTEDGERECORD{..})
putSTRAIGHTEDGERECORD STRAIGHTEDGERECORD{..}
  = do if requiredBitsUB sTRAIGHTEDGERECORD_numBits <= 4 then
         putUB 4 sTRAIGHTEDGERECORD_numBits else
         inconsistent "sTRAIGHTEDGERECORD_numBits (x :: STRAIGHTEDGERECORD)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB sTRAIGHTEDGERECORD_numBits) ++
                " bits to store the value " ++
                  show sTRAIGHTEDGERECORD_numBits ++
                    ", but only have available " ++ show 4)
       putStraightEdge sTRAIGHTEDGERECORD_numBits
         sTRAIGHTEDGERECORD_straightEdge
       return ()

\end{code}

\begin{code}

data StraightEdge = GeneralLine { straightEdge_deltaX :: SB, straightEdge_deltaY :: SB }
                  | VerticalLine { straightEdge_deltaY :: SB }
                  | HorizontalLine { straightEdge_deltaX :: SB }
                  deriving (Eq, Show, Typeable, Data)

getStraightEdge numBits = do
    generalLine <- getFlag
    if generalLine
     then liftM2 GeneralLine (getSB (numBits + 2)) (getSB (numBits + 2))
     else do
      vert <- getFlag
      liftM (if vert then VerticalLine else HorizontalLine) (getSB (numBits + 2))

putStraightEdge numBits se = case se of
    GeneralLine {..}    -> putFlag True >> putSB (numBits + 2) straightEdge_deltaX >> putSB (numBits + 2) straightEdge_deltaY
    VerticalLine {..}   -> putFlag False >> putFlag True >> putSB (numBits + 2) straightEdge_deltaY
    HorizontalLine {..} -> putFlag False >> putFlag False >> putSB (numBits + 2) straightEdge_deltaX

\end{code}

\begin{code}
getCURVEDEDGERECORD
  = do cURVEDEDGERECORD_numBits <- getUB 4
       cURVEDEDGERECORD_controlDeltaX <- getSB
                                           (cURVEDEDGERECORD_numBits + 2)
       cURVEDEDGERECORD_controlDeltaY <- getSB
                                           (cURVEDEDGERECORD_numBits + 2)
       cURVEDEDGERECORD_anchorDeltaX <- getSB
                                          (cURVEDEDGERECORD_numBits + 2)
       cURVEDEDGERECORD_anchorDeltaY <- getSB
                                          (cURVEDEDGERECORD_numBits + 2)
       return (CURVEDEDGERECORD{..})
putCURVEDEDGERECORD CURVEDEDGERECORD{..}
  = do if requiredBitsUB cURVEDEDGERECORD_numBits <= 4 then
         putUB 4 cURVEDEDGERECORD_numBits else
         inconsistent "cURVEDEDGERECORD_numBits (x :: CURVEDEDGERECORD)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB cURVEDEDGERECORD_numBits) ++
                " bits to store the value " ++
                  show cURVEDEDGERECORD_numBits ++
                    ", but only have available " ++ show 4)
       if
         requiredBitsSB cURVEDEDGERECORD_controlDeltaX <=
           cURVEDEDGERECORD_numBits + 2
         then
         putSB (cURVEDEDGERECORD_numBits + 2) cURVEDEDGERECORD_controlDeltaX
         else
         inconsistent
           "cURVEDEDGERECORD_controlDeltaX (x :: CURVEDEDGERECORD)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB cURVEDEDGERECORD_controlDeltaX) ++
                " bits to store the value " ++
                  show cURVEDEDGERECORD_controlDeltaX ++
                    ", but only have available " ++
                      show (cURVEDEDGERECORD_numBits + 2))
       if
         requiredBitsSB cURVEDEDGERECORD_controlDeltaY <=
           cURVEDEDGERECORD_numBits + 2
         then
         putSB (cURVEDEDGERECORD_numBits + 2) cURVEDEDGERECORD_controlDeltaY
         else
         inconsistent
           "cURVEDEDGERECORD_controlDeltaY (x :: CURVEDEDGERECORD)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB cURVEDEDGERECORD_controlDeltaY) ++
                " bits to store the value " ++
                  show cURVEDEDGERECORD_controlDeltaY ++
                    ", but only have available " ++
                      show (cURVEDEDGERECORD_numBits + 2))
       if
         requiredBitsSB cURVEDEDGERECORD_anchorDeltaX <=
           cURVEDEDGERECORD_numBits + 2
         then
         putSB (cURVEDEDGERECORD_numBits + 2) cURVEDEDGERECORD_anchorDeltaX
         else
         inconsistent
           "cURVEDEDGERECORD_anchorDeltaX (x :: CURVEDEDGERECORD)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB cURVEDEDGERECORD_anchorDeltaX) ++
                " bits to store the value " ++
                  show cURVEDEDGERECORD_anchorDeltaX ++
                    ", but only have available " ++
                      show (cURVEDEDGERECORD_numBits + 2))
       if
         requiredBitsSB cURVEDEDGERECORD_anchorDeltaY <=
           cURVEDEDGERECORD_numBits + 2
         then
         putSB (cURVEDEDGERECORD_numBits + 2) cURVEDEDGERECORD_anchorDeltaY
         else
         inconsistent
           "cURVEDEDGERECORD_anchorDeltaY (x :: CURVEDEDGERECORD)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB cURVEDEDGERECORD_anchorDeltaY) ++
                " bits to store the value " ++
                  show cURVEDEDGERECORD_anchorDeltaY ++
                    ", but only have available " ++
                      show (cURVEDEDGERECORD_numBits + 2))
       return ()

\end{code}

p140: DefineShape
\begin{code}
getDefineShape
  = do defineShape_shapeId <- getUI16
       defineShape_shapeBounds <- getRECT
       defineShape_shapes <- getSHAPEWITHSTYLE 1
       return (DefineShape{..})
putDefineShape DefineShape{..}
  = do putUI16 defineShape_shapeId
       putRECT defineShape_shapeBounds
       putSHAPEWITHSTYLE 1 defineShape_shapes
       return ()

\end{code}

p141: DefineShape2
\begin{code}
getDefineShape2
  = do defineShape2_shapeId <- getUI16
       defineShape2_shapeBounds <- getRECT
       defineShape2_shapes <- getSHAPEWITHSTYLE 2
       return (DefineShape2{..})
putDefineShape2 DefineShape2{..}
  = do putUI16 defineShape2_shapeId
       putRECT defineShape2_shapeBounds
       putSHAPEWITHSTYLE 2 defineShape2_shapes
       return ()

\end{code}

p141: DefineShape3
\begin{code}
getDefineShape3
  = do defineShape3_shapeId <- getUI16
       defineShape3_shapeBounds <- getRECT
       defineShape3_shapes <- getSHAPEWITHSTYLE 3
       return (DefineShape3{..})
putDefineShape3 DefineShape3{..}
  = do putUI16 defineShape3_shapeId
       putRECT defineShape3_shapeBounds
       putSHAPEWITHSTYLE 3 defineShape3_shapes
       return ()

\end{code}

p142: DefineShape4
\begin{code}
getDefineShape4
  = do defineShape4_shapeId <- getUI16
       defineShape4_shapeBounds <- getRECT
       defineShape4_edgeBounds <- getRECT
       discardReserved "_reserved (x :: ?)" (getUB 5)
       defineShape4_usesFillWindingRule <- getFlag
       defineShape4_usesNonScalingStrokes <- getFlag
       defineShape4_usesScalingStrokes <- getFlag
       defineShape4_shapes <- getSHAPEWITHSTYLE 4
       return (DefineShape4{..})
putDefineShape4 DefineShape4{..}
  = do putUI16 defineShape4_shapeId
       putRECT defineShape4_shapeBounds
       putRECT defineShape4_edgeBounds
       let defineShape4_reserved = reservedDefault
       if requiredBitsUB defineShape4_reserved <= 5 then
         putUB 5 defineShape4_reserved else
         inconsistent "x :: DefineShape4"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineShape4_reserved) ++
                " bits to store the value " ++
                  show defineShape4_reserved ++
                    ", but only have available " ++ show 5)
       putFlag defineShape4_usesFillWindingRule
       putFlag defineShape4_usesNonScalingStrokes
       putFlag defineShape4_usesScalingStrokes
       putSHAPEWITHSTYLE 4 defineShape4_shapes
       return ()

\end{code}


Chapter 7: Gradients
~~~~~~~~~~~~~~~~~~~~

p145: GRADIENT
\begin{code}
 
data GRADIENT = GRADIENT{gRADIENT_spreadMode :: UB,
                         gRADIENT_interpolationMode :: UB,
                         gRADIENT_gradientRecords :: [GRADRECORD]}
              deriving (Eq, Show, Typeable, Data)
getGRADIENT gRADIENT_shapeVer
  = do gRADIENT_spreadMode <- getUB 2
       gRADIENT_interpolationMode <- getUB 2
       gRADIENT_numGradients <- getUB 4
       gRADIENT_gradientRecords <- genericReplicateM gRADIENT_numGradients
                                     (getGRADRECORD gRADIENT_shapeVer)
       return (GRADIENT{..})
putGRADIENT gRADIENT_shapeVer GRADIENT{..}
  = do if requiredBitsUB gRADIENT_spreadMode <= 2 then
         putUB 2 gRADIENT_spreadMode else
         inconsistent "gRADIENT_spreadMode (x :: GRADIENT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB gRADIENT_spreadMode) ++
                " bits to store the value " ++
                  show gRADIENT_spreadMode ++ ", but only have available " ++ show 2)
       if requiredBitsUB gRADIENT_interpolationMode <= 2 then
         putUB 2 gRADIENT_interpolationMode else
         inconsistent "gRADIENT_interpolationMode (x :: GRADIENT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB gRADIENT_interpolationMode) ++
                " bits to store the value " ++
                  show gRADIENT_interpolationMode ++
                    ", but only have available " ++ show 2)
       let gRADIENT_numGradients = genericLength gRADIENT_gradientRecords
       if requiredBitsUB gRADIENT_numGradients <= 4 then
         putUB 4 gRADIENT_numGradients else
         inconsistent "x :: GRADIENT"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB gRADIENT_numGradients) ++
                " bits to store the value " ++
                  show gRADIENT_numGradients ++
                    ", but only have available " ++ show 4)
       if
         genericLength gRADIENT_gradientRecords /= (gRADIENT_numGradients)
         then
         inconsistent "gRADIENT_gradientRecords (x :: GRADIENT)"
           ("Mismatch with the required length: gRADIENT_numGradients" ++
              show (genericLength gRADIENT_gradientRecords) ++
                " /= " ++ show gRADIENT_numGradients)
         else
         mapM_ (\ x -> putGRADRECORD gRADIENT_shapeVer x)
           gRADIENT_gradientRecords
       return ()

\end{code}

p146: FOCALGRADIENT
\begin{code}
 
data FOCALGRADIENT = FOCALGRADIENT{fOCALGRADIENT_spreadMode :: UB,
                                   fOCALGRADIENT_interpolationMode :: UB,
                                   fOCALGRADIENT_gradientRecords :: [GRADRECORD],
                                   fOCALGRADIENT_focalPoint :: FIXED8}
                   deriving (Eq, Show, Typeable, Data)
getFOCALGRADIENT fOCALGRADIENT_shapeVer
  = do fOCALGRADIENT_spreadMode <- getUB 2
       fOCALGRADIENT_interpolationMode <- getUB 2
       fOCALGRADIENT_numGradients <- getUB 4
       fOCALGRADIENT_gradientRecords <- genericReplicateM
                                          fOCALGRADIENT_numGradients
                                          (getGRADRECORD fOCALGRADIENT_shapeVer)
       fOCALGRADIENT_focalPoint <- getFIXED8
       return (FOCALGRADIENT{..})
putFOCALGRADIENT fOCALGRADIENT_shapeVer FOCALGRADIENT{..}
  = do if requiredBitsUB fOCALGRADIENT_spreadMode <= 2 then
         putUB 2 fOCALGRADIENT_spreadMode else
         inconsistent "fOCALGRADIENT_spreadMode (x :: FOCALGRADIENT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB fOCALGRADIENT_spreadMode) ++
                " bits to store the value " ++
                  show fOCALGRADIENT_spreadMode ++
                    ", but only have available " ++ show 2)
       if requiredBitsUB fOCALGRADIENT_interpolationMode <= 2 then
         putUB 2 fOCALGRADIENT_interpolationMode else
         inconsistent "fOCALGRADIENT_interpolationMode (x :: FOCALGRADIENT)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB fOCALGRADIENT_interpolationMode) ++
                " bits to store the value " ++
                  show fOCALGRADIENT_interpolationMode ++
                    ", but only have available " ++ show 2)
       let fOCALGRADIENT_numGradients
             = genericLength fOCALGRADIENT_gradientRecords
       if requiredBitsUB fOCALGRADIENT_numGradients <= 4 then
         putUB 4 fOCALGRADIENT_numGradients else
         inconsistent "x :: FOCALGRADIENT"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB fOCALGRADIENT_numGradients) ++
                " bits to store the value " ++
                  show fOCALGRADIENT_numGradients ++
                    ", but only have available " ++ show 4)
       if
         genericLength fOCALGRADIENT_gradientRecords /=
           (fOCALGRADIENT_numGradients)
         then
         inconsistent "fOCALGRADIENT_gradientRecords (x :: FOCALGRADIENT)"
           ("Mismatch with the required length: fOCALGRADIENT_numGradients" ++
              show (genericLength fOCALGRADIENT_gradientRecords) ++
                " /= " ++ show fOCALGRADIENT_numGradients)
         else
         mapM_ (\ x -> putGRADRECORD fOCALGRADIENT_shapeVer x)
           fOCALGRADIENT_gradientRecords
       putFIXED8 fOCALGRADIENT_focalPoint
       return ()

\end{code}

p146: GRADRECORD
\begin{code}

data GRADRECORD = GRADRECORD { gRADRECORD_ratio :: UI8, gRADRECORD_color :: Either RGB RGBA }
                deriving (Eq, Show, Typeable, Data)

getGRADRECORD shapeVer = do
    gRADRECORD_ratio <- getUI8
    gRADRECORD_color <- if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
    return $ GRADRECORD {..}

putGRADRECORD shapeVer (GRADRECORD {..}) = do
    putUI8 gRADRECORD_ratio
    case gRADRECORD_color of
        Left rgb   | shapeVer <= 2 -> putRGB rgb
                   | otherwise     -> inconsistent "x :: GRADRECORD" "Must use RGBA rather than RGB for shape version > 2"
        Right rgba | shapeVer > 2  -> putRGBA rgba
                   | otherwise     -> inconsistent "x :: GRADRECORD" "Must use RGB rather than RGBA for shape version <= 2"

\end{code}


Chapter 8: Bitmaps
~~~~~~~~~~~~~~~~~~

p148: DefineBits
\begin{code}
getDefineBits
  = do defineBits_characterID <- getUI16
       defineBits_jPEGData <- getRemainingLazyByteString
       return (DefineBits{..})
putDefineBits DefineBits{..}
  = do putUI16 defineBits_characterID
       putLazyByteString defineBits_jPEGData
       return ()

\end{code}

p148: JPEGTables
\begin{code}
getJPEGTables
  = do jPEGTables_jPEGData <- getRemainingLazyByteString
       return (JPEGTables{..})
putJPEGTables JPEGTables{..}
  = do putLazyByteString jPEGTables_jPEGData
       return ()

\end{code}

p149: DefineBitsJPEG2
\begin{code}
getDefineBitsJPEG2
  = do defineBitsJPEG2_characterID <- getUI16
       defineBitsJPEG2_imageData <- getRemainingLazyByteString
       return (DefineBitsJPEG2{..})
putDefineBitsJPEG2 DefineBitsJPEG2{..}
  = do putUI16 defineBitsJPEG2_characterID
       putLazyByteString defineBitsJPEG2_imageData
       return ()

\end{code}

p149: DefineBitsJPEG3
\begin{code}
getDefineBitsJPEG3
  = do defineBitsJPEG3_characterID <- getUI16
       defineBitsJPEG3_alphaDataOffset <- getUI32
       defineBitsJPEG3_imageData <- genericReplicateM
                                      defineBitsJPEG3_alphaDataOffset
                                      getUI8
       defineBitsJPEG3_bitmapAlphaData <- getRemainingLazyByteString
       return (DefineBitsJPEG3{..})
putDefineBitsJPEG3 DefineBitsJPEG3{..}
  = do putUI16 defineBitsJPEG3_characterID
       let defineBitsJPEG3_alphaDataOffset
             = genericLength defineBitsJPEG3_imageData
       putUI32 defineBitsJPEG3_alphaDataOffset
       if
         genericLength defineBitsJPEG3_imageData /=
           (defineBitsJPEG3_alphaDataOffset)
         then
         inconsistent "defineBitsJPEG3_imageData (x :: DefineBitsJPEG3)"
           ("Mismatch with the required length: defineBitsJPEG3_alphaDataOffset"
              ++
              show (genericLength defineBitsJPEG3_imageData) ++
                " /= " ++ show defineBitsJPEG3_alphaDataOffset)
         else mapM_ (\ x -> putUI8 x) defineBitsJPEG3_imageData
       putLazyByteString defineBitsJPEG3_bitmapAlphaData
       return ()

\end{code}

p150: DefineBitsLossless
\begin{code}
getDefineBitsLossless
  = do defineBitsLossless_characterID <- getUI16
       defineBitsLossless_bitmapFormat <- getUI8
       defineBitsLossless_bitmapWidth <- getUI16
       defineBitsLossless_bitmapHeight <- getUI16
       defineBitsLossless_bitmapColorTableSize <- maybeHas
                                                    (defineBitsLossless_bitmapFormat == 3)
                                                    getUI8
       defineBitsLossless_zlibBitmapData <- getRemainingLazyByteString
       return (DefineBitsLossless{..})
putDefineBitsLossless DefineBitsLossless{..}
  = do putUI16 defineBitsLossless_characterID
       putUI8 defineBitsLossless_bitmapFormat
       putUI16 defineBitsLossless_bitmapWidth
       putUI16 defineBitsLossless_bitmapHeight
       case defineBitsLossless_bitmapColorTableSize of
           Just x | defineBitsLossless_bitmapFormat == 3 -> putUI8 x
                  | otherwise ->
                    inconsistent
                      "defineBitsLossless_bitmapColorTableSize (x :: DefineBitsLossless)"
                      "Should have a Just iff defineBitsLossless_bitmapFormat == 3 is True"
           Nothing | defineBitsLossless_bitmapFormat == 3 ->
                     inconsistent
                       "defineBitsLossless_bitmapColorTableSize (x :: DefineBitsLossless)"
                       "Should have a Nothing iff defineBitsLossless_bitmapFormat == 3 is False"
                   | otherwise -> return ()
       putLazyByteString defineBitsLossless_zlibBitmapData
       return ()

\end{code}

p153: DefineBitsLossless2
\begin{code}
getDefineBitsLossless2
  = do defineBitsLossless2_characterID <- getUI16
       defineBitsLossless2_bitmapFormat <- getUI8
       defineBitsLossless2_bitmapWidth <- getUI16
       defineBitsLossless2_bitmapHeight <- getUI16
       defineBitsLossless2_bitmapColorTableSize <- maybeHas
                                                     (defineBitsLossless2_bitmapFormat == 3)
                                                     getUI8
       defineBitsLossless2_zlibBitmapData <- getRemainingLazyByteString
       return (DefineBitsLossless2{..})
putDefineBitsLossless2 DefineBitsLossless2{..}
  = do putUI16 defineBitsLossless2_characterID
       putUI8 defineBitsLossless2_bitmapFormat
       putUI16 defineBitsLossless2_bitmapWidth
       putUI16 defineBitsLossless2_bitmapHeight
       case defineBitsLossless2_bitmapColorTableSize of
           Just x | defineBitsLossless2_bitmapFormat == 3 -> putUI8 x
                  | otherwise ->
                    inconsistent
                      "defineBitsLossless2_bitmapColorTableSize (x :: DefineBitsLossless2)"
                      "Should have a Just iff defineBitsLossless2_bitmapFormat == 3 is True"
           Nothing | defineBitsLossless2_bitmapFormat == 3 ->
                     inconsistent
                       "defineBitsLossless2_bitmapColorTableSize (x :: DefineBitsLossless2)"
                       "Should have a Nothing iff defineBitsLossless2_bitmapFormat == 3 is False"
                   | otherwise -> return ()
       putLazyByteString defineBitsLossless2_zlibBitmapData
       return ()

\end{code}

p154: DefineBitsJPEG4
\begin{code}
getDefineBitsJPEG4
  = do defineBitsJPEG4_characterID <- getUI16
       defineBitsJPEG4_alphaDataOffset <- getUI32
       defineBitsJPEG4_deblockParam <- getUI16
       defineBitsJPEG4_imageData <- genericReplicateM
                                      defineBitsJPEG4_alphaDataOffset
                                      getUI8
       defineBitsJPEG4_bitmapAlphaData <- getRemainingLazyByteString
       return (DefineBitsJPEG4{..})
putDefineBitsJPEG4 DefineBitsJPEG4{..}
  = do putUI16 defineBitsJPEG4_characterID
       let defineBitsJPEG4_alphaDataOffset
             = genericLength defineBitsJPEG4_imageData
       putUI32 defineBitsJPEG4_alphaDataOffset
       putUI16 defineBitsJPEG4_deblockParam
       if
         genericLength defineBitsJPEG4_imageData /=
           (defineBitsJPEG4_alphaDataOffset)
         then
         inconsistent "defineBitsJPEG4_imageData (x :: DefineBitsJPEG4)"
           ("Mismatch with the required length: defineBitsJPEG4_alphaDataOffset"
              ++
              show (genericLength defineBitsJPEG4_imageData) ++
                " /= " ++ show defineBitsJPEG4_alphaDataOffset)
         else mapM_ (\ x -> putUI8 x) defineBitsJPEG4_imageData
       putLazyByteString defineBitsJPEG4_bitmapAlphaData
       return ()

\end{code}


Chapter 9: Shape Morphing
~~~~~~~~~~~~~~~~~~~~~~~~~

p159: DefineMorphShape
\begin{code}
getDefineMorphShape
  = do defineMorphShape_characterId <- getUI16
       defineMorphShape_startBounds <- getRECT
       defineMorphShape_endBounds <- getRECT
       defineMorphShape_offset <- getUI32
       defineMorphShape_morphFillStyles <- getMORPHFILLSTYLEARRAY
       defineMorphShape_morphLineStyles <- getMORPHLINESTYLEARRAY 1
       defineMorphShape_startEdges <- getSHAPE 3
       defineMorphShape_endEdges <- getSHAPE 3
       return (DefineMorphShape{..})
putDefineMorphShape DefineMorphShape{..}
  = do putUI16 defineMorphShape_characterId
       putRECT defineMorphShape_startBounds
       putRECT defineMorphShape_endBounds
       putUI32 defineMorphShape_offset
       putMORPHFILLSTYLEARRAY defineMorphShape_morphFillStyles
       putMORPHLINESTYLEARRAY 1 defineMorphShape_morphLineStyles
       putSHAPE 3 defineMorphShape_startEdges
       putSHAPE 3 defineMorphShape_endEdges
       return ()

\end{code}

p161: DefineMorphShape2
\begin{code}
getDefineMorphShape2
  = do defineMorphShape2_characterId <- getUI16
       defineMorphShape2_startBounds <- getRECT
       defineMorphShape2_endBounds <- getRECT
       defineMorphShape2_startEdgeBounds <- getRECT
       defineMorphShape2_endEdgeBounds <- getRECT
       discardReserved "_reserved (x :: ?)" (getUB 6)
       defineMorphShape2_usesNonScalingStrokes <- getFlag
       defineMorphShape2_usesScalingStrokes <- getFlag
       defineMorphShape2_offset <- getUI32
       defineMorphShape2_morphFillStyles <- getMORPHFILLSTYLEARRAY
       defineMorphShape2_morphLineStyles <- getMORPHLINESTYLEARRAY 2
       defineMorphShape2_startEdges <- getSHAPE 3
       defineMorphShape2_endEdges <- getSHAPE 3
       return (DefineMorphShape2{..})
putDefineMorphShape2 DefineMorphShape2{..}
  = do putUI16 defineMorphShape2_characterId
       putRECT defineMorphShape2_startBounds
       putRECT defineMorphShape2_endBounds
       putRECT defineMorphShape2_startEdgeBounds
       putRECT defineMorphShape2_endEdgeBounds
       let defineMorphShape2_reserved = reservedDefault
       if requiredBitsUB defineMorphShape2_reserved <= 6 then
         putUB 6 defineMorphShape2_reserved else
         inconsistent "x :: DefineMorphShape2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineMorphShape2_reserved) ++
                " bits to store the value " ++
                  show defineMorphShape2_reserved ++
                    ", but only have available " ++ show 6)
       putFlag defineMorphShape2_usesNonScalingStrokes
       putFlag defineMorphShape2_usesScalingStrokes
       putUI32 defineMorphShape2_offset
       putMORPHFILLSTYLEARRAY defineMorphShape2_morphFillStyles
       putMORPHLINESTYLEARRAY 2 defineMorphShape2_morphLineStyles
       putSHAPE 3 defineMorphShape2_startEdges
       putSHAPE 3 defineMorphShape2_endEdges
       return ()

\end{code}

p163: Morph fill styles
\begin{code}

type MORPHFILLSTYLEARRAY = [MORPHFILLSTYLE]

getMORPHFILLSTYLEARRAY = do
    count <- getUI8
    count <- if count == 0xFF then getUI16 else return (fromIntegral count)
    genericReplicateM count getMORPHFILLSTYLE

putMORPHFILLSTYLEARRAY mfs = do
    let count = length mfs
    putUI8 (fromIntegral $ count `min` 0xFF)
    when (count >= 0xFF) $ putUI16 (fromIntegral count)
    mapM_ putMORPHFILLSTYLE mfs

data MORPHFILLSTYLE = SolidMorphFill { mORPHFILLSTYLE_startColor :: RGBA, mORPHFILLSTYLE_endColor :: RGBA }
                    | LinearGradientMorphFill { mORPHFILLSTYLE_linearRadial :: LinearRadial, mORPHFILLSTYLE_startGradientMatrix :: MATRIX, mORPHFILLSTYLE_endGradientMatrix :: MATRIX, mORPHFILLSTYLE_gradient :: MORPHGRADIENT }
                    | BitmapMorphFill { mORPHFILLSTYLE_repeatingClipped :: RepeatingClipped, mORPHFILLSTYLE_smoothed :: Bool, mORPHFILLSTYLE_bitmapId :: UI16, mORPHFILLSTYLE_startBitmapMatrix :: MATRIX, mORPHFILLSTYLE_endBitmapMatrix :: MATRIX }
                    deriving (Eq, Show, Typeable, Data)

getMORPHFILLSTYLE = do
    fillStyleType <- getUI8
    case fillStyleType of
      0x00 -> liftM2 SolidMorphFill getRGBA getRGBA
      0x10 -> liftM3 (LinearGradientMorphFill Linear) getMATRIX getMATRIX getMORPHGRADIENT
      0x12 -> liftM3 (LinearGradientMorphFill Radial) getMATRIX getMATRIX getMORPHGRADIENT
      0x40 -> liftM3 (BitmapMorphFill Repeating True) getUI16 getMATRIX getMATRIX
      0x41 -> liftM3 (BitmapMorphFill Clipped   True) getUI16 getMATRIX getMATRIX
      0x42 -> liftM3 (BitmapMorphFill Repeating False) getUI16 getMATRIX getMATRIX
      0x43 -> liftM3 (BitmapMorphFill Clipped   False) getUI16 getMATRIX getMATRIX

putMORPHFILLSTYLE mfs = case mfs of
    SolidMorphFill {..}          -> do
        putUI8 0x00
        putRGBA mORPHFILLSTYLE_startColor
        putRGBA mORPHFILLSTYLE_endColor
    LinearGradientMorphFill {..} -> do
        putUI8 (case mORPHFILLSTYLE_linearRadial of Linear -> 0x10; Radial -> 0x12)
        putMATRIX mORPHFILLSTYLE_startGradientMatrix
        putMATRIX mORPHFILLSTYLE_endGradientMatrix
        putMORPHGRADIENT mORPHFILLSTYLE_gradient
    BitmapMorphFill {..} -> do
        putUI8 (case (mORPHFILLSTYLE_repeatingClipped, mORPHFILLSTYLE_smoothed) of (Repeating, True) -> 0x40; (Clipped, True) -> 0x41; (Repeating, False) -> 0x42; (Clipped, False) -> 0x43)
        putUI16 mORPHFILLSTYLE_bitmapId
        putMATRIX mORPHFILLSTYLE_startBitmapMatrix
        putMATRIX mORPHFILLSTYLE_endBitmapMatrix

\end{code}

p163: Morph gradient values
\begin{code}

type MORPHGRADIENT = [MORPHGRADRECORD]

getMORPHGRADIENT = do
    count <- getUI8
    genericReplicateM count getMORPHGRADRECORD

putMORPHGRADIENT ms = do
    putUI8 (genericLength ms)
    mapM_ putMORPHGRADRECORD ms

\end{code}

\begin{code}
 
data MORPHGRADRECORD = MORPHGRADRECORD{mORPHGRADRECORD_startRatio
                                       :: UI8,
                                       mORPHGRADRECORD_startColor :: RGBA,
                                       mORPHGRADRECORD_endRatio :: UI8,
                                       mORPHGRADRECORD_endColor :: RGBA}
                     deriving (Eq, Show, Typeable, Data)
getMORPHGRADRECORD
  = do mORPHGRADRECORD_startRatio <- getUI8
       mORPHGRADRECORD_startColor <- getRGBA
       mORPHGRADRECORD_endRatio <- getUI8
       mORPHGRADRECORD_endColor <- getRGBA
       return (MORPHGRADRECORD{..})
putMORPHGRADRECORD MORPHGRADRECORD{..}
  = do putUI8 mORPHGRADRECORD_startRatio
       putRGBA mORPHGRADRECORD_startColor
       putUI8 mORPHGRADRECORD_endRatio
       putRGBA mORPHGRADRECORD_endColor
       return ()

\end{code}

p165: Morph line styles
\begin{code}

type MORPHLINESTYLEARRAY = Either [MORPHLINESTYLE] [MORPHLINESTYLE2]

getMORPHLINESTYLEARRAY morphVersion = do
    count <- getUI8
    count <- if count == 0xFF then getUI16 else return (fromIntegral count)
    case morphVersion of
      1 -> fmap Left  $ genericReplicateM count getMORPHLINESTYLE
      2 -> fmap Right $ genericReplicateM count getMORPHLINESTYLE2

putMORPHLINESTYLEARRAY morphVersion ei_mls_mls2 = do
    let count = either genericLength genericLength ei_mls_mls2
    putUI8 (fromIntegral $ count `min` 0xFF)
    when (count >= 0xFF) $ putUI16 count
    case ei_mls_mls2 of
        Left mls | morphVersion == 1   -> mapM_ putMORPHLINESTYLE mls
                 | otherwise           -> inconsistent "x :: MORPHLINESTYLEARRAY" "Must use MORPHLINESTYLE2 rather than MORPHLINESTYLE1 for morph version 2"
        Right mls2 | morphVersion == 2 -> mapM_ putMORPHLINESTYLE2 mls2
                   | otherwise         -> inconsistent "x :: MORPHLINESTYLEARRAY" "Must use MORPHLINESTYLE rather than MORPHLINESTYLE2 for morph version 1"

\end{code}

\begin{code}
 
data MORPHLINESTYLE = MORPHLINESTYLE{mORPHLINESTYLE_startWidth ::
                                     UI16,
                                     mORPHLINESTYLE_endWidth :: UI16,
                                     mORPHLINESTYLE_startColor :: RGBA,
                                     mORPHLINESTYLE_endColor :: RGBA}
                    deriving (Eq, Show, Typeable, Data)
getMORPHLINESTYLE
  = do mORPHLINESTYLE_startWidth <- getUI16
       mORPHLINESTYLE_endWidth <- getUI16
       mORPHLINESTYLE_startColor <- getRGBA
       mORPHLINESTYLE_endColor <- getRGBA
       return (MORPHLINESTYLE{..})
putMORPHLINESTYLE MORPHLINESTYLE{..}
  = do putUI16 mORPHLINESTYLE_startWidth
       putUI16 mORPHLINESTYLE_endWidth
       putRGBA mORPHLINESTYLE_startColor
       putRGBA mORPHLINESTYLE_endColor
       return ()

\end{code}

\begin{code}
 
data MORPHLINESTYLE2 = MORPHLINESTYLE2{mORPHLINESTYLE2_startWidth
                                       :: UI16,
                                       mORPHLINESTYLE2_endWidth :: UI16,
                                       mORPHLINESTYLE2_startCapStyle :: UB,
                                       mORPHLINESTYLE2_joinStyle :: UB,
                                       mORPHLINESTYLE2_noHScaleFlag :: Bool,
                                       mORPHLINESTYLE2_noVScaleFlag :: Bool,
                                       mORPHLINESTYLE2_pixelHintingFlag :: Bool,
                                       mORPHLINESTYLE2_noClose :: Bool,
                                       mORPHLINESTYLE2_endCapStyle :: UB,
                                       mORPHLINESTYLE2_miterLimitFactor :: Maybe UI16,
                                       mORPHLINESTYLE2_color :: Maybe (RGBA, RGBA),
                                       mORPHLINESTYLE2_fillType :: Maybe MORPHFILLSTYLE}
                     deriving (Eq, Show, Typeable, Data)
getMORPHLINESTYLE2
  = do mORPHLINESTYLE2_startWidth <- getUI16
       mORPHLINESTYLE2_endWidth <- getUI16
       mORPHLINESTYLE2_startCapStyle <- getUB 2
       mORPHLINESTYLE2_joinStyle <- getUB 2
       mORPHLINESTYLE2_hasFillFlag <- getFlag
       mORPHLINESTYLE2_noHScaleFlag <- getFlag
       mORPHLINESTYLE2_noVScaleFlag <- getFlag
       mORPHLINESTYLE2_pixelHintingFlag <- getFlag
       discardReserved "_reserved (x :: ?)" (getUB 5)
       mORPHLINESTYLE2_noClose <- getFlag
       mORPHLINESTYLE2_endCapStyle <- getUB 2
       mORPHLINESTYLE2_miterLimitFactor <- maybeHas
                                             (mORPHLINESTYLE2_joinStyle == 2)
                                             getUI16
       mORPHLINESTYLE2_color <- maybeHas (not mORPHLINESTYLE2_hasFillFlag)
                                  (do mORPHLINESTYLE2_startColor <- getRGBA
                                      mORPHLINESTYLE2_endColor <- getRGBA
                                      return (mORPHLINESTYLE2_startColor, mORPHLINESTYLE2_endColor))
       mORPHLINESTYLE2_fillType <- maybeHas mORPHLINESTYLE2_hasFillFlag
                                     getMORPHFILLSTYLE
       return (MORPHLINESTYLE2{..})
putMORPHLINESTYLE2 MORPHLINESTYLE2{..}
  = do putUI16 mORPHLINESTYLE2_startWidth
       putUI16 mORPHLINESTYLE2_endWidth
       if requiredBitsUB mORPHLINESTYLE2_startCapStyle <= 2 then
         putUB 2 mORPHLINESTYLE2_startCapStyle else
         inconsistent "mORPHLINESTYLE2_startCapStyle (x :: MORPHLINESTYLE2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB mORPHLINESTYLE2_startCapStyle) ++
                " bits to store the value " ++
                  show mORPHLINESTYLE2_startCapStyle ++
                    ", but only have available " ++ show 2)
       if requiredBitsUB mORPHLINESTYLE2_joinStyle <= 2 then
         putUB 2 mORPHLINESTYLE2_joinStyle else
         inconsistent "mORPHLINESTYLE2_joinStyle (x :: MORPHLINESTYLE2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB mORPHLINESTYLE2_joinStyle) ++
                " bits to store the value " ++
                  show mORPHLINESTYLE2_joinStyle ++
                    ", but only have available " ++ show 2)
       let mORPHLINESTYLE2_hasFillFlag = isJust mORPHLINESTYLE2_fillType
       putFlag mORPHLINESTYLE2_hasFillFlag
       putFlag mORPHLINESTYLE2_noHScaleFlag
       putFlag mORPHLINESTYLE2_noVScaleFlag
       putFlag mORPHLINESTYLE2_pixelHintingFlag
       let mORPHLINESTYLE2_reserved = reservedDefault
       if requiredBitsUB mORPHLINESTYLE2_reserved <= 5 then
         putUB 5 mORPHLINESTYLE2_reserved else
         inconsistent "x :: MORPHLINESTYLE2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB mORPHLINESTYLE2_reserved) ++
                " bits to store the value " ++
                  show mORPHLINESTYLE2_reserved ++
                    ", but only have available " ++ show 5)
       putFlag mORPHLINESTYLE2_noClose
       if requiredBitsUB mORPHLINESTYLE2_endCapStyle <= 2 then
         putUB 2 mORPHLINESTYLE2_endCapStyle else
         inconsistent "mORPHLINESTYLE2_endCapStyle (x :: MORPHLINESTYLE2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB mORPHLINESTYLE2_endCapStyle) ++
                " bits to store the value " ++
                  show mORPHLINESTYLE2_endCapStyle ++
                    ", but only have available " ++ show 2)
       case mORPHLINESTYLE2_miterLimitFactor of
           Just x | mORPHLINESTYLE2_joinStyle == 2 -> putUI16 x
                  | otherwise ->
                    inconsistent
                      "mORPHLINESTYLE2_miterLimitFactor (x :: MORPHLINESTYLE2)"
                      "Should have a Just iff mORPHLINESTYLE2_joinStyle == 2 is True"
           Nothing | mORPHLINESTYLE2_joinStyle == 2 ->
                     inconsistent
                       "mORPHLINESTYLE2_miterLimitFactor (x :: MORPHLINESTYLE2)"
                       "Should have a Nothing iff mORPHLINESTYLE2_joinStyle == 2 is False"
                   | otherwise -> return ()
       case mORPHLINESTYLE2_color of
           Just x | not mORPHLINESTYLE2_hasFillFlag ->
                    case x of
                        (mORPHLINESTYLE2_startColor,
                         mORPHLINESTYLE2_endColor) -> do putRGBA mORPHLINESTYLE2_startColor
                                                         putRGBA mORPHLINESTYLE2_endColor
                                                         return ()
                  | otherwise ->
                    inconsistent "mORPHLINESTYLE2_color (x :: MORPHLINESTYLE2)"
                      "Should have a Just iff not mORPHLINESTYLE2_hasFillFlag is True"
           Nothing | not mORPHLINESTYLE2_hasFillFlag ->
                     inconsistent "mORPHLINESTYLE2_color (x :: MORPHLINESTYLE2)"
                       "Should have a Nothing iff not mORPHLINESTYLE2_hasFillFlag is False"
                   | otherwise -> return ()
       case mORPHLINESTYLE2_fillType of
           Just x | mORPHLINESTYLE2_hasFillFlag -> putMORPHFILLSTYLE x
                  | otherwise ->
                    inconsistent "mORPHLINESTYLE2_fillType (x :: MORPHLINESTYLE2)"
                      "Should have a Just iff mORPHLINESTYLE2_hasFillFlag is True"
           Nothing | mORPHLINESTYLE2_hasFillFlag ->
                     inconsistent "mORPHLINESTYLE2_fillType (x :: MORPHLINESTYLE2)"
                       "Should have a Nothing iff mORPHLINESTYLE2_hasFillFlag is False"
                   | otherwise -> return ()
       return ()

\end{code}


Chapter 10: Fonts and Text
~~~~~~~~~~~~~~~~~~~~~~~~~~

p176: DefineFont
\begin{code}

getDefineFont = do
    defineFont_fontID <- getUI16
    
    offset0 <- getUI16
    let nGlyphs = offset0 `div` 2
    _offsets <- fmap (offset0:) $ genericReplicateM (nGlyphs - 1) getUI16
    
    defineFont_glyphShapeTable <- genericReplicateM nGlyphs (getSHAPE 1)
    return $ DefineFont {..}

putDefineFont (DefineFont {..}) = do
    putUI16 defineFont_fontID
    
    (lengths, shape_puts) <- fmap unzip $ mapM (nestSwfPut . putSHAPE 1) defineFont_glyphShapeTable
    
    -- Compute the offsets from the data, so we don't need to redundantly store them in the data structure
    _ <- foldM (\i len -> putUI16 i >> return (i + len)) 0 lengths
    sequence_ shape_puts

\end{code}

p177: DefineFontInfo
\begin{code}
getDefineFontInfo
  = do defineFontInfo_fontID <- getUI16
       defineFontInfo_fontNameLen <- getUI8
       defineFontInfo_fontName <- genericReplicateM
                                    defineFontInfo_fontNameLen
                                    getUI8
       discardReserved "_reserved (x :: ?)" (getUB 2)
       defineFontInfo_fontFlagsSmallText <- getFlag
       defineFontInfo_fontFlagsShiftJIS <- getFlag
       defineFontInfo_fontFlagsANSI <- getFlag
       defineFontInfo_fontFlagsItalic <- getFlag
       defineFontInfo_fontFlagsBold <- getFlag
       defineFontInfo_fontFlagsWideCodes <- getFlag
       defineFontInfo_codeTable <- if defineFontInfo_fontFlagsWideCodes
                                     then fmap Left (getToEnd getUI16) else
                                     fmap Right (getToEnd getUI8)
       return (DefineFontInfo{..})
putDefineFontInfo DefineFontInfo{..}
  = do putUI16 defineFontInfo_fontID
       let defineFontInfo_fontNameLen
             = genericLength defineFontInfo_fontName
       putUI8 defineFontInfo_fontNameLen
       if
         genericLength defineFontInfo_fontName /=
           (defineFontInfo_fontNameLen)
         then
         inconsistent "defineFontInfo_fontName (x :: DefineFontInfo)"
           ("Mismatch with the required length: defineFontInfo_fontNameLen" ++
              show (genericLength defineFontInfo_fontName) ++
                " /= " ++ show defineFontInfo_fontNameLen)
         else mapM_ (\ x -> putUI8 x) defineFontInfo_fontName
       let defineFontInfo_fontFlagsReserved = reservedDefault
       if requiredBitsUB defineFontInfo_fontFlagsReserved <= 2 then
         putUB 2 defineFontInfo_fontFlagsReserved else
         inconsistent "x :: DefineFontInfo"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineFontInfo_fontFlagsReserved) ++
                " bits to store the value " ++
                  show defineFontInfo_fontFlagsReserved ++
                    ", but only have available " ++ show 2)
       putFlag defineFontInfo_fontFlagsSmallText
       putFlag defineFontInfo_fontFlagsShiftJIS
       putFlag defineFontInfo_fontFlagsANSI
       putFlag defineFontInfo_fontFlagsItalic
       putFlag defineFontInfo_fontFlagsBold
       let defineFontInfo_fontFlagsWideCodes
             = isLeft defineFontInfo_codeTable
       putFlag defineFontInfo_fontFlagsWideCodes
       case defineFontInfo_codeTable of
           Left x | defineFontInfo_fontFlagsWideCodes ->
                    mapM_ (\ x -> putUI16 x) x
                  | otherwise ->
                    inconsistent "defineFontInfo_codeTable (x :: DefineFontInfo)"
                      "Should have a Left iff defineFontInfo_fontFlagsWideCodes is True"
           Right x | defineFontInfo_fontFlagsWideCodes ->
                     inconsistent "defineFontInfo_codeTable (x :: DefineFontInfo)"
                       "Should have a Right iff defineFontInfo_fontFlagsWideCodes is False"
                   | otherwise -> mapM_ (\ x -> putUI8 x) x
       return ()

\end{code}

p180: DefineFontInfo2
\begin{code}
getDefineFontInfo2
  = do defineFontInfo2_fontID <- getUI16
       defineFontInfo2_fontNameLen <- getUI8
       defineFontInfo2_fontName <- genericReplicateM
                                     defineFontInfo2_fontNameLen
                                     getUI8
       discardReserved "_reserved (x :: ?)" (getUB 2)
       defineFontInfo2_fontFlagsSmallText <- getFlag
       defineFontInfo2_fontFlagsShiftJIS <- getFlag
       defineFontInfo2_fontFlagsANSI <- getFlag
       defineFontInfo2_fontFlagsItalic <- getFlag
       defineFontInfo2_fontFlagsBold <- getFlag
       defineFontInfo2_fontFlagsWideCodes <- getFlag
       defineFontInfo2_languageCode <- getLANGCODE
       defineFontInfo2_codeTable <- getToEnd getUI16
       return (DefineFontInfo2{..})
putDefineFontInfo2 DefineFontInfo2{..}
  = do putUI16 defineFontInfo2_fontID
       let defineFontInfo2_fontNameLen
             = genericLength defineFontInfo2_fontName
       putUI8 defineFontInfo2_fontNameLen
       if
         genericLength defineFontInfo2_fontName /=
           (defineFontInfo2_fontNameLen)
         then
         inconsistent "defineFontInfo2_fontName (x :: DefineFontInfo2)"
           ("Mismatch with the required length: defineFontInfo2_fontNameLen"
              ++
              show (genericLength defineFontInfo2_fontName) ++
                " /= " ++ show defineFontInfo2_fontNameLen)
         else mapM_ (\ x -> putUI8 x) defineFontInfo2_fontName
       let defineFontInfo2_fontFlagsReserved = reservedDefault
       if requiredBitsUB defineFontInfo2_fontFlagsReserved <= 2 then
         putUB 2 defineFontInfo2_fontFlagsReserved else
         inconsistent "x :: DefineFontInfo2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineFontInfo2_fontFlagsReserved) ++
                " bits to store the value " ++
                  show defineFontInfo2_fontFlagsReserved ++
                    ", but only have available " ++ show 2)
       putFlag defineFontInfo2_fontFlagsSmallText
       putFlag defineFontInfo2_fontFlagsShiftJIS
       putFlag defineFontInfo2_fontFlagsANSI
       putFlag defineFontInfo2_fontFlagsItalic
       putFlag defineFontInfo2_fontFlagsBold
       putFlag defineFontInfo2_fontFlagsWideCodes
       putLANGCODE defineFontInfo2_languageCode
       mapM_ (\ x -> putUI16 x) defineFontInfo2_codeTable
       return ()

\end{code}

p181: DefineFont2
\begin{code}
getDefineFont2
  = do defineFont2_fontID <- getUI16
       defineFont2_fontFlagsHasLayout <- getFlag
       defineFont2_fontFlagsShiftJIS <- getFlag
       defineFont2_fontFlagsSmallText <- getFlag
       defineFont2_fontFlagsANSI <- getFlag
       defineFont2_fontFlagsWideOffsets <- getFlag
       defineFont2_fontFlagsWideCodes <- getFlag
       defineFont2_fontFlagsItalic <- getFlag
       defineFont2_fontFlagsBold <- getFlag
       defineFont2_languageCode <- getLANGCODE
       defineFont2_fontNameLen <- getUI8
       defineFont2_fontName <- genericReplicateM defineFont2_fontNameLen
                                 getUI8
       defineFont2_numGlyphs <- getUI16
       defineFont2_offsetTable <- if defineFont2_fontFlagsWideOffsets then
                                    fmap Left (genericReplicateM defineFont2_numGlyphs getUI32) else
                                    fmap Right (genericReplicateM defineFont2_numGlyphs getUI16)
       defineFont2_codeTableOffset <- if defineFont2_fontFlagsWideOffsets
                                        then fmap Left getUI32 else fmap Right getUI16
       defineFont2_glyphShapeTable <- genericReplicateM
                                        defineFont2_numGlyphs
                                        (getSHAPE 3)
       defineFont2_codeTable <- if defineFont2_fontFlagsWideCodes then
                                  fmap Left (genericReplicateM defineFont2_numGlyphs getUI16) else
                                  fmap Right (genericReplicateM defineFont2_numGlyphs getUI8)
       defineFont2_fontLayout <- maybeHas defineFont2_fontFlagsHasLayout
                                   (do defineFont2_fontLayoutAscent <- getSI16
                                       defineFont2_fontLayoutDescent <- getSI16
                                       defineFont2_fontLayoutLeading <- getSI16
                                       defineFont2_fontLayoutAdvanceTable <- genericReplicateM
                                                                               defineFont2_numGlyphs
                                                                               getSI16
                                       defineFont2_fontLayoutBoundsTable <- genericReplicateM
                                                                              defineFont2_numGlyphs
                                                                              getRECT
                                       defineFont2_fontLayoutKerningCount <- getUI16
                                       defineFont2_fontLayoutKerningTable <- genericReplicateM
                                                                               defineFont2_fontLayoutKerningCount
                                                                               (getKERNINGRECORD
                                                                                  defineFont2_fontFlagsWideCodes)
                                       return
                                         (defineFont2_fontLayoutAscent,
                                          defineFont2_fontLayoutDescent,
                                          defineFont2_fontLayoutLeading,
                                          defineFont2_fontLayoutAdvanceTable,
                                          defineFont2_fontLayoutBoundsTable,
                                          defineFont2_fontLayoutKerningCount,
                                          defineFont2_fontLayoutKerningTable))
       return (DefineFont2{..})
putDefineFont2 DefineFont2{..}
  = do putUI16 defineFont2_fontID
       let defineFont2_fontFlagsHasLayout = isJust defineFont2_fontLayout
       putFlag defineFont2_fontFlagsHasLayout
       putFlag defineFont2_fontFlagsShiftJIS
       putFlag defineFont2_fontFlagsSmallText
       putFlag defineFont2_fontFlagsANSI
       let defineFont2_fontFlagsWideOffsets
             = isLeft defineFont2_offsetTable
       putFlag defineFont2_fontFlagsWideOffsets
       let defineFont2_fontFlagsWideCodes = isLeft defineFont2_codeTable
       putFlag defineFont2_fontFlagsWideCodes
       putFlag defineFont2_fontFlagsItalic
       putFlag defineFont2_fontFlagsBold
       putLANGCODE defineFont2_languageCode
       let defineFont2_fontNameLen = genericLength defineFont2_fontName
       putUI8 defineFont2_fontNameLen
       if genericLength defineFont2_fontName /= (defineFont2_fontNameLen)
         then
         inconsistent "defineFont2_fontName (x :: DefineFont2)"
           ("Mismatch with the required length: defineFont2_fontNameLen" ++
              show (genericLength defineFont2_fontName) ++
                " /= " ++ show defineFont2_fontNameLen)
         else mapM_ (\ x -> putUI8 x) defineFont2_fontName
       let defineFont2_numGlyphs
             = genericLength defineFont2_glyphShapeTable
       putUI16 defineFont2_numGlyphs
       case defineFont2_offsetTable of
           Left x | defineFont2_fontFlagsWideOffsets ->
                    if genericLength x /= (defineFont2_numGlyphs) then
                      inconsistent
                        "fromLeft (defineFont2_offsetTable (x :: DefineFont2))"
                        ("Mismatch with the required length: defineFont2_numGlyphs" ++
                           show (genericLength x) ++ " /= " ++ show defineFont2_numGlyphs)
                      else mapM_ (\ x -> putUI32 x) x
                  | otherwise ->
                    inconsistent "defineFont2_offsetTable (x :: DefineFont2)"
                      "Should have a Left iff defineFont2_fontFlagsWideOffsets is True"
           Right x | defineFont2_fontFlagsWideOffsets ->
                     inconsistent "defineFont2_offsetTable (x :: DefineFont2)"
                       "Should have a Right iff defineFont2_fontFlagsWideOffsets is False"
                   | otherwise ->
                     if genericLength x /= (defineFont2_numGlyphs) then
                       inconsistent
                         "fromRight (defineFont2_offsetTable (x :: DefineFont2))"
                         ("Mismatch with the required length: defineFont2_numGlyphs" ++
                            show (genericLength x) ++ " /= " ++ show defineFont2_numGlyphs)
                       else mapM_ (\ x -> putUI16 x) x
       case defineFont2_codeTableOffset of
           Left x | defineFont2_fontFlagsWideOffsets -> putUI32 x
                  | otherwise ->
                    inconsistent "defineFont2_codeTableOffset (x :: DefineFont2)"
                      "Should have a Left iff defineFont2_fontFlagsWideOffsets is True"
           Right x | defineFont2_fontFlagsWideOffsets ->
                     inconsistent "defineFont2_codeTableOffset (x :: DefineFont2)"
                       "Should have a Right iff defineFont2_fontFlagsWideOffsets is False"
                   | otherwise -> putUI16 x
       if
         genericLength defineFont2_glyphShapeTable /=
           (defineFont2_numGlyphs)
         then
         inconsistent "defineFont2_glyphShapeTable (x :: DefineFont2)"
           ("Mismatch with the required length: defineFont2_numGlyphs" ++
              show (genericLength defineFont2_glyphShapeTable) ++
                " /= " ++ show defineFont2_numGlyphs)
         else mapM_ (\ x -> putSHAPE 3 x) defineFont2_glyphShapeTable
       case defineFont2_codeTable of
           Left x | defineFont2_fontFlagsWideCodes ->
                    if genericLength x /= (defineFont2_numGlyphs) then
                      inconsistent "fromLeft (defineFont2_codeTable (x :: DefineFont2))"
                        ("Mismatch with the required length: defineFont2_numGlyphs" ++
                           show (genericLength x) ++ " /= " ++ show defineFont2_numGlyphs)
                      else mapM_ (\ x -> putUI16 x) x
                  | otherwise ->
                    inconsistent "defineFont2_codeTable (x :: DefineFont2)"
                      "Should have a Left iff defineFont2_fontFlagsWideCodes is True"
           Right x | defineFont2_fontFlagsWideCodes ->
                     inconsistent "defineFont2_codeTable (x :: DefineFont2)"
                       "Should have a Right iff defineFont2_fontFlagsWideCodes is False"
                   | otherwise ->
                     if genericLength x /= (defineFont2_numGlyphs) then
                       inconsistent "fromRight (defineFont2_codeTable (x :: DefineFont2))"
                         ("Mismatch with the required length: defineFont2_numGlyphs" ++
                            show (genericLength x) ++ " /= " ++ show defineFont2_numGlyphs)
                       else mapM_ (\ x -> putUI8 x) x
       case defineFont2_fontLayout of
           Just x | defineFont2_fontFlagsHasLayout ->
                    case x of
                        (defineFont2_fontLayoutAscent, defineFont2_fontLayoutDescent,
                         defineFont2_fontLayoutLeading, defineFont2_fontLayoutAdvanceTable,
                         defineFont2_fontLayoutBoundsTable,
                         defineFont2_fontLayoutKerningCount,
                         defineFont2_fontLayoutKerningTable) -> do putSI16
                                                                     defineFont2_fontLayoutAscent
                                                                   putSI16
                                                                     defineFont2_fontLayoutDescent
                                                                   putSI16
                                                                     defineFont2_fontLayoutLeading
                                                                   if
                                                                     genericLength
                                                                       defineFont2_fontLayoutAdvanceTable
                                                                       /= (defineFont2_numGlyphs)
                                                                     then
                                                                     inconsistent
                                                                       "frth (fromJust (defineFont2_fontLayout (x :: DefineFont2)))"
                                                                       ("Mismatch with the required length: defineFont2_numGlyphs"
                                                                          ++
                                                                          show
                                                                            (genericLength
                                                                               defineFont2_fontLayoutAdvanceTable)
                                                                            ++
                                                                            " /= " ++
                                                                              show
                                                                                defineFont2_numGlyphs)
                                                                     else
                                                                     mapM_ (\ x -> putSI16 x)
                                                                       defineFont2_fontLayoutAdvanceTable
                                                                   if
                                                                     genericLength
                                                                       defineFont2_fontLayoutBoundsTable
                                                                       /= (defineFont2_numGlyphs)
                                                                     then
                                                                     inconsistent
                                                                       "ffth (fromJust (defineFont2_fontLayout (x :: DefineFont2)))"
                                                                       ("Mismatch with the required length: defineFont2_numGlyphs"
                                                                          ++
                                                                          show
                                                                            (genericLength
                                                                               defineFont2_fontLayoutBoundsTable)
                                                                            ++
                                                                            " /= " ++
                                                                              show
                                                                                defineFont2_numGlyphs)
                                                                     else
                                                                     mapM_ (\ x -> putRECT x)
                                                                       defineFont2_fontLayoutBoundsTable
                                                                   putUI16
                                                                     defineFont2_fontLayoutKerningCount
                                                                   if
                                                                     genericLength
                                                                       defineFont2_fontLayoutKerningTable
                                                                       /=
                                                                       (defineFont2_fontLayoutKerningCount)
                                                                     then
                                                                     inconsistent
                                                                       "svnth (fromJust (defineFont2_fontLayout (x :: DefineFont2)))"
                                                                       ("Mismatch with the required length: defineFont2_fontLayoutKerningCount"
                                                                          ++
                                                                          show
                                                                            (genericLength
                                                                               defineFont2_fontLayoutKerningTable)
                                                                            ++
                                                                            " /= " ++
                                                                              show
                                                                                defineFont2_fontLayoutKerningCount)
                                                                     else
                                                                     mapM_
                                                                       (\ x ->
                                                                          putKERNINGRECORD
                                                                            defineFont2_fontFlagsWideCodes
                                                                            x)
                                                                       defineFont2_fontLayoutKerningTable
                                                                   return ()
                  | otherwise ->
                    inconsistent "defineFont2_fontLayout (x :: DefineFont2)"
                      "Should have a Just iff defineFont2_fontFlagsHasLayout is True"
           Nothing | defineFont2_fontFlagsHasLayout ->
                     inconsistent "defineFont2_fontLayout (x :: DefineFont2)"
                       "Should have a Nothing iff defineFont2_fontFlagsHasLayout is False"
                   | otherwise -> return ()
       return ()

\end{code}

p184: DefineFont3
\begin{code}
getDefineFont3
  = do defineFont3_fontID <- getUI16
       defineFont3_fontFlagsHasLayout <- getFlag
       defineFont3_fontFlagsShiftJIS <- getFlag
       defineFont3_fontFlagsSmallText <- getFlag
       defineFont3_fontFlagsANSI <- getFlag
       defineFont3_fontFlagsWideOffsets <- getFlag
       defineFont3_fontFlagsWideCodes <- getFlag
       defineFont3_fontFlagsItalic <- getFlag
       defineFont3_fontFlagsBold <- getFlag
       defineFont3_languageCode <- getLANGCODE
       defineFont3_fontNameLen <- getUI8
       defineFont3_fontName <- genericReplicateM defineFont3_fontNameLen
                                 getUI8
       defineFont3_numGlyphs <- getUI16
       defineFont3_offsetTable <- if defineFont3_fontFlagsWideOffsets then
                                    fmap Left (genericReplicateM defineFont3_numGlyphs getUI32) else
                                    fmap Right (genericReplicateM defineFont3_numGlyphs getUI16)
       defineFont3_codeTableOffset <- if defineFont3_fontFlagsWideOffsets
                                        then fmap Left getUI32 else fmap Right getUI16
       defineFont3_glyphShapeTable <- genericReplicateM
                                        defineFont3_numGlyphs
                                        (getSHAPE 4)
       defineFont3_codeTable <- genericReplicateM defineFont3_numGlyphs
                                  getUI16
       defineFont3_fontLayout <- maybeHas defineFont3_fontFlagsHasLayout
                                   (do defineFont3_fontLayoutAscent <- getSI16
                                       defineFont3_fontLayoutDescent <- getSI16
                                       defineFont3_fontLayoutLeading <- getSI16
                                       defineFont3_fontLayoutAdvanceTable <- genericReplicateM
                                                                               defineFont3_numGlyphs
                                                                               getSI16
                                       defineFont3_fontLayoutBoundsTable <- genericReplicateM
                                                                              defineFont3_numGlyphs
                                                                              getRECT
                                       defineFont3_fontLayoutKerningCount <- getUI16
                                       defineFont3_fontLayoutKerningTable <- genericReplicateM
                                                                               defineFont3_fontLayoutKerningCount
                                                                               (getKERNINGRECORD
                                                                                  defineFont3_fontFlagsWideCodes)
                                       return
                                         (defineFont3_fontLayoutAscent,
                                          defineFont3_fontLayoutDescent,
                                          defineFont3_fontLayoutLeading,
                                          defineFont3_fontLayoutAdvanceTable,
                                          defineFont3_fontLayoutBoundsTable,
                                          defineFont3_fontLayoutKerningCount,
                                          defineFont3_fontLayoutKerningTable))
       return (DefineFont3{..})
putDefineFont3 DefineFont3{..}
  = do putUI16 defineFont3_fontID
       let defineFont3_fontFlagsHasLayout = isJust defineFont3_fontLayout
       putFlag defineFont3_fontFlagsHasLayout
       putFlag defineFont3_fontFlagsShiftJIS
       putFlag defineFont3_fontFlagsSmallText
       putFlag defineFont3_fontFlagsANSI
       let defineFont3_fontFlagsWideOffsets
             = isLeft defineFont3_offsetTable
       putFlag defineFont3_fontFlagsWideOffsets
       putFlag defineFont3_fontFlagsWideCodes
       putFlag defineFont3_fontFlagsItalic
       putFlag defineFont3_fontFlagsBold
       putLANGCODE defineFont3_languageCode
       let defineFont3_fontNameLen = genericLength defineFont3_fontName
       putUI8 defineFont3_fontNameLen
       if genericLength defineFont3_fontName /= (defineFont3_fontNameLen)
         then
         inconsistent "defineFont3_fontName (x :: DefineFont3)"
           ("Mismatch with the required length: defineFont3_fontNameLen" ++
              show (genericLength defineFont3_fontName) ++
                " /= " ++ show defineFont3_fontNameLen)
         else mapM_ (\ x -> putUI8 x) defineFont3_fontName
       let defineFont3_numGlyphs
             = genericLength defineFont3_glyphShapeTable
       putUI16 defineFont3_numGlyphs
       case defineFont3_offsetTable of
           Left x | defineFont3_fontFlagsWideOffsets ->
                    if genericLength x /= (defineFont3_numGlyphs) then
                      inconsistent
                        "fromLeft (defineFont3_offsetTable (x :: DefineFont3))"
                        ("Mismatch with the required length: defineFont3_numGlyphs" ++
                           show (genericLength x) ++ " /= " ++ show defineFont3_numGlyphs)
                      else mapM_ (\ x -> putUI32 x) x
                  | otherwise ->
                    inconsistent "defineFont3_offsetTable (x :: DefineFont3)"
                      "Should have a Left iff defineFont3_fontFlagsWideOffsets is True"
           Right x | defineFont3_fontFlagsWideOffsets ->
                     inconsistent "defineFont3_offsetTable (x :: DefineFont3)"
                       "Should have a Right iff defineFont3_fontFlagsWideOffsets is False"
                   | otherwise ->
                     if genericLength x /= (defineFont3_numGlyphs) then
                       inconsistent
                         "fromRight (defineFont3_offsetTable (x :: DefineFont3))"
                         ("Mismatch with the required length: defineFont3_numGlyphs" ++
                            show (genericLength x) ++ " /= " ++ show defineFont3_numGlyphs)
                       else mapM_ (\ x -> putUI16 x) x
       case defineFont3_codeTableOffset of
           Left x | defineFont3_fontFlagsWideOffsets -> putUI32 x
                  | otherwise ->
                    inconsistent "defineFont3_codeTableOffset (x :: DefineFont3)"
                      "Should have a Left iff defineFont3_fontFlagsWideOffsets is True"
           Right x | defineFont3_fontFlagsWideOffsets ->
                     inconsistent "defineFont3_codeTableOffset (x :: DefineFont3)"
                       "Should have a Right iff defineFont3_fontFlagsWideOffsets is False"
                   | otherwise -> putUI16 x
       if
         genericLength defineFont3_glyphShapeTable /=
           (defineFont3_numGlyphs)
         then
         inconsistent "defineFont3_glyphShapeTable (x :: DefineFont3)"
           ("Mismatch with the required length: defineFont3_numGlyphs" ++
              show (genericLength defineFont3_glyphShapeTable) ++
                " /= " ++ show defineFont3_numGlyphs)
         else mapM_ (\ x -> putSHAPE 4 x) defineFont3_glyphShapeTable
       if genericLength defineFont3_codeTable /= (defineFont3_numGlyphs)
         then
         inconsistent "defineFont3_codeTable (x :: DefineFont3)"
           ("Mismatch with the required length: defineFont3_numGlyphs" ++
              show (genericLength defineFont3_codeTable) ++
                " /= " ++ show defineFont3_numGlyphs)
         else mapM_ (\ x -> putUI16 x) defineFont3_codeTable
       case defineFont3_fontLayout of
           Just x | defineFont3_fontFlagsHasLayout ->
                    case x of
                        (defineFont3_fontLayoutAscent, defineFont3_fontLayoutDescent,
                         defineFont3_fontLayoutLeading, defineFont3_fontLayoutAdvanceTable,
                         defineFont3_fontLayoutBoundsTable,
                         defineFont3_fontLayoutKerningCount,
                         defineFont3_fontLayoutKerningTable) -> do putSI16
                                                                     defineFont3_fontLayoutAscent
                                                                   putSI16
                                                                     defineFont3_fontLayoutDescent
                                                                   putSI16
                                                                     defineFont3_fontLayoutLeading
                                                                   if
                                                                     genericLength
                                                                       defineFont3_fontLayoutAdvanceTable
                                                                       /= (defineFont3_numGlyphs)
                                                                     then
                                                                     inconsistent
                                                                       "frth (fromJust (defineFont3_fontLayout (x :: DefineFont3)))"
                                                                       ("Mismatch with the required length: defineFont3_numGlyphs"
                                                                          ++
                                                                          show
                                                                            (genericLength
                                                                               defineFont3_fontLayoutAdvanceTable)
                                                                            ++
                                                                            " /= " ++
                                                                              show
                                                                                defineFont3_numGlyphs)
                                                                     else
                                                                     mapM_ (\ x -> putSI16 x)
                                                                       defineFont3_fontLayoutAdvanceTable
                                                                   if
                                                                     genericLength
                                                                       defineFont3_fontLayoutBoundsTable
                                                                       /= (defineFont3_numGlyphs)
                                                                     then
                                                                     inconsistent
                                                                       "ffth (fromJust (defineFont3_fontLayout (x :: DefineFont3)))"
                                                                       ("Mismatch with the required length: defineFont3_numGlyphs"
                                                                          ++
                                                                          show
                                                                            (genericLength
                                                                               defineFont3_fontLayoutBoundsTable)
                                                                            ++
                                                                            " /= " ++
                                                                              show
                                                                                defineFont3_numGlyphs)
                                                                     else
                                                                     mapM_ (\ x -> putRECT x)
                                                                       defineFont3_fontLayoutBoundsTable
                                                                   putUI16
                                                                     defineFont3_fontLayoutKerningCount
                                                                   if
                                                                     genericLength
                                                                       defineFont3_fontLayoutKerningTable
                                                                       /=
                                                                       (defineFont3_fontLayoutKerningCount)
                                                                     then
                                                                     inconsistent
                                                                       "svnth (fromJust (defineFont3_fontLayout (x :: DefineFont3)))"
                                                                       ("Mismatch with the required length: defineFont3_fontLayoutKerningCount"
                                                                          ++
                                                                          show
                                                                            (genericLength
                                                                               defineFont3_fontLayoutKerningTable)
                                                                            ++
                                                                            " /= " ++
                                                                              show
                                                                                defineFont3_fontLayoutKerningCount)
                                                                     else
                                                                     mapM_
                                                                       (\ x ->
                                                                          putKERNINGRECORD
                                                                            defineFont3_fontFlagsWideCodes
                                                                            x)
                                                                       defineFont3_fontLayoutKerningTable
                                                                   return ()
                  | otherwise ->
                    inconsistent "defineFont3_fontLayout (x :: DefineFont3)"
                      "Should have a Just iff defineFont3_fontFlagsHasLayout is True"
           Nothing | defineFont3_fontFlagsHasLayout ->
                     inconsistent "defineFont3_fontLayout (x :: DefineFont3)"
                       "Should have a Nothing iff defineFont3_fontFlagsHasLayout is False"
                   | otherwise -> return ()
       return ()

\end{code}

p186: DefineFontAlignZones
\begin{code}
getDefineFontAlignZones
  = do defineFontAlignZones_fontID <- getUI16
       defineFontAlignZones_cSMTableHint <- getUB 2
       discardReserved "_reserved (x :: ?)" (getUB 6)
       defineFontAlignZones_zoneTable <- getToEnd getZONERECORD
       return (DefineFontAlignZones{..})
putDefineFontAlignZones DefineFontAlignZones{..}
  = do putUI16 defineFontAlignZones_fontID
       if requiredBitsUB defineFontAlignZones_cSMTableHint <= 2 then
         putUB 2 defineFontAlignZones_cSMTableHint else
         inconsistent
           "defineFontAlignZones_cSMTableHint (x :: DefineFontAlignZones)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineFontAlignZones_cSMTableHint) ++
                " bits to store the value " ++
                  show defineFontAlignZones_cSMTableHint ++
                    ", but only have available " ++ show 2)
       let defineFontAlignZones_reserved = reservedDefault
       if requiredBitsUB defineFontAlignZones_reserved <= 6 then
         putUB 6 defineFontAlignZones_reserved else
         inconsistent "x :: DefineFontAlignZones"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineFontAlignZones_reserved) ++
                " bits to store the value " ++
                  show defineFontAlignZones_reserved ++
                    ", but only have available " ++ show 6)
       mapM_ (\ x -> putZONERECORD x) defineFontAlignZones_zoneTable
       return ()

\end{code}

\begin{code}
 
data ZONERECORD = ZONERECORD{zONERECORD_zoneData :: [ZONEDATA],
                             zONERECORD_zoneMaskY :: Bool, zONERECORD_zoneMaskX :: Bool}
                deriving (Eq, Show, Typeable, Data)
getZONERECORD
  = do zONERECORD_numZoneData <- getUI8
       zONERECORD_zoneData <- genericReplicateM zONERECORD_numZoneData
                                getZONEDATA
       discardReserved "_reserved (x :: ?)" (getUB 6)
       zONERECORD_zoneMaskY <- getFlag
       zONERECORD_zoneMaskX <- getFlag
       return (ZONERECORD{..})
putZONERECORD ZONERECORD{..}
  = do let zONERECORD_numZoneData = genericLength zONERECORD_zoneData
       putUI8 zONERECORD_numZoneData
       if genericLength zONERECORD_zoneData /= (zONERECORD_numZoneData)
         then
         inconsistent "zONERECORD_zoneData (x :: ZONERECORD)"
           ("Mismatch with the required length: zONERECORD_numZoneData" ++
              show (genericLength zONERECORD_zoneData) ++
                " /= " ++ show zONERECORD_numZoneData)
         else mapM_ (\ x -> putZONEDATA x) zONERECORD_zoneData
       let zONERECORD_reserved = reservedDefault
       if requiredBitsUB zONERECORD_reserved <= 6 then
         putUB 6 zONERECORD_reserved else
         inconsistent "x :: ZONERECORD"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB zONERECORD_reserved) ++
                " bits to store the value " ++
                  show zONERECORD_reserved ++ ", but only have available " ++ show 6)
       putFlag zONERECORD_zoneMaskY
       putFlag zONERECORD_zoneMaskX
       return ()

\end{code}

\begin{code}
 
data ZONEDATA = ZONEDATA{zONEDATA_alignmentCoordinate :: FLOAT16,
                         zONEDATA_range :: FLOAT16}
              deriving (Eq, Show, Typeable, Data)
getZONEDATA
  = do zONEDATA_alignmentCoordinate <- getFLOAT16
       zONEDATA_range <- getFLOAT16
       return (ZONEDATA{..})
putZONEDATA ZONEDATA{..}
  = do putFLOAT16 zONEDATA_alignmentCoordinate
       putFLOAT16 zONEDATA_range
       return ()

\end{code}

p188: Kerning record
\begin{code}
 
data KERNINGRECORD = KERNINGRECORD{kERNINGRECORD_fontKerningCodes
                                   :: Either (UI16, UI16) (UI8, UI8),
                                   kERNINGRECORD_fontKerningAdjustment :: SI16}
                   deriving (Eq, Show, Typeable, Data)
getKERNINGRECORD kERNINGRECORD_fontFlagsWideCodes
  = do kERNINGRECORD_fontKerningCodes <- if
                                           kERNINGRECORD_fontFlagsWideCodes then
                                           fmap Left (liftM2 (,) getUI16 getUI16) else
                                           fmap Right (liftM2 (,) getUI8 getUI8)
       kERNINGRECORD_fontKerningAdjustment <- getSI16
       return (KERNINGRECORD{..})
putKERNINGRECORD kERNINGRECORD_fontFlagsWideCodes KERNINGRECORD{..}
  = do case kERNINGRECORD_fontKerningCodes of
           Left x | kERNINGRECORD_fontFlagsWideCodes ->
                    case x of
                        (x1, x2) -> do putUI16 x1
                                       putUI16 x2
                  | otherwise ->
                    inconsistent "kERNINGRECORD_fontKerningCodes (x :: KERNINGRECORD)"
                      "Should have a Left iff kERNINGRECORD_fontFlagsWideCodes is True"
           Right x | kERNINGRECORD_fontFlagsWideCodes ->
                     inconsistent "kERNINGRECORD_fontKerningCodes (x :: KERNINGRECORD)"
                       "Should have a Right iff kERNINGRECORD_fontFlagsWideCodes is False"
                   | otherwise ->
                     case x of
                         (x1, x2) -> do putUI8 x1
                                        putUI8 x2
       putSI16 kERNINGRECORD_fontKerningAdjustment
       return ()

\end{code}

p188: DefineFontName
\begin{code}
getDefineFontName
  = do defineFontName_fontID <- getUI16
       defineFontName_fontName <- getSTRING
       defineFontName_fontCopyright <- getSTRING
       return (DefineFontName{..})
putDefineFontName DefineFontName{..}
  = do putUI16 defineFontName_fontID
       putSTRING defineFontName_fontName
       putSTRING defineFontName_fontCopyright
       return ()

\end{code}

p189: DefineText
\begin{code}
getDefineText
  = do defineText_characterID <- getUI16
       defineText_textBounds <- getRECT
       defineText_textMatrix <- getMATRIX
       defineText_glyphBits <- getUI8
       defineText_advanceBits <- getUI8
       defineText_textRecords <- getTEXTRECORDS 1 defineText_glyphBits
                                   defineText_advanceBits
       return (DefineText{..})
putDefineText DefineText{..}
  = do putUI16 defineText_characterID
       putRECT defineText_textBounds
       putMATRIX defineText_textMatrix
       putUI8 defineText_glyphBits
       putUI8 defineText_advanceBits
       putTEXTRECORDS 1 defineText_glyphBits defineText_advanceBits
         defineText_textRecords
       return ()

\end{code}

\begin{code}

type TEXTRECORDS = [TEXTRECORD]

getTEXTRECORDS textVer glyphBits advanceBits = go
  where
    go = do
      look <- lookAhead getUI8
      if look == 0
       then do
         discardKnown "x :: TEXTRECORDS" "TEXTRECORDS array should be 0 terminated" 0 getUI8
         return []
       else do
         x <- getTEXTRECORD textVer glyphBits advanceBits
         fmap (x:) go

putTEXTRECORDS textVer glyphBits advanceBits rs = do
    mapM_ (putTEXTRECORD textVer glyphBits advanceBits) rs
    putUI8 0

\end{code}

p190: Text records

Note that the TEXTRECORD must be padded (despite this not being mentioned
in the specification) because GLYPHENTRY may be of a size which is not a multiple of 8.

\begin{code}
 
data TEXTRECORD = TEXTRECORD{tEXTRECORD_textRecordType :: Bool,
                             tEXTRECORD_fontID :: Maybe UI16,
                             tEXTRECORD_textColor :: Maybe (Either RGBA RGB),
                             tEXTRECORD_xOffset :: Maybe SI16, tEXTRECORD_yOffset :: Maybe SI16,
                             tEXTRECORD_textHeight :: Maybe UI16,
                             tEXTRECORD_glyphEntries :: [GLYPHENTRY]}
                deriving (Eq, Show, Typeable, Data)
getTEXTRECORD tEXTRECORD_textVer tEXTRECORD_glyphBits
  tEXTRECORD_advanceBits
  = do tEXTRECORD_textRecordType <- getFlag
       discardReserved "_reserved (x :: ?)" (getUB 3)
       tEXTRECORD_styleFlagsHasFont <- getFlag
       tEXTRECORD_styleFlagsHasColor <- getFlag
       tEXTRECORD_styleFlagsHasYOffset <- getFlag
       tEXTRECORD_styleFlagsHasXOffset <- getFlag
       tEXTRECORD_fontID <- maybeHas tEXTRECORD_styleFlagsHasFont getUI16
       tEXTRECORD_textColor <- maybeHas tEXTRECORD_styleFlagsHasColor
                                 (if tEXTRECORD_textVer == 2 then fmap Left getRGBA else
                                    fmap Right getRGB)
       tEXTRECORD_xOffset <- maybeHas tEXTRECORD_styleFlagsHasXOffset
                               getSI16
       tEXTRECORD_yOffset <- maybeHas tEXTRECORD_styleFlagsHasYOffset
                               getSI16
       tEXTRECORD_textHeight <- maybeHas tEXTRECORD_styleFlagsHasFont
                                  getUI16
       tEXTRECORD_glyphCount <- getUI8
       tEXTRECORD_glyphEntries <- genericReplicateM tEXTRECORD_glyphCount
                                    (getGLYPHENTRY tEXTRECORD_glyphBits tEXTRECORD_advanceBits)
       discardReserved "_reserved (x :: ?)" byteAlign
       return (TEXTRECORD{..})
putTEXTRECORD tEXTRECORD_textVer tEXTRECORD_glyphBits
  tEXTRECORD_advanceBits TEXTRECORD{..}
  = do putFlag tEXTRECORD_textRecordType
       let tEXTRECORD_styleFlagsReserved = reservedDefault
       if requiredBitsUB tEXTRECORD_styleFlagsReserved <= 3 then
         putUB 3 tEXTRECORD_styleFlagsReserved else
         inconsistent "x :: TEXTRECORD"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB tEXTRECORD_styleFlagsReserved) ++
                " bits to store the value " ++
                  show tEXTRECORD_styleFlagsReserved ++
                    ", but only have available " ++ show 3)
       let tEXTRECORD_styleFlagsHasFont = isJust tEXTRECORD_fontID
       putFlag tEXTRECORD_styleFlagsHasFont
       let tEXTRECORD_styleFlagsHasColor = isJust tEXTRECORD_textColor
       putFlag tEXTRECORD_styleFlagsHasColor
       let tEXTRECORD_styleFlagsHasYOffset = isJust tEXTRECORD_yOffset
       putFlag tEXTRECORD_styleFlagsHasYOffset
       let tEXTRECORD_styleFlagsHasXOffset = isJust tEXTRECORD_xOffset
       putFlag tEXTRECORD_styleFlagsHasXOffset
       case tEXTRECORD_fontID of
           Just x | tEXTRECORD_styleFlagsHasFont -> putUI16 x
                  | otherwise ->
                    inconsistent "tEXTRECORD_fontID (x :: TEXTRECORD)"
                      "Should have a Just iff tEXTRECORD_styleFlagsHasFont is True"
           Nothing | tEXTRECORD_styleFlagsHasFont ->
                     inconsistent "tEXTRECORD_fontID (x :: TEXTRECORD)"
                       "Should have a Nothing iff tEXTRECORD_styleFlagsHasFont is False"
                   | otherwise -> return ()
       case tEXTRECORD_textColor of
           Just x | tEXTRECORD_styleFlagsHasColor ->
                    case x of
                        Left x | tEXTRECORD_textVer == 2 -> putRGBA x
                               | otherwise ->
                                 inconsistent "fromJust (tEXTRECORD_textColor (x :: TEXTRECORD))"
                                   "Should have a Left iff tEXTRECORD_textVer == 2 is True"
                        Right x | tEXTRECORD_textVer == 2 ->
                                  inconsistent "fromJust (tEXTRECORD_textColor (x :: TEXTRECORD))"
                                    "Should have a Right iff tEXTRECORD_textVer == 2 is False"
                                | otherwise -> putRGB x
                  | otherwise ->
                    inconsistent "tEXTRECORD_textColor (x :: TEXTRECORD)"
                      "Should have a Just iff tEXTRECORD_styleFlagsHasColor is True"
           Nothing | tEXTRECORD_styleFlagsHasColor ->
                     inconsistent "tEXTRECORD_textColor (x :: TEXTRECORD)"
                       "Should have a Nothing iff tEXTRECORD_styleFlagsHasColor is False"
                   | otherwise -> return ()
       case tEXTRECORD_xOffset of
           Just x | tEXTRECORD_styleFlagsHasXOffset -> putSI16 x
                  | otherwise ->
                    inconsistent "tEXTRECORD_xOffset (x :: TEXTRECORD)"
                      "Should have a Just iff tEXTRECORD_styleFlagsHasXOffset is True"
           Nothing | tEXTRECORD_styleFlagsHasXOffset ->
                     inconsistent "tEXTRECORD_xOffset (x :: TEXTRECORD)"
                       "Should have a Nothing iff tEXTRECORD_styleFlagsHasXOffset is False"
                   | otherwise -> return ()
       case tEXTRECORD_yOffset of
           Just x | tEXTRECORD_styleFlagsHasYOffset -> putSI16 x
                  | otherwise ->
                    inconsistent "tEXTRECORD_yOffset (x :: TEXTRECORD)"
                      "Should have a Just iff tEXTRECORD_styleFlagsHasYOffset is True"
           Nothing | tEXTRECORD_styleFlagsHasYOffset ->
                     inconsistent "tEXTRECORD_yOffset (x :: TEXTRECORD)"
                       "Should have a Nothing iff tEXTRECORD_styleFlagsHasYOffset is False"
                   | otherwise -> return ()
       case tEXTRECORD_textHeight of
           Just x | tEXTRECORD_styleFlagsHasFont -> putUI16 x
                  | otherwise ->
                    inconsistent "tEXTRECORD_textHeight (x :: TEXTRECORD)"
                      "Should have a Just iff tEXTRECORD_styleFlagsHasFont is True"
           Nothing | tEXTRECORD_styleFlagsHasFont ->
                     inconsistent "tEXTRECORD_textHeight (x :: TEXTRECORD)"
                       "Should have a Nothing iff tEXTRECORD_styleFlagsHasFont is False"
                   | otherwise -> return ()
       let tEXTRECORD_glyphCount = genericLength tEXTRECORD_glyphEntries
       putUI8 tEXTRECORD_glyphCount
       if genericLength tEXTRECORD_glyphEntries /= (tEXTRECORD_glyphCount)
         then
         inconsistent "tEXTRECORD_glyphEntries (x :: TEXTRECORD)"
           ("Mismatch with the required length: tEXTRECORD_glyphCount" ++
              show (genericLength tEXTRECORD_glyphEntries) ++
                " /= " ++ show tEXTRECORD_glyphCount)
         else
         mapM_
           (\ x ->
              putGLYPHENTRY tEXTRECORD_glyphBits tEXTRECORD_advanceBits x)
           tEXTRECORD_glyphEntries
       let tEXTRECORD_padding = reservedDefault
       const flushBits (tEXTRECORD_padding :: ())
       return ()

\end{code}

p192: Glyph entry
\begin{code}
 
data GLYPHENTRY = GLYPHENTRY{gLYPHENTRY_glyphIndex :: UB,
                             gLYPHENTRY_glyphAdvance :: SB}
                deriving (Eq, Show, Typeable, Data)
getGLYPHENTRY gLYPHENTRY_glyphBits gLYPHENTRY_advanceBits
  = do gLYPHENTRY_glyphIndex <- getUB gLYPHENTRY_glyphBits
       gLYPHENTRY_glyphAdvance <- getSB gLYPHENTRY_advanceBits
       return (GLYPHENTRY{..})
putGLYPHENTRY gLYPHENTRY_glyphBits gLYPHENTRY_advanceBits
  GLYPHENTRY{..}
  = do if
         requiredBitsUB gLYPHENTRY_glyphIndex <= gLYPHENTRY_glyphBits then
         putUB gLYPHENTRY_glyphBits gLYPHENTRY_glyphIndex else
         inconsistent "gLYPHENTRY_glyphIndex (x :: GLYPHENTRY)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB gLYPHENTRY_glyphIndex) ++
                " bits to store the value " ++
                  show gLYPHENTRY_glyphIndex ++
                    ", but only have available " ++ show gLYPHENTRY_glyphBits)
       if requiredBitsSB gLYPHENTRY_glyphAdvance <= gLYPHENTRY_advanceBits
         then putSB gLYPHENTRY_advanceBits gLYPHENTRY_glyphAdvance else
         inconsistent "gLYPHENTRY_glyphAdvance (x :: GLYPHENTRY)"
           ("Bit count incorrect: required " ++
              show (requiredBitsSB gLYPHENTRY_glyphAdvance) ++
                " bits to store the value " ++
                  show gLYPHENTRY_glyphAdvance ++
                    ", but only have available " ++ show gLYPHENTRY_advanceBits)
       return ()

\end{code}

p192: DefineText2
\begin{code}
getDefineText2
  = do defineText2_characterID <- getUI16
       defineText2_textBounds <- getRECT
       defineText2_textMatrix <- getMATRIX
       defineText2_glyphBits <- getUI8
       defineText2_advanceBits <- getUI8
       defineText2_textRecords <- getTEXTRECORDS 2 defineText2_glyphBits
                                    defineText2_advanceBits
       return (DefineText2{..})
putDefineText2 DefineText2{..}
  = do putUI16 defineText2_characterID
       putRECT defineText2_textBounds
       putMATRIX defineText2_textMatrix
       putUI8 defineText2_glyphBits
       putUI8 defineText2_advanceBits
       putTEXTRECORDS 2 defineText2_glyphBits defineText2_advanceBits
         defineText2_textRecords
       return ()

\end{code}

p193: DefineEditText
\begin{code}
getDefineEditText
  = do defineEditText_characterID <- getUI16
       defineEditText_bounds <- getRECT
       defineEditText_hasText <- getFlag
       defineEditText_wordWrap <- getFlag
       defineEditText_multiline <- getFlag
       defineEditText_password <- getFlag
       defineEditText_readOnly <- getFlag
       defineEditText_hasTextColor <- getFlag
       defineEditText_hasMaxLength <- getFlag
       defineEditText_hasFont <- getFlag
       defineEditText_hasFontClass <- getFlag
       defineEditText_autoSize <- getFlag
       defineEditText_hasLayout <- getFlag
       defineEditText_noSelect <- getFlag
       defineEditText_border <- getFlag
       defineEditText_wasStatic <- getFlag
       defineEditText_hTML <- getFlag
       defineEditText_useOutlines <- getFlag
       defineEditText_fontID <- maybeHas defineEditText_hasFont getUI16
       defineEditText_fontClass <- maybeHas defineEditText_hasFontClass
                                     getSTRING
       defineEditText_fontHeight <- maybeHas defineEditText_hasFont
                                      getUI16
       defineEditText_textColor <- maybeHas defineEditText_hasTextColor
                                     getRGBA
       defineEditText_maxLength <- maybeHas defineEditText_hasMaxLength
                                     getUI16
       defineEditText_layout <- maybeHas defineEditText_hasLayout
                                  (do defineEditText_layoutAlign <- getUI8
                                      defineEditText_layoutLeftMargin <- getUI16
                                      defineEditText_layoutRightMargin <- getUI16
                                      defineEditText_layoutIndent <- getUI16
                                      defineEditText_layoutLeading <- getSI16
                                      return
                                        (defineEditText_layoutAlign,
                                         defineEditText_layoutLeftMargin,
                                         defineEditText_layoutRightMargin,
                                         defineEditText_layoutIndent, defineEditText_layoutLeading))
       defineEditText_variableName <- getSTRING
       defineEditText_initialText <- maybeHas defineEditText_hasText
                                       getSTRING
       return (DefineEditText{..})
putDefineEditText DefineEditText{..}
  = do putUI16 defineEditText_characterID
       putRECT defineEditText_bounds
       let defineEditText_hasText = isJust defineEditText_initialText
       putFlag defineEditText_hasText
       putFlag defineEditText_wordWrap
       putFlag defineEditText_multiline
       putFlag defineEditText_password
       putFlag defineEditText_readOnly
       let defineEditText_hasTextColor = isJust defineEditText_textColor
       putFlag defineEditText_hasTextColor
       let defineEditText_hasMaxLength = isJust defineEditText_maxLength
       putFlag defineEditText_hasMaxLength
       let defineEditText_hasFont = isJust defineEditText_fontID
       putFlag defineEditText_hasFont
       let defineEditText_hasFontClass = isJust defineEditText_fontClass
       putFlag defineEditText_hasFontClass
       putFlag defineEditText_autoSize
       let defineEditText_hasLayout = isJust defineEditText_layout
       putFlag defineEditText_hasLayout
       putFlag defineEditText_noSelect
       putFlag defineEditText_border
       putFlag defineEditText_wasStatic
       putFlag defineEditText_hTML
       putFlag defineEditText_useOutlines
       case defineEditText_fontID of
           Just x | defineEditText_hasFont -> putUI16 x
                  | otherwise ->
                    inconsistent "defineEditText_fontID (x :: DefineEditText)"
                      "Should have a Just iff defineEditText_hasFont is True"
           Nothing | defineEditText_hasFont ->
                     inconsistent "defineEditText_fontID (x :: DefineEditText)"
                       "Should have a Nothing iff defineEditText_hasFont is False"
                   | otherwise -> return ()
       case defineEditText_fontClass of
           Just x | defineEditText_hasFontClass -> putSTRING x
                  | otherwise ->
                    inconsistent "defineEditText_fontClass (x :: DefineEditText)"
                      "Should have a Just iff defineEditText_hasFontClass is True"
           Nothing | defineEditText_hasFontClass ->
                     inconsistent "defineEditText_fontClass (x :: DefineEditText)"
                       "Should have a Nothing iff defineEditText_hasFontClass is False"
                   | otherwise -> return ()
       case defineEditText_fontHeight of
           Just x | defineEditText_hasFont -> putUI16 x
                  | otherwise ->
                    inconsistent "defineEditText_fontHeight (x :: DefineEditText)"
                      "Should have a Just iff defineEditText_hasFont is True"
           Nothing | defineEditText_hasFont ->
                     inconsistent "defineEditText_fontHeight (x :: DefineEditText)"
                       "Should have a Nothing iff defineEditText_hasFont is False"
                   | otherwise -> return ()
       case defineEditText_textColor of
           Just x | defineEditText_hasTextColor -> putRGBA x
                  | otherwise ->
                    inconsistent "defineEditText_textColor (x :: DefineEditText)"
                      "Should have a Just iff defineEditText_hasTextColor is True"
           Nothing | defineEditText_hasTextColor ->
                     inconsistent "defineEditText_textColor (x :: DefineEditText)"
                       "Should have a Nothing iff defineEditText_hasTextColor is False"
                   | otherwise -> return ()
       case defineEditText_maxLength of
           Just x | defineEditText_hasMaxLength -> putUI16 x
                  | otherwise ->
                    inconsistent "defineEditText_maxLength (x :: DefineEditText)"
                      "Should have a Just iff defineEditText_hasMaxLength is True"
           Nothing | defineEditText_hasMaxLength ->
                     inconsistent "defineEditText_maxLength (x :: DefineEditText)"
                       "Should have a Nothing iff defineEditText_hasMaxLength is False"
                   | otherwise -> return ()
       case defineEditText_layout of
           Just x | defineEditText_hasLayout ->
                    case x of
                        (defineEditText_layoutAlign, defineEditText_layoutLeftMargin,
                         defineEditText_layoutRightMargin, defineEditText_layoutIndent,
                         defineEditText_layoutLeading) -> do putUI8
                                                               defineEditText_layoutAlign
                                                             putUI16 defineEditText_layoutLeftMargin
                                                             putUI16
                                                               defineEditText_layoutRightMargin
                                                             putUI16 defineEditText_layoutIndent
                                                             putSI16 defineEditText_layoutLeading
                                                             return ()
                  | otherwise ->
                    inconsistent "defineEditText_layout (x :: DefineEditText)"
                      "Should have a Just iff defineEditText_hasLayout is True"
           Nothing | defineEditText_hasLayout ->
                     inconsistent "defineEditText_layout (x :: DefineEditText)"
                       "Should have a Nothing iff defineEditText_hasLayout is False"
                   | otherwise -> return ()
       putSTRING defineEditText_variableName
       case defineEditText_initialText of
           Just x | defineEditText_hasText -> putSTRING x
                  | otherwise ->
                    inconsistent "defineEditText_initialText (x :: DefineEditText)"
                      "Should have a Just iff defineEditText_hasText is True"
           Nothing | defineEditText_hasText ->
                     inconsistent "defineEditText_initialText (x :: DefineEditText)"
                       "Should have a Nothing iff defineEditText_hasText is False"
                   | otherwise -> return ()
       return ()

\end{code}

p196: CSMTextSettings
\begin{code}
getCSMTextSettings
  = do cSMTextSettings_textID <- getUI16
       cSMTextSettings_useFlashType <- getUB 2
       cSMTextSettings_gridFit <- getUB 3
       discardReserved "_reserved (x :: ?)" (getUB 3)
       cSMTextSettings_thickness <- getFLOAT
       cSMTextSettings_sharpness <- getFLOAT
       discardReserved "_reserved (x :: ?)" getUI8
       return (CSMTextSettings{..})
putCSMTextSettings CSMTextSettings{..}
  = do putUI16 cSMTextSettings_textID
       if requiredBitsUB cSMTextSettings_useFlashType <= 2 then
         putUB 2 cSMTextSettings_useFlashType else
         inconsistent "cSMTextSettings_useFlashType (x :: CSMTextSettings)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB cSMTextSettings_useFlashType) ++
                " bits to store the value " ++
                  show cSMTextSettings_useFlashType ++
                    ", but only have available " ++ show 2)
       if requiredBitsUB cSMTextSettings_gridFit <= 3 then
         putUB 3 cSMTextSettings_gridFit else
         inconsistent "cSMTextSettings_gridFit (x :: CSMTextSettings)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB cSMTextSettings_gridFit) ++
                " bits to store the value " ++
                  show cSMTextSettings_gridFit ++
                    ", but only have available " ++ show 3)
       let cSMTextSettings_reserved = reservedDefault
       if requiredBitsUB cSMTextSettings_reserved <= 3 then
         putUB 3 cSMTextSettings_reserved else
         inconsistent "x :: CSMTextSettings"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB cSMTextSettings_reserved) ++
                " bits to store the value " ++
                  show cSMTextSettings_reserved ++
                    ", but only have available " ++ show 3)
       putFLOAT cSMTextSettings_thickness
       putFLOAT cSMTextSettings_sharpness
       let cSMTextSettings_reserved = reservedDefault
       putUI8 cSMTextSettings_reserved
       return ()

\end{code}

p198: DefineFont4
\begin{code}
getDefineFont4
  = do defineFont4_fontID <- getUI16
       discardReserved "_reserved (x :: ?)" (getUB 5)
       defineFont4_fontFlagsHasFontData <- getFlag
       defineFont4_fontFlagsItalic <- getFlag
       defineFont4_fontFlagsBold <- getFlag
       defineFont4_fontName <- getSTRING
       defineFont4_fontData <- getRemainingLazyByteString
       return (DefineFont4{..})
putDefineFont4 DefineFont4{..}
  = do putUI16 defineFont4_fontID
       let defineFont4_fontFlagsReserved = reservedDefault
       if requiredBitsUB defineFont4_fontFlagsReserved <= 5 then
         putUB 5 defineFont4_fontFlagsReserved else
         inconsistent "x :: DefineFont4"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineFont4_fontFlagsReserved) ++
                " bits to store the value " ++
                  show defineFont4_fontFlagsReserved ++
                    ", but only have available " ++ show 5)
       putFlag defineFont4_fontFlagsHasFontData
       putFlag defineFont4_fontFlagsItalic
       putFlag defineFont4_fontFlagsBold
       putSTRING defineFont4_fontName
       putLazyByteString defineFont4_fontData
       return ()

\end{code}


Chapter 11: Sounds
~~~~~~~~~~~~~~~~~~

p202: DefineSound
\begin{code}
getDefineSound
  = do defineSound_soundId <- getUI16
       defineSound_soundFormat <- getUB 4
       defineSound_soundRate <- getUB 2
       defineSound_soundSize <- getFlag
       defineSound_soundType <- getFlag
       defineSound_soundSampleCount <- getUI32
       defineSound_soundData <- getRemainingLazyByteString
       return (DefineSound{..})
putDefineSound DefineSound{..}
  = do putUI16 defineSound_soundId
       if requiredBitsUB defineSound_soundFormat <= 4 then
         putUB 4 defineSound_soundFormat else
         inconsistent "defineSound_soundFormat (x :: DefineSound)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineSound_soundFormat) ++
                " bits to store the value " ++
                  show defineSound_soundFormat ++
                    ", but only have available " ++ show 4)
       if requiredBitsUB defineSound_soundRate <= 2 then
         putUB 2 defineSound_soundRate else
         inconsistent "defineSound_soundRate (x :: DefineSound)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineSound_soundRate) ++
                " bits to store the value " ++
                  show defineSound_soundRate ++
                    ", but only have available " ++ show 2)
       putFlag defineSound_soundSize
       putFlag defineSound_soundType
       putUI32 defineSound_soundSampleCount
       putLazyByteString defineSound_soundData
       return ()

\end{code}

p204: StartSound
\begin{code}
getStartSound
  = do startSound_soundId <- getUI16
       startSound_soundInfo <- getSOUNDINFO
       return (StartSound{..})
putStartSound StartSound{..}
  = do putUI16 startSound_soundId
       putSOUNDINFO startSound_soundInfo
       return ()

\end{code}

p205: StartSound2
\begin{code}
getStartSound2
  = do startSound2_soundClassName <- getSTRING
       startSound2_soundInfo <- getSOUNDINFO
       return (StartSound2{..})
putStartSound2 StartSound2{..}
  = do putSTRING startSound2_soundClassName
       putSOUNDINFO startSound2_soundInfo
       return ()

\end{code}

p205: SOUNDINFO
\begin{code}
 
data SOUNDINFO = SOUNDINFO{sOUNDINFO_syncStop :: Bool,
                           sOUNDINFO_syncNoMultiple :: Bool, sOUNDINFO_inPoint :: Maybe UI32,
                           sOUNDINFO_outPoint :: Maybe UI32,
                           sOUNDINFO_loopCount :: Maybe UI16,
                           sOUNDINFO_env :: Maybe (UI8, [SOUNDENVELOPE])}
               deriving (Eq, Show, Typeable, Data)
getSOUNDINFO
  = do discardReserved "_reserved (x :: ?)" (getUB 2)
       sOUNDINFO_syncStop <- getFlag
       sOUNDINFO_syncNoMultiple <- getFlag
       sOUNDINFO_hasEnvelope <- getFlag
       sOUNDINFO_hasLoops <- getFlag
       sOUNDINFO_hasOutPoint <- getFlag
       sOUNDINFO_hasInPoint <- getFlag
       sOUNDINFO_inPoint <- maybeHas sOUNDINFO_hasInPoint getUI32
       sOUNDINFO_outPoint <- maybeHas sOUNDINFO_hasOutPoint getUI32
       sOUNDINFO_loopCount <- maybeHas sOUNDINFO_hasLoops getUI16
       sOUNDINFO_env <- maybeHas sOUNDINFO_hasEnvelope
                          (do sOUNDINFO_envPoints <- getUI8
                              sOUNDINFO_envelopeRecords <- genericReplicateM sOUNDINFO_envPoints
                                                             getSOUNDENVELOPE
                              return (sOUNDINFO_envPoints, sOUNDINFO_envelopeRecords))
       return (SOUNDINFO{..})
putSOUNDINFO SOUNDINFO{..}
  = do let sOUNDINFO_reserved = reservedDefault
       if requiredBitsUB sOUNDINFO_reserved <= 2 then
         putUB 2 sOUNDINFO_reserved else
         inconsistent "x :: SOUNDINFO"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB sOUNDINFO_reserved) ++
                " bits to store the value " ++
                  show sOUNDINFO_reserved ++ ", but only have available " ++ show 2)
       putFlag sOUNDINFO_syncStop
       putFlag sOUNDINFO_syncNoMultiple
       let sOUNDINFO_hasEnvelope = isJust sOUNDINFO_env
       putFlag sOUNDINFO_hasEnvelope
       let sOUNDINFO_hasLoops = isJust sOUNDINFO_loopCount
       putFlag sOUNDINFO_hasLoops
       let sOUNDINFO_hasOutPoint = isJust sOUNDINFO_outPoint
       putFlag sOUNDINFO_hasOutPoint
       let sOUNDINFO_hasInPoint = isJust sOUNDINFO_inPoint
       putFlag sOUNDINFO_hasInPoint
       case sOUNDINFO_inPoint of
           Just x | sOUNDINFO_hasInPoint -> putUI32 x
                  | otherwise ->
                    inconsistent "sOUNDINFO_inPoint (x :: SOUNDINFO)"
                      "Should have a Just iff sOUNDINFO_hasInPoint is True"
           Nothing | sOUNDINFO_hasInPoint ->
                     inconsistent "sOUNDINFO_inPoint (x :: SOUNDINFO)"
                       "Should have a Nothing iff sOUNDINFO_hasInPoint is False"
                   | otherwise -> return ()
       case sOUNDINFO_outPoint of
           Just x | sOUNDINFO_hasOutPoint -> putUI32 x
                  | otherwise ->
                    inconsistent "sOUNDINFO_outPoint (x :: SOUNDINFO)"
                      "Should have a Just iff sOUNDINFO_hasOutPoint is True"
           Nothing | sOUNDINFO_hasOutPoint ->
                     inconsistent "sOUNDINFO_outPoint (x :: SOUNDINFO)"
                       "Should have a Nothing iff sOUNDINFO_hasOutPoint is False"
                   | otherwise -> return ()
       case sOUNDINFO_loopCount of
           Just x | sOUNDINFO_hasLoops -> putUI16 x
                  | otherwise ->
                    inconsistent "sOUNDINFO_loopCount (x :: SOUNDINFO)"
                      "Should have a Just iff sOUNDINFO_hasLoops is True"
           Nothing | sOUNDINFO_hasLoops ->
                     inconsistent "sOUNDINFO_loopCount (x :: SOUNDINFO)"
                       "Should have a Nothing iff sOUNDINFO_hasLoops is False"
                   | otherwise -> return ()
       case sOUNDINFO_env of
           Just x | sOUNDINFO_hasEnvelope ->
                    case x of
                        (sOUNDINFO_envPoints, sOUNDINFO_envelopeRecords) -> do putUI8
                                                                                 sOUNDINFO_envPoints
                                                                               if
                                                                                 genericLength
                                                                                   sOUNDINFO_envelopeRecords
                                                                                   /=
                                                                                   (sOUNDINFO_envPoints)
                                                                                 then
                                                                                 inconsistent
                                                                                   "snd (fromJust (sOUNDINFO_env (x :: SOUNDINFO)))"
                                                                                   ("Mismatch with the required length: sOUNDINFO_envPoints"
                                                                                      ++
                                                                                      show
                                                                                        (genericLength
                                                                                           sOUNDINFO_envelopeRecords)
                                                                                        ++
                                                                                        " /= " ++
                                                                                          show
                                                                                            sOUNDINFO_envPoints)
                                                                                 else
                                                                                 mapM_
                                                                                   (\ x ->
                                                                                      putSOUNDENVELOPE
                                                                                        x)
                                                                                   sOUNDINFO_envelopeRecords
                                                                               return ()
                  | otherwise ->
                    inconsistent "sOUNDINFO_env (x :: SOUNDINFO)"
                      "Should have a Just iff sOUNDINFO_hasEnvelope is True"
           Nothing | sOUNDINFO_hasEnvelope ->
                     inconsistent "sOUNDINFO_env (x :: SOUNDINFO)"
                       "Should have a Nothing iff sOUNDINFO_hasEnvelope is False"
                   | otherwise -> return ()
       return ()

\end{code}

p206: SOUNDENVELOPE
\begin{code}
 
data SOUNDENVELOPE = SOUNDENVELOPE{sOUNDENVELOPE_pos44 :: UI32,
                                   sOUNDENVELOPE_leftLevel :: UI16,
                                   sOUNDENVELOPE_rightLevel :: UI16}
                   deriving (Eq, Show, Typeable, Data)
getSOUNDENVELOPE
  = do sOUNDENVELOPE_pos44 <- getUI32
       sOUNDENVELOPE_leftLevel <- getUI16
       sOUNDENVELOPE_rightLevel <- getUI16
       return (SOUNDENVELOPE{..})
putSOUNDENVELOPE SOUNDENVELOPE{..}
  = do putUI32 sOUNDENVELOPE_pos44
       putUI16 sOUNDENVELOPE_leftLevel
       putUI16 sOUNDENVELOPE_rightLevel
       return ()

\end{code}

p207: SoundStreamHead
\begin{code}
getSoundStreamHead
  = do discardReserved "_reserved (x :: ?)" (getUB 4)
       soundStreamHead_playbackSoundRate <- getUB 2
       soundStreamHead_playbackSoundSize <- getFlag
       soundStreamHead_playbackSoundType <- getFlag
       soundStreamHead_streamSoundCompression <- getUB 4
       soundStreamHead_streamSoundRate <- getUB 2
       soundStreamHead_streamSoundSize <- getFlag
       soundStreamHead_streamSoundType <- getFlag
       soundStreamHead_streamSoundSampleCount <- getUI16
       soundStreamHead_latencySeek <- maybeHas
                                        (soundStreamHead_streamSoundCompression == 2)
                                        getSI16
       return (SoundStreamHead{..})
putSoundStreamHead SoundStreamHead{..}
  = do let soundStreamHead_reserved = reservedDefault
       if requiredBitsUB soundStreamHead_reserved <= 4 then
         putUB 4 soundStreamHead_reserved else
         inconsistent "x :: SoundStreamHead"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead_reserved) ++
                " bits to store the value " ++
                  show soundStreamHead_reserved ++
                    ", but only have available " ++ show 4)
       if requiredBitsUB soundStreamHead_playbackSoundRate <= 2 then
         putUB 2 soundStreamHead_playbackSoundRate else
         inconsistent
           "soundStreamHead_playbackSoundRate (x :: SoundStreamHead)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead_playbackSoundRate) ++
                " bits to store the value " ++
                  show soundStreamHead_playbackSoundRate ++
                    ", but only have available " ++ show 2)
       putFlag soundStreamHead_playbackSoundSize
       putFlag soundStreamHead_playbackSoundType
       if requiredBitsUB soundStreamHead_streamSoundCompression <= 4 then
         putUB 4 soundStreamHead_streamSoundCompression else
         inconsistent
           "soundStreamHead_streamSoundCompression (x :: SoundStreamHead)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead_streamSoundCompression) ++
                " bits to store the value " ++
                  show soundStreamHead_streamSoundCompression ++
                    ", but only have available " ++ show 4)
       if requiredBitsUB soundStreamHead_streamSoundRate <= 2 then
         putUB 2 soundStreamHead_streamSoundRate else
         inconsistent
           "soundStreamHead_streamSoundRate (x :: SoundStreamHead)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead_streamSoundRate) ++
                " bits to store the value " ++
                  show soundStreamHead_streamSoundRate ++
                    ", but only have available " ++ show 2)
       putFlag soundStreamHead_streamSoundSize
       putFlag soundStreamHead_streamSoundType
       putUI16 soundStreamHead_streamSoundSampleCount
       case soundStreamHead_latencySeek of
           Just x | soundStreamHead_streamSoundCompression == 2 -> putSI16 x
                  | otherwise ->
                    inconsistent "soundStreamHead_latencySeek (x :: SoundStreamHead)"
                      "Should have a Just iff soundStreamHead_streamSoundCompression == 2 is True"
           Nothing | soundStreamHead_streamSoundCompression == 2 ->
                     inconsistent "soundStreamHead_latencySeek (x :: SoundStreamHead)"
                       "Should have a Nothing iff soundStreamHead_streamSoundCompression == 2 is False"
                   | otherwise -> return ()
       return ()

\end{code}

p209: SoundStreamHead2
\begin{code}
getSoundStreamHead2
  = do discardReserved "_reserved (x :: ?)" (getUB 4)
       soundStreamHead2_playbackSoundRate <- getUB 2
       soundStreamHead2_playbackSoundSize <- getFlag
       soundStreamHead2_playbackSoundType <- getFlag
       soundStreamHead2_streamSoundCompression <- getUB 4
       soundStreamHead2_streamSoundRate <- getUB 2
       soundStreamHead2_streamSoundSize <- getFlag
       soundStreamHead2_streamSoundType <- getFlag
       soundStreamHead2_streamSoundSampleCount <- getUI16
       soundStreamHead2_latencySeek <- maybeHas
                                         (soundStreamHead2_streamSoundCompression == 2)
                                         getSI16
       return (SoundStreamHead2{..})
putSoundStreamHead2 SoundStreamHead2{..}
  = do let soundStreamHead2_reserved = reservedDefault
       if requiredBitsUB soundStreamHead2_reserved <= 4 then
         putUB 4 soundStreamHead2_reserved else
         inconsistent "x :: SoundStreamHead2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead2_reserved) ++
                " bits to store the value " ++
                  show soundStreamHead2_reserved ++
                    ", but only have available " ++ show 4)
       if requiredBitsUB soundStreamHead2_playbackSoundRate <= 2 then
         putUB 2 soundStreamHead2_playbackSoundRate else
         inconsistent
           "soundStreamHead2_playbackSoundRate (x :: SoundStreamHead2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead2_playbackSoundRate) ++
                " bits to store the value " ++
                  show soundStreamHead2_playbackSoundRate ++
                    ", but only have available " ++ show 2)
       putFlag soundStreamHead2_playbackSoundSize
       putFlag soundStreamHead2_playbackSoundType
       if requiredBitsUB soundStreamHead2_streamSoundCompression <= 4 then
         putUB 4 soundStreamHead2_streamSoundCompression else
         inconsistent
           "soundStreamHead2_streamSoundCompression (x :: SoundStreamHead2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead2_streamSoundCompression) ++
                " bits to store the value " ++
                  show soundStreamHead2_streamSoundCompression ++
                    ", but only have available " ++ show 4)
       if requiredBitsUB soundStreamHead2_streamSoundRate <= 2 then
         putUB 2 soundStreamHead2_streamSoundRate else
         inconsistent
           "soundStreamHead2_streamSoundRate (x :: SoundStreamHead2)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB soundStreamHead2_streamSoundRate) ++
                " bits to store the value " ++
                  show soundStreamHead2_streamSoundRate ++
                    ", but only have available " ++ show 2)
       putFlag soundStreamHead2_streamSoundSize
       putFlag soundStreamHead2_streamSoundType
       putUI16 soundStreamHead2_streamSoundSampleCount
       case soundStreamHead2_latencySeek of
           Just x | soundStreamHead2_streamSoundCompression == 2 -> putSI16 x
                  | otherwise ->
                    inconsistent "soundStreamHead2_latencySeek (x :: SoundStreamHead2)"
                      "Should have a Just iff soundStreamHead2_streamSoundCompression == 2 is True"
           Nothing | soundStreamHead2_streamSoundCompression == 2 ->
                     inconsistent "soundStreamHead2_latencySeek (x :: SoundStreamHead2)"
                       "Should have a Nothing iff soundStreamHead2_streamSoundCompression == 2 is False"
                   | otherwise -> return ()
       return ()

\end{code}

p210: SoundStreamBlock
\begin{code}
getSoundStreamBlock
  = do soundStreamBlock_streamSoundData <- getRemainingLazyByteString
       return (SoundStreamBlock{..})
putSoundStreamBlock SoundStreamBlock{..}
  = do putLazyByteString soundStreamBlock_streamSoundData
       return ()

\end{code}


Chapter 12: Buttons
~~~~~~~~~~~~~~~~~~~

p224: Button record
\begin{code}

type BUTTONRECORDS = [BUTTONRECORD]

getBUTTONRECORDS buttonVer = go
  where
    go = do
      look <- lookAhead getUI8
      if look == 0
        then do
          discardKnown "x :: BUTTONRECORDS" "BUTTONRECORDS array should be 0 terminated" 0 getUI8
          return []
        else do
          x <- getBUTTONRECORD buttonVer
          fmap (x:) go

putBUTTONRECORDS buttonVer rs = do
    mapM_ (putBUTTONRECORD buttonVer) rs
    putUI8 0

\end{code}

\begin{code}
 
data BUTTONRECORD = BUTTONRECORD{bUTTONRECORD_buttonHasBlendMode ::
                                 Bool,
                                 bUTTONRECORD_buttonHasFilterList :: Bool,
                                 bUTTONRECORD_buttonStateHitTest :: Bool,
                                 bUTTONRECORD_buttonStateDown :: Bool,
                                 bUTTONRECORD_buttonStateOver :: Bool,
                                 bUTTONRECORD_buttonStateUp :: Bool,
                                 bUTTONRECORD_characterID :: UI16, bUTTONRECORD_placeDepth :: UI16,
                                 bUTTONRECORD_placeMatrix :: MATRIX,
                                 bUTTONRECORD_buttonDisplay ::
                                 Maybe (CXFORMWITHALPHA, Maybe FILTERLIST, Maybe UI8)}
                  deriving (Eq, Show, Typeable, Data)
getBUTTONRECORD bUTTONRECORD_buttonVer
  = do discardReserved "_reserved (x :: ?)" (getUB 2)
       bUTTONRECORD_buttonHasBlendMode <- getFlag
       bUTTONRECORD_buttonHasFilterList <- getFlag
       bUTTONRECORD_buttonStateHitTest <- getFlag
       bUTTONRECORD_buttonStateDown <- getFlag
       bUTTONRECORD_buttonStateOver <- getFlag
       bUTTONRECORD_buttonStateUp <- getFlag
       bUTTONRECORD_characterID <- getUI16
       bUTTONRECORD_placeDepth <- getUI16
       bUTTONRECORD_placeMatrix <- getMATRIX
       bUTTONRECORD_buttonDisplay <- maybeHas
                                       (bUTTONRECORD_buttonVer == 2)
                                       (do bUTTONRECORD_buttonDisplayColorTransform <- getCXFORMWITHALPHA
                                           bUTTONRECORD_buttonDisplayFilterList <- maybeHas
                                                                                     bUTTONRECORD_buttonHasFilterList
                                                                                     getFILTERLIST
                                           bUTTONRECORD_buttonDisplayBlendMode <- maybeHas
                                                                                    bUTTONRECORD_buttonHasBlendMode
                                                                                    getUI8
                                           return
                                             (bUTTONRECORD_buttonDisplayColorTransform,
                                              bUTTONRECORD_buttonDisplayFilterList,
                                              bUTTONRECORD_buttonDisplayBlendMode))
       return (BUTTONRECORD{..})
putBUTTONRECORD bUTTONRECORD_buttonVer BUTTONRECORD{..}
  = do let bUTTONRECORD_buttonReserved = reservedDefault
       if requiredBitsUB bUTTONRECORD_buttonReserved <= 2 then
         putUB 2 bUTTONRECORD_buttonReserved else
         inconsistent "x :: BUTTONRECORD"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB bUTTONRECORD_buttonReserved) ++
                " bits to store the value " ++
                  show bUTTONRECORD_buttonReserved ++
                    ", but only have available " ++ show 2)
       putFlag bUTTONRECORD_buttonHasBlendMode
       putFlag bUTTONRECORD_buttonHasFilterList
       putFlag bUTTONRECORD_buttonStateHitTest
       putFlag bUTTONRECORD_buttonStateDown
       putFlag bUTTONRECORD_buttonStateOver
       putFlag bUTTONRECORD_buttonStateUp
       putUI16 bUTTONRECORD_characterID
       putUI16 bUTTONRECORD_placeDepth
       putMATRIX bUTTONRECORD_placeMatrix
       case bUTTONRECORD_buttonDisplay of
           Just x | bUTTONRECORD_buttonVer == 2 ->
                    case x of
                        (bUTTONRECORD_buttonDisplayColorTransform,
                         bUTTONRECORD_buttonDisplayFilterList,
                         bUTTONRECORD_buttonDisplayBlendMode) -> do putCXFORMWITHALPHA
                                                                      bUTTONRECORD_buttonDisplayColorTransform
                                                                    case
                                                                      bUTTONRECORD_buttonDisplayFilterList
                                                                      of
                                                                        Just x |
                                                                                 bUTTONRECORD_buttonHasFilterList
                                                                                 -> putFILTERLIST x
                                                                               | otherwise ->
                                                                                 inconsistent
                                                                                   "snd (fromJust (bUTTONRECORD_buttonDisplay (x :: BUTTONRECORD)))"
                                                                                   "Should have a Just iff bUTTONRECORD_buttonHasFilterList is True"
                                                                        Nothing |
                                                                                  bUTTONRECORD_buttonHasFilterList
                                                                                  ->
                                                                                  inconsistent
                                                                                    "snd (fromJust (bUTTONRECORD_buttonDisplay (x :: BUTTONRECORD)))"
                                                                                    "Should have a Nothing iff bUTTONRECORD_buttonHasFilterList is False"
                                                                                | otherwise ->
                                                                                  return ()
                                                                    case
                                                                      bUTTONRECORD_buttonDisplayBlendMode
                                                                      of
                                                                        Just x |
                                                                                 bUTTONRECORD_buttonHasBlendMode
                                                                                 -> putUI8 x
                                                                               | otherwise ->
                                                                                 inconsistent
                                                                                   "thd (fromJust (bUTTONRECORD_buttonDisplay (x :: BUTTONRECORD)))"
                                                                                   "Should have a Just iff bUTTONRECORD_buttonHasBlendMode is True"
                                                                        Nothing |
                                                                                  bUTTONRECORD_buttonHasBlendMode
                                                                                  ->
                                                                                  inconsistent
                                                                                    "thd (fromJust (bUTTONRECORD_buttonDisplay (x :: BUTTONRECORD)))"
                                                                                    "Should have a Nothing iff bUTTONRECORD_buttonHasBlendMode is False"
                                                                                | otherwise ->
                                                                                  return ()
                                                                    return ()
                  | otherwise ->
                    inconsistent "bUTTONRECORD_buttonDisplay (x :: BUTTONRECORD)"
                      "Should have a Just iff bUTTONRECORD_buttonVer == 2 is True"
           Nothing | bUTTONRECORD_buttonVer == 2 ->
                     inconsistent "bUTTONRECORD_buttonDisplay (x :: BUTTONRECORD)"
                       "Should have a Nothing iff bUTTONRECORD_buttonVer == 2 is False"
                   | otherwise -> return ()
       return ()

\end{code}

p225: DefineButton
\begin{code}
getDefineButton
  = do defineButton_buttonId <- getUI16
       defineButton_characters <- getBUTTONRECORDS 1
       defineButton_actions <- getACTIONRECORDS
       return (DefineButton{..})
putDefineButton DefineButton{..}
  = do putUI16 defineButton_buttonId
       putBUTTONRECORDS 1 defineButton_characters
       putACTIONRECORDS defineButton_actions
       return ()

\end{code}

p226: DefineButton2
\begin{code}
getDefineButton2
  = do defineButton2_buttonId <- getUI16
       discardReserved "_reserved (x :: ?)" (getUB 7)
       defineButton2_trackAsMenu <- getFlag
       defineButton2_actionOffset <- getUI16
       defineButton2_characters <- getBUTTONRECORDS 2
       defineButton2_characterEndFlag <- getUI8
       defineButton2_actions <- getBUTTONCONDACTIONS
       return (DefineButton2{..})
putDefineButton2 DefineButton2{..}
  = do putUI16 defineButton2_buttonId
       let defineButton2_reservedFlags = reservedDefault
       if requiredBitsUB defineButton2_reservedFlags <= 7 then
         putUB 7 defineButton2_reservedFlags else
         inconsistent "x :: DefineButton2"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineButton2_reservedFlags) ++
                " bits to store the value " ++
                  show defineButton2_reservedFlags ++
                    ", but only have available " ++ show 7)
       putFlag defineButton2_trackAsMenu
       putUI16 defineButton2_actionOffset
       putBUTTONRECORDS 2 defineButton2_characters
       putUI8 defineButton2_characterEndFlag
       putBUTTONCONDACTIONS defineButton2_actions
       return ()

\end{code}

\begin{code}
 
data BUTTONCONDACTION = BUTTONCONDACTION{bUTTONCONDACTION_condIdleToOverDown
                                         :: Bool,
                                         bUTTONCONDACTION_condOutDownToIdle :: Bool,
                                         bUTTONCONDACTION_condOutDownToOverDown :: Bool,
                                         bUTTONCONDACTION_condOverDownToOutDown :: Bool,
                                         bUTTONCONDACTION_condOverDownToOverUp :: Bool,
                                         bUTTONCONDACTION_condOverUpToOverDown :: Bool,
                                         bUTTONCONDACTION_condOverUpToIdle :: Bool,
                                         bUTTONCONDACTION_condIdleToOverUp :: Bool,
                                         bUTTONCONDACTION_condKeyPress :: UB,
                                         bUTTONCONDACTION_condOverDownToIdle :: Bool,
                                         bUTTONCONDACTION_actions :: ACTIONRECORDS}
                      deriving (Eq, Show, Typeable, Data)
getBUTTONCONDACTION
  = do bUTTONCONDACTION_condIdleToOverDown <- getFlag
       bUTTONCONDACTION_condOutDownToIdle <- getFlag
       bUTTONCONDACTION_condOutDownToOverDown <- getFlag
       bUTTONCONDACTION_condOverDownToOutDown <- getFlag
       bUTTONCONDACTION_condOverDownToOverUp <- getFlag
       bUTTONCONDACTION_condOverUpToOverDown <- getFlag
       bUTTONCONDACTION_condOverUpToIdle <- getFlag
       bUTTONCONDACTION_condIdleToOverUp <- getFlag
       bUTTONCONDACTION_condKeyPress <- getUB 7
       bUTTONCONDACTION_condOverDownToIdle <- getFlag
       bUTTONCONDACTION_actions <- getACTIONRECORDS
       return (BUTTONCONDACTION{..})
putBUTTONCONDACTION BUTTONCONDACTION{..}
  = do putFlag bUTTONCONDACTION_condIdleToOverDown
       putFlag bUTTONCONDACTION_condOutDownToIdle
       putFlag bUTTONCONDACTION_condOutDownToOverDown
       putFlag bUTTONCONDACTION_condOverDownToOutDown
       putFlag bUTTONCONDACTION_condOverDownToOverUp
       putFlag bUTTONCONDACTION_condOverUpToOverDown
       putFlag bUTTONCONDACTION_condOverUpToIdle
       putFlag bUTTONCONDACTION_condIdleToOverUp
       if requiredBitsUB bUTTONCONDACTION_condKeyPress <= 7 then
         putUB 7 bUTTONCONDACTION_condKeyPress else
         inconsistent
           "bUTTONCONDACTION_condKeyPress (x :: BUTTONCONDACTION)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB bUTTONCONDACTION_condKeyPress) ++
                " bits to store the value " ++
                  show bUTTONCONDACTION_condKeyPress ++
                    ", but only have available " ++ show 7)
       putFlag bUTTONCONDACTION_condOverDownToIdle
       putACTIONRECORDS bUTTONCONDACTION_actions
       return ()

\end{code}

\begin{code}

type BUTTONCONDACTIONS = [BUTTONCONDACTION]

getBUTTONCONDACTIONS = condM isEmpty (return []) $ do
    offset <- getUI16
    x <- (if offset /= 0 then nestSwfGet (fromIntegral $ offset - 2) else id) getBUTTONCONDACTION
    
    if offset == 0
     then return []
     else fmap (x:) getBUTTONCONDACTIONS

putBUTTONCONDACTIONS [] = return ()
putBUTTONCONDACTIONS [r] = do
    putUI16 0
    putBUTTONCONDACTION r
putBUTTONCONDACTIONS (r:rs) = do
    (len, put) <- nestSwfPut (putBUTTONCONDACTION r)
    
    putUI16 (len + 2)
    put
    
    putBUTTONCONDACTIONS rs

\end{code}

p228: DefineButtonCxform
\begin{code}
getDefineButtonCxform
  = do defineButtonCxform_buttonId <- getUI16
       defineButtonCxform_buttonColorTransform <- getCXFORM
       return (DefineButtonCxform{..})
putDefineButtonCxform DefineButtonCxform{..}
  = do putUI16 defineButtonCxform_buttonId
       putCXFORM defineButtonCxform_buttonColorTransform
       return ()

\end{code}

p229: DefineButtonSound
\begin{code}
getDefineButtonSound
  = do defineButtonSound_buttonId <- getUI16
       defineButtonSound_buttonSoundChar0 <- getUI16
       defineButtonSound_buttonSoundInfo0 <- maybeHas
                                               (defineButtonSound_buttonSoundChar0 /= 0)
                                               getSOUNDINFO
       defineButtonSound_buttonSoundChar1 <- getUI16
       defineButtonSound_buttonSoundInfo1 <- maybeHas
                                               (defineButtonSound_buttonSoundChar1 /= 0)
                                               getSOUNDINFO
       defineButtonSound_buttonSoundChar2 <- getUI16
       defineButtonSound_buttonSoundInfo2 <- maybeHas
                                               (defineButtonSound_buttonSoundChar2 /= 0)
                                               getSOUNDINFO
       defineButtonSound_buttonSoundChar3 <- getUI16
       defineButtonSound_buttonSoundInfo3 <- maybeHas
                                               (defineButtonSound_buttonSoundChar3 /= 0)
                                               getSOUNDINFO
       return (DefineButtonSound{..})
putDefineButtonSound DefineButtonSound{..}
  = do putUI16 defineButtonSound_buttonId
       putUI16 defineButtonSound_buttonSoundChar0
       case defineButtonSound_buttonSoundInfo0 of
           Just x | defineButtonSound_buttonSoundChar0 /= 0 -> putSOUNDINFO x
                  | otherwise ->
                    inconsistent
                      "defineButtonSound_buttonSoundInfo0 (x :: DefineButtonSound)"
                      "Should have a Just iff defineButtonSound_buttonSoundChar0 /= 0 is True"
           Nothing | defineButtonSound_buttonSoundChar0 /= 0 ->
                     inconsistent
                       "defineButtonSound_buttonSoundInfo0 (x :: DefineButtonSound)"
                       "Should have a Nothing iff defineButtonSound_buttonSoundChar0 /= 0 is False"
                   | otherwise -> return ()
       putUI16 defineButtonSound_buttonSoundChar1
       case defineButtonSound_buttonSoundInfo1 of
           Just x | defineButtonSound_buttonSoundChar1 /= 0 -> putSOUNDINFO x
                  | otherwise ->
                    inconsistent
                      "defineButtonSound_buttonSoundInfo1 (x :: DefineButtonSound)"
                      "Should have a Just iff defineButtonSound_buttonSoundChar1 /= 0 is True"
           Nothing | defineButtonSound_buttonSoundChar1 /= 0 ->
                     inconsistent
                       "defineButtonSound_buttonSoundInfo1 (x :: DefineButtonSound)"
                       "Should have a Nothing iff defineButtonSound_buttonSoundChar1 /= 0 is False"
                   | otherwise -> return ()
       putUI16 defineButtonSound_buttonSoundChar2
       case defineButtonSound_buttonSoundInfo2 of
           Just x | defineButtonSound_buttonSoundChar2 /= 0 -> putSOUNDINFO x
                  | otherwise ->
                    inconsistent
                      "defineButtonSound_buttonSoundInfo2 (x :: DefineButtonSound)"
                      "Should have a Just iff defineButtonSound_buttonSoundChar2 /= 0 is True"
           Nothing | defineButtonSound_buttonSoundChar2 /= 0 ->
                     inconsistent
                       "defineButtonSound_buttonSoundInfo2 (x :: DefineButtonSound)"
                       "Should have a Nothing iff defineButtonSound_buttonSoundChar2 /= 0 is False"
                   | otherwise -> return ()
       putUI16 defineButtonSound_buttonSoundChar3
       case defineButtonSound_buttonSoundInfo3 of
           Just x | defineButtonSound_buttonSoundChar3 /= 0 -> putSOUNDINFO x
                  | otherwise ->
                    inconsistent
                      "defineButtonSound_buttonSoundInfo3 (x :: DefineButtonSound)"
                      "Should have a Just iff defineButtonSound_buttonSoundChar3 /= 0 is True"
           Nothing | defineButtonSound_buttonSoundChar3 /= 0 ->
                     inconsistent
                       "defineButtonSound_buttonSoundInfo3 (x :: DefineButtonSound)"
                       "Should have a Nothing iff defineButtonSound_buttonSoundChar3 /= 0 is False"
                   | otherwise -> return ()
       return ()

\end{code}


Chapter 13: Sprites and Movie Clips
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p233: DefineSprite
\begin{code}
getDefineSprite
  = do defineSprite_spriteID <- getUI16
       defineSprite_frameCount <- getUI16
       defineSprite_controlTags <- getToEnd getTag
       return (DefineSprite{..})
putDefineSprite DefineSprite{..}
  = do putUI16 defineSprite_spriteID
       putUI16 defineSprite_frameCount
       mapM_ (\ x -> putTag x) defineSprite_controlTags
       return ()

\end{code}


Chapter 14: Video
~~~~~~~~~~~~~~~~~

p251: DefineVideoStream
\begin{code}
getDefineVideoStream
  = do defineVideoStream_characterID <- getUI16
       defineVideoStream_numFrames <- getUI16
       defineVideoStream_width <- getUI16
       defineVideoStream_height <- getUI16
       discardReserved "_reserved (x :: ?)" (getUB 4)
       defineVideoStream_videoFlagsDeblocking <- getUB 3
       defineVideoStream_videoFlagsSmoothing <- getFlag
       defineVideoStream_codecID <- getUI8
       return (DefineVideoStream{..})
putDefineVideoStream DefineVideoStream{..}
  = do putUI16 defineVideoStream_characterID
       putUI16 defineVideoStream_numFrames
       putUI16 defineVideoStream_width
       putUI16 defineVideoStream_height
       let defineVideoStream_videoFlagsReserved = reservedDefault
       if requiredBitsUB defineVideoStream_videoFlagsReserved <= 4 then
         putUB 4 defineVideoStream_videoFlagsReserved else
         inconsistent "x :: DefineVideoStream"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineVideoStream_videoFlagsReserved) ++
                " bits to store the value " ++
                  show defineVideoStream_videoFlagsReserved ++
                    ", but only have available " ++ show 4)
       if requiredBitsUB defineVideoStream_videoFlagsDeblocking <= 3 then
         putUB 3 defineVideoStream_videoFlagsDeblocking else
         inconsistent
           "defineVideoStream_videoFlagsDeblocking (x :: DefineVideoStream)"
           ("Bit count incorrect: required " ++
              show (requiredBitsUB defineVideoStream_videoFlagsDeblocking) ++
                " bits to store the value " ++
                  show defineVideoStream_videoFlagsDeblocking ++
                    ", but only have available " ++ show 3)
       putFlag defineVideoStream_videoFlagsSmoothing
       putUI8 defineVideoStream_codecID
       return ()

\end{code}

p252: VideoFrame
\begin{code}
getStreamID
  = do streamID_streamID <- getUI16
       streamID_frameNum <- getUI16
       streamID_videoData <- getRemainingLazyByteString
       return (StreamID{..})
putStreamID StreamID{..}
  = do putUI16 streamID_streamID
       putUI16 streamID_frameNum
       putLazyByteString streamID_videoData
       return ()

\end{code}


Chapter 15: Binary data
~~~~~~~~~~~~~~~~~~~~~~~

p253: DefineBinaryData
\begin{code}
getDefineBinaryData
  = do defineBinaryData_tag <- getUI16
       discardReserved "_reserved (x :: ?)" getUI32
       defineBinaryData_data <- getRemainingLazyByteString
       return (DefineBinaryData{..})
putDefineBinaryData DefineBinaryData{..}
  = do putUI16 defineBinaryData_tag
       let defineBinaryData_reserved = reservedDefault
       putUI32 defineBinaryData_reserved
       putLazyByteString defineBinaryData_data
       return ()

\end{code}

\end{code}


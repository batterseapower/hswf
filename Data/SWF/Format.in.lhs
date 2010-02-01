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
4) Asserts to validate semantic junk is in agreement with each other in all custom data types
  * And assertions for NBits!
5) Simplify away generated consistency checks that are trivially true
6) Overflow checks on UB/SB/FB fields, since the data types used to represent them are imprecise
7) Generate comments on constructor fields and add them to custom ones
8) Represent some [UI8] as ByteString?
9) Add String sources to the inconsistency checks for debugging
10) Clean up names in putters and getters: don't give excluded fields record-prefixed names
11) Review all handled tags to ensure that changing the size due to the roundtrip won't screw up any offset fields.. hmm!


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
\begin{record}
FIXED
Field   Type Comments
Decimal UI16 Unsigned fractional part of the number, multiplied by 65535
Integer SI16 Signed integer part of the number
\end{record}

\begin{code}

fIXEDToRational :: FIXED -> Rational
fIXEDToRational fixed = fromIntegral (fIXED_integer fixed) + (fromIntegral (fIXED_decimal fixed) % 65535)

fIXEDToFractional :: Fractional a => FIXED -> a
fIXEDToFractional = fromRational . fIXEDToRational

\end{code}

\begin{record}
FIXED8
Field   Type Comment
Decimal UI8  Unsigned fractional part of the number, multiplied by 255
Integer SI8  Signed integer part of the number
\end{record}

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

requiredBitsSB :: Integral a => SB -> a
requiredBitsSB 0 = 0
requiredBitsSB x = (+1) . ceiling . logBase 2 . (+1) . fromIntegral . abs $ x

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
requiredBitsFB = (+16) . requiredBitsSB . fromIntegral . fIXED_integer

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
\begin{record}
RGB
Field Type Comment
Red   UI8  Red color value
Green UI8  Green color value
Blue  UI8  Blue color value
\end{record}

p19: RGBA color record/ARGB color record
\begin{record}
RGBA
Field Type Comment
Red   UI8  Red color value
Green UI8  Green color value
Blue  UI8  Blue color value
Alpha UI8  alpha value defining opacity
\end{record}

\begin{record}
ARGB
Field Type Comment
Alpha UI8  alpha value defining opacity
Red   UI8  Red color value
Green UI8  Green color value
Blue  UI8  Blue color value
\end{record}

p20: Rectangle record
\begin{record}
RECT
Field   Type      Comment
Nbits   UB[5]     Bits used for each subsequent field
Xmin    SB[Nbits] x minimum position for rectangle in twips
Xmax    SB[Nbits] x maximum position for rectangle in twips
Ymin    SB[Nbits] y minimum position for rectangle in twips
Ymax    SB[Nbits] y maximum position for rectangle in twips
Padding PADDING8  Padding to byte boundary
\end{record}

p20: MATRIX record

\begin{record}
MATRIX
Field         Type                             Comment
HasScale      UB[1]                            Has scale values if equal to 1
ScaleBits     If HasScale = 1, UB[5]           Bits in each scale value field
ScaleX        If HasScale = 1, FB[ScaleBits]   x scale value
ScaleY        If HasScale = 1, FB[ScaleBits]   y scale value
HasRotate     UB[1]                            Has rotate and skew values if equal to 1
RotateBits    If HasRotate = 1, UB[5]          Bits in each rotate value field
RotateSkew0   If HasRotate = 1, FB[RotateBits] First rotate and skew value
RotateSkew1   If HasRotate = 1, FB[RotateBits] Second rotate and skew value
TranslateBits UB[5]                            Bits in each translate value field
TranslateX    SB[TranslateBits]                x translate value in twips
TranslateY    SB[TranslateBits]                y translate value in twips
Padding       PADDING8                         Padding to byte boundary
\end{record}

p22: Color transform record
\begin{record}
CXFORM
Field         Type                           Comment
HasAddTerms   UB[1]                          Has color addition values if equal to 1
HasMultTerms  UB[1]                          Has color multiply values if equal to 1
Nbits         UB[4]                          Bits in each value field
RedMultTerm   If HasMultTerms = 1, SB[Nbits] Red multiply value
GreenMultTerm If HasMultTerms = 1, SB[Nbits] Green multiply value
BlueMultTerm  If HasMultTerms = 1, SB[Nbits] Blue multiply value
RedAddTerm    If HasAddTerms = 1, SB[Nbits]  Red addition value
GreenAddTerm  If HasAddTerms = 1, SB[Nbits]  Green addition value
BlueAddTerm   If HasAddTerms = 1, SB[Nbits]  Blue addition value
Padding       PADDING8                       Padding to byte boundary
\end{record}

p23: Color transform with alpha record

\begin{record}
CXFORMWITHALPHA
Field         Type                           Comment
HasAddTerms   UB[1]                          Has color addition values if equal to 1
HasMultTerms  UB[1]                          Has color multiply values if equal to 1
Nbits         UB[4]                          Bits in each value field
RedMultTerm   If HasMultTerms = 1, SB[Nbits] Red multiply value
GreenMultTerm If HasMultTerms = 1, SB[Nbits] Green multiply value
BlueMultTerm  If HasMultTerms = 1, SB[Nbits] Blue multiply value
AlphaMultTerm If HasMultTerms = 1, SB[Nbits] Alpha multiply value
RedAddTerm    If HasAddTerms = 1, SB[Nbits]  Red addition value
GreenAddTerm  If HasAddTerms = 1, SB[Nbits]  Green addition value
BlueAddTerm   If HasAddTerms = 1, SB[Nbits]  Blue addition value
AlphaAddTerm  If HasAddTerms = 1, SB[Nbits]  Transparency addition value
Padding       PADDING8                       Padding to byte boundary
\end{record}


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
\genconstructors{tag}
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

\genfunctions{tag}

p34: PlaceObject
\begin{record}
PlaceObject
Field          Type              Comment
Header         RECORDHEADER      Tag type = 4
CharacterId    UI16              ID of character to place
Depth          UI16              Depth of character
Matrix         MATRIX            Transform matrix data
ColorTransform (optional) CXFORM Color transform data
\end{record}

p35: PlaceObject2
\begin{record}
PlaceObject2
Field                      Type                                          Comment
Header                     RECORDHEADER                                  Tag type = 26
PlaceFlagHasClipActions    UB[1]                                         SWF 5 and later: has clip actions (sprite characters only). Otherwise: always 0
PlaceFlagHasClipDepth      UB[1]                                         Has clip depth
PlaceFlagHasName           UB[1]                                         Has name
PlaceFlagHasRatio          UB[1]                                         Has ratio
PlaceFlagHasColorTransform UB[1]                                         Has color transform
PlaceFlagHasMatrix         UB[1]                                         Has matrix
PlaceFlagHasCharacter      UB[1]                                         Places a character
PlaceFlagMove              UB[1]                                         Defines a character to be moved
Depth                      UI16                                          Depth of character
CharacterId                If PlaceFlagHasCharacter UI16                 ID of character to place
Matrix                     If PlaceFlagHasMatrix MATRIX                  Transform matrix data
ColorTransform             If PlaceFlagHasColorTransform CXFORMWITHALPHA Color transform data
Ratio                      If PlaceFlagHasRatio UI16                     
Name                       If PlaceFlagHasName STRING                    Name of character
ClipDepth                  If PlaceFlagHasClipDepth UI16                 Clip depth (see “Clipping layers” on page 32)
ClipActions                If PlaceFlagHasClipActions CLIPACTIONS        SWF 5 and later: Clip Actions Data
\end{record}

\begin{record}
CLIPACTIONS
Field             Type              Comment
Reserved          UI16              Must be 0
AllEventFlags     CLIPEVENTFLAGS    All events used in these clipactions
ClipActionRecords CLIPACTIONRECORDS Individual event handlers
\end{record}

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
\begin{record}
PlaceObject3
Field                      Type                                                                              Comments
Header                     RECORDHEADER                                                                      Tag type = 70
PlaceFlagHasClipActions    UB[1]                                                                             SWF 5 and later: has clip actions (sprite characters only) Otherwise: always 0
PlaceFlagHasClipDepth      UB[1]                                                                             Has clip depth
PlaceFlagHasName           UB[1]                                                                             Has name
PlaceFlagHasRatio          UB[1]                                                                             Has ratio
PlaceFlagHasColorTransform UB[1]                                                                             Has color transform
PlaceFlagHasMatrix         UB[1]                                                                             Has matrix
PlaceFlagHasCharacter      UB[1]                                                                             Places a character
PlaceFlagMove              UB[1]                                                                             Defines a character to be moved
Reserved                   UB[3]                                                                             Must be 0
PlaceFlagHasImage          UB[1]                                                                             Has class name or character ID of bitmap to place. If PlaceFlagHasClassName, use ClassName. If PlaceFlagHasCharacter, use CharacterId
PlaceFlagHasClassName      UB[1]                                                                             Has class name of object to place
PlaceFlagHasCacheAsBitmap  UB[1]                                                                             Enables bitmap caching
PlaceFlagHasBlendMode      UB[1]                                                                             Has blend mode
PlaceFlagHasFilterList     UB[1]                                                                             Has filter list
Depth                      UI16                                                                              Depth of character
ClassName                  If PlaceFlagHasClassName or (PlaceFlagHasImage and PlaceFlagHasCharacter), STRING Name of the class to place
CharacterId                If PlaceFlagHasCharacter, UI16                                                    ID of character to place
Matrix                     If PlaceFlagHasMatrix, MATRIX                                                     Transform matrix data
ColorTransform             If PlaceFlagHasColorTransform, CXFORMWITHALPHA                                    Color transform data
Ratio                      If PlaceFlagHasRatio, UI16                                                        
Name                       If PlaceFlagHasName, STRING                                                       Name of character
ClipDepth                  If PlaceFlagHasClipDepth, UI16                                                    Clip depth (see Clipping layers)
SurfaceFilterList          If PlaceFlagHasFilterList, FILTERLIST                                             List of filters on this object
BlendMode                  If PlaceFlagHasBlendMode, BlendMode                                               0 or 1 = normal 2 = layer 3 = multiply 4 = screen 5 = lighten 6 = darken 7 = difference 8 = add 9 = subtract 10 = invert 11 = alpha 12 = erase 13 = overlay 14 = hardlight. Values 15 to 255 are reserved.
BitmapCache                If PlaceFlagHasCacheAsBitmap, UI8                                                 0 = Bitmap cache disabled 1-255 = Bitmap cache enabled
ClipActions                If PlaceFlagHasClipActions, CLIPACTIONS                                           SWF 5 and later: Clip Actions Data
\end{record}

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
\begin{record}
COLORMATRIXFILTER
Field  Type      Comment
Matrix FLOAT[20] Color matrix values
\end{record}

p43: Convolution filter
\begin{record}
CONVOLUTIONFILTER
Field         Type                     Comment
MatrixX       UI8                      Horizontal matrix size
MatrixY       UI8                      Vertical matrix size
Divisor       FLOAT                    Divisor applied to the matrix values
Bias          FLOAT                    Bias applied to the matrix values
Matrix        FLOAT[MatrixX * MatrixY] Matrix values
DefaultColor  RGBA                     Default color for pixels outside the image
Reserved      UB[6]                    Must be 0
Clamp         UB[1]                    Clamp mode
PreserveAlpha UB[1]                    Preserve the alpha
\end{record}

p44: Blur filter
\begin{record}
BLURFILTER
Field    Type  Comment
BlurX    FIXED Horizontal blur amount
BlurY    FIXED Vertical blur amount
Passes   UB[5] Number of blur passes
Reserved UB[3] Must be 0
\end{record}

p45: Drop Shadow filter
\begin{record}
DROPSHADOWFILTER
Field           Type    Comment
DropShadowColor RGBA    Color of the shadow
BlurX           FIXED   Horizontal blur amount
BlurY           FIXED   Vertical blur amount
Angle           FIXED   Radian angle of the drop shadow
Distance        FIXED   Distance of the drop shadow
Strength        FIXED8  Strength of the drop shadow
InnerShadow     UB[1]   Inner shadow mode
Knockout        UB[1]   Knockout mode
CompositeSource UB[1]   Composite source. Always 1
Passes          UB[5]   Number of blur passes
\end{record}

p46: Glow filter
\begin{record}
GLOWFILTER
Field           Type   Comment
GlowColor       RGBA   Color of the shadow
BlurX           FIXED  Horizontal blur amount
BlurY           FIXED  Vertical blur amount
Strength        FIXED8 Strength of the glow
InnerGlow       UB[1]  Inner glow mode
Knockout        UB[1]  Knockout mode
CompositeSource UB[1]  Composite source. Always 1
Passes          UB[5]  Number of blur passes
\end{record}

p48: Bevel filter
\begin{record}
BEVELFILTER
Field           Type   Comment
ShadowColor     RGBA   Color of the shadow
HighlightColor  RGBA   Color of the highlight
BlurX           FIXED  Horizontal blur amount
BlurY           FIXED  Vertical blur amount
Angle           FIXED  Radian angle of the drop shadow
Distance        FIXED  Distance of the drop shadow
Strength        FIXED8 Strength of the drop shadow
InnerShadow     UB[1]  Inner shadow mode
Knockout        UB[1]  Knockout mode
CompositeSource UB[1]  Composite source. Always 1
OnTop           UB[1]  OnTop mode
Passes          UB[4]  Number of blur passes
\end{record}

p48: Gradient Glow and Gradient Bevel filters
\begin{record}
GRADIENTGLOWFILTER
Field           Type            Comment
NumColors       UI8             Number of colors in the gradient
GradientColors  RGBA[NumColors] Gradient colors
GradientRatio   UI8[NumColors]  Gradient ratios
BlurX           FIXED           Horizontal blur amount
BlurY           FIXED           Vertical blur amount
Angle           FIXED           Radian angle of the gradient glow
Distance        FIXED           Distance of the gradient glow
Strength        FIXED8          Strength of the gradient glow
InnerShadow     UB[1]           Inner glow mode
Knockout        UB[1]           Knockout mode
CompositeSource UB[1]           Composite source. Always 1
OnTop           UB[1]           OnTop mode
Passes          UB[4]           Number of blur passes
\end{record}

\begin{record}
GRADIENTBEVELFILTER
Field           Type            Comment
NumColors       UI8             Number of colors in the gradient
GradientColors  RGBA[NumColors] Gradient colors
GradientRatio   UI8[NumColors]  Gradient ratios
BlurX           FIXED           Horizontal blur amount
BlurY           FIXED           Vertical blur amount
Angle           FIXED           Radian angle of the gradient bevel
Distance        FIXED           Distance of the gradient bevel
Strength        FIXED8          Strength of the gradient bevel
InnerShadow     UB[1]           Inner bevel mode
Knockout        UB[1]           Knockout mode
CompositeSource UB[1]           Composite source. Always 1
OnTop           UB[1]           OnTop mode
Passes          UB[4]           Number of blur passes
\end{record}

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
\begin{record}
RemoveObject
Field       Type         Comment
Header      RECORDHEADER Tag type = 5
CharacterId UI16         ID of character to remove
Depth       UI16         Depth of character
\end{record}

p52: RemoveObject2
\begin{record}
RemoveObject2
Field  Type         Comment
Header RECORDHEADER Tag type = 28
Depth  UI16         Depth of character
\end{record}

p52: ShowFrame
\begin{record}
ShowFrame
Field  Type         Comment
Header RECORDHEADER Tag type = 1
\end{record}


Chapter 4: Control Tags
~~~~~~~~~~~~~~~~~~~~~~~

p53: SetBackgroundColor
\begin{record}
SetBackgroundColor
Field           Type         Comment
Header          RECORDHEADER Tag type = 9
BackgroundColor RGB          Color of the display background
\end{record}

p53: FrameLabel
\begin{record}
FrameLabel
Field           Type           Comment
Header          RECORDHEADER   Tag type = 43
Name            STRING         Label for frame
NamedAnchorFlag (optional) UI8 SWF 6 or later. Always 1
\end{record}

p54: Protect
\begin{record}
Protect
Field  Type         Comment
Header RECORDHEADER Tag type = 24
\end{record}

p55: End
\begin{record}
End
Field  Type         Comment
Header RECORDHEADER Tag type = 0
\end{record}

p55: ExportAssets
\begin{record}
ExportAssets
Field  Type         Comment
Header RECORDHEADER Tag type = 56
Count  UI16         Number of assets to export
Tag1   UI16         First character ID to export
Name1  STRING       Identifier for first exported character
TagN   UI16         Last character ID to export
NameN  STRING       Identifier for last exported character
\end{record}

p56: ImportAssets
\begin{record}
ImportAssets
Field  Type         Comment
Header RECORDHEADER Tag type = 57
URL    STRING       URL where the source SWF file can be found
Count  UI16         Number of assets to import
Tag1   UI16         Character ID to use for first imported character in importing SWF file (need not match character ID in exporting SWF file)
Name1  STRING       Identifier for first imported character (must match an identifier in exporting SWF file)
TagN   UI16         Character ID to use for last imported character in importing SWF file
NameN  STRING       Identifier for last imported character
\end{record}

p57: EnableDebugger
\begin{record}
EnableDebugger
Field    Type         Comment
Header   RECORDHEADER Tag type = 58
Password STRING       MD5-encrypted password
\end{record}

p57: EnableDebugger2
\begin{record}
EnableDebugger2
Field    Type         Comment
Header   RECORDHEADER Tag type = 64
Reserved UI16         Always 0
Password STRING       MD5-encrypted password
\end{record}

p58: ScriptLimits
\begin{record}
ScriptLimits
Field                Type         Comment
Header               RECORDHEADER Tag type = 65
MaxRecursionDepth    UI16         Maximum recursion depth
ScriptTimeoutSeconds UI16         Maximum ActionScript processing time before script stuck dialog box displays
\end{record}

p58: SetTabIndex
\begin{record}
SetTabIndex
Field    Type         Comment
Header   RECORDHEADER Tag type = 66
Depth    UI16         Depth of character
TabIndex UI16         Tab order value
\end{record}

p59: FileAttributes
\begin{record}
FileAttributes
Field         Type         Comment
Header        RECORDHEADER Tag type = 69
Reserved      UB[1]        Must be 0
UseDirectBlit UB[1]        If 1, the SWF file uses hardware acceleration to blit graphics to the screen, where such acceleration is available. If 0, the SWF file will not use hardware accelerated graphics facilities. Minimum file version is 10.
UseGPU        UB[1]        If 1, the SWF file uses GPU compositing features when drawing graphics, where such acceleration is available. If 0, the SWF file will not use hardware accelerated graphics facilities. Minimum file version is 10.
HasMetadata   UB[1]        If 1, the SWF file contains the Metadata tag. If 0, the SWF file does not contain the Metadata tag.
ActionScript3 UB[1]        If 1, this SWF uses ActionScript 3.0. If 0, this SWF uses ActionScript 1.0 or 2.0. Minimum file format version is 9.
Reserved      UB[2]        Must be 0
UseNetwork    UB[1]        If 1, this SWF file is given network file access when loaded locally. If 0, this SWF file is given local file access when loaded locally.
Reserved      UB[24]       Must be 0
\end{record}

p60: ImportAssets2
\begin{record}
ImportAssets2
Field    Type         Comment
Header   RECORDHEADER Tag type = 71
URL      STRING       URL where the source SWF file can be found
Reserved UI8          Must be 1
Reserved UI8          Must be 0
Count    UI16         Number of assets to import
Tag1     UI16         Character ID to use for first imported character in importing SWF file (need not match character ID in exporting SWF file)
Name1    STRING       Identifier for first imported character (must match an identifier in exporting SWF file) ...
TagN     UI16         Character ID to use for last imported character in importing SWF file
NameN    STRING       Identifier for last imported character
\end{record}

p62: SymbolClass
\begin{record}
SymbolClass
Field      Type                      Comment
Header     RECORDHEADER              Tag type = 76
NumSymbols UI16                      Number of symbols that will be associated by this tag.
TagsNames  <UI16,STRING>[NumSymbols] The 16-bit character tag ID for the symbol to associate, and the fully-qualified name of the ActionScript 3.0 class with which to associate. The class must have already been declared by a DoABC tag.
\end{record}

p64: Metadata
\begin{record}
Metadata
Field    Type         Comment
Header   RECORDHEADER Tag type = 77
Metadata STRING       XML Metadata
\end{record}

p65: DefineScalingGrid
\begin{record}
DefineScalingGrid
Field       Type         Comment
Header      RECORDHEADER Tag type = 78
CharacterId UI16         ID of sprite or button character upon which the scaling grid will be applied.
Splitter    RECT         Center region of 9-slice grid
\end{record}

p66: DefineSceneAndFrameLabelData
\begin{record}
DefineSceneAndFrameLabelData
Field           Type                                 Comment
Header          RECORDHEADER                         Tag type = 86
SceneCount      EncodedU32                           Number of scenes
OffsetNames     <EncodedU32,STRING>[SceneCount]      Frame offset and names for scenes
FrameLabelCount EncodedU32                           Number of frame labels
FrameNumLabels  <EncodedU32,STRING>[FrameLabelCount] Frame number (zero-based, global to symbol) and string for frame labels
\end{record}


Chapter 5: Actions
~~~~~~~~~~~~~~~~~~

p68: DoAction
\begin{record}
DoAction
Field         Type          Comment
Header        RECORDHEADER  Tag type = 12
Actions       ACTIONRECORDS List of actions to perform (see following table, ActionRecord)
\end{record}

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
\genconstructors{action}
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

\genfunctions{action}

p69: ActionGotoFrame
\begin{record}
ActionGotoFrame
Field           Type               Comment
ActionGotoFrame ACTIONRECORDHEADER ActionCode = 0x81; Length is always 2
Frame           UI16               Frame index
\end{record}

p69: ActionGetURL
\begin{record}
ActionGetURL
Field        Type               Comment
ActionGetURL ACTIONRECORDHEADER ActionCode = 0x83
UrlString    STRING             Target URL string
TargetString STRING             Target string
\end{record}

p69: ActionNextFrame
\begin{record}
ActionNextFrame
Field           Type               Comment
ActionNextFrame ACTIONRECORDHEADER ActionCode = 0x04
\end{record}

p70: ActionPreviousFrame
\begin{record}
ActionPreviousFrame
Field               Type               Comment
ActionPreviousFrame ACTIONRECORDHEADER ActionCode = 0x05
\end{record}

p70: ActionPlay
\begin{record}
ActionPlay
Field      Type               Comment
ActionPlay ACTIONRECORDHEADER ActionCode = 0x06
\end{record}

p70: ActionStop
\begin{record}
ActionStop
Field      Type               Comment
ActionStop ACTIONRECORDHEADER ActionCode = 0x07
\end{record}

p70: ActionToggleQuality
\begin{record}
ActionToggleQuality
Field               Type               Comment
ActionToggleQuality ACTIONRECORDHEADER ActionCode = 0x08
\end{record}

p70: ActionStopSounds
\begin{record}
ActionStopSounds
Field            Type               Comment
ActionStopSounds ACTIONRECORDHEADER ActionCode = 0x09
\end{record}

p71: ActionWaitForFrame
\begin{record}
ActionWaitForFrame
Field              Type               Comment
ActionWaitForFrame ACTIONRECORDHEADER ActionCode = 0x8A; Length is always 3
Frame              UI16               Frame to wait for
SkipCount          UI8                Number of actions to skip if frame is not loaded
\end{record}

p71: ActionSetTarget
\begin{record}
ActionSetTarget
Field           Type               Comment
ActionSetTarget ACTIONRECORDHEADER ActionCode = 0x8B
TargetName      STRING             Target of action target
\end{record}

p71: ActionGoToLabel
\begin{record}
ActionGoToLabel
Field           Type               Comment
ActionGoToLabel ACTIONRECORDHEADER ActionCode = 0x8C
Label           STRING             Frame label
\end{record}

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
\begin{record}
ActionPop
Field     Type               Comment
ActionPop ACTIONRECORDHEADER ActionCode = 0x17
\end{record}

p76: ActionAdd
\begin{record}
ActionAdd
Field     Type               Comment
ActionAdd ACTIONRECORDHEADER ActionCode = 0x0A
\end{record}

p76: ActionSubtract
\begin{record}
ActionSubtract
Field          Type               Comment
ActionSubtract ACTIONRECORDHEADER ActionCode = 0x0B
\end{record}

p76: ActionMultiply
\begin{record}
ActionMultiply
Field          Type               Comment
ActionMultiply ACTIONRECORDHEADER ActionCode = 0x0C
\end{record}

p77: ActionDivide
\begin{record}
ActionDivide
Field        Type               Comment
ActionDivide ACTIONRECORDHEADER ActionCode = 0x0D
\end{record}

p77: ActionEquals
\begin{record}
ActionEquals
Field        Type               Comment
ActionEquals ACTIONRECORDHEADER ActionCode = 0x0E
\end{record}

p78: ActionLess
\begin{record}
ActionLess
Field      Type               Comment
ActionLess ACTIONRECORDHEADER ActionCode = 0x0F
\end{record}

p78: ActionAnd
\begin{record}
ActionAnd
Field     Type               Comment
ActionAnd ACTIONRECORDHEADER ActionCode = 0x10
\end{record}

p79: ActionOr
\begin{record}
ActionOr
Field    Type               Comment
ActionOr ACTIONRECORDHEADER ActionCode = 0x11
\end{record}

p79: ActionNot
\begin{record}
ActionNot
Field     Type               Comment
ActionNot ACTIONRECORDHEADER ActionCode = 0x12
\end{record}

p80: ActionStringEquals
\begin{record}
ActionStringEquals
Field              Type               Comment
ActionStringEquals ACTIONRECORDHEADER ActionCode = 0x13
\end{record}

p80: ActionStringLength
\begin{record}
ActionStringLength
Field              Type               Comment
ActionStringLength ACTIONRECORDHEADER ActionCode = 0x14
\end{record}

p80: ActionStringAdd
\begin{record}
ActionStringAdd
Field           Type               Comment
ActionStringAdd ACTIONRECORDHEADER ActionCode = 0x21
\end{record}

p81: ActionStringExtract
\begin{record}
ActionStringExtract
Field               Type               Comment
ActionStringExtract ACTIONRECORDHEADER ActionCode = 0x15
\end{record}

p81: ActionStringLess
\begin{record}
ActionStringLess
Field            Type               Comment
ActionStringLess ACTIONRECORDHEADER ActionCode = 0x29
\end{record}

p81: ActionMBStringLength
\begin{record}
ActionMBStringLength
Field                Type               Comment
ActionMBStringLength ACTIONRECORDHEADER ActionCode = 0x31
\end{record}

p82: ActionMBStringExtract
\begin{record}
ActionMBStringExtract
Field                 Type               Comment
ActionMBStringExtract ACTIONRECORDHEADER ActionCode = 0x35
\end{record}

p82: ActionToInteger
\begin{record}
ActionToInteger
Field           Type               Comment
ActionToInteger ACTIONRECORDHEADER ActionCode = 0x18
\end{record}

p83: ActionCharToAscii
\begin{record}
ActionCharToAscii
Field             Type               Comment
ActionCharToAscii ACTIONRECORDHEADER ActionCode = 0x32
\end{record}

p83: ActionAsciiToChar
\begin{record}
ActionAsciiToChar
Field             Type               Comment
ActionAsciiToChar ACTIONRECORDHEADER ActionCode = 0x33
\end{record}

p83: ActionMBCharToAscii
\begin{record}
ActionMBCharToAscii
Field               Type               Comment
ActionMBCharToAscii ACTIONRECORDHEADER ActionCode = 0x36
\end{record}

p84: ActionMBAsciiToChar
\begin{record}
ActionMBAsciiToChar
Field               Type               Comment
ActionMBAsciiToChar ACTIONRECORDHEADER ActionCode = 0x37
\end{record}

p84: ActionJump
\begin{record}
ActionJump
Field        Type               Comment
ActionJump   ACTIONRECORDHEADER ActionCode = 0x99
BranchOffset SI16               Offset
\end{record}

p84: ActionIf
\begin{record}
ActionIf
Field        Type               Comment
ActionIf     ACTIONRECORDHEADER ActionCode = 0x9D
BranchOffset SI16               Offset
\end{record}

p85: ActionCall
\begin{record}
ActionCall
Field      Type               Comment
ActionCall ACTIONRECORDHEADER ActionCode = 0x9E
\end{record}

p86: ActionGetVariable
\begin{record}
ActionGetVariable
Field             Type               Comment
ActionGetVariable ACTIONRECORDHEADER ActionCode = 0x1C
\end{record}

p86: ActionSetVariable
\begin{record}
ActionSetVariable
Field             Type               Comment
ActionSetVariable ACTIONRECORDHEADER ActionCode = 0x1D
\end{record}

p87: ActionGetURL2
\begin{record}
ActionGetURL2
Field             Type               Comment
ActionGetURL2     ACTIONRECORDHEADER ActionCode = 0x9A; Length is always 1
SendVarsMethod    UB[2]              0 = None, 1 = GET, 2 = POST 
Reserved          UB[4]              Always 0
LoadTargetFlag    UB[1]              0 = Target is a browser window 1 = Target is a path to a sprite
LoadVariablesFlag UB[1]              0 = No variables to load 1 = Load variables
\end{record}

p88: ActionGotoFrame2
\begin{record}
ActionGotoFrame2
Field            Type                       Comment
ActionGotoFrame2 ACTIONRECORDHEADER         ActionCode = 0x9F
Reserved         UB[6]                      Always 0
SceneBiasFlag    UB[1]                      Scene bias flag
PlayFlag         UB[1]                      0 = Go to frame and stop 1 = Go to frame and play
SceneBias        If SceneBiasFlag = 1, UI16 Number to be added to frame determined by stack argument
\end{record}

p89: ActionSetTarget2
\begin{record}
ActionSetTarget2
Field            Type               Comment
ActionSetTarget2 ACTIONRECORDHEADER ActionCode = 0x20
\end{record}

p89: ActionGetProperty
\begin{record}
ActionGetProperty
Field             Type               Comment
ActionGetProperty ACTIONRECORDHEADER ActionCode = 0x22
\end{record}

p90: ActionSetProperty
\begin{record}
ActionSetProperty
Field             Type               Comment
ActionSetProperty ACTIONRECORDHEADER ActionCode = 0x23
\end{record}

p90: ActionCloneSprite
\begin{record}
ActionCloneSprite
Field             Type               Comment
ActionCloneSprite ACTIONRECORDHEADER ActionCode = 0x24
\end{record}

p91: ActionRemoveSprite
\begin{record}
ActionRemoveSprite
Field              Type               Comment
ActionRemoveSprite ACTIONRECORDHEADER ActionCode = 0x25
\end{record}

p91: ActionStartDrag
\begin{record}
ActionStartDrag
Field           Type               Comment
ActionStartDrag ACTIONRECORDHEADER ActionCode = 0x27
\end{record}

p92: ActionEndDrag
\begin{record}
ActionEndDrag
Field         Type               Comment
ActionEndDrag ACTIONRECORDHEADER ActionCode = 0x28
\end{record}

p92: ActionWaitForFrame2
\begin{record}
ActionWaitForFrame2
Field               Type               Comment
ActionWaitForFrame2 ACTIONRECORDHEADER ActionCode = 0x8D; Length is always 1
SkipCount           UI8                The number of actions to skip
\end{record}

p92: ActionTrace
\begin{record}
ActionTrace
Field       Type               Comment
ActionTrace ACTIONRECORDHEADER ActionCode = 0x26
\end{record}

p93: ActionGetTime
\begin{record}
ActionGetTime
Field         Type               Comment
ActionGetTime ACTIONRECORDHEADER ActionCode = 0x34
\end{record}

p93: ActionRandomNumber
\begin{record}
ActionRandomNumber
Field              Type               Comment
ActionRandomNumber ACTIONRECORDHEADER ActionCode = 0x30
\end{record}

p95: ActionCallFunction
\begin{record}
ActionCallFunction
Field              Type               Comment
ActionCallFunction ACTIONRECORDHEADER ActionCode = 0x3D
\end{record}

p95: ActionCallMethod
\begin{record}
ActionCallMethod
Field            Type               Comment
ActionCallMethod ACTIONRECORDHEADER ActionCode = 0x52
\end{record}

p96: ActionConstantPool
\begin{record}
ActionConstantPool
Field              Type               Comment
ActionConstantPool ACTIONRECORDHEADER ActionCode = 0x88
Count              UI16               Number of constants to follow
ConstantPool       STRING[Count]      String constants
\end{record}

p97: ActionDefineFunction
\begin{record}
ActionDefineFunction
Field                Type               Comment
ActionDefineFunction ACTIONRECORDHEADER ActionCode = 0x9B
FunctionName         STRING             Function name, empty if anonymous
NumParams            UI16               # of parameters
Params               STRING[NumParams]  Paramaters
CodeSize             UI16               # of bytes of code that follow
\end{record}

p98: ActionDefineLocal
\begin{record}
ActionDefineLocal
Field             Type               Comment
ActionDefineLocal ACTIONRECORDHEADER ActionCode = 0x3C
\end{record}

p98: ActionDefineLocal2
\begin{record}
ActionDefineLocal2
Field              Type               Comment
ActionDefineLocal2 ACTIONRECORDHEADER ActionCode = 0x41
\end{record}

p98: ActionDelete
\begin{record}
ActionDelete
Field        Type               Comment
ActionDelete ACTIONRECORDHEADER ActionCode = 0x3A
\end{record}

p99: ActionDelete2
\begin{record}
ActionDelete2
Field         Type               Comment
ActionDelete2 ACTIONRECORDHEADER ActionCode = 0x3B
\end{record}

p99: ActionEnumerate
\begin{record}
ActionEnumerate
Field           Type               Comment
ActionEnumerate ACTIONRECORDHEADER ActionCode = 0x46
\end{record}

p99: ActionEquals2
\begin{record}
ActionEquals2
Field         Type               Comment
ActionEquals2 ACTIONRECORDHEADER ActionCode = 0x49
\end{record}

p100: ActionGetMember
\begin{record}
ActionGetMember
Field           Type               Comment
ActionGetMember ACTIONRECORDHEADER ActionCode = 0x4E
\end{record}

p101: ActionInitArray
\begin{record}
ActionInitArray
Field           Type               Comment
ActionInitArray ACTIONRECORDHEADER ActionCode = 0x42
\end{record}

p101: ActionInitObject
\begin{record}
ActionInitObject
Field            Type               Comment
ActionInitObject ACTIONRECORDHEADER ActionCode = 0x43
\end{record}

p102: ActionNewMethod
\begin{record}
ActionNewMethod
Field           Type               Comment
ActionNewMethod ACTIONRECORDHEADER ActionCode = 0x53
\end{record}

p103: ActionNewObject
\begin{record}
ActionNewObject
Field           Type               Comment
ActionNewObject ACTIONRECORDHEADER ActionCode = 0x40
\end{record}

p103: ActionSetMember
\begin{record}
ActionSetMember
Field           Type               Comment
ActionSetMember ACTIONRECORDHEADER ActionCode = 0x4F
\end{record}

p104: ActionTargetPath
\begin{record}
ActionTargetPath
Field            Type               Comment
ActionTargetPath ACTIONRECORDHEADER ActionCode = 0x45
\end{record}

p104: ActionWith
\begin{record}
ActionWith
Field      Type               Comment
ActionWith ACTIONRECORDHEADER ActionCode = 0x94
Size       UI16               # of bytes of code that follow
\end{record}

p105: ActionToNumber
\begin{record}
ActionToNumber
Field          Type               Comment
ActionToNumber ACTIONRECORDHEADER ActionCode = 0x4A
\end{record}

p105: ActionToString
\begin{record}
ActionToString
Field          Type               Comment
ActionToString ACTIONRECORDHEADER ActionCode = 0x4B
\end{record}

p106: ActionTypeOf
\begin{record}
ActionTypeOf
Field        Type               Comment
ActionTypeOf ACTIONRECORDHEADER ActionCode = 0x44
\end{record}

p106: ActionAdd2
\begin{record}
ActionAdd2
Field      Type               Comment
ActionAdd2 ACTIONRECORDHEADER ActionCode = 0x47
\end{record}

p107: ActionLess2
\begin{record}
ActionLess2
Field       Type               Comment
ActionLess2 ACTIONRECORDHEADER ActionCode = 0x48
\end{record}

p107: ActionModulo
\begin{record}
ActionModulo
Field        Type               Comment
ActionModulo ACTIONRECORDHEADER ActionCode = 0x3F
\end{record}

p107: ActionBitAnd
\begin{record}
ActionBitAnd
Field        Type               Comment
ActionBitAnd ACTIONRECORDHEADER ActionCode = 0x60
\end{record}

p108: ActionBitLShift
\begin{record}
ActionBitLShift
Field           Type               Comment
ActionBitLShift ACTIONRECORDHEADER ActionCode = 0x63
\end{record}

p108: ActionBitOr
\begin{record}
ActionBitOr
Field       Type               Comment
ActionBitOr ACTIONRECORDHEADER ActionCode = 0x61
\end{record}

p109: ActionBitRShift
\begin{record}
ActionBitRShift
Field           Type               Comment
ActionBitRShift ACTIONRECORDHEADER ActionCode = 0x64
\end{record}

p109: ActionBitURShift
\begin{record}
ActionBitURShift
Field            Type               Comment
ActionBitURShift ACTIONRECORDHEADER ActionCode = 0x65
\end{record}

p110: ActionBitXor
\begin{record}
ActionBitXor
Field        Type               Comment
ActionBitXor ACTIONRECORDHEADER ActionCode = 0x62
\end{record}

p110: ActionDecrement
\begin{record}
ActionDecrement
Field           Type               Comment
ActionDecrement ACTIONRECORDHEADER ActionCode = 0x51
\end{record}

p110: ActionIncrement
\begin{record}
ActionIncrement
Field           Type               Comment
ActionIncrement ACTIONRECORDHEADER ActionCode = 0x50
\end{record}

p111: ActionPushDuplicate
\begin{record}
ActionPushDuplicate
Field               Type               Comment
ActionPushDuplicate ACTIONRECORDHEADER ActionCode = 0x4C
\end{record}

p111: ActionReturn
\begin{record}
ActionReturn
Field        Type               Comment
ActionReturn ACTIONRECORDHEADER ActionCode = 0x3E
\end{record}

p111: ActionStackSwap
\begin{record}
ActionStackSwap
Field           Type               Comment
ActionStackSwap ACTIONRECORDHEADER ActionCode = 0x4D
\end{record}

p111: ActionStoreRegister
\begin{record}
ActionStoreRegister
Field               Type               Comment
ActionStoreRegister ACTIONRECORDHEADER ActionCode = 0x87
RegisterNumber      UI8
\end{record}

p112: DoInitAction
\begin{record}
DoInitAction
Field    Type          Comment
Header   RECORDHEADER  Tag type = 59
SpriteID UI16          Sprite to which these actions apply
Actions  ACTIONRECORDS List of actions to perform
\end{record}

p113: ActionInstanceOf
\begin{record}
ActionInstanceOf
Field            Type               Comment
ActionInstanceOf ACTIONRECORDHEADER ActionCode = 0x54
\end{record}

p113: ActionEnumerate2
\begin{record}
ActionEnumerate2
Field            Type               Comment
ActionEnumerate2 ACTIONRECORDHEADER ActionCode = 0x55
\end{record}

p114: ActionStrictEquals
\begin{record}
ActionStrictEquals
Field              Type               Comment
ActionStrictEquals ACTIONRECORDHEADER ActionCode = 0x66
\end{record}

p114: ActionGreater
\begin{record}
ActionGreater
Field         Type               Comment
ActionGreater ACTIONRECORDHEADER ActionCode = 0x67
\end{record}

p115: ActionStringGreater
\begin{record}
ActionStringGreater
Field               Type               Comment
ActionStringGreater ACTIONRECORDHEADER ActionCode = 0x68
\end{record}

p116: ActionDefineFunction2
\begin{record}
ActionDefineFunction2
Field                 Type                     Comment
ActionDefineFunction2 ACTIONRECORDHEADER       ActionCode = 0x8E
FunctionName          STRING                   Name of function, empty if anonymous
NumParams             UI16                     of parameters
RegisterCount         UI8                      Number of registers to allocate, up to 255 registers (from 0 to 254)
PreloadParentFlag     UB[1]                    0 = Don’t preload _parent into register 1 = Preload _parent into register
PreloadRootFlag       UB[1]                    0 = Don’t preload _root into register 1 = Preload _root into register
SuppressSuperFlag     UB[1]                    0 = Create super variable 1 = Don’t create super variable
PreloadSuperFlag      UB[1]                    0 = Don’t preload super into register 1 = Preload super into register
SuppressArgumentsFlag UB[1]                    0 = Create arguments variable 1 = Don’t create arguments variable
PreloadArgumentsFlag  UB[1]                    0 = Don’t preload arguments into register 1 = Preload arguments into register
SuppressThisFlag      UB[1]                    0 = Create this variable 1 = Don’t create this variable
PreloadThisFlag       UB[1]                    0 = Don’t preload this into register 1 = Preload this into register
Reserved              UB[7]                    Always 0
PreloadGlobalFlag     UB[1]                    0 = Don’t preload _global into register 1 = Preload _global into register
Parameters            REGISTERPARAM[NumParams] See REGISTERPARAM, following
CodeSize              UI16                     # of bytes of code that follow
\end{record}

\begin{record}
REGISTERPARAM
Field     Type   Comment
Register  UI8    For each parameter to the function, a register can be specified. If the register specified is zero, the parameter is created as a variable named ParamName in the activation object, which can be referenced with ActionGetVariable and ActionSetVariable. If the register specified is nonzero, the parameter is copied into the register, and it can be referenced with ActionPush and ActionStoreRegister, and no variable is created in the activation object
ParamName STRING Parameter name
\end{record}

p119: ActionExtends
\begin{record}
ActionExtends
Field         Type               Comment
ActionExtends ACTIONRECORDHEADER ActionCode = 0x69
\end{record}

p119: ActionCastOp
\begin{record}
ActionCastOp
Field        Type               Comment
ActionCastOp ACTIONRECORDHEADER ActionCode = 0x2B
\end{record}

p120: ActionImplementsOp
\begin{record}
ActionImplementsOp
Field              Type               Comment
ActionImplementsOp ACTIONRECORDHEADER ActionCode = 0x2C
\end{record}

p121: ActionTy
\begin{record}
ActionTry
Field               Type                               Comment
ActionTry           ACTIONRECORDHEADER                 ActionCode = 0x8F
Reserved            UB[5]                              Always zero
CatchInRegisterFlag UB[1]                              0 - Do not put caught object into register (instead, store in named variable) 1 - Put caught object into register (do not store in named variable)
FinallyBlockFlag    UB[1]                              0 - No finally block 1 - Has finally block
CatchBlockFlag      UB[1]                              0 - No catch block 1 - Has catch block
TrySize             UI16                               Length of the try block
CatchSize           UI16                               Length of the catch block
FinallySize         UI16                               Length of the finally block
CatchName           If CatchInRegisterFlag = 0, STRING Name of the catch variable
CatchRegister       If CatchInRegisterFlag = 1, UI8    Register to catch into
TryBody             UI8[TrySize]                       Body of the try block
CatchBody           UI8[CatchSize]                     Body of the catch block, if any
FinallyBody         UI8[FinallySize]                   Body of the finally block, if any
\end{record}

p122: ActionThrow
\begin{record}
ActionThrow
Field       Type               Comment
ActionThrow ACTIONRECORDHEADER ActionCode = 0x2A
\end{record}

p123: DoABC
\begin{record}
DoABC
Field     Type          Comment
Header    RECORDHEADER  Tag type = 82
Flags     UI32          A 32-bit flags value, which may contain the following bits set: kDoAbcLazyInitializeFlag = 1: Indicates that the ABC  block should not be executed immediately, but only parsed. A later finddef may cause its scripts to execute.
Name      STRING        The name assigned to the bytecode.
ABCData   BYTE[]        A block of .abc bytecode to be parsed by the ActionScript 3.0 virtual machine, up to the end of the tag.
\end{record}


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

\begin{record}
LINESTYLE2
Field            Type                             Comment
Width            UI16                             Width of line in twips.
StartCapStyle    UB[2]                            Start cap style: 0 = Round cap 1 = No cap 2 = Square cap
JoinStyle        UB[2]                            Join style: 0 = Round join 1 = Bevel join 2 = Miter join
HasFillFlag      UB[1]                            If 1, fill is defined in FillType. If 0, uses Color field.
NoHScaleFlag     UB[1]                            If 1, stroke thickness will not scale if the object is scaled horizontally.
NoVScaleFlag     UB[1]                            If 1, stroke thickness will not scale if the object is scaled vertically.
PixelHintingFlag UB[1]                            If 1, all anchors will be aligned to full pixels.
Reserved         UB[5]                            Must be 0.
NoClose          UB[1]                            If 1, stroke will not be closed if the stroke’s last point matches its first point. Flash Player will apply caps instead of a join.
EndCapStyle      UB[2]                            End cap style: 0 = Round cap 1 = No cap 2 = Square cap
MiterLimitFactor If JoinStyle = 2, UI16           Miter limit factor is an 8.8 fixed-point value.
Color            If HasFillFlag = 0, RGBA         Color value including alpha channel.
FillType         If HasFillFlag = 1, FILLSTYLE(4) Fill style for this stroke.
\end{record}

p133: Shape Structures
\begin{record}
SHAPE(ShapeVer)
Field        Type                                             Comment
NumFillBits  UB[4]                                            Number of fill index bits.
NumLineBits  UB[4]                                            Number of line index bits.
ShapeRecords SHAPERECORDS(ShapeVer, NumFillBits, NumLineBits) Shape records (see following).
\end{record}

\begin{record}
SHAPEWITHSTYLE(ShapeVer)
Field        Type                                             Comment
FillStyles   FILLSTYLEARRAY(ShapeVer)                         Array of fill styles.
LineStyles   LINESTYLEARRAY(ShapeVer)                         Array of line styles.
NumFillBits  UB[4]                                            Number of fill index bits.
NumLineBits  UB[4]                                            Number of line index bits.
ShapeRecords SHAPERECORDS(ShapeVer, NumFillBits, NumLineBits) Shape records (see following).
\end{record}

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
\genconstructors{shaperecord}
                 deriving (Eq, Show, Typeable, Data)

\end{code}

NB: the various SHAPERECORDs are intentionally not padded to byte
align them, because they are packed together on disk. The entire
array will be aligned as a unit, however.

This appears to be the exact opposite of what the spec says happens!

\begin{record}
STYLECHANGERECORD(ShapeVer, FillBits, LineBits)
Field           Type                                        Comment
StateNewStyles  UB[1]                                       New styles flag. Used by DefineShape2 and DefineShape3 only.
StateLineStyle  UB[1]                                       Line style change flag.
StateFillStyle1 UB[1]                                       Fill style 1 change flag.
StateFillStyle0 UB[1]                                       Fill style 0 change flag.
StateMoveTo     UB[1]                                       Move to flag.
MoveBits        If StateMoveTo, UB[5]                       Move bit count.
MoveDeltaX      If StateMoveTo, SB[MoveBits]                Delta X value.
MoveDeltaY      If StateMoveTo, SB[MoveBits]                Delta Y value.
FillStyle0      If StateFillStyle0, UB[FillBits]            Fill 0 Style.
FillStyle1      If StateFillStyle1, UB[FillBits]            Fill 1 Style.
LineStyle       If StateLineStyle, UB[LineBits]             Line Style.
NewFillStyles   If StateNewStyles, FILLSTYLEARRAY(ShapeVer) Array of new fill styles.
NewLineStyles   If StateNewStyles, LINESTYLEARRAY(ShapeVer) Array of new line styles.
NewNumFillBits  If StateNewStyles, UB[4]                    Number of fill index bits for new styles.
NewNumLineBits  If StateNewStyles, UB[4]                    Number of line index bits for new styles.
\end{record}

\begin{record}
STRAIGHTEDGERECORD
Field           Type                  Comment
NumBits         UB[4]                 Number of bits per value (2 less than the actual number).
StraightEdge    StraightEdge(NumBits) Position information
\end{record}

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

\begin{record}
CURVEDEDGERECORD
Field         Type          Comment
NumBits       UB[4]         Number of bits per value (2 less than the actual number).
ControlDeltaX SB[NumBits+2] X control point change.
ControlDeltaY SB[NumBits+2] Y control point change.
AnchorDeltaX  SB[NumBits+2] X anchor point change.
AnchorDeltaY  SB[NumBits+2] Y anchor point change.
\end{record}

p140: DefineShape
\begin{record}
DefineShape
Field       Type              Comment
Header      RECORDHEADER      Tag type = 2
ShapeId     UI16              ID for this character.
ShapeBounds RECT              Bounds of the shape.
Shapes      SHAPEWITHSTYLE(1) Shape information.
\end{record}

p141: DefineShape2
\begin{record}
DefineShape2
Field       Type              Comment
Header      RECORDHEADER      Tag type = 22
ShapeId     UI16              ID for this character.
ShapeBounds RECT              Bounds of the shape.
Shapes      SHAPEWITHSTYLE(2) Shape information.
\end{record}

p141: DefineShape3
\begin{record}
DefineShape3
Field       Type              Comment
Header      RECORDHEADER      Tag type = 32
ShapeId     UI16              ID for this character.
ShapeBounds RECT              Bounds of the shape.
Shapes      SHAPEWITHSTYLE(3) Shape information.
\end{record}

p142: DefineShape4
\begin{record}
DefineShape4
Field                 Type              Comment
Header                RECORDHEADER      Tag type = 83
ShapeId               UI16              ID for this character.
ShapeBounds           RECT              Bounds of the shape.
EdgeBounds            RECT              Bounds of the shape, excluding strokes.
Reserved              UB[5]             Must be 0.
UsesFillWindingRule   UB[1]             If 1, use fill winding rule. Minimum file format version is SWF 10
UsesNonScalingStrokes UB[1]             If 1, the shape contains at least one non-scaling stroke.
UsesScalingStrokes    UB[1]             If 1, the shape contains at least one scaling stroke.
Shapes                SHAPEWITHSTYLE(4) Shape information
\end{record}


Chapter 7: Gradients
~~~~~~~~~~~~~~~~~~~~

p145: GRADIENT
\begin{record}
GRADIENT(ShapeVer)
Field             Type                               Comment
SpreadMode        UB[2]                              0 = Pad mode 1 = Reflect mode 2 = Repeat mode 3 = Reserved
InterpolationMode UB[2]                              0 = Normal RGB mode interpolation 1 = Linear RGB mode interpolation 2 and 3 = Reserved
NumGradients      UB[4]                              1 to 15
GradientRecords   GRADRECORD(ShapeVer)[NumGradients] Gradient records (see following)
\end{record}

p146: FOCALGRADIENT
\begin{record}
FOCALGRADIENT(ShapeVer)
Field             Type                               Comment
SpreadMode        UB[2]                              0 = Pad mode 1 = Reflect mode 2 = Repeat mode 3 = Reserved
InterpolationMode UB[2]                              0 = Normal RGB mode interpolation 1 = Linear RGB mode interpolation 2 and 3 = Reserved
NumGradients      UB[4]                              1 to 15
GradientRecords   GRADRECORD(ShapeVer)[NumGradients] Gradient records (see following)
FocalPoint        FIXED8                             Focal point location
\end{record}

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
\begin{record}
DefineBits
Field       Type         Comment
Header      RECORDHEADER Tag type = 6
CharacterID UI16         ID for this character
JPEGData    BYTE[]       JPEG compressed image
\end{record}

p148: JPEGTables
\begin{record}
JPEGTables
Field    Type         Comment
Header   RECORDHEADER Tag type = 8
JPEGData BYTE[]       JPEG encoding table
\end{record}

p149: DefineBitsJPEG2
\begin{record}
DefineBitsJPEG2
Field       Type         Comment
Header      RECORDHEADER Tag type = 21
CharacterID UI16         ID for this character
ImageData   BYTE[]       Compressed image data in either JPEG, PNG, or GIF89a format
\end{record}

p149: DefineBitsJPEG3
\begin{record}
DefineBitsJPEG3
Field           Type                 Comment
Header          RECORDHEADER         Tag type = 35
CharacterID     UI16                 ID for this character.
AlphaDataOffset UI32                 Count of bytes in ImageData.
ImageData       UI8[AlphaDataOffset] Compressed image data in either JPEG, PNG, or GIF89a format
BitmapAlphaData BYTE[]               ZLIB compressed array of alpha data. Only supported when tag contains JPEG data. One byte per pixel. Total size after decompression must equal (width * height) of JPEG image.
\end{record}

p150: DefineBitsLossless
\begin{record}
DefineBitsLossless
Field                Type                     Comment
Header               RECORDHEADER             Tag type = 20
CharacterID          UI16                     ID for this character
BitmapFormat         UI8                      Format of compressed data 3 = 8-bit colormapped image 4 = 15-bit RGB image 5 = 24-bit RGB image
BitmapWidth          UI16                     Width of bitmap image
BitmapHeight         UI16                     Height of bitmap image
BitmapColorTableSize If BitmapFormat = 3, UI8 This value is one less than the actual number of colors in the color table, allowing for up to 256 colors.
ZlibBitmapData       BYTE[]                   ZLIB compressed bitmap data. If BitmapFormat = 3, COLORMAPDATA If BitmapFormat = 4 or 5, BITMAPDATA
\end{record}

p153: DefineBitsLossless2
\begin{record}
DefineBitsLossless2
Field                Type                     Comment
Header               RECORDHEADER             Tag type = 36
CharacterID          UI16                     ID for this character
BitmapFormat         UI8                      Format of compressed data 3 = 8-bit colormapped image 5 = 32-bit ARGB        image
BitmapWidth          UI16                     Width of bitmap image
BitmapHeight         UI16                     Height of bitmap image
BitmapColorTableSize If BitmapFormat = 3, UI8 This value is one less than the actual number of colors in the color table, allowing for up to 256 colors.
ZlibBitmapData       BYTE[]                   ZLIB compressed bitmap data. If BitmapFormat = 3, ALPHACOLORMAPDATA If BitmapFormat = 4 or 5, ALPHABITMAPDATA
\end{record}

p154: DefineBitsJPEG4
\begin{record}
DefineBitsJPEG4
Field           Type                 Comment
Header          RECORDHEADER         Tag type = 90
CharacterID     UI16                 ID for this character.
AlphaDataOffset UI32                 Count of bytes in ImageData.
DeblockParam    UI16                 Parameter to be fed into the deblocking filter. The parameter describes a relative strength of the deblocking filter from 0- 100% expressed in a normalized 8.8 fixed point format.
ImageData       UI8[AlphaDataOffset] Compressed image data in either JPEG, PNG, or GIF89a format.
BitmapAlphaData BYTE[]               ZLIB compressed array of alpha data. Only supported when tag contains JPEG data. One byte per pixel. Total size after decompression must equal (width * height) of JPEG image.
\end{record}


Chapter 9: Shape Morphing
~~~~~~~~~~~~~~~~~~~~~~~~~

p159: DefineMorphShape
\begin{record}
DefineMorphShape
Field           Type                   Comment
Header          RECORDHEADER           Tag type = 46
CharacterId     UI16                   ID for this character
StartBounds     RECT                   Bounds of the start shape
EndBounds       RECT                   Bounds of the end shape
Offset          UI32                   Indicates offset to EndEdges
MorphFillStyles MORPHFILLSTYLEARRAY    Fill style information is stored in the same manner as for a standard shape; however, each fill consists of interleaved information based on a single style type to accommodate morphing.
MorphLineStyles MORPHLINESTYLEARRAY(1) Line style information is stored in the same manner as for a standard shape; however, each line consists of interleaved information based on a single style type to accommodate morphing.
StartEdges      SHAPE(3)               Contains the set of edges and the style bits that indicate style changes (for example, MoveTo, FillStyle, and LineStyle). Number of edges must equal the number of edges in EndEdges.
EndEdges        SHAPE(3)               Contains only the set of edges, with no style information. Number of edges must equal the number of edges in StartEdges.
\end{record}

p161: DefineMorphShape2
\begin{record}
DefineMorphShape2
Field                 Type                   Comment
Header                RECORDHEADER           Tag type = 84
CharacterId           UI16                   ID for this character
StartBounds           RECT                   Bounds of the start shape
EndBounds             RECT                   Bounds of the end shape
StartEdgeBounds       RECT                   Bounds of the start shape, excluding strokes
EndEdgeBounds         RECT                   Bounds of the end shape, excluding strokes
Reserved              UB[6]                  Must be 0
UsesNonScalingStrokes UB[1]                  If 1, the shape contains at least one non-scaling stroke.
UsesScalingStrokes    UB[1]                  If 1, the shape contains at least one scaling stroke.
Offset                UI32                   Indicates offset to EndEdges
MorphFillStyles       MORPHFILLSTYLEARRAY    Fill style information is stored in the same manner as for a standard shape; however, each fill consists of interleaved information based on a single style type to accommodate morphing.
MorphLineStyles       MORPHLINESTYLEARRAY(2) Line style information is stored in the same manner as for a standard shape; however, each line consists of interleaved information based on a single style type to accommodate morphing.
StartEdges            SHAPE(3)               Contains the set of edges and the style bits that indicate style changes (for example, MoveTo, FillStyle, and LineStyle). Number of edges must equal the number of edges in EndEdges.
EndEdges              SHAPE(3)               Contains only the set of edges, with no style information. Number of edges must equal the number of edges in StartEdges.
\end{record}

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

\begin{record}
MORPHGRADRECORD
Field      Type Comment
StartRatio UI8  Ratio value for start shape.
StartColor RGBA Color of gradient for start shape.
EndRatio   UI8  Ratio value for end shape.
EndColor   RGBA Color of gradient for end shape
\end{record}

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

\begin{record}
MORPHLINESTYLE
Field      Type Comment
StartWidth UI16 Width of line in start shape in twips.
EndWidth   UI16 Width of line in end shape in twips.
StartColor RGBA Color value including alpha channel information for start shape.
EndColor   RGBA Color value including alpha channel information for end shape.
\end{record}

\begin{record}
MORPHLINESTYLE2
Field            Type                               Comment
StartWidth       UI16                               Width of line in start shape in twips.
EndWidth         UI16                               Width of line in end shape in twips.
StartCapStyle    UB[2]                              Start-cap style: 0 = Round cap 1 = No cap 2 = Square cap
JoinStyle        UB[2]                              Join style: 0 = Round join 1 = Bevel join 2 = Miter join
HasFillFlag      UB[1]                              If 1, fill is defined in FillType. If 0, uses StartColor and EndColor fields.
NoHScaleFlag     UB[1]                              If 1, stroke thickness will not scale if the object is scaled horizontally.
NoVScaleFlag     UB[1]                              If 1, stroke thickness will not scale if the object is scaled vertically.
PixelHintingFlag UB[1]                              If 1, all anchors will be aligned to full pixels.
Reserved         UB[5]                              Must be 0.
NoClose          UB[1]                              If 1, stroke will not be closed if the stroke’s last point matches its first point. Flash Player will apply caps instead of a join.
EndCapStyle      UB[2]                              End-cap style: 0 = Round cap 1 = No cap 2 = Square cap
MiterLimitFactor If JoinStyle = 2, UI16             Miter limit factor as an 8.8 fixed-point value.
StartColor       If HasFillFlag = 0, RGBA           Color value including alpha channel information for start shape.
EndColor         If HasFillFlag = 0, RGBA           Color value including alpha channel information for end shape.
FillType         If HasFillFlag = 1, MORPHFILLSTYLE Fill style.
\end{record}


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
\begin{record}
DefineFontInfo
Field              Type                                           Comment
Header             RECORDHEADER                                   Tag type = 13
FontID             UI16                                           Font ID this information is for.
FontNameLen        UI8                                            Length of font name.
FontName           UI8[FontNameLen]                               Name of the font (see following).
FontFlagsReserved  UB[2]                                          Reserved bit fields.
FontFlagsSmallText UB[1]                                          SWF 7 file format or later: Font is small. Character glyphs are aligned on pixel boundaries for dynamic and input text.
FontFlagsShiftJIS  UB[1]                                          ShiftJIS character codes.
FontFlagsANSI      UB[1]                                          ANSI character codes.
FontFlagsItalic    UB[1]                                          Font is italic.
FontFlagsBold      UB[1]                                          Font is bold.
FontFlagsWideCodes UB[1]                                          If 1, CodeTable is UI16 array; otherwise, CodeTable is UI8 array.
CodeTable          If FontFlagsWideCodes, UI16[] Otherwise, UI8[] Glyph to code table, sorted in ascending order.
\end{record}

p180: DefineFontInfo2
\begin{record}
DefineFontInfo2
Field              Type             Comment
Header             RECORDHEADER     Tag type = 62
FontID             UI16             Font ID this information is for.
FontNameLen        UI8              Length of font name.
FontName           UI8[FontNameLen] Name of the font.
FontFlagsReserved  UB[2]            Reserved bit fields.
FontFlagsSmallText UB[1]            SWF 7 or later: Font is small. Character glyphs are aligned on pixel boundaries for dynamic and input text.
FontFlagsShiftJIS  UB[1]            Always 0.
FontFlagsANSI      UB[1]            Always 0.
FontFlagsItalic    UB[1]            Font is italic.
FontFlagsBold      UB[1]            Font is bold.
FontFlagsWideCodes UB[1]            Always 1.
LanguageCode       LANGCODE         Language ID.                       
CodeTable          UI16[]           Glyph to code table in UCS-2, sorted in ascending order.
\end{record}

p181: DefineFont2
\begin{record}
DefineFont2          
Field                  Type                                                                             Comment
Header                 RECORDHEADER                                                                     Tag type = 48
FontID                 UI16                                                                             ID for this font character.
FontFlagsHasLayout     UB[1]                                                                            Has font metrics/layout information.
FontFlagsShiftJIS      UB[1]                                                                            ShiftJIS encoding.
FontFlagsSmallText     UB[1]                                                                            SWF 7 or later: Font is small. Character glyphs are aligned on pixel boundaries for dynamic and input text.
FontFlagsANSI          UB[1]                                                                            ANSI encoding.
FontFlagsWideOffsets   UB[1]                                                                            If 1, uses 32 bit offsets.
FontFlagsWideCodes     UB[1]                                                                            If 1, font uses 16-bit codes; otherwise font uses 8 bit codes.
FontFlagsItalic        UB[1]                                                                            Italic Font.
FontFlagsBold          UB[1]                                                                            Bold Font.
LanguageCode           LANGCODE                                                                         SWF 5 or earlier: always 0 SWF 6 or later: language code
FontNameLen            UI8                                                                              Length of name.
FontName               UI8[FontNameLen]                                                                 Name of font (see DefineFontInfo).
NumGlyphs              UI16                                                                             Count of glyphs in font. May be zero for device fonts.
OffsetTable            If FontFlagsWideOffsets, UI32[NumGlyphs] Otherwise UI16[NumGlyphs]               Same as in DefineFont.
CodeTableOffset        If FontFlagsWideOffsets, UI32 Otherwise UI16                                     Byte count from start of OffsetTable to start of CodeTable.
GlyphShapeTable        SHAPE(3)[NumGlyphs]                                                              Same as in DefineFont.
CodeTable              If FontFlagsWideCodes, UI16[NumGlyphs] Otherwise UI8[NumGlyphs]                  Sorted in ascending order. Always UCS-2 in SWF 6 or later.
FontLayoutAscent       If FontFlagsHasLayout, SI16                                                      Font ascender height.
FontLayoutDescent      If FontFlagsHasLayout, SI16                                                      Font descender height.
FontLayoutLeading      If FontFlagsHasLayout, SI16                                                      Font leading height (see following).
FontLayoutAdvanceTable If FontFlagsHasLayout, SI16[NumGlyphs]                                           Advance value to be used for each glyph in dynamic glyph text.
FontLayoutBoundsTable  If FontFlagsHasLayout, RECT[NumGlyphs]                                           Not used in Flash Player through version 7 (but must be present).
FontLayoutKerningCount If FontFlagsHasLayout, UI16                                                      Not used in Flash Player through version 7 (always set to 0 to save space).
FontLayoutKerningTable If FontFlagsHasLayout, KERNINGRECORD(FontFlagsWideCodes)[FontLayoutKerningCount] Not used in Flash Player through version 7 (omit with KerningCount of 0).
\end{record}

p184: DefineFont3
\begin{record}
DefineFont3
Field                  Type                                                                             Comment
Header                 RECORDHEADER                                                                     Tag type = 75
FontID                 UI16                                                                             ID for this font character.
FontFlagsHasLayout     UB[1]                                                                            Has font metrics/layout information.
FontFlagsShiftJIS      UB[1]                                                                            ShiftJIS encoding.
FontFlagsSmallText     UB[1]                                                                            SWF 7 or later: Font is small. Character glyphs are aligned on pixel boundaries for dynamic and input text.
FontFlagsANSI          UB[1]                                                                            ANSI encoding.
FontFlagsWideOffsets   UB[1]                                                                            If 1, uses 32 bit offsets.
FontFlagsWideCodes     UB[1]                                                                            Must be 1.
FontFlagsItalic        UB[1]                                                                            Italic Font.
FontFlagsBold          UB[1]                                                                            Bold Font.
LanguageCode           LANGCODE                                                                         SWF 5 or earlier: always 0SWF 6 or later: language code
FontNameLen            UI8                                                                              Length of name.
FontName               UI8[FontNameLen]                                                                 Name of font (see DefineFontInfo).
NumGlyphs              UI16                                                                             Count of glyphs in font. May be zero for device fonts.
OffsetTable            If FontFlagsWideOffsets, UI32[NumGlyphs] Otherwise UI16[NumGlyphs]               Same as in DefineFont.
CodeTableOffset        If FontFlagsWideOffsets, UI32 Otherwise UI16                                     Byte count from start of OffsetTable to start of CodeTable.
GlyphShapeTable        SHAPE(4)[NumGlyphs]                                                              Same as in DefineFont.
CodeTable              UI16[NumGlyphs]                                                                  Sorted in ascending order. Always UCS-2 in SWF 6 or later.
FontLayoutAscent       If FontFlagsHasLayout, SI16                                                      Font ascender height.
FontLayoutDescent      If FontFlagsHasLayout, SI16                                                      Font descender height.
FontLayoutLeading      If FontFlagsHasLayout, SI16                                                      Font leading height (see following).
FontLayoutAdvanceTable If FontFlagsHasLayout, SI16[NumGlyphs]                                           Advance value to be used for each glyph in dynamic glyph text.
FontLayoutBoundsTable  If FontFlagsHasLayout, RECT[NumGlyphs]                                           Not used in Flash Player through version 7 (but must be present).
FontLayoutKerningCount If FontFlagsHasLayout, UI16                                                      Not used in Flash Player through version 7 (always set to 0 to save space).
FontLayoutKerningTable If FontFlagsHasLayout, KERNINGRECORD(FontFlagsWideCodes)[FontLayoutKerningCount] Not used in Flash Player through version 7 (omit with KerningCount of 0).
\end{record}

p186: DefineFontAlignZones
\begin{record}
DefineFontAlignZones
Field        Type         Comment
Header       RECORDHEADER Tag type = 73
FontID       UI16         ID of font to use, specified by DefineFont3.
CSMTableHint UB[2]        Font thickness hint. Refers to the thickness of the typical stroke used in the font. 0 = thin 1 = medium 2 = thick. Flash Player maintains a selection of CSM tables for many fonts. However, if the font is not found in Flash Player's internal table, this hint is used to choose an appropriate table.
Reserved     UB[6]        Must be 0.
ZoneTable    ZONERECORD[] Alignment zone information for each glyph
\end{record}

\begin{record}
ZONERECORD
Field       Type                  Comment
NumZoneData UI8                   Number of ZoneData entries. Always 2.
ZoneData    ZONEDATA[NumZoneData] Compressed alignment zone information.
Reserved    UB[6]                 Must be 0.
ZoneMaskY   UB[1]                 Set if there are Y alignment zones.
ZoneMaskX   UB[1]                 Set if there are X alignment zones.
\end{record}

\begin{record}
ZONEDATA
Field               Type    Comment
AlignmentCoordinate FLOAT16 X (left) or Y (baseline) coordinate of the alignment zone.
Range               FLOAT16 Width or height of the alignment zone.
\end{record}

p188: Kerning record
\begin{record}
KERNINGRECORD(FontFlagsWideCodes)
Field                 Type                                                     Comment
FontKerningCodes      If FontFlagsWideCodes, <UI16, UI16> Otherwise <UI8, UI8> Character code of the left and right character respectively.
FontKerningAdjustment SI16                                                     Adjustment relative to left character’s advance value
\end{record}

p188: DefineFontName
\begin{record}
DefineFontName
Field         Type         Comment
Header        RECORDHEADER Tag type = 88
FontID        UI16         ID for this font to which this refers
FontName      STRING       Name of the font. For fonts starting as Type 1, this is the PostScript FullName. For fonts starting in sfnt formats such as TrueType and OpenType, this is name ID 4, platform ID 1, language ID 0 (Full name, Mac OS, English).
FontCopyright STRING       Arbitrary string of copyright information
\end{record}

p189: DefineText
\begin{record}
DefineText
Field            Type                                   Comment
Header           RECORDHEADER                           Tag type = 11
CharacterID      UI16                                   ID for this text character.
TextBounds       RECT                                   Bounds of the text.
TextMatrix       MATRIX                                 Transformation matrix for the text.
GlyphBits        UI8                                    Bits in each glyph index.
AdvanceBits      UI8                                    Bits in each advance value.
TextRecords      TEXTRECORDS(1, GlyphBits, AdvanceBits) Text records.
\end{record}

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

\begin{record}
TEXTRECORD(TextVer, GlyphBits, AdvanceBits)
Field                Type                                                      Comment
TextRecordType       UB[1]                                                     Always 1.
StyleFlagsReserved   UB[3]                                                     Always 0.
StyleFlagsHasFont    UB[1]                                                     1 if text font specified.
StyleFlagsHasColor   UB[1]                                                     1 if text color specified.
StyleFlagsHasYOffset UB[1]                                                     1 if y offset specified.
StyleFlagsHasXOffset UB[1]                                                     1 if x offset specified.
FontID               If StyleFlagsHasFont, UI16                                Font ID for following text.
TextColor            If StyleFlagsHasColor, If TextVer = 2, RGBA Otherwise RGB Font color for following text.
XOffset              If StyleFlagsHasXOffset, SI16                             x offset for following text.
YOffset              If StyleFlagsHasYOffset, SI16                             y offset for following text.
TextHeight           If StyleFlagsHasFont, UI16                                Font height for following text.
GlyphCount           UI8                                                       Number of glyphs in record.
GlyphEntries         GLYPHENTRY(GlyphBits, AdvanceBits)[GlyphCount]            Glyph entry (see following).
Padding              PADDING8                                                  Padding to byte boundary
\end{record}

p192: Glyph entry
\begin{record}
GLYPHENTRY(GlyphBits, AdvanceBits)
Field        Type            Comment
GlyphIndex   UB[GlyphBits]   Glyph index into current font.
GlyphAdvance SB[AdvanceBits] x advance value for glyph.
\end{record}

p192: DefineText2
\begin{record}
DefineText2
Field            Type                                   Comment
Header           RECORDHEADER                           Tag type = 33
CharacterID      UI16                                   ID for this text character.
TextBounds       RECT                                   Bounds of the text.
TextMatrix       MATRIX                                 Transformation matrix.
GlyphBits        UI8                                    Bits in each glyph index.
AdvanceBits      UI8                                    Bits in each advance value.
TextRecords      TEXTRECORDS(2, GlyphBits, AdvanceBits) Text records.
\end{record}

p193: DefineEditText
\begin{record}
DefineEditText
Field             Type                    Comment
Header            RECORDHEADER            Tag type = 37
CharacterID       UI16                    ID for this dynamic text character.
Bounds            RECT                    Rectangle that completely encloses the text field.
HasText           UB[1]                   0 = text field has no default text. 1 = text field initially displays the string specified by InitialText.
WordWrap          UB[1]                   0 = text will not wrap and will scroll sideways. 1 = text will wrap automatically when the end of line is reached.
Multiline         UB[1]                   0 = text field is one line only. 1 = text field is multi-line and scrollable.
Password          UB[1]                   0 = characters are displayed as typed. 1 = all characters are displayed as an asterisk.
ReadOnly          UB[1]                   0 = text editing is enabled. 1 = text editing is disabled
HasTextColor      UB[1]                   0 = use default color. 1 = use specified color (TextColor).
HasMaxLength      UB[1]                   0 = length of text is unlimited. 1 = maximum length of string is specified by MaxLength.
HasFont           UB[1]                   0 = use default font. 1 = use specified font (FontID) and height (FontHeight). (Can’t be true if HasFontClass is true).
HasFontClass      UB[1]                   0 = no fontClass, 1 = fontClass and Height specified for this text. (can't be true if HasFont is true). Supported in Flash Player 9.0.45.0 and later.
AutoSize          UB[1]                   0 = fixed size. 1 = sizes to content (SWF 6 or later only).
HasLayout         UB[1]                   Layout information provided.
NoSelect          UB[1]                   Enables or disables interactive text selection.
Border            UB[1]                   Causes a border to be drawn around the text field.
WasStatic         UB[1]                   0 = Authored as dynamic text 1 = Authored as static text
HTML              UB[1]                   0 = plaintext content. 1 = HTML content (see following).
UseOutlines       UB[1]                   0 = use device font. 1 = use glyph font.
FontID            If HasFont, UI16        ID of font to use.
FontClass         If HasFontClass, STRING Class name of font to be loaded from another SWF and used for this text.
FontHeight        If HasFont, UI16        Height of font in twips.
TextColor         If HasTextColor, RGBA   Color of text.
MaxLength         If HasMaxLength, UI16   Text is restricted to this length.
LayoutAlign       If HasLayout, UI8       0 = Left 1 = Right 2 = Center 3 = Justify
LayoutLeftMargin  If HasLayout, UI16      Left margin in twips.
LayoutRightMargin If HasLayout, UI16      Right margin in twips.
LayoutIndent      If HasLayout, UI16      Indent in twips.
LayoutLeading     If HasLayout, SI16      Leading in twips (vertical distance between bottom of descender of one line and top of ascender of the next).
VariableName      STRING                  Name of the variable where the contents of the text field are stored. May be qualified with dot syntax or slash syntax for non-global variables.
InitialText       If HasText STRING       Text that is initially displayed.
\end{record}

p196: CSMTextSettings
\begin{record}
CSMTextSettings
Field          Type         Comment
Header         RECORDHEADER Tag type = 74
TextID         UI16         ID for the DefineText, DefineText2, or DefineEditText to which this tag applies.
UseFlashType   UB[2]        0 = use normal renderer. 1 = use advanced text rendering engine.
GridFit        UB[3]        0 = Do not use grid fitting. AlignmentZones and LCD sub-pixel information will not be used. 1 = Pixel grid fit. Only supported for left-aligned dynamic text. This setting provides the ultimate in advanced anti-aliased text readability, with crisp letters aligned to pixels. 2 = Sub-pixel grid fit. Align letters to the 1/3 pixel used by LCD monitors. Can also improve quality for CRT output.
Reserved       UB[3]        Must be 0.
Thickness      FLOAT        The thickness attribute for the associated text field. Set to 0.0 to use the default (anti-aliasing table) value.
Sharpness      FLOAT        The sharpness attribute for the associated text field. Set to 0.0 to use the default (anti-aliasing table) value.
Reserved       UI8          Must be 0.
\end{record}

p198: DefineFont4
\begin{record}
DefineFont4
Field                Type         Comment
Header               RECORDHEADER Tag type = 91
FontID               UI16         ID for this font character.
FontFlagsReserved    UB[5]        Reserved bit fields.
FontFlagsHasFontData UB[1]        Font is embedded. Font tag includes SFNT font data block.
FontFlagsItalic      UB[1]        Italic font
FontFlagsBold        UB[1]        Bold font
FontName             STRING       Name of the font.
FontData             BYTE[]       When present, this is an OpenType CFF font, as defined in the OpenType specification at www.microsoft.com/typography/otspec. The following tables must be present: ‘CFF ’, ‘cmap’, ‘head’, 'maxp’, ‘OS/2’, ‘post’, and either (a) ‘hhea’ and ‘hmtx’, or (b) ‘vhea’, ‘vmtx’, and ‘VORG’. The ‘cmap’ table must include one of the following kinds of Unicode ‘cmap’ subtables: (0, 4), (0, 3), (3, 10), (3, 1), or (3, 0) [notation: (platform ID, platformspecific encoding ID)]. Tables such as ‘GSUB’, ‘GPOS’, ‘GDEF’, and ‘BASE’ may also be present. Only present for embedded fonts
\end{record}


Chapter 11: Sounds
~~~~~~~~~~~~~~~~~~

p202: DefineSound
\begin{record}
DefineSound                                                                                                               Sound
Field            Type         Comment
Header           RECORDHEADER Tag type = 14
SoundId          UI16         ID for this sound.
SoundFormat      UB[4]        Format of SoundData. See “Audio coding formats” on page 201.                                                                             
SoundRate        UB[2]        The sampling rate. This is ignored for Nellymoser and Speex codecs. 5.5kHz is not allowed for MP3. 0 = 5.5 kHz 1 = 11 kHz 2 = 22 kHz 3 = 44 kHz
SoundSize        UB[1]        Size of each sample. This parameter only pertains to uncompressed formats. This is ignored for compressed formats which always decode to 16 bits internally. 0 = snd8Bit 1 = snd16Bit
SoundType        UB[1]        Mono or stereo sound. This is ignored for Nellymoser and Speex. 0 = sndMono 1 = sndStereo
SoundSampleCount UI32         Number of samples. Not affected by mono/stereo setting; for stereo sounds this is the number of sample pairs.
SoundData        BYTE[]       The sound data; varies by format.
\end{record}

p204: StartSound
\begin{record}
StartSound
Field     Type         Comment
Header    RECORDHEADER Tag type = 15
SoundId   UI16         ID of sound character to play.
SoundInfo SOUNDINFO    Sound style information.
\end{record}

p205: StartSound2
\begin{record}
StartSound2
Field          Type         Comment
Header         RECORDHEADER Tag type = 89
SoundClassName STRING       Name of the sound class to play.
SoundInfo      SOUNDINFO    Sound style information.
\end{record}

p205: SOUNDINFO
\begin{record}
SOUNDINFO
Field           Type                                     Comment
Reserved        UB[2]                                    Always 0.
SyncStop        UB[1]                                    Stop the sound now.
SyncNoMultiple  UB[1]                                    Don’t start the sound if already playing.
HasEnvelope     UB[1]                                    Has envelope information.
HasLoops        UB[1]                                    Has loop information.
HasOutPoint     UB[1]                                    Has out-point information.
HasInPoint      UB[1]                                    Has in-point information.
InPoint         If HasInPoint, UI32                      Number of samples to skip at beginning of sound.
OutPoint        If HasOutPoint, UI32                     Position in samples of last sample to play.
LoopCount       If HasLoops, UI16                        Sound loop count.
EnvPoints       If HasEnvelope, UI8                      Sound Envelope point count.
EnvelopeRecords If HasEnvelope, SOUNDENVELOPE[EnvPoints] Sound
\end{record}

p206: SOUNDENVELOPE
\begin{record}
SOUNDENVELOPE
Field      Type Comment
Pos44      UI32 Position of envelope point as a number of 44 kHz samples. Multiply accordingly if using a sampling rate less than 44 kHz.
LeftLevel  UI16 Volume level for left channel. Minimum is 0, maximum is 32768.
RightLevel UI16 Volume level for right channel. Minimum is 0, maximum is 32768.
\end{record}

p207: SoundStreamHead
\begin{record}
SoundStreamHead
Field                  Type                                Comment
Header                 RECORDHEADER                        Tag type = 18
Reserved               UB[4]                               Always zero.
PlaybackSoundRate      UB[2]                               Playback sampling rate 0 = 5.5 kHz 1 = 11 kHz 2 = 22 kHz 3 = 44 kHz
PlaybackSoundSize      UB[1]                               Playback sample size. Always 1 (16 bit).
PlaybackSoundType      UB[1]                               Number of playback channels: mono or stereo. 0 = sndMono 1 = sndStereo
StreamSoundCompression UB[4]                               Format of streaming sound data. 1 = ADPCM SWF 4 and later only: 2 = MP3                       
StreamSoundRate        UB[2]                               The sampling rate of the streaming sound data. 0 = 5.5 kHz 1 = 11 kHz 2 = 22 kHz 3 = 44 kHz
StreamSoundSize        UB[1]                               The sample size of the streaming sound data. Always 1 (16 bit).
StreamSoundType        UB[1]                               Number of channels in the streaming sound data. 0 = sndMono 1 = sndStereo
StreamSoundSampleCount UI16                                Average number of samples in each SoundStreamBlock. Not affected by mono/stereo setting; for stereo sounds this is the number of sample pairs.
LatencySeek            If StreamSoundCompression = 2, SI16 See “MP3 sound data” on page 216. The value here should match the SeekSamples field in the first SoundStreamBlock for this stream.
\end{record}

p209: SoundStreamHead2
\begin{record}
SoundStreamHead2
Field                  Type                                 Comment
Header                 RECORDHEADER                         Tag type = 45
Reserved               UB[4]                                Always zero.
PlaybackSoundRate      UB[2]                                Playback sampling rate. 0 = 5.5 kHz 1 = 11 kHz 2 = 22 kHz 3 = 44 kHz
PlaybackSoundSize      UB[1]                                Playback sample size. 0 = 8-bit 1 = 16-bit                                     
PlaybackSoundType      UB[1]                                Number of playback channels. 0 = sndMono 1 = sndStereo
StreamSoundCompression UB[4]                                Format of SoundData. See “Audio coding formats” on page 201.                                                         
StreamSoundRate        UB[2]                                The sampling rate of the streaming sound data. 5.5 kHz is not allowed for MP3. 0 = 5.5 kHz 1 = 11 kHz 2 = 22 kHz 3 = 44 kHz
StreamSoundSize        UB[1]                                Size of each sample. Always 16 bit for compressed formats. May be 8 or 16 bit for uncompressed formats. 0 = 8-bit 1 = 16-bit                        
StreamSoundType        UB[1]                                Number of channels in the streaming sound data. 0 = sndMono 1 = sndStereo
StreamSoundSampleCount UI16                                 Average number of samples in each SoundStreamBlock. Not affected by mono/stereo setting; for stereo sounds this is the number of sample pairs.
LatencySeek            If StreamSoundCompression = 2, SI16  See MP3 sound data. The value here should match the SeekSamples field in the first SoundStreamBlock for this stream.
\end{record}

p210: SoundStreamBlock
\begin{record}
SoundStreamBlock
Field           Type         Comment
Header          RECORDHEADER Tag type = 19
StreamSoundData BYTE[]       Compressed sound data
\end{record}


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

\begin{record}
BUTTONRECORD(ButtonVer)
Field                       Type                                                     Comment
ButtonReserved              UB[2]                                                    Reserved bits; always 0
ButtonHasBlendMode          UB[1]                                                    0 = No blend mode 1 = Has blend mode (SWF 8 and later only)
ButtonHasFilterList         UB[1]                                                    0 = No filter list 1 = Has filter list (SWF 8 and later only)
ButtonStateHitTest          UB[1]                                                    Present in hit test state
ButtonStateDown             UB[1]                                                    Present in down state
ButtonStateOver             UB[1]                                                    Present in over state
ButtonStateUp               UB[1]                                                    Present in up state
CharacterID                 UI16                                                     ID of character to place
PlaceDepth                  UI16                                                     Depth at which to place character
PlaceMatrix                 MATRIX                                                   Transformation matrix for character placement
ButtonDisplayColorTransform If ButtonVer = 2, CXFORMWITHALPHA                        Character color transform
ButtonDisplayFilterList     If ButtonVer = 2, If ButtonHasFilterList = 1, FILTERLIST List of filters on this button
ButtonDisplayBlendMode      If ButtonVer = 2, If ButtonHasBlendMode  = 1, UI8        0 or 1 = normal 2 = layer 3 = multiply 4 = screen 5 = lighten 6 = darken 7 = difference 8 = add 9 = subtract 10 = invert 11 = alpha 12 = erase 13 = overlay 14 = hardlight. Values 15 to 255 are reserved.
\end{record}

p225: DefineButton
\begin{record}
DefineButton
Field            Type             Comment
Header           RECORDHEADER     Tag type = 7
ButtonId         UI16             ID for this character
Characters       BUTTONRECORDS(1) Characters that make up the button
Actions          ACTIONRECORDS    Actions to perform
\end{record}

p226: DefineButton2
\begin{record}
DefineButton2
Field             Type              Comment
Header            RECORDHEADER      Tag type = 34
ButtonId          UI16              ID for this character
ReservedFlags     UB[7]             Always 0
TrackAsMenu       UB[1]             0 = track as normal button 1 = track as menu button
ActionOffset      UI16              Offset in bytes from start of this field to the first BUTTONCONDACTION, or 0 if no actions occur
Characters        BUTTONRECORDS(2)  Characters that make up the button
CharacterEndFlag  UI8               Must be 0
Actions           BUTTONCONDACTIONS Actions to execute at particular button events
\end{record}

\begin{record}
BUTTONCONDACTION
Field                 Type          Comment
CondIdleToOverDown    UB[1]         Idle to OverDown
CondOutDownToIdle     UB[1]         OutDown to Idle
CondOutDownToOverDown UB[1]         OutDown to OverDown
CondOverDownToOutDown UB[1]         OverDown to OutDown
CondOverDownToOverUp  UB[1]         OverDown to OverUp
CondOverUpToOverDown  UB[1]         OverUp to OverDown
CondOverUpToIdle      UB[1]         OverUp to Idle
CondIdleToOverUp      UB[1]         Idle to OverUp
CondKeyPress          UB[7]         SWF 4 or later: key code.Otherwise always 0. Valid key codes: 1 = left arrow 2 = right arrow 3 = home 4 = end 5 = insert 6 = delete 8 = backspace 13 = enter 14 = up arrow 15 = down arrow 16 = page up 17 = page down 18 = tab 19 = escape 32 to 126: follows ASCII 
CondOverDownToIdle    UB[1]         OverDown to Idle
Actions               ACTIONRECORDS Actions to perform. See DoAction.
\end{record}

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
\begin{record}
DefineButtonCxform
Field                Type         Comment
Header               RECORDHEADER Tag type = 23
ButtonId             UI16         Button ID for this information
ButtonColorTransform CXFORM       Character color transform
\end{record}

p229: DefineButtonSound
\begin{record}
DefineButtonSound
Field            Type                                Comment
Header           RECORDHEADER                        Tag type = 17
ButtonId         UI16                                The ID of the button these sounds apply to.
ButtonSoundChar0 UI16                                Sound ID for OverUpToIdle
ButtonSoundInfo0 If ButtonSoundChar0 != 0, SOUNDINFO Sound style for OverUpToIdle
ButtonSoundChar1 UI16                                Sound ID for IdleToOverUp
ButtonSoundInfo1 If ButtonSoundChar1 != 0, SOUNDINFO Sound style for IdleToOverUp
ButtonSoundChar2 UI16                                Sound ID for OverUpToOverDown
ButtonSoundInfo2 If ButtonSoundChar2 != 0, SOUNDINFO Sound style for OverUpToOverDown
ButtonSoundChar3 UI16                                Sound ID for OverDownToOverUp
ButtonSoundInfo3 If ButtonSoundChar3 != 0, SOUNDINFO Sound style for OverDownToOverUp
\end{record}


Chapter 13: Sprites and Movie Clips
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p233: DefineSprite
\begin{record}
DefineSprite
Field       Type         Comment
Header      RECORDHEADER Tag type = 39
SpriteID    UI16         Character ID of sprite
FrameCount  UI16         Number of frames in sprite
ControlTags Tag[]        A series of tags
\end{record}


Chapter 14: Video
~~~~~~~~~~~~~~~~~

p251: DefineVideoStream
\begin{record}
DefineVideoStream
Field                Type         Comment
Header               RECORDHEADER Tag type = 60
CharacterID          UI16         ID for this video character
NumFrames            UI16         Number of VideoFrame tags that makes up this stream
Width                UI16         Width in pixels
Height               UI16         Height in pixels
VideoFlagsReserved   UB[4]        Must be 0
VideoFlagsDeblocking UB[3]        000 = use VIDEOPACKET value 001 = off 010 = Level 1 (Fast deblocking filter) 011 = Level 2 (VP6 only, better deblocking filter) 100 = Level 3 (VP6 only, better deblocking plus fast deringing filter) 101 = Level 4 (VP6 only, better deblocking plus better deringing filter) 110 = Reserved 111 = Reserved
VideoFlagsSmoothing  UB[1]        0 = smoothing off (faster) 1 = smoothing on (higher quality)
CodecID              UI8          2 = Sorenson H.263 3 = Screen video (SWF 7 and later only) 4 = VP6 (SWF 8 and later only) 5 = VP6 video with alpha channel (SWF 8 and later only)
\end{record}

p252: VideoFrame
\begin{record}
StreamID
Field     Type         Comment
Header    RECORDHEADER Tag type = 61
StreamID  UI16         ID of video stream character of which this frame is a part
FrameNum  UI16         Sequence number of this frame within its video stream
VideoData BYTE[]       Video frame payload
\end{record}


Chapter 15: Binary data
~~~~~~~~~~~~~~~~~~~~~~~

p253: DefineBinaryData
\begin{record}
DefineBinaryData
Field    Type         Comment
Header   RECORDHEADER Tag type = 87
Tag      UI16         16-bit character ID
Reserved UI32         Reserved space; must be 0
Data     BYTE[]       A blob of binary data, up to the end of the tag
\end{record}

\end{code}

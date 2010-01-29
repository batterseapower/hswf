\begin{code}
module Main where

import Binary
import Utilities

import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Ratio
import Data.List

import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

import System.Environment
import System.IO.Unsafe

\end{code}

Chapter 1: Basic Data Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

p12: Integer types and byte order
\begin{code}

type SI8 = Int8
type SI16 = Int16
type SI32 = Int32

getSI8 :: SwfGet SI8
getSI8 = fmap fromIntegral getWord8

getSI16 :: SwfGet SI16
getSI16 = fmap fromIntegral getWord16

getSI32 :: SwfGet SI32
getSI32 = fmap fromIntegral getWord32

--type SI18[n]
--type SI16[n]

type UI8 = Word8
type UI16 = Word16
type UI32 = Word32

getUI8 :: SwfGet UI8
getUI8 = fmap fromIntegral getWord8

getUI16 :: SwfGet UI16
getUI16 = fmap fromIntegral getWord16

getUI32 :: SwfGet UI32
getUI32 = fmap fromIntegral getWord32

--type UI8[n]
--type U116[n]
--type U124[n]
--type U132[n]
--type U164[n]

\end{code}

p12: Fixed-point numbers
\begin{code}

data FIXED = FIXED SI16 UI16
data FIXED8 = FIXED8 SI8 UI8

getFIXED :: SwfGet FIXED
getFIXED = liftM2 (flip FIXED) getUI16 getSI16

getFIXED8 :: SwfGet FIXED8
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

getFLOAT16 :: SwfGet FLOAT16
getFLOAT16 = do
    w <- getWord16
    let sign  = (w `shiftR` 15) .&. 0x1
        expon = (w `shiftR` 10) .&. 0x1F - 16
        manti = (w `shiftR` 0)  .&. 0x3FF
        promote = (realToFrac :: CFloat -> Float) . storableCast
    return $ FLOAT16 $ (if sign /= 0 then negate else id) $ (1 + (promote manti / 0x400)) * 2 ^^ expon

getFLOAT :: SwfGet FLOAT
getFLOAT = fmap word32ToDouble getWord32

getDOUBLE :: SwfGet DOUBLE
getDOUBLE = fmap word64ToDouble getWord64

word32ToDouble :: Word32 -> Float
word32ToDouble = (realToFrac :: CFloat -> Float) . storableCast

word64ToDouble :: Word64 -> Double
word64ToDouble = (realToFrac :: CDouble -> Double) . storableCast

storableCast :: (Storable a, Storable b) => a -> b
storableCast w = unsafePerformIO $ with w $ peek . castPtr


-- Page 14: encoded integers

newtype ENCODEDU32 = ENCODEDU32 UI32

getENCODEDU32 :: SwfGet ENCODEDU32
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

type SB = SI32
type UB = UI32
type FB = FIXED

signextend :: (Integral a, Integral b) => a -> Word32 -> b
signextend nbits bits = fromIntegral $ bits .|. complement (2 ^ nbits - 1)

 -- 1110b = -2
 -- 0x30000 (19 bit) = 196608
getSB :: Integral a => a -> SwfGet SB
getSB nbits = fmap (signextend nbits) (getBits nbits)
  where 
  
 -- 1110b = 14
getUB :: Integral a => a -> SwfGet UB
getUB = getBits

 -- 0x30000 (19 bit) = 3.0
getFB :: Integral a => a -> SwfGet FB
getFB nbits = fmap parse (getBits nbits)
  where parse bits = FIXED (signextend (nbits - 16) $ bits `shiftR` 16) (fromIntegral $ bits .&. 0xFFFF)

getBitCount :: Int -> SwfGet Int
getBitCount = fmap fromIntegral . getUB

getFlag :: SwfGet Bool
getFlag = fmap (/= 0) (getSB 1)

\end{code}

p17: String values
\begin{code}

-- SWF <= 5: ANSI or shift-JIS encoding. No way to tell.
-- SWF >  5: UTF-8
type STRING = ByteString

getSTRING :: SwfGet STRING
getSTRING = getLazyByteStringNul

\end{code}

p18: Language code
\begin{code}

data LANGCODE = None | Latin | Japanese | Korean | SimplifiedChinese | TraditionalChinese | Unrecognized UI8

getLANGCODE :: SwfGet LANGCODE
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
Field Type      Comment
Nbits UB[5]     Bits used for each subsequent field
Xmin  SB[Nbits] x minimum position for rectangle in twips
Xmax  SB[Nbits] x maximum position for rectangle in twips
Ymin  SB[Nbits] y minimum position for rectangle in twips
Ymax  SB[Nbits] y maximum position for rectangle in twips
\end{record}

p20: MATRIX record

\begin{record}
MATRIX
Field          Type                              Comment
HasScale       UB[1]                             Has scale values if equal to 1
NScaleBits     If HasScale = 1, UB[5]            Bits in each scale value field
ScaleX         If HasScale = 1, FB[NScaleBits]   x scale value
ScaleY         If HasScale = 1, FB[NScaleBits]   y scale value
HasRotate      UB[1]                             Has rotate and skew values if equal to 1
NRotateBits    If HasRotate = 1, UB[5]           Bits in each rotate value field
RotateSkew0    If HasRotate = 1, FB[NRotateBits] First rotate and skew value
RotateSkew1    If HasRotate = 1, FB[NRotateBits] Second rotate and skew value
NTranslateBits UB[5]                             Bits in each translate value field
TranslateX     SB[NTranslateBits]                x translate value in twips
TranslateY     SB[NTranslateBits]                y translate value in twips
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
\end{record}


Chapter 2: SWF Structure Summary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p25: The SWF header
\begin{code}

data SwfFileHeader = SwfFileHeader { compressed :: Bool, version :: UI8, fileLength :: UI32 {- after decompression -}, frameSize :: RECT {- Twips -}, frameRate :: FIXED8, frameCount :: UI16 }

getSwfFileHeader :: SwfGet (SwfFileHeader, ByteString)
getSwfFileHeader = do
    signature_1 <- fmap fromIntegral getWord8
    compressed <- case lookup signature_1 [(ord 'F', False), (ord 'C', True)] of
        Just c  -> return c
        Nothing -> fail "SWF signature byte 1 unrecognised"
    signature_2 <- fmap fromIntegral getWord8
    assertM (signature_2 == ord 'W') "SWF signature byte 2 wrong"
    signature_3 <- fmap fromIntegral getWord8
    assertM (signature_3 == ord 'S') "SWF signature byte 3 wrong"
    version <- getUI8
    fileLength <- getUI32
    
    (if compressed then modify (\e -> e { swfVersion = version }) . decompressRemainder (fromIntegral fileLength) else id) $ do
        frameSize <- getRECT
        -- TODO: assert XMin/YMin are 0
        frameRate <- getFIXED8
        frameCount <- getUI16
        
        rest <- getRemainingLazyByteString
        return (SwfFileHeader {..}, rest)

\end{code}

p27: Tag format
\begin{code}

data RECORDHEADER = RECORDHEADER { rECORDHEADER_tagType :: UI16, rECORDHEADER_tagLength :: SI32 }

getRECORDHEADER :: SwfGet RECORDHEADER
getRECORDHEADER = do
    tagCodeAndLength <- getUI16
    let tagCode = (tagCodeAndLength `shiftR` 6) .&. 0x3FF
        tagLength = tagCodeAndLength .&. 0x3F
    tagLength <- if tagLength == 0x3F then getSI32 else return (fromIntegral tagLength)
    return $ RECORDHEADER tagCode tagLength

\end{code}

Chapter 3: The Display List
~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}

data RECORD = RECORD { rECORD_recordHeader :: RECORDHEADER, rECORD_recordTag :: Tag }

data Tag = PlaceObject3 { placeObject3_placeFlagMove :: Bool, placeObject3_placeFlagHasImage :: Bool, placeObject3_placeFlagHasClassName :: Bool, placeObject3_depth :: UI16, placeObject3_className :: Maybe STRING, placeObject3_characterId :: Maybe UI16, placeObject3_matrix :: Maybe MATRIX, placeObject3_colorTransform :: Maybe CXFORMWITHALPHA, placeObject3_ratio :: Maybe UI16, placeObject3_name :: Maybe STRING, placeObject3_clipDepth :: Maybe UI16, placeObject3_surfaceFilterList :: Maybe FILTERLIST, placeObject3_blendMode :: Maybe BlendMode, placeObject3_bitmapCache :: Maybe UI8, placeObject3_clipActions :: Maybe CLIPACTIONS }
\genconstructors{tag}
         | UnknownTag ByteString

getRECORD = do
    rECORD_recordHeader@(RECORDHEADER {..}) <- getRECORDHEADER

    let mb_getter = case rECORDHEADER_tagType of
          70 -> Just getPlaceObject3
          _  -> generatedTagGetters rECORDHEADER_tagType

    rECORD_recordTag <- nestSwfGet (fromIntegral rECORDHEADER_tagLength) $ case mb_getter of
      Nothing     -> fmap UnknownTag getRemainingLazyByteString
      Just getter -> getter

    return $ RECORD {..}

\end{code}

\gengetters{tag}

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

\begin{code}

data CLIPACTIONS = CLIPACTIONS { cLIPACTIONS_allEventFlags :: CLIPEVENTFLAGS, cLIPACTIONS_clipActionRecords :: [CLIPACTIONRECORD] }

getCLIPACTIONS = do
    _reserved <- getUI16
    cLIPACTIONS_allEventFlags <- getCLIPEVENTFLAGS
    let go = do clipActionRecord <- getCLIPACTIONRECORD
                end <- fmap null $ lookAhead getCLIPEVENTFLAGS
                if end then return []
                       else fmap (clipActionRecord:) $ go
    cLIPACTIONS_clipActionRecords <- go
    _clipActionEndFlag <- getCLIPEVENTFLAGS
    return $ CLIPACTIONS {..}

data CLIPACTIONRECORD = CLIPACTIONRECORD { cLIPACTIONRECORD_eventFlags :: CLIPEVENTFLAGS, cLIPACTIONRECORD_keyCode :: Maybe UI8, cLIPACTIONRECORD_actions :: [ACTIONRECORD] }

getCLIPACTIONRECORD = do
    cLIPACTIONRECORD_eventFlags <- getCLIPEVENTFLAGS
    actionRecordSize <- getUI32
    (cLIPACTIONRECORD_keyCode, cLIPACTIONRECORD_actions) <- nestSwfGet (fromIntegral actionRecordSize) $ do
        keyCode <- maybeHas (ClipEventKeyPress `elem` cLIPACTIONRECORD_eventFlags) getUI8
        let go = do action <- getACTIONRECORD
                    condM isEmpty
                      (return [])
                      (fmap (action:) go)
        actions <- go
        return (keyCode, actions)
    return $ CLIPACTIONRECORD {..}

\end{code}

p38: PlaceObject3
\begin{code}

getPlaceObject3 = do
    [placeFlagHasClipActions, placeFlagHasClipDepth, placeFlagHasName,
     placeFlagHasRatio, placeFlagHasColorTransform, placeFlagHasMatrix,
     placeFlagHasCharacter, placeObject3_placeFlagMove] <- sequence (replicate 8 getFlag)
    _reserved <- getUB 3
    [placeObject3_placeFlagHasImage, placeObject3_placeFlagHasClassName, placeFlagHasCacheAsBitmap,
     placeFlagHasBlendMode, placeFlagHasFilterList] <- sequence (replicate 5 getFlag)
    placeObject3_depth <- getUI16
    placeObject3_className <- maybeHas (placeObject3_placeFlagHasClassName || (placeObject3_placeFlagHasImage && placeFlagHasCharacter)) getSTRING
    placeObject3_characterId <- maybeHas placeFlagHasCharacter getUI16
    placeObject3_matrix <- maybeHas placeFlagHasMatrix getMATRIX
    placeObject3_colorTransform <- maybeHas placeFlagHasColorTransform getCXFORMWITHALPHA
    placeObject3_ratio <- maybeHas placeFlagHasRatio getUI16
    placeObject3_name <- maybeHas placeFlagHasName getSTRING
    placeObject3_clipDepth <- maybeHas placeFlagHasClipDepth getUI16
    placeObject3_surfaceFilterList <- maybeHas placeFlagHasFilterList getFILTERLIST
    placeObject3_blendMode <- maybeHas placeFlagHasBlendMode getBlendMode
    placeObject3_bitmapCache <- maybeHas placeFlagHasCacheAsBitmap getUI8
    placeObject3_clipActions <- maybeHas placeFlagHasClipActions getCLIPACTIONS
    return $ PlaceObject3 {..}

data BlendMode = Normal0 | Normal1 | Layer | Multiply | Screen | Lighten | Darken
               | Difference | Add | Subtract | Invert | Alpha | Erase | Overlay
               | Hardlight | UnknownBlendMode UI8

getBlendMode = do
    i <- getUI8
    return $ case lookup i ([0..] `zip` [Normal0, Normal1, Layer, Multiply, Screen, Lighten,
                                         Darken, Difference, Add, Subtract, Invert, Alpha,
                                         Erase, Overlay, Hardlight]) of
      Just bm -> bm
      Nothing -> UnknownBlendMode i

type FILTERLIST = [FILTER]

getFILTERLIST = do
    numberOfFilters <- getUI8
    sequence $ genericReplicate numberOfFilters getFILTER

data FILTER = DropShadowFilter DROPSHADOWFILTER
            | BlurFilter BLURFILTER
            | GlowFilter GLOWFILTER
            | BevelFilter BEVELFILTER
            | GradientGlowFilter GRADIENTGLOWFILTER
            | ConvolutionFilter CONVOLUTIONFILTER
            | ColorMatrixFilter COLORMATRIXFILTER
            | GradientBevelFilter GRADIENTBEVELFILTER

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

data CLIPEVENTFLAG = ClipEventKeyUp | ClipEventKeyDown | ClipEventMouseUp | ClipEventMouseDown | ClipEventMouseMove
                   | ClipEventUnload | ClipEventEnterFrame | ClipEventLoad | ClipEventDragOver | ClipEventRollOut
                   | ClipEventRollOver | ClipEventReleaseOutside | ClipEventRelease | ClipEventPress | ClipEventInitialize
                   | ClipEventData | ClipEventConstruct | ClipEventKeyPress | ClipEventDragOut
                   deriving (Eq)

type CLIPEVENTFLAGS = [CLIPEVENTFLAG]

getCLIPEVENTFLAGS = do
    let f cefs cef = getFlag >>= \b -> return $ if b then cef:cefs else cefs
    cefs <- foldM f [] [ClipEventKeyUp, ClipEventKeyDown, ClipEventMouseUp, ClipEventMouseDown,
                        ClipEventMouseMove, ClipEventUnload, ClipEventEnterFrame, ClipEventLoad,
                        ClipEventDragOver, ClipEventRollOut, ClipEventReleaseOutside, ClipEventRelease,
                        ClipEventPress, ClipEventInitialize, ClipEventData]
    
    version <- fmap swfVersion get
    if (version <= 5)
     then return cefs
     else do
        _reserved <- getUB 5
        cefs <- foldM f cefs [ClipEventConstruct, ClipEventKeyPress, ClipEventDragOut]
        _reserved <- getFlag
        return cefs
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


Chapter 1: Basic Data Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

p53: SetBackgroundColor
\begin{record}
SetBackgroundColor
Field           Type         Comment
Header          RECORDHEADER Tag type = 9
BackgroundColor RGB          Color of the display background
\end{record}

p68: ACTIONRECORD
\begin{code}

data ACTIONRECORDHEADER = ACTIONRECORDHEADER { actionCode :: UI8, actionLength :: Maybe UI16 }

getACTIONRECORDHEADER = do
    actionCode <- getUI8
    actionLength <- maybeHas ((actionCode .&. 0x80) /= 0) getUI16
    return $ ACTIONRECORDHEADER {..}

data ACTIONRECORD = ACTIONRECORD { actionRecordHeader :: ACTIONRECORDHEADER, actionRecordAction :: Action }

data Action = ActionGotoFrame { frame :: UI16 }
            | UnknownAction (Maybe ByteString)

getACTIONRECORD = do
    actionRecordHeader@(ACTIONRECORDHEADER {..}) <- getACTIONRECORDHEADER
    
    actionRecordAction <- case actionLength of
       -- No payload: tags < 0x80
      Nothing -> case actionCode of
        _ -> return $ UnknownAction Nothing
       -- Payload: tags >= 0x81
      Just actionLength -> nestSwfGet (fromIntegral actionLength) $ case actionCode of
        0x81 -> fmap ActionGotoFrame getUI16
        _    -> fmap (UnknownAction . Just) getRemainingLazyByteString
    
    return $ ACTIONRECORD {..}

main :: IO ()
main = do
    [file] <- getArgs
    bs <- BS.readFile file
    let (header, _rest) = runSwfGet (SwfGetEnv { swfVersion = error "swfVersion not known yet!" }) bs getSwfFileHeader
    print $ version header
\end{code}

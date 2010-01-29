\begin{code}
module Main where

import Binary
import Utilities

import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Ratio

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

newtype EncodedU32 = EncodedU32 UI32
                   deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

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

data Tag = UnknownTag ByteString
         | PlaceObject3 { placeObject3_placeFlagMove :: Bool, placeObject3_placeFlagHasImage :: Bool, placeObject3_placeFlagHasClassName :: Bool, placeObject3_depth :: UI16, placeObject3_className :: Maybe STRING, placeObject3_characterId :: Maybe UI16, placeObject3_matrix :: Maybe MATRIX, placeObject3_colorTransform :: Maybe CXFORMWITHALPHA, placeObject3_ratio :: Maybe UI16, placeObject3_name :: Maybe STRING, placeObject3_clipDepth :: Maybe UI16, placeObject3_surfaceFilterList :: Maybe FILTERLIST, placeObject3_blendMode :: Maybe BlendMode, placeObject3_bitmapCache :: Maybe UI8, placeObject3_clipActions :: Maybe CLIPACTIONS }
         | DoAction { doAction_actions :: [ACTIONRECORD] }
         | DoInitAction { doInitAction_spriteID :: UI16, doInitAction_actions :: [ACTIONRECORD] }
         | DefineFont { defineFont_fontID :: UI16, defineFont_glyphShapeTable :: [SHAPE] }
\genconstructors{tag}

getRECORD = do
    rECORD_recordHeader@(RECORDHEADER {..}) <- getRECORDHEADER

    let mb_getter = case rECORDHEADER_tagType of
          10 -> Just getDefineFont
          12 -> Just getDoAction
          59 -> Just getDoInitAction
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
     placeFlagHasCharacter, placeObject3_placeFlagMove] <- replicateM 8 getFlag
    _reserved <- getUB 3
    [placeObject3_placeFlagHasImage, placeObject3_placeFlagHasClassName, placeFlagHasCacheAsBitmap,
     placeFlagHasBlendMode, placeFlagHasFilterList] <- replicateM 5 getFlag
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
    genericReplicateM numberOfFilters getFILTER

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
\begin{code}

getDoAction = do
    doAction_actions <- getACTIONRECORDs
    return $ DoAction {..}

getACTIONRECORDs = do
    look <- lookAhead getUI8
    case look of
      0 -> getUI8 >> return []
      _ -> do
        actionRecord <- getACTIONRECORD
        fmap (actionRecord:) getACTIONRECORDs

\end{code}

p68: ACTIONRECORD
\begin{code}

data ACTIONRECORDHEADER = ACTIONRECORDHEADER { aCTIONRECORDHEADER_actionCode :: UI8, aCTIONRECORDHEADER_actionLength :: Maybe UI16 }

getACTIONRECORDHEADER = do
    aCTIONRECORDHEADER_actionCode <- getUI8
    aCTIONRECORDHEADER_actionLength <- maybeHas ((aCTIONRECORDHEADER_actionCode .&. 0x80) /= 0) getUI16
    return $ ACTIONRECORDHEADER {..}

data ACTIONRECORD = ACTIONRECORD { aCTIONRECORD_actionRecordHeader :: ACTIONRECORDHEADER, aCTIONRECORD_actionRecordAction :: Action }

data Action = UnknownAction (Maybe ByteString)
            | ActionPush { actionPush_actionPushLiteral :: ActionPushLiteral }
\genconstructors{action}

getACTIONRECORD = do
    aCTIONRECORD_actionRecordHeader@(ACTIONRECORDHEADER {..}) <- getACTIONRECORDHEADER
    
    let mb_getter = case aCTIONRECORDHEADER_actionCode of
                      0x96 -> Just getActionPush
                      _    -> generatedActionGetters aCTIONRECORDHEADER_actionCode
    
    aCTIONRECORD_actionRecordAction <- case aCTIONRECORDHEADER_actionLength of
       -- No payload: tags < 0x80
      Nothing -> case aCTIONRECORDHEADER_actionCode of
        _ -> return $ UnknownAction Nothing
       -- Payload: tags >= 0x81
      Just aCTIONRECORDHEADER_actionLength -> nestSwfGet (fromIntegral aCTIONRECORDHEADER_actionLength) $ case mb_getter of
        Just getter -> getter
        Nothing     -> fmap (UnknownAction . Just) getRemainingLazyByteString
    
    return $ ACTIONRECORD {..}

\end{code}

\gengetters{action}

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
\begin{code}

getDoInitAction = do
    doInitAction_spriteID <- getUI16
    doInitAction_actions <- getACTIONRECORDs
    return $ DoInitAction {..}

\end{code}

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

data LinearRadial = Linear | Radial

data RepeatingClipped = Repeating | Clipped

data FILLSTYLE = SolidFill { fILLSTYLE_color :: Either RGB RGBA }
               | GradientFill { fILLSTYLE_linearRadial :: LinearRadial, fILLSTYLE_gradientMatrix :: MATRIX, fILLSTYLE_gradient :: GRADIENT }
               | FocalRadialGradientFill { fILLSTYLE_gradientMatrix :: MATRIX, fILLSTYLE_focalGradient :: FOCALGRADIENT }
               | BitmapFill { fILLSTYLE_repeatingClipped :: RepeatingClipped, fILLSTYLE_smoothed :: Bool, fILLSTYLE_bitmapId :: UI16, fILLSTYLE_bitmapMatrix :: MATRIX }

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


data LINESTYLE = LINESTYLE { lINESTYLE_width :: UI16, lINESTYLE_color :: Either RGB RGBA }

getLINESTYLE shapeVer = do
    lINESTYLE_width <- getUI16
    lINESTYLE_color <- if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
    return $ LINESTYLE {..}

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

getSHAPERECORDS shapeVer fillBits lineBits = go
  where
    go = do
      edgeRecord <- getFlag
      if edgeRecord
       then do
          straightEdge <- getFlag
          if straightEdge
           then getSTRAIGHTEDGERECORD >>= \x -> fmap (x:) go
           else getCURVEDEDGERECORD >>= \x -> fmap (x:) go
       else do
          eos <- lookAhead (getUB 5)
          if eos == 0
           then getUB 5 >> byteAlign >> return []
           else getSTYLECHANGERECORD shapeVer fillBits lineBits >>= \x -> fmap (x:) go

data SHAPERECORD
\genconstructors{shaperecord}

\end{code}

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
FillStyles      If StateNewStyles, FILLSTYLEARRAY(ShapeVer) Array of new fill styles.
LineStyles      If StateNewStyles, LINESTYLEARRAY(ShapeVer) Array of new line styles.
NumFillBits     If StateNewStyles, UB[4]                    Number of fill index bits for new styles.
NumLineBits     If StateNewStyles, UB[4]                    Number of line index bits for new styles.
Reserved        UB[]                                        Padding to byte boundary
\end{record}

\begin{record}
STRAIGHTEDGERECORD
Field           Type                  Comment
NumBits         UB[4]                 Number of bits per value (2 less than the actual number).
StraightEdge    StraightEdge(NumBits) Position information
Reserved        UB[]                  Padding to byte boundary
\end{record}

\begin{code}

data StraightEdge = GeneralLine { straightEdge_deltaX :: SB, straightEdge_deltaY :: SB }
                  | VerticalLine { straightEdge_deltaY :: SB }
                  | HorizontalLine { straightEdge_deltaX :: SB }

getStraightEdge numBits = do
    generalLine <- getFlag
    if generalLine
     then liftM2 GeneralLine (getSB (numBits + 2)) (getSB (numBits + 2))
     else do
      vert <- getFlag
      liftM (if vert then VerticalLine else HorizontalLine) (getSB (numBits + 2))

\end{code}

\begin{record}
CURVEDEDGERECORD
Field         Type          Comment
TypeFlag      UB[1]         This is an edge record. Always 1.
StraightFlag  UB[1]         Curved edge. Always 0.
NumBits       UB[4]         Number of bits per value (2 less than the actual number).
ControlDeltaX SB[NumBits+2] X control point change.
ControlDeltaY SB[NumBits+2] Y control point change.
AnchorDeltaX  SB[NumBits+2] X anchor point change.
AnchorDeltaY  SB[NumBits+2] Y anchor point change.
Reserved      UB[]          Padding to byte boundary
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

getGRADRECORD shapeVer = do
    gRADRECORD_ratio <- getUI8
    gRADRECORD_color <- if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
    return $ GRADRECORD {..}

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

data MORPHFILLSTYLE = SolidMorphFill { mORPHFILLSTYLE_startColor :: RGBA, mORPHFILLSTYLE_endColor :: RGBA }
                    | LinearGradientMorphFill { mORPHFILLSTYLE_linearRadial :: LinearRadial, mORPHFILLSTYLE_startGradientMatrix :: MATRIX, mORPHFILLSTYLE_endGradientMatrix :: MATRIX, mORPHFILLSTYLE_gradient :: MORPHGRADIENT }
                    | BitmapMorphFill { mORPHFILLSTYLE_repeatingClipped :: RepeatingClipped, mORPHFILLSTYLE_smoothed :: Bool, mORPHFILLSTYLE_bitmapId :: UI16, mORPHFILLSTYLE_startBitmapMatrix :: MATRIX, mORPHFILLSTYLE_endBitmapMatrix :: MATRIX }

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

\end{code}

p163: Morph gradient values
\begin{code}

type MORPHGRADIENT = [MORPHGRADRECORD]

getMORPHGRADIENT = do
    count <- getUI8
    genericReplicateM count getMORPHGRADRECORD

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
  where go = do
          eor <- lookAhead getUI8
          if eor == 0
           then getUI8 >> return []
           else getTEXTRECORD textVer glyphBits advanceBits >>= \x -> fmap (x:) go

\end{code}

p190: Text records
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

\begin{code}

\end{code}

\begin{code}

main :: IO ()
main = do
    [file] <- getArgs
    bs <- BS.readFile file
    let (header, _rest) = runSwfGet (SwfGetEnv { swfVersion = error "swfVersion not known yet!" }) bs getSwfFileHeader
    print $ version header
\end{code}

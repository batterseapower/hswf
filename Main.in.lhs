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
\genconstructors{tag}

getRECORD = do
    rECORD_recordHeader@(RECORDHEADER {..}) <- getRECORDHEADER

    let mb_getter = case rECORDHEADER_tagType of
          12 -> Just getDoAction
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
    let go = do look <- lookAhead getUI8
                case look of
                  0 -> return []
                  _ -> do
                    actionRecord <- getACTIONRECORD
                    fmap (actionRecord:) go
    doAction_actions <- go
    return $ DoAction {..}

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









\begin{code}

main :: IO ()
main = do
    [file] <- getArgs
    bs <- BS.readFile file
    let (header, _rest) = runSwfGet (SwfGetEnv { swfVersion = error "swfVersion not known yet!" }) bs getSwfFileHeader
    print $ version header
\end{code}

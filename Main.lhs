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
\begin{code}
 
data RGB = RGB{rGB_red :: UI8, rGB_green :: UI8, rGB_blue :: UI8}
getRGB
  = do rGB_red <- getUI8
       rGB_green <- getUI8
       rGB_blue <- getUI8
       return (RGB{..})

\end{code}

p19: RGBA color record/ARGB color record
\begin{code}
 
data RGBA = RGBA{rGBA_red :: UI8, rGBA_green :: UI8,
                 rGBA_blue :: UI8, rGBA_alpha :: UI8}
getRGBA
  = do rGBA_red <- getUI8
       rGBA_green <- getUI8
       rGBA_blue <- getUI8
       rGBA_alpha <- getUI8
       return (RGBA{..})

\end{code}

\begin{code}
 
data ARGB = ARGB{aRGB_alpha :: UI8, aRGB_red :: UI8,
                 aRGB_green :: UI8, aRGB_blue :: UI8}
getARGB
  = do aRGB_alpha <- getUI8
       aRGB_red <- getUI8
       aRGB_green <- getUI8
       aRGB_blue <- getUI8
       return (ARGB{..})

\end{code}

p20: Rectangle record
\begin{code}
 
data RECT = RECT{rECT_nbits :: UB, rECT_xmin :: SB,
                 rECT_xmax :: SB, rECT_ymin :: SB, rECT_ymax :: SB}
getRECT
  = do rECT_nbits <- getUB 5
       rECT_xmin <- getSB rECT_nbits
       rECT_xmax <- getSB rECT_nbits
       rECT_ymin <- getSB rECT_nbits
       rECT_ymax <- getSB rECT_nbits
       return (RECT{..})

\end{code}

p20: MATRIX record

\begin{code}
 
data MATRIX = MATRIX{mATRIX_scale :: Maybe (UB, FB, FB),
                     mATRIX_rotate :: Maybe (UB, FB, FB), mATRIX_nTranslateBits :: UB,
                     mATRIX_translateX :: SB, mATRIX_translateY :: SB}
getMATRIX
  = do mATRIX_hasScale <- getFlag
       mATRIX_scale <- maybeHas mATRIX_hasScale
                         (do mATRIX_nScaleBits <- getUB 5
                             mATRIX_scaleX <- getFB mATRIX_nScaleBits
                             mATRIX_scaleY <- getFB mATRIX_nScaleBits
                             return (mATRIX_nScaleBits, mATRIX_scaleX, mATRIX_scaleY))
       mATRIX_hasRotate <- getFlag
       mATRIX_rotate <- maybeHas mATRIX_hasRotate
                          (do mATRIX_nRotateBits <- getUB 5
                              mATRIX_rotateSkew0 <- getFB mATRIX_nRotateBits
                              mATRIX_rotateSkew1 <- getFB mATRIX_nRotateBits
                              return
                                (mATRIX_nRotateBits, mATRIX_rotateSkew0, mATRIX_rotateSkew1))
       mATRIX_nTranslateBits <- getUB 5
       mATRIX_translateX <- getSB mATRIX_nTranslateBits
       mATRIX_translateY <- getSB mATRIX_nTranslateBits
       return (MATRIX{..})

\end{code}

p22: Color transform record
\begin{code}
 
data CXFORM = CXFORM{cXFORM_hasAddTerms :: Bool,
                     cXFORM_hasMultTerms :: Bool, cXFORM_nbits :: UB,
                     cXFORM_redMultTerm :: Maybe SB, cXFORM_greenMultTerm :: Maybe SB,
                     cXFORM_blueMultTerm :: Maybe SB, cXFORM_redAddTerm :: Maybe SB,
                     cXFORM_greenAddTerm :: Maybe SB, cXFORM_blueAddTerm :: Maybe SB}
getCXFORM
  = do cXFORM_hasAddTerms <- getFlag
       cXFORM_hasMultTerms <- getFlag
       cXFORM_nbits <- getUB 4
       cXFORM_redMultTerm <- maybeHas cXFORM_hasMultTerms
                               (getSB cXFORM_nbits)
       cXFORM_greenMultTerm <- maybeHas cXFORM_hasMultTerms
                                 (getSB cXFORM_nbits)
       cXFORM_blueMultTerm <- maybeHas cXFORM_hasMultTerms
                                (getSB cXFORM_nbits)
       cXFORM_redAddTerm <- maybeHas cXFORM_hasAddTerms
                              (getSB cXFORM_nbits)
       cXFORM_greenAddTerm <- maybeHas cXFORM_hasAddTerms
                                (getSB cXFORM_nbits)
       cXFORM_blueAddTerm <- maybeHas cXFORM_hasAddTerms
                               (getSB cXFORM_nbits)
       return (CXFORM{..})

\end{code}

p23: Color transform with alpha record

\begin{code}
 
data CXFORMWITHALPHA = CXFORMWITHALPHA{cXFORMWITHALPHA_hasAddTerms
                                       :: Bool,
                                       cXFORMWITHALPHA_hasMultTerms :: Bool,
                                       cXFORMWITHALPHA_nbits :: UB,
                                       cXFORMWITHALPHA_redMultTerm :: Maybe SB,
                                       cXFORMWITHALPHA_greenMultTerm :: Maybe SB,
                                       cXFORMWITHALPHA_blueMultTerm :: Maybe SB,
                                       cXFORMWITHALPHA_alphaMultTerm :: Maybe SB,
                                       cXFORMWITHALPHA_redAddTerm :: Maybe SB,
                                       cXFORMWITHALPHA_greenAddTerm :: Maybe SB,
                                       cXFORMWITHALPHA_blueAddTerm :: Maybe SB,
                                       cXFORMWITHALPHA_alphaAddTerm :: Maybe SB}
getCXFORMWITHALPHA
  = do cXFORMWITHALPHA_hasAddTerms <- getFlag
       cXFORMWITHALPHA_hasMultTerms <- getFlag
       cXFORMWITHALPHA_nbits <- getUB 4
       cXFORMWITHALPHA_redMultTerm <- maybeHas
                                        cXFORMWITHALPHA_hasMultTerms
                                        (getSB cXFORMWITHALPHA_nbits)
       cXFORMWITHALPHA_greenMultTerm <- maybeHas
                                          cXFORMWITHALPHA_hasMultTerms
                                          (getSB cXFORMWITHALPHA_nbits)
       cXFORMWITHALPHA_blueMultTerm <- maybeHas
                                         cXFORMWITHALPHA_hasMultTerms
                                         (getSB cXFORMWITHALPHA_nbits)
       cXFORMWITHALPHA_alphaMultTerm <- maybeHas
                                          cXFORMWITHALPHA_hasMultTerms
                                          (getSB cXFORMWITHALPHA_nbits)
       cXFORMWITHALPHA_redAddTerm <- maybeHas cXFORMWITHALPHA_hasAddTerms
                                       (getSB cXFORMWITHALPHA_nbits)
       cXFORMWITHALPHA_greenAddTerm <- maybeHas
                                         cXFORMWITHALPHA_hasAddTerms
                                         (getSB cXFORMWITHALPHA_nbits)
       cXFORMWITHALPHA_blueAddTerm <- maybeHas cXFORMWITHALPHA_hasAddTerms
                                        (getSB cXFORMWITHALPHA_nbits)
       cXFORMWITHALPHA_alphaAddTerm <- maybeHas
                                         cXFORMWITHALPHA_hasAddTerms
                                         (getSB cXFORMWITHALPHA_nbits)
       return (CXFORMWITHALPHA{..})

\end{code}


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

getRECORD = do
    rECORD_recordHeader@(RECORDHEADER {..}) <- getRECORDHEADER

    let mb_getter = case rECORDHEADER_tagType of
          12 -> Just getDoAction
          59 -> Just getDoInitAction
          70 -> Just getPlaceObject3
          _  -> generatedTagGetters rECORDHEADER_tagType

    rECORD_recordTag <- nestSwfGet (fromIntegral rECORDHEADER_tagLength) $ case mb_getter of
      Nothing     -> fmap UnknownTag getRemainingLazyByteString
      Just getter -> getter

    return $ RECORD {..}

\end{code}

\begin{code}
generatedTagGetters tagType
  = case tagType of
        4 -> Just getPlaceObject
        26 -> Just getPlaceObject2
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
        82 -> Just getDoABC
        2 -> Just getDefineShape
        22 -> Just getDefineShape2
        32 -> Just getDefineShape3
        83 -> Just getDefineShape4
        _ -> Nothing

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

\end{code}

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
\begin{code}
 
data COLORMATRIXFILTER = COLORMATRIXFILTER{cOLORMATRIXFILTER_matrix
                                           :: [FLOAT]}
getCOLORMATRIXFILTER
  = do cOLORMATRIXFILTER_matrix <- sequence
                                     (genericReplicate 20 getFLOAT)
       return (COLORMATRIXFILTER{..})

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
getCONVOLUTIONFILTER
  = do cONVOLUTIONFILTER_matrixX <- getUI8
       cONVOLUTIONFILTER_matrixY <- getUI8
       cONVOLUTIONFILTER_divisor <- getFLOAT
       cONVOLUTIONFILTER_bias <- getFLOAT
       cONVOLUTIONFILTER_matrix <- sequence
                                     (genericReplicate
                                        (cONVOLUTIONFILTER_matrixX * cONVOLUTIONFILTER_matrixY)
                                        getFLOAT)
       cONVOLUTIONFILTER_defaultColor <- getRGBA
       _cONVOLUTIONFILTER_reserved <- getUB 6
       cONVOLUTIONFILTER_clamp <- getFlag
       cONVOLUTIONFILTER_preserveAlpha <- getFlag
       return (CONVOLUTIONFILTER{..})

\end{code}

p44: Blur filter
\begin{code}
 
data BLURFILTER = BLURFILTER{bLURFILTER_blurX :: FIXED,
                             bLURFILTER_blurY :: FIXED, bLURFILTER_passes :: UB}
getBLURFILTER
  = do bLURFILTER_blurX <- getFIXED
       bLURFILTER_blurY <- getFIXED
       bLURFILTER_passes <- getUB 5
       _bLURFILTER_reserved <- getUB 3
       return (BLURFILTER{..})

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

\end{code}

p46: Glow filter
\begin{code}
 
data GLOWFILTER = GLOWFILTER{gLOWFILTER_glowColor :: RGBA,
                             gLOWFILTER_blurX :: FIXED, gLOWFILTER_blurY :: FIXED,
                             gLOWFILTER_strength :: FIXED8, gLOWFILTER_innerGlow :: Bool,
                             gLOWFILTER_knockout :: Bool, gLOWFILTER_compositeSource :: Bool,
                             gLOWFILTER_passes :: UB}
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

\end{code}

p48: Gradient Glow and Gradient Bevel filters
\begin{code}
 
data GRADIENTGLOWFILTER = GRADIENTGLOWFILTER{gRADIENTGLOWFILTER_numColors
                                             :: UI8,
                                             gRADIENTGLOWFILTER_gradientColors :: [RGBA],
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
getGRADIENTGLOWFILTER
  = do gRADIENTGLOWFILTER_numColors <- getUI8
       gRADIENTGLOWFILTER_gradientColors <- sequence
                                              (genericReplicate gRADIENTGLOWFILTER_numColors
                                                 getRGBA)
       gRADIENTGLOWFILTER_gradientRatio <- sequence
                                             (genericReplicate gRADIENTGLOWFILTER_numColors getUI8)
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

\end{code}

\begin{code}
 
data GRADIENTBEVELFILTER = GRADIENTBEVELFILTER{gRADIENTBEVELFILTER_numColors
                                               :: UI8,
                                               gRADIENTBEVELFILTER_gradientColors :: [RGBA],
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
getGRADIENTBEVELFILTER
  = do gRADIENTBEVELFILTER_numColors <- getUI8
       gRADIENTBEVELFILTER_gradientColors <- sequence
                                               (genericReplicate gRADIENTBEVELFILTER_numColors
                                                  getRGBA)
       gRADIENTBEVELFILTER_gradientRatio <- sequence
                                              (genericReplicate gRADIENTBEVELFILTER_numColors
                                                 getUI8)
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

\end{code}

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
\begin{code}
getRemoveObject
  = do removeObject_characterId <- getUI16
       removeObject_depth <- getUI16
       return (RemoveObject{..})

\end{code}

p52: RemoveObject2
\begin{code}
getRemoveObject2
  = do removeObject2_depth <- getUI16
       return (RemoveObject2{..})

\end{code}

p52: ShowFrame
\begin{code}
getShowFrame = do return (ShowFrame{..})

\end{code}


Chapter 4: Control Tags
~~~~~~~~~~~~~~~~~~~~~~~

p53: SetBackgroundColor
\begin{code}
getSetBackgroundColor
  = do setBackgroundColor_backgroundColor <- getRGB
       return (SetBackgroundColor{..})

\end{code}

p53: FrameLabel
\begin{code}
getFrameLabel
  = do frameLabel_name <- getSTRING
       frameLabel_namedAnchorFlag <- maybeHasM (fmap not isEmpty) getUI8
       return (FrameLabel{..})

\end{code}

p54: Protect
\begin{code}
getProtect = do return (Protect{..})

\end{code}

p55: End
\begin{code}
getEnd = do return (End{..})

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

\end{code}

p57: EnableDebugger
\begin{code}
getEnableDebugger
  = do enableDebugger_password <- getSTRING
       return (EnableDebugger{..})

\end{code}

p57: EnableDebugger2
\begin{code}
getEnableDebugger2
  = do _enableDebugger2_reserved <- getUI16
       enableDebugger2_password <- getSTRING
       return (EnableDebugger2{..})

\end{code}

p58: ScriptLimits
\begin{code}
getScriptLimits
  = do scriptLimits_maxRecursionDepth <- getUI16
       scriptLimits_scriptTimeoutSeconds <- getUI16
       return (ScriptLimits{..})

\end{code}

p58: SetTabIndex
\begin{code}
getSetTabIndex
  = do setTabIndex_depth <- getUI16
       setTabIndex_tabIndex <- getUI16
       return (SetTabIndex{..})

\end{code}

p59: FileAttributes
\begin{code}
getFileAttributes
  = do _fileAttributes_reserved <- getFlag
       fileAttributes_useDirectBlit <- getFlag
       fileAttributes_useGPU <- getFlag
       fileAttributes_hasMetadata <- getFlag
       fileAttributes_actionScript3 <- getFlag
       _fileAttributes_reserved <- getUB 2
       fileAttributes_useNetwork <- getFlag
       _fileAttributes_reserved <- getUB 24
       return (FileAttributes{..})

\end{code}

p60: ImportAssets2
\begin{code}
getImportAssets2
  = do importAssets2_uRL <- getSTRING
       _importAssets2_reserved <- getUI8
       _importAssets2_reserved <- getUI8
       importAssets2_count <- getUI16
       importAssets2_tag1 <- getUI16
       importAssets2_name1 <- getSTRING
       importAssets2_tagN <- getUI16
       importAssets2_nameN <- getSTRING
       return (ImportAssets2{..})

\end{code}

p62: SymbolClass
\begin{code}
getSymbolClass
  = do symbolClass_numSymbols <- getUI16
       symbolClass_tagsNames <- sequence
                                  (genericReplicate symbolClass_numSymbols
                                     (liftM2 (,) getUI16 getSTRING))
       return (SymbolClass{..})

\end{code}

p64: Metadata
\begin{code}
getMetadata
  = do metadata_metadata <- getSTRING
       return (Metadata{..})

\end{code}

p65: DefineScalingGrid
\begin{code}
getDefineScalingGrid
  = do defineScalingGrid_characterId <- getUI16
       defineScalingGrid_splitter <- getRECT
       return (DefineScalingGrid{..})

\end{code}

p66: DefineSceneAndFrameLabelData
\begin{code}
getDefineSceneAndFrameLabelData
  = do defineSceneAndFrameLabelData_sceneCount <- getEncodedU32
       defineSceneAndFrameLabelData_offsetNames <- sequence
                                                     (genericReplicate
                                                        defineSceneAndFrameLabelData_sceneCount
                                                        (liftM2 (,) getEncodedU32 getSTRING))
       defineSceneAndFrameLabelData_frameLabelCount <- getEncodedU32
       defineSceneAndFrameLabelData_frameNumLabels <- sequence
                                                        (genericReplicate
                                                           defineSceneAndFrameLabelData_frameLabelCount
                                                           (liftM2 (,) getEncodedU32 getSTRING))
       return (DefineSceneAndFrameLabelData{..})

\end{code}


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
      0 -> return []
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

\begin{code}
generatedActionGetters actionCode
  = case actionCode of
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

\end{code}

p69: ActionGotoFrame
\begin{code}
getActionGotoFrame
  = do actionGotoFrame_frame <- getUI16
       return (ActionGotoFrame{..})

\end{code}

p69: ActionGetURL
\begin{code}
getActionGetURL
  = do actionGetURL_urlString <- getSTRING
       actionGetURL_targetString <- getSTRING
       return (ActionGetURL{..})

\end{code}

p69: ActionNextFrame
\begin{code}
getActionNextFrame = do return (ActionNextFrame{..})

\end{code}

p70: ActionPreviousFrame
\begin{code}
getActionPreviousFrame = do return (ActionPreviousFrame{..})

\end{code}

p70: ActionPlay
\begin{code}
getActionPlay = do return (ActionPlay{..})

\end{code}

p70: ActionStop
\begin{code}
getActionStop = do return (ActionStop{..})

\end{code}

p70: ActionToggleQuality
\begin{code}
getActionToggleQuality = do return (ActionToggleQuality{..})

\end{code}

p70: ActionStopSounds
\begin{code}
getActionStopSounds = do return (ActionStopSounds{..})

\end{code}

p71: ActionWaitForFrame
\begin{code}
getActionWaitForFrame
  = do actionWaitForFrame_frame <- getUI16
       actionWaitForFrame_skipCount <- getUI8
       return (ActionWaitForFrame{..})

\end{code}

p71: ActionSetTarget
\begin{code}
getActionSetTarget
  = do actionSetTarget_targetName <- getSTRING
       return (ActionSetTarget{..})

\end{code}

p71: ActionGoToLabel
\begin{code}
getActionGoToLabel
  = do actionGoToLabel_label <- getSTRING
       return (ActionGoToLabel{..})

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
\begin{code}
getActionPop = do return (ActionPop{..})

\end{code}

p76: ActionAdd
\begin{code}
getActionAdd = do return (ActionAdd{..})

\end{code}

p76: ActionSubtract
\begin{code}
getActionSubtract = do return (ActionSubtract{..})

\end{code}

p76: ActionMultiply
\begin{code}
getActionMultiply = do return (ActionMultiply{..})

\end{code}

p77: ActionDivide
\begin{code}
getActionDivide = do return (ActionDivide{..})

\end{code}

p77: ActionEquals
\begin{code}
getActionEquals = do return (ActionEquals{..})

\end{code}

p78: ActionLess
\begin{code}
getActionLess = do return (ActionLess{..})

\end{code}

p78: ActionAnd
\begin{code}
getActionAnd = do return (ActionAnd{..})

\end{code}

p79: ActionOr
\begin{code}
getActionOr = do return (ActionOr{..})

\end{code}

p79: ActionNot
\begin{code}
getActionNot = do return (ActionNot{..})

\end{code}

p80: ActionStringEquals
\begin{code}
getActionStringEquals = do return (ActionStringEquals{..})

\end{code}

p80: ActionStringLength
\begin{code}
getActionStringLength = do return (ActionStringLength{..})

\end{code}

p80: ActionStringAdd
\begin{code}
getActionStringAdd = do return (ActionStringAdd{..})

\end{code}

p81: ActionStringExtract
\begin{code}
getActionStringExtract = do return (ActionStringExtract{..})

\end{code}

p81: ActionStringLess
\begin{code}
getActionStringLess = do return (ActionStringLess{..})

\end{code}

p81: ActionMBStringLength
\begin{code}
getActionMBStringLength = do return (ActionMBStringLength{..})

\end{code}

p82: ActionMBStringExtract
\begin{code}
getActionMBStringExtract = do return (ActionMBStringExtract{..})

\end{code}

p82: ActionToInteger
\begin{code}
getActionToInteger = do return (ActionToInteger{..})

\end{code}

p83: ActionCharToAscii
\begin{code}
getActionCharToAscii = do return (ActionCharToAscii{..})

\end{code}

p83: ActionAsciiToChar
\begin{code}
getActionAsciiToChar = do return (ActionAsciiToChar{..})

\end{code}

p83: ActionMBCharToAscii
\begin{code}
getActionMBCharToAscii = do return (ActionMBCharToAscii{..})

\end{code}

p84: ActionMBAsciiToChar
\begin{code}
getActionMBAsciiToChar = do return (ActionMBAsciiToChar{..})

\end{code}

p84: ActionJump
\begin{code}
getActionJump
  = do actionJump_branchOffset <- getSI16
       return (ActionJump{..})

\end{code}

p84: ActionIf
\begin{code}
getActionIf
  = do actionIf_branchOffset <- getSI16
       return (ActionIf{..})

\end{code}

p85: ActionCall
\begin{code}
getActionCall = do return (ActionCall{..})

\end{code}

p86: ActionGetVariable
\begin{code}
getActionGetVariable = do return (ActionGetVariable{..})

\end{code}

p86: ActionSetVariable
\begin{code}
getActionSetVariable = do return (ActionSetVariable{..})

\end{code}

p87: ActionGetURL2
\begin{code}
getActionGetURL2
  = do actionGetURL2_sendVarsMethod <- getUB 2
       _actionGetURL2_reserved <- getUB 4
       actionGetURL2_loadTargetFlag <- getFlag
       actionGetURL2_loadVariablesFlag <- getFlag
       return (ActionGetURL2{..})

\end{code}

p88: ActionGotoFrame2
\begin{code}
getActionGotoFrame2
  = do _actionGotoFrame2_reserved <- getUB 6
       actionGotoFrame2_sceneBiasFlag <- getFlag
       actionGotoFrame2_playFlag <- getFlag
       actionGotoFrame2_sceneBias <- maybeHas
                                       actionGotoFrame2_sceneBiasFlag
                                       getUI16
       return (ActionGotoFrame2{..})

\end{code}

p89: ActionSetTarget2
\begin{code}
getActionSetTarget2 = do return (ActionSetTarget2{..})

\end{code}

p89: ActionGetProperty
\begin{code}
getActionGetProperty = do return (ActionGetProperty{..})

\end{code}

p90: ActionSetProperty
\begin{code}
getActionSetProperty = do return (ActionSetProperty{..})

\end{code}

p90: ActionCloneSprite
\begin{code}
getActionCloneSprite = do return (ActionCloneSprite{..})

\end{code}

p91: ActionRemoveSprite
\begin{code}
getActionRemoveSprite = do return (ActionRemoveSprite{..})

\end{code}

p91: ActionStartDrag
\begin{code}
getActionStartDrag = do return (ActionStartDrag{..})

\end{code}

p92: ActionEndDrag
\begin{code}
getActionEndDrag = do return (ActionEndDrag{..})

\end{code}

p92: ActionWaitForFrame2
\begin{code}
getActionWaitForFrame2
  = do actionWaitForFrame2_skipCount <- getUI8
       return (ActionWaitForFrame2{..})

\end{code}

p92: ActionTrace
\begin{code}
getActionTrace = do return (ActionTrace{..})

\end{code}

p93: ActionGetTime
\begin{code}
getActionGetTime = do return (ActionGetTime{..})

\end{code}

p93: ActionRandomNumber
\begin{code}
getActionRandomNumber = do return (ActionRandomNumber{..})

\end{code}

p95: ActionCallFunction
\begin{code}
getActionCallFunction = do return (ActionCallFunction{..})

\end{code}

p95: ActionCallMethod
\begin{code}
getActionCallMethod = do return (ActionCallMethod{..})

\end{code}

p96: ActionConstantPool
\begin{code}
getActionConstantPool
  = do actionConstantPool_count <- getUI16
       actionConstantPool_constantPool <- sequence
                                            (genericReplicate actionConstantPool_count getSTRING)
       return (ActionConstantPool{..})

\end{code}

p97: ActionDefineFunction
\begin{code}
getActionDefineFunction
  = do actionDefineFunction_functionName <- getSTRING
       actionDefineFunction_numParams <- getUI16
       actionDefineFunction_params <- sequence
                                        (genericReplicate actionDefineFunction_numParams getSTRING)
       actionDefineFunction_codeSize <- getUI16
       return (ActionDefineFunction{..})

\end{code}

p98: ActionDefineLocal
\begin{code}
getActionDefineLocal = do return (ActionDefineLocal{..})

\end{code}

p98: ActionDefineLocal2
\begin{code}
getActionDefineLocal2 = do return (ActionDefineLocal2{..})

\end{code}

p98: ActionDelete
\begin{code}
getActionDelete = do return (ActionDelete{..})

\end{code}

p99: ActionDelete2
\begin{code}
getActionDelete2 = do return (ActionDelete2{..})

\end{code}

p99: ActionEnumerate
\begin{code}
getActionEnumerate = do return (ActionEnumerate{..})

\end{code}

p99: ActionEquals2
\begin{code}
getActionEquals2 = do return (ActionEquals2{..})

\end{code}

p100: ActionGetMember
\begin{code}
getActionGetMember = do return (ActionGetMember{..})

\end{code}

p101: ActionInitArray
\begin{code}
getActionInitArray = do return (ActionInitArray{..})

\end{code}

p101: ActionInitObject
\begin{code}
getActionInitObject = do return (ActionInitObject{..})

\end{code}

p102: ActionNewMethod
\begin{code}
getActionNewMethod = do return (ActionNewMethod{..})

\end{code}

p103: ActionNewObject
\begin{code}
getActionNewObject = do return (ActionNewObject{..})

\end{code}

p103: ActionSetMember
\begin{code}
getActionSetMember = do return (ActionSetMember{..})

\end{code}

p104: ActionTargetPath
\begin{code}
getActionTargetPath = do return (ActionTargetPath{..})

\end{code}

p104: ActionWith
\begin{code}
getActionWith
  = do actionWith_size <- getUI16
       return (ActionWith{..})

\end{code}

p105: ActionToNumber
\begin{code}
getActionToNumber = do return (ActionToNumber{..})

\end{code}

p105: ActionToString
\begin{code}
getActionToString = do return (ActionToString{..})

\end{code}

p106: ActionTypeOf
\begin{code}
getActionTypeOf = do return (ActionTypeOf{..})

\end{code}

p106: ActionAdd2
\begin{code}
getActionAdd2 = do return (ActionAdd2{..})

\end{code}

p107: ActionLess2
\begin{code}
getActionLess2 = do return (ActionLess2{..})

\end{code}

p107: ActionModulo
\begin{code}
getActionModulo = do return (ActionModulo{..})

\end{code}

p107: ActionBitAnd
\begin{code}
getActionBitAnd = do return (ActionBitAnd{..})

\end{code}

p108: ActionBitLShift
\begin{code}
getActionBitLShift = do return (ActionBitLShift{..})

\end{code}

p108: ActionBitOr
\begin{code}
getActionBitOr = do return (ActionBitOr{..})

\end{code}

p109: ActionBitRShift
\begin{code}
getActionBitRShift = do return (ActionBitRShift{..})

\end{code}

p109: ActionBitURShift
\begin{code}
getActionBitURShift = do return (ActionBitURShift{..})

\end{code}

p110: ActionBitXor
\begin{code}
getActionBitXor = do return (ActionBitXor{..})

\end{code}

p110: ActionDecrement
\begin{code}
getActionDecrement = do return (ActionDecrement{..})

\end{code}

p110: ActionIncrement
\begin{code}
getActionIncrement = do return (ActionIncrement{..})

\end{code}

p111: ActionPushDuplicate
\begin{code}
getActionPushDuplicate = do return (ActionPushDuplicate{..})

\end{code}

p111: ActionReturn
\begin{code}
getActionReturn = do return (ActionReturn{..})

\end{code}

p111: ActionStackSwap
\begin{code}
getActionStackSwap = do return (ActionStackSwap{..})

\end{code}

p111: ActionStoreRegister
\begin{code}
getActionStoreRegister
  = do actionStoreRegister_registerNumber <- getUI8
       return (ActionStoreRegister{..})

\end{code}

p112: DoInitAction
\begin{code}

getDoInitAction = do
    doInitAction_spriteID <- getUI16
    doInitAction_actions <- getACTIONRECORDs
    return $ DoInitAction {..}

\end{code}

p113: ActionInstanceOf
\begin{code}
getActionInstanceOf = do return (ActionInstanceOf{..})

\end{code}

p113: ActionEnumerate2
\begin{code}
getActionEnumerate2 = do return (ActionEnumerate2{..})

\end{code}

p114: ActionStrictEquals
\begin{code}
getActionStrictEquals = do return (ActionStrictEquals{..})

\end{code}

p114: ActionGreater
\begin{code}
getActionGreater = do return (ActionGreater{..})

\end{code}

p115: ActionStringGreater
\begin{code}
getActionStringGreater = do return (ActionStringGreater{..})

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
       _actionDefineFunction2_reserved <- getUB 7
       actionDefineFunction2_preloadGlobalFlag <- getFlag
       actionDefineFunction2_parameters <- sequence
                                             (genericReplicate actionDefineFunction2_numParams
                                                getREGISTERPARAM)
       actionDefineFunction2_codeSize <- getUI16
       return (ActionDefineFunction2{..})

\end{code}

\begin{code}
 
data REGISTERPARAM = REGISTERPARAM{rEGISTERPARAM_register :: UI8,
                                   rEGISTERPARAM_paramName :: STRING}
getREGISTERPARAM
  = do rEGISTERPARAM_register <- getUI8
       rEGISTERPARAM_paramName <- getSTRING
       return (REGISTERPARAM{..})

\end{code}

p119: ActionExtends
\begin{code}
getActionExtends = do return (ActionExtends{..})

\end{code}

p119: ActionCastOp
\begin{code}
getActionCastOp = do return (ActionCastOp{..})

\end{code}

p120: ActionImplementsOp
\begin{code}
getActionImplementsOp = do return (ActionImplementsOp{..})

\end{code}

p121: ActionTy
\begin{code}
getActionTry
  = do _actionTry_reserved <- getUB 5
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
       actionTry_tryBody <- sequence
                              (genericReplicate actionTry_trySize getUI8)
       actionTry_catchBody <- sequence
                                (genericReplicate actionTry_catchSize getUI8)
       actionTry_finallyBody <- sequence
                                  (genericReplicate actionTry_finallySize getUI8)
       return (ActionTry{..})

\end{code}

p122: ActionThrow
\begin{code}
getActionThrow = do return (ActionThrow{..})

\end{code}

p123: DoABC
\begin{code}
getDoABC
  = do doABC_flags <- getUI32
       doABC_name <- getSTRING
       doABC_aBCData <- getRemainingLazyByteString
       return (DoABC{..})

\end{code}


Chapter 6: Shapes
~~~~~~~~~~~~~~~~~

p127: Fill styles
\begin{code}

data FILLSTYLEARRAY = FILLSTYLEARRAY { fILLSTYLEARRAY_fillStyleCount :: UI16, fILLSTYLEARRAY_fillStyles :: [FILLSTYLE] }

getFILLSTYLEARRAY shapeVer = do
    fillStyleCount <- getUI8
    fILLSTYLEARRAY_fillStyleCount <- case fillStyleCount of
        0xFF -> getUI16
        _    -> return $ fromIntegral fillStyleCount
    fILLSTYLEARRAY_fillStyles <- sequence $ genericReplicate fILLSTYLEARRAY_fillStyleCount (getFILLSTYLE shapeVer)
    return $ FILLSTYLEARRAY {..}

data FILLSTYLE = SolidFill { fILLSTYLE_color :: Either RGB RGBA }
               | LinearGradientFill { fILLSTYLE_gradientMatrix :: MATRIX, fILLSTYLE_gradient :: GRADIENT }
               | RadialGradientFill { fILLSTYLE_gradientMatrix :: MATRIX, fILLSTYLE_gradient :: GRADIENT }
               | FocalRadialGradientFill { fILLSTYLE_gradientMatrix :: MATRIX, fILLSTYLE_focalGradient :: FOCALGRADIENT }
               | RepeatingBitmapFill { fILLSTYLE_bitmapId :: UI16, fILLSTYLE_bitmapMatrix :: MATRIX }
               | ClippedBitmapFill { fILLSTYLE_bitmapId :: UI16, fILLSTYLE_bitmapMatrix :: MATRIX }
               | NonSmoothedRepeatingBitmapFill { fILLSTYLE_bitmapId :: UI16, fILLSTYLE_bitmapMatrix :: MATRIX }
               | NonSmoothedClippedBitmapFill { fILLSTYLE_bitmapId :: UI16, fILLSTYLE_bitmapMatrix :: MATRIX }

getFILLSTYLE shapeVer = do
    fillStyleType <- getUI8
    case fillStyleType of
        0x00 -> fmap SolidFill $ if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
        0x10 -> liftM2 LinearGradientFill getMATRIX (getGRADIENT shapeVer)
        0x12 -> liftM2 RadialGradientFill getMATRIX (getGRADIENT shapeVer)
        0x13 -> liftM2 FocalRadialGradientFill getMATRIX (getFOCALGRADIENT shapeVer)
        0x40 -> liftM2 RepeatingBitmapFill getUI16 getMATRIX
        0x41 -> liftM2 ClippedBitmapFill getUI16 getMATRIX
        0x42 -> liftM2 NonSmoothedRepeatingBitmapFill getUI16 getMATRIX
        0x43 -> liftM2 NonSmoothedClippedBitmapFill getUI16 getMATRIX

\end{code}

p130: Line styles
\begin{code}

data LINESTYLEARRAY = LINESTYLEARRAY { lINESTYLEARRAY_lineStyleCount :: UI16, lINESTYLEARRAY_lineStyles :: Either [LINESTYLE] [LINESTYLE2] }

getLINESTYLEARRAY shapeVer = do
    lineStyleCount <- getUI8
    lINESTYLEARRAY_lineStyleCount <- case lineStyleCount of
        0xFF -> getUI16
        _    -> return $ fromIntegral lineStyleCount
    lINESTYLEARRAY_lineStyles <- if shapeVer <= 3
                                 then fmap Left  $ sequence $ genericReplicate lINESTYLEARRAY_lineStyleCount (getLINESTYLE shapeVer)
                                 else fmap Right $ sequence $ genericReplicate lINESTYLEARRAY_lineStyleCount getLINESTYLE2
    return $ LINESTYLEARRAY {..}


data LINESTYLE = LINESTYLE { lINESTYLE_width :: UI16, lINESTYLE_color :: Either RGB RGBA }

getLINESTYLE shapeVer = do
    lINESTYLE_width <- getUI16
    lINESTYLE_color <- if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
    return $ LINESTYLE {..}

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
getLINESTYLE2
  = do lINESTYLE2_width <- getUI16
       lINESTYLE2_startCapStyle <- getUB 2
       lINESTYLE2_joinStyle <- getUB 2
       lINESTYLE2_hasFillFlag <- getFlag
       lINESTYLE2_noHScaleFlag <- getFlag
       lINESTYLE2_noVScaleFlag <- getFlag
       lINESTYLE2_pixelHintingFlag <- getFlag
       _lINESTYLE2_reserved <- getUB 5
       lINESTYLE2_noClose <- getFlag
       lINESTYLE2_endCapStyle <- getUB 2
       lINESTYLE2_miterLimitFactor <- maybeHas (lINESTYLE2_joinStyle == 2)
                                        getUI16
       lINESTYLE2_color <- maybeHas (not lINESTYLE2_hasFillFlag) getRGBA
       lINESTYLE2_fillType <- maybeHas lINESTYLE2_hasFillFlag
                                (getFILLSTYLE 4)
       return (LINESTYLE2{..})

\end{code}

p133: Shape Structures
\begin{code}
 
data SHAPE = SHAPE{sHAPE_numFillBits :: UB,
                   sHAPE_numLineBits :: UB, sHAPE_shapeRecords :: SHAPERECORDS}
getSHAPE sHAPE_shapeVer
  = do sHAPE_numFillBits <- getUB 4
       sHAPE_numLineBits <- getUB 4
       sHAPE_shapeRecords <- getSHAPERECORDS sHAPE_shapeVer
                               sHAPE_numFillBits
                               sHAPE_numLineBits
       return (SHAPE{..})

\end{code}

\begin{code}
 
data SHAPEWITHSTYLE = SHAPEWITHSTYLE{sHAPEWITHSTYLE_fillStyles ::
                                     FILLSTYLEARRAY,
                                     sHAPEWITHSTYLE_lineStyles :: LINESTYLEARRAY,
                                     sHAPEWITHSTYLE_numFillBits :: UB,
                                     sHAPEWITHSTYLE_numLineBits :: UB,
                                     sHAPEWITHSTYLE_shapeRecords :: SHAPERECORDS}
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

\end{code}

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
           then byteAlign >> return []
           else getSTYLECHANGERECORD shapeVer fillBits lineBits >>= \x -> fmap (x:) go

data SHAPERECORD
         =  STYLECHANGERECORD{sTYLECHANGERECORD_stateNewStyles :: Bool,
                    sTYLECHANGERECORD_move :: Maybe (UB, SB, SB),
                    sTYLECHANGERECORD_fillStyle0 :: Maybe UB,
                    sTYLECHANGERECORD_fillStyle1 :: Maybe UB,
                    sTYLECHANGERECORD_lineStyle :: Maybe UB,
                    sTYLECHANGERECORD_fillStyles :: Maybe FILLSTYLEARRAY,
                    sTYLECHANGERECORD_lineStyles :: Maybe LINESTYLEARRAY,
                    sTYLECHANGERECORD_um :: Maybe (UB, UB)}
         |  STRAIGHTEDGERECORD{sTRAIGHTEDGERECORD_numBits :: UB,
                     sTRAIGHTEDGERECORD_straightEdge :: StraightEdge}
         |  CURVEDEDGERECORD{cURVEDEDGERECORD_typeFlag :: Bool,
                   cURVEDEDGERECORD_straightFlag :: Bool,
                   cURVEDEDGERECORD_numBits :: UB,
                   cURVEDEDGERECORD_controlDeltaX :: SB,
                   cURVEDEDGERECORD_controlDeltaY :: SB,
                   cURVEDEDGERECORD_anchorDeltaX :: SB,
                   cURVEDEDGERECORD_anchorDeltaY :: SB}

\end{code}

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
       sTYLECHANGERECORD_fillStyles <- maybeHas
                                         sTYLECHANGERECORD_stateNewStyles
                                         (getFILLSTYLEARRAY sTYLECHANGERECORD_shapeVer)
       sTYLECHANGERECORD_lineStyles <- maybeHas
                                         sTYLECHANGERECORD_stateNewStyles
                                         (getLINESTYLEARRAY sTYLECHANGERECORD_shapeVer)
       sTYLECHANGERECORD_um <- maybeHas sTYLECHANGERECORD_stateNewStyles
                                 (do sTYLECHANGERECORD_numFillBits <- getUB 4
                                     sTYLECHANGERECORD_numLineBits <- getUB 4
                                     return
                                       (sTYLECHANGERECORD_numFillBits,
                                        sTYLECHANGERECORD_numLineBits))
       _sTYLECHANGERECORD_reserved <- byteAlign
       return (STYLECHANGERECORD{..})

\end{code}

\begin{code}
getSTRAIGHTEDGERECORD
  = do sTRAIGHTEDGERECORD_numBits <- getUB 4
       sTRAIGHTEDGERECORD_straightEdge <- getStraightEdge
                                            sTRAIGHTEDGERECORD_numBits
       _sTRAIGHTEDGERECORD_reserved <- byteAlign
       return (STRAIGHTEDGERECORD{..})

\end{code}

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

\begin{code}
getCURVEDEDGERECORD
  = do cURVEDEDGERECORD_typeFlag <- getFlag
       cURVEDEDGERECORD_straightFlag <- getFlag
       cURVEDEDGERECORD_numBits <- getUB 4
       cURVEDEDGERECORD_controlDeltaX <- getSB
                                           (cURVEDEDGERECORD_numBits + 2)
       cURVEDEDGERECORD_controlDeltaY <- getSB
                                           (cURVEDEDGERECORD_numBits + 2)
       cURVEDEDGERECORD_anchorDeltaX <- getSB
                                          (cURVEDEDGERECORD_numBits + 2)
       cURVEDEDGERECORD_anchorDeltaY <- getSB
                                          (cURVEDEDGERECORD_numBits + 2)
       _cURVEDEDGERECORD_reserved <- byteAlign
       return (CURVEDEDGERECORD{..})

\end{code}

p140: DefineShape
\begin{code}
getDefineShape
  = do defineShape_shapeId <- getUI16
       defineShape_shapeBounds <- getRECT
       defineShape_shapes <- getSHAPEWITHSTYLE 1
       return (DefineShape{..})

\end{code}

p141: DefineShape2
\begin{code}
getDefineShape2
  = do defineShape2_shapeId <- getUI16
       defineShape2_shapeBounds <- getRECT
       defineShape2_shapes <- getSHAPEWITHSTYLE 2
       return (DefineShape2{..})

\end{code}

p141: DefineShape3
\begin{code}
getDefineShape3
  = do defineShape3_shapeId <- getUI16
       defineShape3_shapeBounds <- getRECT
       defineShape3_shapes <- getSHAPEWITHSTYLE 3
       return (DefineShape3{..})

\end{code}

p142: DefineShape4
\begin{code}
getDefineShape4
  = do defineShape4_shapeId <- getUI16
       defineShape4_shapeBounds <- getRECT
       defineShape4_edgeBounds <- getRECT
       _defineShape4_reserved <- getUB 5
       defineShape4_usesFillWindingRule <- getFlag
       defineShape4_usesNonScalingStrokes <- getFlag
       defineShape4_usesScalingStrokes <- getFlag
       defineShape4_shapes <- getSHAPEWITHSTYLE 4
       return (DefineShape4{..})

\end{code}


Chapter 7: Gradients
~~~~~~~~~~~~~~~~~~~~~~~~~~~

p145: GRADIENT
\begin{code}
 
data GRADIENT = GRADIENT{gRADIENT_spreadMode :: UB,
                         gRADIENT_interpolationMode :: UB,
                         gRADIENT_gradientRecords :: [GRADRECORD]}
getGRADIENT gRADIENT_shapeVer
  = do gRADIENT_spreadMode <- getUB 2
       gRADIENT_interpolationMode <- getUB 2
       gRADIENT_numGradients <- getUB 4
       gRADIENT_gradientRecords <- sequence
                                     (genericReplicate gRADIENT_numGradients
                                        (getGRADRECORD gRADIENT_shapeVer))
       return (GRADIENT{..})

\end{code}

p146: FOCALGRADIENT
\begin{code}
 
data FOCALGRADIENT = FOCALGRADIENT{fOCALGRADIENT_spreadMode :: UB,
                                   fOCALGRADIENT_interpolationMode :: UB,
                                   fOCALGRADIENT_gradientRecords :: [GRADRECORD],
                                   fOCALGRADIENT_focalPoint :: FIXED8}
getFOCALGRADIENT fOCALGRADIENT_shapeVer
  = do fOCALGRADIENT_spreadMode <- getUB 2
       fOCALGRADIENT_interpolationMode <- getUB 2
       fOCALGRADIENT_numGradients <- getUB 4
       fOCALGRADIENT_gradientRecords <- sequence
                                          (genericReplicate fOCALGRADIENT_numGradients
                                             (getGRADRECORD fOCALGRADIENT_shapeVer))
       fOCALGRADIENT_focalPoint <- getFIXED8
       return (FOCALGRADIENT{..})

\end{code}

p146: GRADRECORD
\begin{code}

data GRADRECORD = GRADRECORD { gRADRECORD_ratio :: UI8, gRADRECORD_color :: Either RGB RGBA }

getGRADRECORD shapeVer = do
    gRADRECORD_ratio <- getUI8
    gRADRECORD_color <- if shapeVer <= 2 then fmap Left getRGB else fmap Right getRGBA
    return $ GRADRECORD {..}

\end{code}

\begin{code}

main :: IO ()
main = do
    [file] <- getArgs
    bs <- BS.readFile file
    let (header, _rest) = runSwfGet (SwfGetEnv { swfVersion = error "swfVersion not known yet!" }) bs getSwfFileHeader
    print $ version header
\end{code}


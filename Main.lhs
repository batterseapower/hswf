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

data Tag = PlaceObject3 { placeObject3_placeFlagMove :: Bool, placeObject3_placeFlagHasImage :: Bool, placeObject3_placeFlagHasClassName :: Bool, placeObject3_depth :: UI16, placeObject3_className :: Maybe STRING, placeObject3_characterId :: Maybe UI16, placeObject3_matrix :: Maybe MATRIX, placeObject3_colorTransform :: Maybe CXFORMWITHALPHA, placeObject3_ratio :: Maybe UI16, placeObject3_name :: Maybe STRING, placeObject3_clipDepth :: Maybe UI16, placeObject3_surfaceFilterList :: Maybe FILTERLIST, placeObject3_blendMode :: Maybe BlendMode, placeObject3_bitmapCache :: Maybe UI8, placeObject3_clipActions :: Maybe CLIPACTIONS }
         | DoAction { doAction_actions :: [ACTIONRECORD] }
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
         |  SymbolClass{symbolClass_numSymbols :: UI16,
              symbolClass_tag1 :: UI16, symbolClass_name1 :: STRING,
              symbolClass_tagN :: UI16, symbolClass_nameN :: STRING}
         |  Metadata{metadata_metadata :: STRING}
         |  DefineScalingGrid{defineScalingGrid_characterId :: UI16,
                    defineScalingGrid_splitter :: RECT}
         |  DefineSceneAndFrameLabelData{defineSceneAndFrameLabelData_sceneCount
                               :: EncodedU32,
                               defineSceneAndFrameLabelData_offset1 :: EncodedU32,
                               defineSceneAndFrameLabelData_name1 :: STRING,
                               defineSceneAndFrameLabelData_offsetN :: EncodedU32,
                               defineSceneAndFrameLabelData_nameN :: STRING,
                               defineSceneAndFrameLabelData_frameLabelCount :: EncodedU32,
                               defineSceneAndFrameLabelData_frameNum1 :: EncodedU32,
                               defineSceneAndFrameLabelData_frameLabel1 :: STRING,
                               defineSceneAndFrameLabelData_frameNumN :: EncodedU32,
                               defineSceneAndFrameLabelData_frameLabelN :: STRING}
         | UnknownTag ByteString

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
       symbolClass_tag1 <- getUI16
       symbolClass_name1 <- getSTRING
       symbolClass_tagN <- getUI16
       symbolClass_nameN <- getSTRING
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
       defineSceneAndFrameLabelData_offset1 <- getEncodedU32
       defineSceneAndFrameLabelData_name1 <- getSTRING
       defineSceneAndFrameLabelData_offsetN <- getEncodedU32
       defineSceneAndFrameLabelData_nameN <- getSTRING
       defineSceneAndFrameLabelData_frameLabelCount <- getEncodedU32
       defineSceneAndFrameLabelData_frameNum1 <- getEncodedU32
       defineSceneAndFrameLabelData_frameLabel1 <- getSTRING
       defineSceneAndFrameLabelData_frameNumN <- getEncodedU32
       defineSceneAndFrameLabelData_frameLabelN <- getSTRING
       return (DefineSceneAndFrameLabelData{..})

\end{code}


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


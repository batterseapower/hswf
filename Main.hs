module Main where

import Binary
import Utilities

import Control.Arrow ((***))

import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Ratio

import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

import System.Environment
import System.IO.Unsafe


-- Chapter 1: Basic Data Types
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~


-- p11: Coordinates and twips

type Twips = Integer
type LogicalPixels = Integer

twipsToLogicalPixels :: Twips -> LogicalPixels
twipsToLogicalPixels = (`div` 20)


data Point = Point { x :: Twips, y :: Twips }


-- p12: Integer types and byte order

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


-- p12: Fixed-point numbers

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

signextend :: Integral a => Int -> Word32 -> a
signextend nbits bits = fromIntegral $ bits .|. complement (2 ^ nbits - 1)

 -- 1110b = -2
 -- 0x30000 (19 bit) = 196608
getSB :: Int -> SwfGet SB
getSB nbits = fmap (signextend nbits) (getBits nbits)
  where 
  
 -- 1110b = 14
getUB :: Int -> SwfGet UB
getUB = getBits

 -- 0x30000 (19 bit) = 3.0
getFB :: Int -> SwfGet FB
getFB nbits = fmap parse (getBits nbits)
  where parse bits = FIXED (signextend (nbits - 16) $ bits `shiftR` 16) (fromIntegral $ bits .&. 0xFFFF)

getBitCount :: Int -> SwfGet Int
getBitCount = fmap fromIntegral . getUB

getFlag :: SwfGet Bool
getFlag = fmap (/= 0) (getSB 1)


-- p17: String values

-- SWF <= 5: ANSI or shift-JIS encoding. No way to tell.
-- SWF >  5: UTF-8
type STRING = ByteString

getSTRING :: SwfGet STRING
getSTRING = getLazyByteStringNul


-- p18: Language code

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
    nbits <- getBitCount 5
    xMin <- getSB nbits
    xMax <- getSB nbits
    yMin <- getSB nbits
    yMax <- getSB nbits
    return $ RECT {..}


-- p20: MATRIX record

data MATRIX = MATRIX { scale :: Maybe (FIXED, FIXED), rotateSkew :: Maybe (FIXED, FIXED), translate :: (SI32, SI32) }

getMATRIX = do
    hasScale <- getFlag
    scale <- maybeHas hasScale $ do
        nScaleBits <- getBitCount 5
        scaleX <- getFB nScaleBits
        scaleY <- getFB nScaleBits
        return (scaleX, scaleY)
    
    hasRotate <- getFlag
    rotateSkew <- maybeHas hasRotate $ do
        nRotateBits <- getBitCount 5
        rotateSkew0 <- getFB nRotateBits
        rotateSkew1 <- getFB nRotateBits
        return (rotateSkew0, rotateSkew1)
    
    nTranslateBits <- getBitCount 5
    translate <- liftM2 (,) (getSB nTranslateBits) (getSB nTranslateBits)
    
    return $ MATRIX {..}

transformByMATRIX :: Fractional a => (a, a) -> MATRIX -> (a, a)
transformByMATRIX (x, y) m = (x *  (fst scale') + y * snd rotateSkew' + fromIntegral (fst (translate m)),
                              x * fst rotateSkew' + y * snd scale' + fromIntegral (snd (translate m)))
  where scale' = maybe (0, 0) (fIXEDToFractional *** fIXEDToFractional) $ scale m
        rotateSkew' = maybe (0, 0) (fIXEDToFractional *** fIXEDToFractional) $ rotateSkew m


-- p22: Color transform record

data CXFORM = CXFORM { multTerms :: Maybe (SI32, SI32, SI32), addTerms :: Maybe (SI32, SI32, SI32) }

getCXFORM = do
    hasAddTerms <- getFlag
    hasMultTerms <- getFlag
    nbits <- getBitCount 4
  
    multTerms <- maybeHas hasMultTerms $ liftM3 (,,) (getSB nbits) (getSB nbits) (getSB nbits)
    addTerms <- maybeHas hasAddTerms $ liftM3 (,,) (getSB nbits) (getSB nbits) (getSB nbits)
    
    return $ CXFORM {..}

transformByCXFORM :: Integral a => (a, a, a) -> CXFORM -> (a, a, a)
transformByCXFORM rgb c = (component fst3, component snd3, component thd3)
   where multTerms' = multTerms c `orElse` (1, 1, 1)
         addTerms' = addTerms c `orElse` (0, 0, 0)
         clamp x = round $ max 0 $ min 255 $ x
         component :: Integral a => (forall b. (b, b, b) -> b) -> a
         component sel = clamp ((fromIntegral (sel rgb) * fromIntegral (sel multTerms') / (256.0 :: Rational)) + fromIntegral (sel addTerms'))


-- p23: Color transform with alpha record

data CXFORMWITHALPHA = CXFORMWITHALPHA { multTermsWithAlpha :: Maybe (SI32, SI32, SI32, SI32), addTermsWithAlpha :: Maybe (SI32, SI32, SI32, SI32) }

getCXFORMWITHALPHA = do
    hasAddTerms <- getFlag
    hasMultTerms <- getFlag
    nbits <- getBitCount 4
  
    multTermsWithAlpha <- maybeHas hasMultTerms $ liftM4 (,,,) (getSB nbits) (getSB nbits) (getSB nbits) (getSB nbits)
    addTermsWithAlpha <- maybeHas hasAddTerms $ liftM4 (,,,) (getSB nbits) (getSB nbits) (getSB nbits) (getSB nbits)
    
    return $ CXFORMWITHALPHA {..}

transformByCXFORMWITHALPHA :: Integral a => (a, a, a, a) -> CXFORMWITHALPHA -> (a, a, a, a)
transformByCXFORMWITHALPHA rgba c = (component fst4, component snd4, component thd4, component fth4)
   where multTerms' = multTermsWithAlpha c `orElse` (1, 1, 1, 1)
         addTerms' = addTermsWithAlpha c `orElse` (0, 0, 0, 0)
         clamp x = round $ max 0 $ min 255 $ x
         component :: Integral a => (forall b. (b, b, b, b) -> b) -> a
         component sel = clamp ((fromIntegral (sel rgba) * fromIntegral (sel multTerms') / (256.0 :: Rational)) + fromIntegral (sel addTerms'))


-- Chapter 2: SWF Structure Summary
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


-- p25: The SWF header

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
    
    modify (\e -> e { swfVersion = version }) $ decompressRemainder (fromIntegral fileLength) $ do
        frameSize <- getRECT
        -- TODO: assert XMin/YMin are 0
        frameRate <- getFIXED8
        frameCount <- getUI16
        
        rest <- getRemainingLazyByteString
        return (SwfFileHeader {..}, rest)


-- p27: Tag format

data RECORDHEADER = RECORDHEADER { tagType :: UI16, tagLength :: SI32 }

getRECORDHEADER :: SwfGet RECORDHEADER
getRECORDHEADER = do
    tagCodeAndLength <- getUI16
    let tagCode = (tagCodeAndLength `shiftR` 6) .&. 0x3FF
        tagLength = tagCodeAndLength .&. 0x3F
    tagLength <- if tagLength == 0x3F then getSI32 else return (fromIntegral tagLength)
    return $ RECORDHEADER tagCode tagLength


-- Chapter 3: The Display List
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~

data RECORD = RECORD { recordHeader :: RECORDHEADER, recordTag :: Tag }

data Tag = PlaceObject { placeObject_characterId :: UI16, depth :: UI16, placeObject_matrix :: MATRIX, placeObject_colorTransform :: Maybe CXFORM }
         | PlaceObject2 { placeFlagMove :: Bool, depth :: UI16, placeObject2_characterId :: Maybe UI16, placeObject2_matrix :: Maybe MATRIX, placeObject2_colorTransform :: Maybe CXFORMWITHALPHA, ratio :: Maybe UI16, name :: Maybe STRING, clipDepth :: Maybe UI16, clipActions :: Maybe CLIPACTIONS }
         | UnknownTag ByteString

getRECORD = do
    recordHeader@(RECORDHEADER {..}) <- getRECORDHEADER

    let mb_getter = case tagType of
          4  -> Just getPlaceObject
          26 -> Just getPlaceObject2
          _  -> Nothing

    recordTag <- nestSwfGet (fromIntegral tagLength) $ case mb_getter of
      Nothing     -> fmap UnknownTag getRemainingLazyByteString
      Just getter -> getter

    return $ RECORD {..}


-- p34: PlaceObject

getPlaceObject = do
    placeObject_characterId <- getUI16
    depth <- getUI16
    placeObject_matrix <- getMATRIX
    placeObject_colorTransform <- condM isEmpty (return Nothing) (fmap Just getCXFORM)
    return $ PlaceObject {..}


-- p35: PlaceObject2

getPlaceObject2 = do
    [placeFlagHasClipActions, placeFlagHasClipDepth, placeFlagHasName,
     placeFlagHasRatio, placeFlagHasColorTransform, placeFlagHasMatrix,
     placeFlagHasCharacter, placeFlagMove] <- sequence (replicate 8 getFlag)
    depth <- getUI16
    placeObject2_characterId <- maybeHas placeFlagHasCharacter getUI16
    placeObject2_matrix <- maybeHas placeFlagHasMatrix getMATRIX
    placeObject2_colorTransform <- maybeHas placeFlagHasColorTransform getCXFORMWITHALPHA
    ratio <- maybeHas placeFlagHasRatio getUI16
    name <- maybeHas placeFlagHasName getSTRING
    clipDepth <- maybeHas placeFlagHasClipDepth getUI16
    clipActions <- maybeHas placeFlagHasClipActions getCLIPACTIONS
    return $ PlaceObject2 {..}

data CLIPACTIONS = CLIPACTIONS { allEventFlags :: CLIPEVENTFLAGS, clipActionRecords :: [CLIPACTIONRECORD] }

getCLIPACTIONS = do
    _reserved <- getUI16
    allEventFlags <- getCLIPEVENTFLAGS
    let go = do clipActionRecord <- getCLIPACTIONRECORD
                end <- fmap null $ lookAhead getCLIPEVENTFLAGS
                if end then return []
                       else fmap (clipActionRecord:) $ go
    clipActionRecords <- go
    _clipActionEndFlag <- getCLIPEVENTFLAGS
    return $ CLIPACTIONS {..}

data CLIPACTIONRECORD = CLIPACTIONRECORD { eventFlags :: CLIPEVENTFLAGS, keyCode :: Maybe UI8, actions :: [ACTIONRECORD] }

getCLIPACTIONRECORD = do
    eventFlags <- getCLIPEVENTFLAGS
    actionRecordSize <- getUI32
    (keyCode, actions) <- nestSwfGet (fromIntegral actionRecordSize) $ do
        keyCode <- maybeHas (ClipEventKeyPress `elem` eventFlags) getUI8
        let go = do action <- getACTIONRECORD
                    condM isEmpty
                      (return [])
                      (fmap (action:) go)
        actions <- go
        return (keyCode, actions)
    return $ CLIPACTIONRECORD {..}


-- p50: CLIPEVENTFLAGS

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


-- p68: ACTIONRECORD

data ACTIONRECORDHEADER = ACTIONRECORDHEADER { actionCode :: UI8, actionLength :: Maybe UI16 }

getACTIONRECORDHEADER = do
    actionCode <- getUI8
    actionLength <- maybeHasF (actionCode .&. 0x80) getUI16
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

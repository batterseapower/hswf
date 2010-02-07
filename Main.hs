module Main (main) where

import Args
import Data.SWF

import Control.Monad

import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Generics
import Data.Int
import Data.Word
import Data.List

import System.FilePath

import Text.PrettyPrint.HughesPJ


main :: IO ()
main = do
    hswf <- getArgs
    
    forM_ (files hswf) $ \file -> do
      putStrLn $ "### Reading " ++ file
      swf <- fmap getSwf $ BS.readFile file

      case hswf of
        Debug{..} -> do
          if incremental
           then do
            print $ swf { swf_tags = [] }
            forM_ (swf_tags swf) print
           else
            print swf
        Decode{..} -> do
          writeFile (replaceExtension file ".out.hs") $ show $ vcat [
              text "import qualified Data.ByteString.Lazy as BS",
              text "import Data.Char",
              text "import Data.SWF.Format",
              text "import Data.Ratio",
              text "",
              text "main = BS.writeFile" <+> text (show (replaceExtension file ".out.swf")) <+> text "(putSwf swf)",
              text "",
              text "bs x = BS.pack (map (fromIntegral . ord) x)",
              text "",
              text "swf =",
              nest 2 (prettyHaskell swf)
            ]


prettyHaskell :: Data a => a -> Doc
prettyHaskell = algebraic `extQ` string `extQ` bytestring `extQ` fixed8 `extQ` fixed `extQ`
                  numeric (undefined :: Word8) `extQ` numeric (undefined :: Word16) `extQ` numeric (undefined :: Word32) `extQ`
                  numeric (undefined :: Int8) `extQ` numeric (undefined :: Int16) `extQ` numeric (undefined :: Int32)
  where
    algebraic :: Data a => a -> Doc
    algebraic t
      | Just (FlipArrow f) <- dataCast1 (FlipArrow list) = f t
      | Just d <- tupleDataCast (\xs -> parens $ hsep $ punctuate (char ',') $ map (\(WithData x) -> prettyHaskell x) xs) t = d
      | null field_names = hang (text (showConstr constr)) 2 (vcat [parens doc | doc <- gmapQ prettyHaskell t])
      | otherwise = hang (text (showConstr constr) <+> char '{') 2 (
                  nest 2 (vcat $ punctuate (char ',') [text field <+> char '=' <+> doc
                                                     | (field, doc) <- field_names `zip` gmapQ prettyHaskell t]) $$
                  char '}'
                )
      where constr = toConstr t
            field_names = constrFields constr

    string :: String -> Doc
    string = text . show

    bytestring :: BS.ByteString -> Doc
    bytestring bs = text "bs" <+> string (map (chr . fromIntegral) (BS.unpack bs))

    numeric :: Integral a => a -> a -> Doc
    numeric _ = text . show

    fixed8 :: FIXED8 -> Doc
    fixed8 x = text "rationalToFIXED8" <+> parens (text $ show $ fIXED8ToRational x)
    
    fixed :: FIXED -> Doc
    fixed x = text "rationalToFIXED" <+> parens (text $ show $ fIXEDToRational x)

    list :: Data a => [a] -> Doc
    list [] = text "[]"
    list [x] = char '[' <> prettyHaskell x <> char ']'
    list xs = char '[' $$ vcat (punctuate (char ',') (map prettyHaskell xs)) $$ char ']'

newtype FlipArrow b a = FlipArrow (a -> b)

data WithData = forall a. Data a => WithData a

instance Show WithData where show _ = "WithData"

tupleDataCast :: Data a => ([WithData] -> c) -> a -> Maybe c
tupleDataCast f t | Just (s, _) <- find (\(_, tcon) -> dataTypeName (dataTypeOf t) == dataTypeName (constrType tcon) && toConstr t == tcon) tuples
                   -- See http://hackage.haskell.org/trac/ghc/ticket/3866
                  = Just (f [gmapQi i WithData t | i <- [0..s - 1]])
                  | otherwise = Nothing
  where tuples = [2..] `zip` [toConstr ((), ()), toConstr ((), (), ()), toConstr ((), (), (), ()), toConstr ((), (), (), (), ()), toConstr ((), (), (), (), (), ())]
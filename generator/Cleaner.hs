module Main where

import Data.Char
import Data.List


-- CONVOLUTIONFILTER
-- Field Type Comment
-- MatrixX UI8 Horizontal matrix size
-- MatrixY UI8 Vertical matrix size
-- Divisor FLOAT Divisor applied to the
-- matrix values
-- Bias FLOAT Bias applied to the matrix
-- values
-- Matrix FLOAT[MatrixX * MatrixY] Matrix values
-- DefaultColor RGBA Default color for pixels
-- outside the image
-- Reserved UB[6] Must be 0
-- Clamp UB[1] Clamp mode
-- PreserveAlpha UB[1] Preserve the alpha

main = interact (unlines . fixupIndentation . fixupLineBreaks . waitForAll . lines)

waitForAll :: [String] -> [String]
waitForAll xs = length xs `seq` xs

fixupLineBreaks :: [String] -> [String]
fixupLineBreaks [] = []
fixupLineBreaks [l] = [l]
fixupLineBreaks (l1:l2:ls)
  | isLower (head l2) = fixupLineBreaks ((l1 ++ ' ':l2):ls)
  | otherwise         = l1 : fixupLineBreaks (l2:ls)

-- CONVOLUTIONFILTER
-- Field Type Comment
-- MatrixX UI8 Horizontal matrix size
-- MatrixY UI8 Vertical matrix size
-- Divisor FLOAT Divisor applied to the matrix values
-- Bias FLOAT Bias applied to the matrix values
-- Matrix FLOAT[MatrixX * MatrixY] Matrix values
-- DefaultColor RGBA Default color for pixels outside the image
-- Reserved UB[6] Must be 0
-- Clamp UB[1] Clamp mode
-- PreserveAlpha UB[1] Preserve the alpha

fixupIndentation :: [String] -> [String]
fixupIndentation (name:ls) = name : [pad fieldname typindent ++ pad typ commentindent ++ comment | (fieldname, typ, comment) <- considered_ls]
  where
    pad s n = s ++ replicate (n - length s) ' '
    
    considered_ls = zipWith consider (False:repeat True) ls
    
    (fieldnames, typs, comments) = unzip3 considered_ls
    typindent = maximum (map length fieldnames) + 1
    commentindent = maximum (map length typs) + 1
    
    consider bodyline l = (fieldname, unwords typ, unwords comment)
      where
        fieldname:ws = words l
        (typ, comment) = spanRev (\w -> (length w <= 1 || not (looksLikeType w)) && (w /= "Type" || bodyline) && not (partOfArrayExpr w)) ws

    looksLikeType w = length (fst (span (\c -> isUpper c || isDigit c) w)) >= 2
    partOfArrayExpr w = any (`isInfixOf` w) ["*", "+", "]", "["]

    spanRev p xs = case span p (reverse xs) of (as, bs) -> (reverse bs, reverse as)

-- CONVOLUTIONFILTER
-- Field         Type                     Comment
-- MatrixX       UI8                      Horizontal matrix size
-- MatrixY       UI8                      Vertical matrix size
-- Divisor       FLOAT                    Divisor applied to the matrix values
-- Bias          FLOAT                    Bias applied to the matrix values
-- Matrix        FLOAT[MatrixX * MatrixY] Matrix values
-- DefaultColor  RGBA                     Default color for pixels outside the image
-- Reserved      UB[6]                    Must be 0
-- Clamp         UB[1]                    Clamp mode
-- PreserveAlpha UB[1]                    Preserve the alpha

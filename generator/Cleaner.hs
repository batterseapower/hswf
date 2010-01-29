{-# LANGUAGE PatternGuards #-}
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

main = interact (unlines . waitForAll . wrapRecord . fixupIndentation . fixupLineBreaks . killBlanks . lines)

waitForAll :: [String] -> [String]
waitForAll xs = length xs `seq` xs

killBlanks :: [String] -> [String]
killBlanks = filter (not . null) . map strip
  where strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

fixupLineBreaks :: [String] -> [String]
fixupLineBreaks [] = []
fixupLineBreaks [l] = [l]
fixupLineBreaks (l1:l2:ls)
  | (l2c:_) <- l2
  , let l2ws = words l2
  , isLower l2c || isDigit l2c || isSymbol l2c || length l2ws < 3 || "[" `isInfixOf` (head l2ws) || head l2ws == "If"
  = fixupLineBreaks ((l1 ++ ' ':l2):ls)
  | otherwise
  = l1 : fixupLineBreaks (l2:ls)

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
fixupIndentation (name:ls)
  | all (not . isSpace) name
  = name : go ls
  | otherwise
  = head (words (head ls)) : go (name : ls)
  where
    go ls = [pad fieldname typindent ++ pad typ commentindent ++ comment | (fieldname, typ, comment) <- considered_ls]
      where
        pad s n = s ++ replicate (n - length s) ' '
    
        considered_ls = zipWith consider (True:repeat False) ls
    
        (fieldnames, typs, comments) = unzip3 considered_ls
        typindent = maximum (map length fieldnames) + 1
        commentindent = maximum (map length typs) + 1
    
        consider headerline l = (fieldname, unwords typ, unwords comment)
          where
            fieldname:ws = words l
            (typ, comment) = breakRev partOfTypeField ws

            partOfTypeField w = looksLikeType w || (w == "Type" && headerline) || partOfArrayExpr w

            looksLikeType w = length w > 1 &&
                              w /= "ID" && w /= "SWF" && w /= "URL" &&
                              length (fst (span (\c -> isUpper c || isDigit c) (dropPrefix "Encoded" w))) >= 2 &&
                              all (not . (`isInfixOf` w)) [")", "("] &&
                              not (all isDigit w)
            partOfArrayExpr w = any (`isInfixOf` w) ["*", "+", "]", "["]

            dropPrefix pr xs | take (length pr) xs == pr = drop (length pr) xs
                             | otherwise                 = xs

            breakRev p xs = case break p (reverse xs) of (as, bs) -> (reverse bs, reverse as)

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

wrapRecord ls = "\\begin{record}" : ls ++ ["\\end{record}"]

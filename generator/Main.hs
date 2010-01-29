{-# LANGUAGE NamedFieldPuns, PatternGuards #-}
module Main where

import Control.Arrow (first)
import Control.Monad

import Data.Char
import Data.List
import Data.List.Split (split, keepDelimsR, condense, oneOf)
import Data.Maybe

import System.Environment
import System.IO

import Text.ParserCombinators.Parsec hiding (oneOf)
import Text.ParserCombinators.Parsec.Expr

import Language.Haskell.Exts.Syntax hiding (Type, Assoc(..))
import qualified Language.Haskell.Exts.Syntax as LHE
import qualified Language.Haskell.Exts.Pretty as LHEP


main :: IO ()
main = do
    [file] <- getArgs
    chunks <- fmap parseFile $ readFile file
    
    --mapM_ (hPutStrLn stderr . show) [r | RecordChunk r <- chunks]
    --mapM_ (hPutStrLn stderr . LHEP.prettyPrint) $ concat [recordToDecls r | RecordChunk r <- chunks]
    
    let (mb_taginfos, lss) = unzip $ map (unParseChunk $ catMaybes mb_taginfos) chunks
    putStrLn $ unlines $ concat lss


unParseChunk _ (NonRecordChunk ls) = (Nothing, ls)
unParseChunk _ (RecordChunk r) = (mb_taginfo, codeBlock decls)
  where (mb_taginfo, decls) = recordToDecls r
unParseChunk taginfos GenTagGettersChunk = (Nothing, codeBlock [decl])
  where decl = FunBind [Match noSrcLoc (Ident "generatedTagGetters") [PVar (Ident "tagType")] Nothing (UnGuardedRhs dispatcher) (BDecls [])]
        dispatcher = Case (var "tagType") (alts ++ [default_alt])
        alts = [Alt noSrcLoc (PLit (Int tagType)) (UnGuardedAlt (App (Con $ qname "Just") (var $ "get" ++ recordName))) (BDecls []) | (tagType, recordName, _) <- taginfos]
        default_alt = Alt noSrcLoc PWildCard (UnGuardedAlt (Con $ qname "Nothing")) (BDecls [])
unParseChunk taginfos GenTagConstructorsChunk = (Nothing, ["         |" ++ LHEP.prettyPrint datacon | (_, _, datacon) <- taginfos])

codeBlock ls = "\\begin{code}" : map LHEP.prettyPrint ls ++ ["", "\\end{code}"]


data Chunk = NonRecordChunk [String]
           | RecordChunk Record
           | GenTagGettersChunk
           | GenTagConstructorsChunk
           deriving (Show)

type RecordName = String

data Record = Record {
    record_name :: RecordName,
    record_fields :: [Field]
  } deriving (Show)

type FieldName = String

data Field = Field {
    field_name :: FieldName,
    field_type :: Type,
    field_comment :: String
  } deriving (Show)

type TyCon = String

data Type = Type (Maybe FieldExpr) TyCon (Maybe FieldExpr)
          deriving (Show)

data FieldExpr = LitE Int
               | FieldE FieldName
               | OpE FieldOp FieldExpr FieldExpr
               deriving (Show)

data FieldOp = Plus | Mult | Equals
             deriving (Show)

type TagType = Integer


parseFile :: String -> [Chunk]
parseFile contents = goNo [] (lines contents)
  where
    goNo acc [] = [NonRecordChunk acc]
    goNo acc (l:ls)
      | Just chunk <- lookup l commands = NonRecordChunk acc : chunk : goNo [] ls
      | l == "\\begin{record}"          = NonRecordChunk acc : goYes [] ls
      | otherwise                       = goNo (acc ++ [l]) ls
      where commands = [("\\gengetters{tag}",      GenTagGettersChunk),
                        ("\\genconstructors{tag}", GenTagConstructorsChunk)]
    
    goYes acc [] = error "Unclosed record!"
    goYes acc (l:ls)
      | l == "\\end{record}" = RecordChunk (parseRecordLines acc) : goNo [] ls
      | otherwise            = goYes (acc ++ [l]) ls

parseRecordLines :: [String] -> Record
parseRecordLines (name:headers:ls) = Record name fields
  where
    header_words = map length $ split (keepDelimsR $ condense $ oneOf " ") headers
    [name_offset, type_offset, _end_offset] = case header_words of [a, b, c] -> [a, b, c]; _ -> error ("parseRecordLines headers:\n" ++ show header_words)
    fields = [Field { field_name = strip name, field_type = parseType (strip typ_str), field_comment = comment }
             | l <- ls
             , let (name, l')         = splitAt name_offset l
                   (typ_str, comment) = splitAt type_offset l'
             ]
    
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseType :: String -> Type
parseType s = case parse typ "<memory>" s of Left errs -> error (unlines [s, show errs]); Right ty -> ty
  where
    typ = do
        mb_condition <- optionMaybe condition
        nm <- tycon
        mb_lexpr <- optionMaybe (between (char '[') (char ']') expr)
        return $ Type mb_condition nm mb_lexpr
    
    condition = do { string "If"; spaces; e <- expr; char ','; spaces; return e }
            <?> "condition"
    
    tycon = many1 alphaNum
        <?> "type constructor"
    
    expr = buildExpressionParser table (do { e <- factor; spaces; return e })
       <?> "expression"
    
    table = [[op "*" (OpE Mult) AssocLeft], [op "+" (OpE Plus) AssocLeft], [op "=" (OpE Equals) AssocLeft]]
            where op s f assoc = Infix (do{ string s; spaces; return f}) assoc
    
    factor = between (char '(') (char ')') expr
         <|> field
         <|> literal
         <?> "array length expression factor"
    
    field = do { x <- letter; xs <- many alphaNum; return (FieldE $ x:xs) }
    
    literal = fmap (LitE . read) $ many1 digit


recordToDecls :: Record -> (Maybe (TagType, RecordName, QualConDecl), [Decl])
recordToDecls (Record { record_name, record_fields })
  | (Field "Header" (Type Nothing "RECORDHEADER" Nothing) comment):record_fields <- record_fields
  , let tag_type = read $ drop (length "Tag type = ") comment
  , (datacon, getter) <- process record_fields
  = (Just (tag_type, record_name, datacon), [getter])
  
  | (datacon, getter) <- process record_fields
  = (Nothing, [DataDecl noSrcLoc DataType [] (Ident record_name) [] [datacon] [], getter])
  where
    process record_fields = (datacon, getter)
      where
        datacon = QualConDecl noSrcLoc [] [] (RecDecl (Ident record_name) recfields)
        getter = PatBind noSrcLoc (PVar (Ident $ "get" ++ record_name)) Nothing (UnGuardedRhs getexpr) (BDecls [])
        
        defuser = defuseFieldName record_name
        (field_getters, field_mb_types) = unzip $ map (fieldToSyntax defuser) record_fields
        
        recfields = [([Ident $ defuser field_name], UnBangedTy field_type) | (Field { field_name }, Just field_type) <- record_fields `zip` field_mb_types]
        getexpr = Do $ field_getters ++ [Qualifier $ App (var "return") (RecConstr (qname record_name) [FieldWildcard])]

fieldToSyntax :: (FieldName -> String) -> Field -> (Stmt, Maybe LHE.Type)
fieldToSyntax defuser (Field { field_name, field_type=Type mb_condexpr tycon mb_lenexpr })
  = (Generator noSrcLoc bind_to genexp', if should_exclude then Nothing else Just ty')
  where
    should_exclude = "Reserved" `isInfixOf` field_name
    bind_to = PVar $ Ident $ (if should_exclude then ('_':) else id) $ defuser field_name
    
    (genexp', ty') = case mb_condexpr of
        Nothing       -> (genexp, ty)
        Just condexpr -> (App (App (var "maybeHas") (fieldExprToSyntax defuser condexpr)) genexp, TyApp (TyCon (qname "Maybe")) ty)
    
    (genexp, ty) = case (tycon, mb_lenexpr) of
      ("UB", Just (LitE 1))
        -> (var "getFlag", LHE.TyCon (qname "Bool"))
      (tycon, Just lenexpr)
        | tycon `elem` ["UB", "SB", "FB"]
        -> (App (var $ "get" ++ tycon) (fieldExprToSyntax defuser lenexpr), TyCon (qname tycon))
        | otherwise
        -> (App (var "sequence") (App (App (var "genericReplicate") (fieldExprToSyntax defuser lenexpr)) (var $ "get" ++ tycon)), TyList (TyCon (qname tycon)))
      (tycon, Nothing)
        -> (var $ "get" ++ tycon, TyCon (qname tycon))

fieldExprToSyntax _        (LitE i) = Lit (Int $ fromIntegral i)
fieldExprToSyntax defuser (FieldE x) = var (defuser x)
fieldExprToSyntax defuser (OpE op e1 e2) = InfixApp (fieldExprToSyntax defuser e1) (QVarOp $ UnQual $ Symbol $ eop) (fieldExprToSyntax defuser e2)
  where eop = case op of Plus -> "+"; Mult -> "*"; Equals -> "=="

var = Var . UnQual . Ident
qname = UnQual . Ident
noSrcLoc = SrcLoc "<unknown>" 0 0

defuseFieldName record_name field_name = toVarName record_name ++ '_':toVarName field_name
  where toVarName (c:s) = toLower c : s
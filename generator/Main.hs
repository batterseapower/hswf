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

import Numeric

import Language.Haskell.Exts.Syntax hiding (Type, TyCon, Assoc(..))
import qualified Language.Haskell.Exts.Syntax as LHE
import qualified Language.Haskell.Exts.Pretty as LHEP


main :: IO ()
main = do
    [file] <- getArgs
    chunks <- fmap parseFile $ readFile file
    
    --mapM_ (hPutStrLn stderr . show) [r | RecordChunk r <- chunks]
    --mapM_ (hPutStrLn stderr . LHEP.prettyPrint) $ concat [recordToDecls r | RecordChunk r <- chunks]
    
    let (mb_specialinfos, lss) = unzip $ map (unParseChunk $ catMaybes mb_specialinfos) chunks
    putStrLn $ unlines $ concat lss


unParseChunk :: [SpecialInfo] -> Chunk -> (Maybe SpecialInfo, [String])
unParseChunk _ (NonRecordChunk ls) = (Nothing, ls)
unParseChunk _ (RecordChunk r) = (mb_specialinfo, codeBlock decls)
  where (mb_specialinfo, decls) = recordToDecls $ r { record_fields = identifyExclusions $ identifyComposites $ simplify $ record_fields r }
unParseChunk specialinfos (GenGettersChunk gen) = (Nothing, codeBlock [decl])
  where (varName, getterName) = case gen of
            Tag         -> ("tagType",    "generatedTagGetters")
            Action      -> ("actionCode", "generatedActionGetters")
            ShapeRecord -> error "No getter for generated shape records"
    
        decl = FunBind [Match noSrcLoc (Ident getterName) [PVar (Ident varName)] Nothing (UnGuardedRhs dispatcher) (BDecls [])]
        dispatcher = Case (var varName) (alts ++ [default_alt])
        alts = [Alt noSrcLoc pat (UnGuardedAlt (App (con "Just") (var $ "get" ++ recordName))) (BDecls []) | (gen', pat, recordName, _) <- specialinfos, gen' == gen]
        default_alt = Alt noSrcLoc PWildCard (UnGuardedAlt (con "Nothing")) (BDecls [])
unParseChunk specialinfos (GenConstructorsChunk gen) = (Nothing, ["         " ++ (if firstalt then "=" else "|") ++ LHEP.prettyPrint datacon | (firstalt, datacon) <- (exhaustive : repeat False) `zip` datacons])
  where exhaustive = gen == ShapeRecord
        datacons = [datacon | (gen', _, _, datacon) <- specialinfos, gen' == gen]

codeBlock ls = "\\begin{code}" : map LHEP.prettyPrint ls ++ ["", "\\end{code}"]


data Generatable = Tag | Action | ShapeRecord
                 deriving (Eq, Show)

type SpecialInfo = (Generatable, Pat, RecordName, QualConDecl)

data Chunk = NonRecordChunk [String]
           | RecordChunk Record
           | GenGettersChunk Generatable
           | GenConstructorsChunk Generatable
           deriving (Show)

type RecordName = String

data Record = Record {
    record_name :: RecordName,
    record_params :: [FieldName],
    record_fields :: [Field]
  } deriving (Show)

type FieldName = String

data Field = Field {
    field_name :: FieldName,
    field_type :: Type,
    field_comment :: String,
    field_excluded :: Maybe WhyExcluded
  } deriving (Show)

data WhyExcluded = IsReserved | IsPresenceFlag FieldName | IsLength FieldName
                 deriving (Show)

data TyCon = TyCon String [FieldExpr]
           deriving (Show)

data Type = Type { type_tycons :: [TyCon], type_repeats :: Repeats }
          | IfThenType { type_cond :: FieldExpr, type_then :: Type }
          | IfThenElseType { type_cond :: FieldExpr, type_then :: Type, type_else :: Type }
          | CompositeType { type_fields :: [Field] } -- Inserted by analysis
          deriving (Show)

data Repeats = Once
             | NumberOfTimes FieldExpr
             | OptionallyAtEnd
             | RepeatsUntilEnd
             deriving (Show)

data FieldExpr = LitE Int
               | FieldE FieldName
               | UnOpE FieldUnOp FieldExpr
               | BinOpE FieldBinOp FieldExpr FieldExpr
               deriving (Eq, Show)

data FieldUnOp = Not
               deriving (Eq, Show)

data FieldBinOp = Plus | Mult | Equals | Or | And
                deriving (Eq, Show)

type_cond_maybe :: Type -> Maybe FieldExpr
type_cond_maybe (IfThenType { type_cond })     = Just type_cond
type_cond_maybe (IfThenElseType { type_cond }) = Just type_cond
type_cond_maybe _                              = Nothing

parseFile :: String -> [Chunk]
parseFile contents = goNo [] (lines contents)
  where
    goNo acc [] = [NonRecordChunk acc]
    goNo acc (l:ls)
      | Just chunk <- lookup l commands = NonRecordChunk acc : chunk : goNo [] ls
      | l == "\\begin{record}"          = NonRecordChunk acc : goYes [] ls
      | otherwise                       = goNo (acc ++ [l]) ls
      where commands = [("\\gengetters{tag}",              GenGettersChunk Tag),
                        ("\\genconstructors{tag}",         GenConstructorsChunk Tag),
                        ("\\gengetters{action}",           GenGettersChunk Action),
                        ("\\genconstructors{action}",      GenConstructorsChunk Action),
                        ("\\genconstructors{shaperecord}", GenConstructorsChunk ShapeRecord)]
    
    goYes acc [] = error "Unclosed record!"
    goYes acc (l:ls)
      | l == "\\end{record}" = RecordChunk (parseRecordLines acc) : goNo [] ls
      | otherwise            = goYes (acc ++ [l]) ls

parseRecordLines :: [String] -> Record
parseRecordLines (header_line:headers:ls) = Record name params fields
  where
    (name, params) = parseExactly headerline header_line
    
    header_words = map length $ split (keepDelimsR $ condense $ oneOf " ") headers
    [name_offset, type_offset, _end_offset] = case header_words of [a, b, c] -> [a, b, c]; _ -> error ("parseRecordLines headers:\n" ++ show header_words)
    
    fields = [Field { field_name = strip name, field_type = typ, field_comment = comment, field_excluded = Nothing }
             | l <- ls
             , let (name, l')         = splitAt name_offset l
                   (typ_str, comment) = splitAt type_offset l'
                   typ = case parseType (strip typ_str) of Left errs -> error (unlines [name, typ_str, show errs]); Right typ -> typ
             ]
    
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


parseExactly :: Parser a -> String -> a
parseExactly ma s = case parse ma "<memory>" s of Left errs -> error (show errs); Right a -> a

parseType :: String -> Either ParseError Type
parseType s = parse (do { t <- typ; eof; return t }) "<memory>" s

typ = try conditional
  <|> basetyp
  <?> "type"

basetyp = do
    optional <- optionality
    cons <- fmap return tycon <|> tycons
    repeats <- if optional then return OptionallyAtEnd else repeatspecifier
    return $ Type cons repeats

repeatspecifier = between (char '[') (char ']' >> spaces) (fmap NumberOfTimes expr <|> return RepeatsUntilEnd)
              <|> return Once
              <?> "repeat specifier"

optionality = do { string "(optional)"; spaces; return True }
          <|> return False
          <?> "optionality clause"

conditional = do
    string "If"; spaces
    e <- expr;
    optional (char ','); spaces
    tt <- typ;
    mb_tf <- optionMaybe $ do
        string "Otherwise"; spaces
        optional (char ','); spaces
        typ
    return $ maybe (IfThenType e tt) (IfThenElseType e tt) mb_tf

tycon = do { tc <- many1 alphaNum; mb_args <- optionMaybe arguments; spaces; return $ TyCon tc (fromMaybe [] mb_args) }
    <?> "type constructor"

fieldname = do { x <- letter; xs <- many alphaNum; return (x:xs) }

arguments = between (char '(') (char ')' >> spaces) (sepBy expr (char ',' >> spaces))
        <?> "arguments"

parameters = between (char '(') (char ')' >> spaces) (sepBy fieldname (char ',' >> spaces))
         <?> "parameters"

tycons = between (char '<') (char '>' >> spaces) $ sepBy tycon (char ',' >> spaces)

expr = buildExpressionParser table (do { e <- factor; spaces; return e })
   <?> "expression"

table = [[op "*" (BinOpE Mult) AssocLeft],
         [op "+" (BinOpE Plus) AssocLeft],
         [op "=" (BinOpE Equals) AssocLeft],
         [op "and" (BinOpE And) AssocLeft],
         [op "or" (BinOpE Or) AssocLeft]]
        where op s f assoc = Infix (do{ string s; spaces; return f}) assoc

factor = between (char '(') (char ')' >> spaces) expr
     <|> field
     <|> literal
     <?> "array length expression factor"

field = skiptspaces $ fmap FieldE fieldname

literal = skiptspaces $ fmap (LitE . read) $ many1 digit

headerline = do { name <- many1 alphaNum; mb_params <- optionMaybe parameters; return (name, fromMaybe [] mb_params) }

skiptspaces ma = do
    a <- ma
    spaces
    return a


simplify :: [Field] -> [Field]
simplify = map simplifyOne
  where simplifyOne f = f { field_type = fmapFieldExpr (simplifyFieldExpr True) (field_type f) }
        fmapFieldExpr f ty@(IfThenType { type_cond, type_then }) = ty { type_cond = f type_cond, type_then = fmapFieldExpr f type_then }
        fmapFieldExpr f ty@(IfThenElseType { type_cond, type_then, type_else }) = ty { type_cond = f type_cond, type_then = fmapFieldExpr f type_then, type_else = fmapFieldExpr f type_else }
        fmapFieldExpr _ ty = ty

identifyComposites :: [Field] -> [Field]
identifyComposites [] = []
identifyComposites (f:fs)
  | IfThenType cond typ <- field_type f
  , f <- f { field_type = typ }
  , (fs1, fs2) <- spanMaybe (\f' -> case field_type f' of IfThenType cond' typ' | cond' == cond -> Just (f' { field_type = typ' }); _ -> Nothing) fs
  , not (null fs1)
  , Just name <- compositeName (map field_name $ f:fs1)
  , let composite_typ = IfThenType cond $ CompositeType $ f : fs1
  = Field name composite_typ "" Nothing : identifyComposites fs2
  | otherwise
  = f : identifyComposites fs

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f = go
  where go []     = ([], [])
        go (x:xs) | Just y <- f x = first (y:) $ go xs
                  | otherwise     = ([], x:xs)

compositeName :: [String] -> Maybe String
compositeName names = commonPrefix (map (dropWhile (== 'N')) names) `fallback` commonSuffix names
  where
    commonPrefix = go []
      where go acc ((c:cs):css)
              | all ((== c) . head) css = Just $ fromMaybe (acc ++ [c]) $ go (acc ++ [c]) (cs:map tail css)
              | otherwise               = Nothing

    commonSuffix = fmap reverse . commonPrefix . reverse

    fallback mb1 mb2 = maybe mb2 Just mb1


identifyExclusions :: [Field] -> [Field]
identifyExclusions [] = []
identifyExclusions (f:fs)
  | "Reserved" `isInfixOf` field_name f
  = f { field_excluded = Just IsReserved } : identifyExclusions fs
  
  -- TODO: only do these two if no other fields mention this one?
  
  | [controls_field] <- [other_f | other_f <- fs
                                 , Just (FieldE cond_field_name) <- [type_cond_maybe (field_type other_f)]
                                 , cond_field_name == field_name f]
  = f { field_excluded = Just (IsPresenceFlag $ field_name controls_field) } : identifyExclusions fs
  
  | [controls_field] <- [other_f | other_f <- fs
                                 , Type { type_repeats=NumberOfTimes (FieldE len_field_name) } <- [field_type other_f]
                                 , len_field_name == field_name f]
  = f { field_excluded = Just (IsLength $ field_name controls_field) } : identifyExclusions fs
  
  | otherwise
  = f : identifyExclusions fs


recordToDecls :: Record -> (Maybe SpecialInfo, [Decl])
recordToDecls (Record { record_name, record_params, record_fields })
  | (Field "Header" (Type [TyCon "RECORDHEADER" []] Once) comment Nothing):record_fields <- record_fields
  , let tag_type = read $ drop (length "Tag type = ") comment
  , (datacon, getter) <- process record_fields
  = (Just (Tag, PLit (Int tag_type), record_name, datacon), [getter])
  
  | (Field field_name (Type [TyCon "ACTIONRECORDHEADER" []] Once) comment Nothing):record_fields <- record_fields
  , field_name == record_name
  , [(action_code, _)] <- readHex $ drop (length "ActionCode = 0x") comment
  , (datacon, getter) <- process record_fields
  = (Just (Action, PLit (Int action_code), record_name, datacon), [getter])
  
  | record_name `elem` ["STYLECHANGERECORD", "STRAIGHTEDGERECORD", "CURVEDEDGERECORD"]
  , (datacon, getter) <- process record_fields
  = (Just (ShapeRecord, error "ShapeRecord pattern", record_name, datacon), [getter])
  
  | (datacon, getter) <- process record_fields
  = (Nothing, [DataDecl noSrcLoc DataType [] (Ident record_name) [] [datacon] [], getter])
  where
    process record_fields = (datacon, getter)
      where
        datacon = QualConDecl noSrcLoc [] [] (RecDecl (Ident record_name) recfields)
        getter = FunBind [Match noSrcLoc (Ident $ "get" ++ record_name) (map (PVar . Ident . defuser) record_params) Nothing (UnGuardedRhs getexpr) (BDecls [])]
        
        defuser = defuseFieldName record_name
        (field_getters, field_mb_types) = unzip $ map (fieldToSyntax defuser) record_fields
        
        recfields = [([Ident $ defuser field_name], UnBangedTy field_type) | (Field { field_name }, Just field_type) <- record_fields `zip` field_mb_types]
        getexpr = Do $ field_getters ++ [Qualifier $ App (var "return") (RecConstr (qname record_name) [FieldWildcard])]

fieldToSyntax :: (FieldName -> String) -> Field -> (Stmt, Maybe LHE.Type)
fieldToSyntax defuser field = (Generator noSrcLoc bind_to genexp, if should_exclude then Nothing else Just ty)
  where
    should_exclude = isJust (field_excluded field)
    (genexp, ty) = typeToSyntax defuser (field_type field)
    bind_to = PVar $ Ident $ (case field_excluded field of Just IsReserved -> ('_':); _ -> id) $ defuser (field_name field)

typeToSyntax :: (FieldName -> String) -> Type -> (Exp, LHE.Type)
typeToSyntax defuser typ = case typ of
    CompositeType fields
      -> (Do $ getters ++ [Qualifier $ App (var "return") (Tuple components)], TyTuple Boxed (catMaybes mb_tys))
      where
        components = [Var (UnQual bound_to) | (Generator _ (PVar bound_to) _, Just _) <- gettertys]
        gettertys = map (fieldToSyntax defuser) fields
        (getters, mb_tys) = unzip gettertys

    IfThenType condexpr typ
      -> (App (App (var "maybeHas") (fieldExprToSyntax defuser condexpr)) genexp, maybeTy_ ty)
      where (genexp, ty) = typeToSyntax defuser typ
    
    IfThenElseType condexpr typt typf
      -> (If (fieldExprToSyntax defuser condexpr) (fmap_ (con "Left") genexpt) (fmap_ (con "Right") genexpf), eitherTy_ tyt tyf)
      where (genexpt, tyt) = typeToSyntax defuser typt
            (genexpf, tyf) = typeToSyntax defuser typf
            
            fmap_ efun efunctor = App (App (var "fmap") efun) efunctor
            eitherTy_ ty1 ty2 = TyApp (TyApp (LHE.TyCon (qname "Either")) ty1) ty2
    
    Type [TyCon "UB" []] (NumberOfTimes (LitE 1))
      -> (var "getFlag", LHE.TyCon (qname "Bool"))
    
    Type [TyCon tycon []] (NumberOfTimes lenexpr)
      | tycon `elem` ["UB", "SB", "FB"]
      -> (App (var $ "get" ++ tycon) (fieldExprToSyntax defuser lenexpr), LHE.TyCon (qname tycon))
    
    Type [TyCon "UB" []] RepeatsUntilEnd
      -> (var "byteAlign", TyTuple Boxed [])
    
    Type [TyCon "BYTE" []] RepeatsUntilEnd
      -> (var "getRemainingLazyByteString", LHE.TyCon (qname "ByteString"))
    
    Type tycons repeats -> case repeats of
        Once
          -> (onegenexp, onety)
        NumberOfTimes lenexpr
          -> (App (App (var "genericReplicateM") (fieldExprToSyntax defuser lenexpr)) onegenexp, TyList onety)
        OptionallyAtEnd
          -> (App (App (var "maybeHasM") (App (App (var "fmap") (var "not")) (var "isEmpty"))) onegenexp, maybeTy_ onety)
        RepeatsUntilEnd
          -> (App (var "getToEnd") onegenexp, TyList onety)
      where
        (onegenexp, onety) = case tycons of
          [tycon] -> (getOne tycon, tyOne tycon)
          _       -> (apps (var $ "liftM" ++ show ntycons) (var ("(" ++ replicate (ntycons - 1) ',' ++ ")") : map getOne tycons), TyTuple Boxed (map tyOne tycons))
        
        ntycons = length tycons
        getOne (TyCon tycon args) = apps (var $ "get" ++ tycon) (map (fieldExprToSyntax defuser) args)
        tyOne (TyCon tycon _)     = LHE.TyCon (qname tycon)

  where
    maybeTy_ = TyApp (LHE.TyCon (qname "Maybe"))

fieldExprToSyntax _       (LitE i) = Lit (Int $ fromIntegral i)
fieldExprToSyntax defuser (FieldE x) = var (defuser x)
fieldExprToSyntax defuser (UnOpE op e) = App eop (fieldExprToSyntax defuser e)
  where eop = case op of Not -> var "not"
fieldExprToSyntax defuser (BinOpE op e1 e2) = InfixApp (fieldExprToSyntax defuser e1) (QVarOp $ UnQual $ Symbol $ eop) (fieldExprToSyntax defuser e2)
  where eop = case op of Plus -> "+"; Mult -> "*"; Equals -> "=="; And -> "&&"; Or -> "||"

simplifyFieldExpr True (BinOpE Equals e1 (LitE 1)) = simplifyFieldExpr True e1
simplifyFieldExpr True (BinOpE Equals e1 (LitE 0)) = UnOpE Not (simplifyFieldExpr True e1)
simplifyFieldExpr True (BinOpE op e1 e2) = BinOpE op (simplifyFieldExpr e1ty e1) (simplifyFieldExpr e2ty e2)
  where (e1ty, e2ty) | op `elem` [And, Or] = (True, True)
                     | otherwise           = (False, False)
simplifyFieldExpr True (UnOpE Not e) = UnOpE Not (simplifyFieldExpr True e)
simplifyFieldExpr _    e = e

var = Var . UnQual . Ident
qname = UnQual . Ident
con = Con . qname
noSrcLoc = SrcLoc "<unknown>" 0 0
apps = foldl App

defuseFieldName record_name field_name = toVarName record_name ++ '_':toVarName field_name
  where toVarName (c:s) = toLower c : s
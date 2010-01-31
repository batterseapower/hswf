{-# LANGUAGE NamedFieldPuns, PatternGuards #-}
module Main where

import Control.Arrow (first)
import Control.Monad

import Data.Char
import Data.List
import Data.List.Split (split, keepDelimsR, condense, oneOf)
import Data.Maybe
import qualified Data.Map as M

import System.Environment
import System.IO

import Text.ParserCombinators.Parsec hiding (oneOf)
import Text.ParserCombinators.Parsec.Expr

import Numeric

import Language.Haskell.Exts.Syntax hiding (Type, TyCon, Assoc(..))
import qualified Language.Haskell.Exts.Syntax as LHE
import qualified Language.Haskell.Exts.Pretty as LHEP


-- TODO:
--  * Mark reserved fields with _ instead of detecting them


main :: IO ()
main = do
    [file] <- getArgs
    chunks <- fmap parseFile $ readFile file
    
    --mapM_ (hPutStrLn stderr . show) [r | RecordChunk r <- chunks]
    --mapM_ (hPutStrLn stderr . LHEP.prettyPrint) $ concat [recordToDecls r | RecordChunk r <- chunks]
    
    let (specialinfos, lss) = unzip $ map (unParseChunk $ unionSpecialInfos specialinfos) chunks
    putStrLn $ unlines $ concat lss


unParseChunk :: SpecialInfo -> Chunk -> (SpecialInfo, [String])
unParseChunk _ (NonRecordChunk ls)
  = (emptySpecialInfo, ls)
unParseChunk _ (RecordChunk r)
  = (specialinfo, codeBlock decls)
  where (specialinfo, decls) = recordToDecls $ r { record_fields = identifyExclusions $ identifyComposites $ simplify $ record_fields r }
unParseChunk (getter_special, putter_special, _) (GenFunctionsChunk gen)
  = (emptySpecialInfo, codeBlock [mkDispatcher "Getters" getter_special,
                                  mkDispatcher "Putters" putter_special])
  where varName = map toLower (show gen)
    
        mkDispatcher thing thingmap
          = FunBind [Match noSrcLoc (Ident $ "generated" ++ show gen ++ thing) [PVar (Ident varName)] Nothing (UnGuardedRhs dispatcher) (BDecls [])]
          where
            dispatcher = Case (var varName) (alts ++ [default_alt])
            alts = [Alt noSrcLoc pat (UnGuardedAlt (App (con "Just") (Var $ UnQual thingname))) (BDecls []) | (pat, thingname) <- M.findWithDefault [] gen thingmap]
            default_alt = Alt noSrcLoc PWildCard (UnGuardedAlt (con "Nothing")) (BDecls [])
unParseChunk (_, _, datacon_special) (GenConstructorsChunk gen)
  = (emptySpecialInfo, ["         " ++ (if firstalt then "=" else "|") ++ LHEP.prettyPrint datacon | (firstalt, datacon) <- (exhaustive : repeat False) `zip` datacons])
  where exhaustive = gen == ShapeRecord
        datacons = M.findWithDefault [] gen datacon_special

codeBlock ls = "\\begin{code}" : map LHEP.prettyPrint ls ++ ["", "\\end{code}"]


data Generatable = Tag | Action | ShapeRecord
                 deriving (Eq, Ord, Show)

type SpecialInfo = (M.Map Generatable [(Pat, Name)], -- Dispatch to getter with given name
                    M.Map Generatable [(Pat, Name)], -- Dispatch to putter with given name
                    M.Map Generatable [QualConDecl]) -- Output specified data constructor

emptySpecialInfo = (M.empty, M.empty, M.empty)

unionSpecialInfos :: [SpecialInfo] -> SpecialInfo
unionSpecialInfos sis = case unzip3 sis of
    (si1s, si2s, si3s) -> (M.unionsWith (++) si1s, M.unionsWith (++) si2s, M.unionsWith (++) si3s)


data Chunk = NonRecordChunk [String]
           | RecordChunk Record
           | GenFunctionsChunk Generatable
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

data FieldBinOp = Plus | Mult | Equals | NotEquals | Or | And
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
      where commands = [("\\genfunctions{tag}",            GenFunctionsChunk Tag),
                        ("\\genconstructors{tag}",         GenConstructorsChunk Tag),
                        ("\\genfunctions{action}",         GenFunctionsChunk Action),
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
         [op "=" (BinOpE Equals) AssocLeft, op "!=" (BinOpE NotEquals) AssocLeft],
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
  , let composite_typ  = IfThenType cond $ CompositeType $ f : fs1
        composite_name = compositeName (map field_name $ f:fs1)
  = Field composite_name composite_typ "" Nothing : identifyComposites fs2
  | otherwise
  = f : identifyComposites fs


compositeName :: [String] -> String
compositeName names
  = case commonPrefix names `fallback` commonSuffix names of Just name -> name; Nothing -> error ("Nothing in common: " ++ show names)
  where
    commonPrefix :: Eq a => [[a]] -> Maybe [a]
    commonPrefix = go []
      where go acc ((c:cs):css)
              | all ((== c) . head) css = Just $ fromMaybe (acc ++ [c]) $ go (acc ++ [c]) (cs:map tail css)
              | otherwise               = Nothing

    commonSuffix = fmap reverse . commonPrefix . map reverse

    fallback mb1 mb2 = maybe mb2 Just mb1


identifyExclusions :: [Field] -> [Field]
identifyExclusions [] = []
identifyExclusions (f:fs)
  | any (`isInfixOf` field_name f) ["Reserved", "Padding"]
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


recordToDecls :: Record -> (SpecialInfo, [Decl])
recordToDecls (Record { record_name, record_params, record_fields })
  | (Field "Header" (Type [TyCon "RECORDHEADER" []] Once) comment Nothing):record_fields <- record_fields
  , let tag_type = read $ drop (length "Tag type = ") comment
  , (datacon, datacon_name, getter, getter_name, putter, putter_name) <- process record_fields
  = ((M.singleton Tag [(PLit (Int tag_type), getter_name)],
      M.singleton Tag [(PRec datacon_name [PFieldWildcard], putter_name)],
      M.singleton Tag [datacon]),
     [getter, putter])
  
  | (Field field_name (Type [TyCon "ACTIONRECORDHEADER" []] Once) comment Nothing):record_fields <- record_fields
  , field_name == record_name
  , [(action_code, _)] <- readHex $ drop (length "ActionCode = 0x") comment
  , (datacon, datacon_name, getter, getter_name, putter, putter_name) <- process record_fields
  = ((M.singleton Action [(PLit (Int action_code), getter_name)],
      M.singleton Action [(PRec datacon_name [PFieldWildcard], putter_name)],
      M.singleton Action [datacon]),
     [getter, putter])
  
  | record_name `elem` ["STYLECHANGERECORD", "STRAIGHTEDGERECORD", "CURVEDEDGERECORD"]
  , (datacon, _, getter, _, putter, _) <- process record_fields
  = ((M.empty, M.empty, M.singleton ShapeRecord [datacon]),
     [getter, putter])
  
  | (datacon, _, getter, _, putter, _) <- process record_fields
  = ((M.empty, M.empty, M.empty),
     [DataDecl noSrcLoc DataType [] (Ident record_name) [] [datacon] derivng, getter, putter])
  where
    derivng = [(qname "Eq", []), (qname "Show", []), (qname "Typeable", []), (qname "Data", [])]
    
    process record_fields = (datacon, datacon_name, getter, getter_name, putter, putter_name)
      where
        datacon_name = qname record_name
        datacon = QualConDecl noSrcLoc [] [] (RecDecl (Ident record_name) recfields)
        recfields = [([bndr], UnBangedTy typ) | (bndr, typ) <- present_fields]
        
        defuser = defuseFieldName record_name
        params_bndrs = map (PVar . Ident . defuser) record_params
        
        getter_name = Ident $ "get" ++ record_name
        getter = FunBind [Match noSrcLoc getter_name params_bndrs Nothing (UnGuardedRhs getexpr) (BDecls [])]
        
        putter_name = Ident $ "put" ++ record_name
        putter = FunBind [Match noSrcLoc putter_name (params_bndrs ++ [PRec datacon_name [PFieldWildcard]]) Nothing (UnGuardedRhs putexpr) (BDecls [])]
        
        (present_fields, getexpr, putexpr) = fieldsToSyntax defuser record_fields (\_ -> RecConstr (qname record_name) [FieldWildcard])

fieldsToSyntax :: (FieldName -> String) -> [Field] -> ([Name] -> Exp) -> ([(Name, LHE.Type)], Exp, Exp)
fieldsToSyntax defuser fields finish
  = (present_fields,
     Do $ getter_stmts ++ [Qualifier $ App (var "return") $ finish (map fst present_fields)],
     Do $ putter_stmts ++ [Qualifier $ App (var "return") (Tuple [])])
  where
    present_fields = [(bndr, typ) | (bndr, Right typ) <- bndrs `zip` storages]
    
    getter_stmt bndr getter = Generator noSrcLoc (PVar bndr) getter
    getter_stmts = zipWith getter_stmt bndrs getters
    
    putter_stmt bndr (Left we) putter = LetStmt (BDecls [PatBind noSrcLoc (PVar bndr) Nothing (UnGuardedRhs $ whyExcludedToSyntax defuser we) (BDecls [])])
    putter_stmt bndr (Right _) putter = Qualifier $ putter (Var (UnQual bndr))
    putter_stmts = zipWith3 putter_stmt bndrs storages putters
    
    (bndrs, getters, putters, storages) = unzip4 $ map (fieldToSyntax defuser) fields

fieldToSyntax :: (FieldName -> String) -> Field -> (Name, Exp, Exp -> Exp, Either WhyExcluded LHE.Type)
fieldToSyntax defuser field
  = (Ident $ (case field_excluded field of Just IsReserved -> ('_':); _ -> id) $ defuser (field_name field),
     getexp,
     putexp,
     maybe (Right ty) Left (field_excluded field))
  where (getexp, putexp, ty) = typeToSyntax defuser (field_type field)

typeToSyntax :: (FieldName -> String) -> Type -> (Exp, Exp -> Exp, LHE.Type)
typeToSyntax defuser typ = case typ of
    CompositeType fields
      -> (getter,
          \e -> caseTupleKnownNames_ present_bndrs e putter,
          TyTuple Boxed present_typs)
      where
        (present_bndrs, present_typs) = unzip present_fields
        (present_fields, getter, putter) = fieldsToSyntax defuser fields (Tuple . map (Var . UnQual))

    IfThenType condexpr typ -- TODO: check consistency
      -> (App (App (var "maybeHas") (fieldExprToSyntax defuser condexpr)) getexp,
          \e -> caseMaybe_ e putexp (App (var "return") (Tuple [])),
          maybeTy_ ty)
      where (getexp, putexp, ty) = typeToSyntax defuser typ
    
    IfThenElseType condexpr typt typf -- TODO: check consistency
      -> (If (fieldExprToSyntax defuser condexpr) (fmap_ (con "Left") getexpt) (fmap_ (con "Right") getexpf),
          \e -> caseEither_ e putexpt putexpf,
          eitherTy_ tyt tyf)
      where (getexpt, putexpt, tyt) = typeToSyntax defuser typt
            (getexpf, putexpf, tyf) = typeToSyntax defuser typf
            
            fmap_ efun efunctor = App (App (var "fmap") efun) efunctor
            eitherTy_ ty1 ty2 = TyApp (TyApp (LHE.TyCon (qname "Either")) ty1) ty2
    
    Type [TyCon "UB" []] (NumberOfTimes (LitE 1))
      -> (var "getFlag",
          App $ var "putFlag",
          LHE.TyCon (qname "Bool"))
    
    Type [TyCon tycon []] (NumberOfTimes lenexpr)
      | tycon `elem` ["UB", "SB", "FB"]
      -> (App (var $ "get" ++ tycon) lenexpr_syn,
          App $ App (var $ "put" ++ tycon) lenexpr_syn,
          LHE.TyCon (qname tycon))
      where lenexpr_syn = fieldExprToSyntax defuser lenexpr
    
    Type [TyCon "BYTE" []] RepeatsUntilEnd
      -> (var "getRemainingLazyByteString",
          App $ var "putLazyByteString",
          LHE.TyCon (qname "ByteString"))
    
    Type tycons repeats -> case repeats of
        Once
          -> (onegenexp, oneputexp, onety)
        NumberOfTimes lenexpr
          -> (App (App (var "genericReplicateM") (fieldExprToSyntax defuser lenexpr)) onegenexp,
              mapM__ (reifyLambda oneputexp), -- TODO: check consistency
              TyList onety)
        OptionallyAtEnd
          -> (App (App (var "maybeHasM") (App (App (var "fmap") (var "not")) (var "isEmpty"))) onegenexp,
              \e -> caseMaybe_ e oneputexp (App (var "return") (Tuple [])),
              maybeTy_ onety)
        RepeatsUntilEnd
          -> (App (var "getToEnd") onegenexp,
              mapM__ (reifyLambda oneputexp),
              TyList onety)
      where
        (onegenexp, oneputexp, onety) = case tycons of
          [tycon] -> (getOne tycon, putOne tycon, tyOne tycon)
          _       -> (lift_ (Con con : map getOne tycons),
                      \e -> caseTuple_ ntycons e $ \xs -> Do $ zipWith (\tycon x -> Qualifier $ putOne tycon (Var $ UnQual x)) tycons xs,
                      TyTuple Boxed (map tyOne tycons))
          where ntycons = length tycons
                lift_   = apps (var $ "liftM" ++ show ntycons)
                con     = qname $ "(" ++ replicate (ntycons - 1) ',' ++ ")"
        
        fieldExprs = map (fieldExprToSyntax defuser)
        
        getOne (TyCon "PADDING8" []) = var "byteAlign"
        getOne (TyCon tycon args)    = apps (var $ "get" ++ tycon) (fieldExprs args)
        
        putOne (TyCon "PADDING8" []) = const $ var "flushBits"
        putOne (TyCon tycon args)    = App $ apps (var $ "put" ++ tycon) (fieldExprs args)
        
        tyOne (TyCon tycon _)        = LHE.TyCon (qname tycon)


whyExcludedToSyntax _       IsReserved          = Lit (Int 0)
whyExcludedToSyntax defuser (IsPresenceFlag fn) = App (var "isJust") (var $ defuser fn)
whyExcludedToSyntax defuser (IsLength fn)       = App (var "genericLength") (var $ defuser fn)

fieldExprToSyntax _       (LitE i) = Lit (Int $ fromIntegral i)
fieldExprToSyntax defuser (FieldE x) = var (defuser x)
fieldExprToSyntax defuser (UnOpE op e) = App eop (fieldExprToSyntax defuser e)
  where eop = case op of Not -> var "not"
fieldExprToSyntax defuser (BinOpE op e1 e2) = InfixApp (fieldExprToSyntax defuser e1) (QVarOp $ UnQual $ Symbol $ eop) (fieldExprToSyntax defuser e2)
  where eop = case op of Plus -> "+"; Mult -> "*"; Equals -> "=="; NotEquals -> "/="; And -> "&&"; Or -> "||"


simplifyFieldExpr True (BinOpE Equals e1 (LitE 1)) = simplifyFieldExpr True e1
simplifyFieldExpr True (BinOpE Equals e1 (LitE 0)) = UnOpE Not (simplifyFieldExpr True e1)
simplifyFieldExpr True (BinOpE op e1 e2) = BinOpE op (simplifyFieldExpr e1ty e1) (simplifyFieldExpr e2ty e2)
  where (e1ty, e2ty) | op `elem` [And, Or] = (True, True)
                     | otherwise           = (False, False)
simplifyFieldExpr True (UnOpE Not e) = UnOpE Not (simplifyFieldExpr True e)
simplifyFieldExpr _    e = e


defuseFieldName record_name field_name = toVarName record_name ++ '_':toVarName field_name
  where toVarName (c:s) = toLower c : s


maybeTy_ = TyApp (LHE.TyCon (qname "Maybe"))

reifyLambda oneputexp = Lambda noSrcLoc [PVar $ Ident "x"] (oneputexp $ var "x")
mapM__ ef exs = App (App (var "mapM_") ef) exs

caseMaybe_ e e_just e_nothing
  = Case e [Alt noSrcLoc (PApp (qname "Just") [PVar $ Ident "x"]) (UnGuardedAlt $ e_just (var "x")) (BDecls []),
            Alt noSrcLoc (PApp (qname "Nothing") [])              (UnGuardedAlt e_nothing)          (BDecls [])]

caseEither_ e e_left e_right
  = Case e [Alt noSrcLoc (PApp (qname "Left")  [PVar $ Ident "x"]) (UnGuardedAlt $ e_left (var "x"))  (BDecls []),
            Alt noSrcLoc (PApp (qname "Right") [PVar $ Ident "x"]) (UnGuardedAlt $ e_right (var "x")) (BDecls [])]

caseTuple_ n e e_branch = caseTupleKnownNames_ xs e (e_branch xs)
  where xs = map (Ident . ("x" ++) . show) [1..n]

caseTupleKnownNames_ xs e e_branch
  = Case e [Alt noSrcLoc (PTuple (map PVar xs)) (UnGuardedAlt e_branch) (BDecls [])]


var = Var . UnQual . Ident
qname = UnQual . Ident
con = Con . qname
noSrcLoc = SrcLoc "<unknown>" 0 0
apps = foldl App


spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f = go
  where go []     = ([], [])
        go (x:xs) | Just y <- f x = first (y:) $ go xs
                  | otherwise     = ([], x:xs)

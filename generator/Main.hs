{-# LANGUAGE NamedFieldPuns, PatternGuards, DeriveDataTypeable #-}
module Main where

import Control.Arrow (first)
import Control.Monad

import Data.Char
import Data.Data (Data, Typeable)
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
--  * Don't generate consistency checks for condexpr and
--    lenexpr that are the basis for excluding a field, since
--    they will always be consistent with the rematerialised value


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
unParseChunk (dispatcher_special, _) (GenFunctionsChunk gen)
  = (emptySpecialInfo, codeBlock [getter_decl, putter_decl, types_decl])
  where
    dispatcher_decl dispatching dispatcher = FunBind [Match noSrcLoc (Ident $ "generated" ++ show gen ++ dispatching) [PVar (Ident var_name)] Nothing (UnGuardedRhs $ dispatcher var_name) (BDecls [])]
      where var_name = map toLower (show gen)
    
    getter_decl = dispatcher_decl "Getters" dispatcher
      where
        dispatcher x = Case (var x) (alts ++ [default_alt])
        alts = [Alt noSrcLoc (PLit lit) (UnGuardedAlt (App (con "Just") (Var gettername))) (BDecls []) | (lit, gettername, _, _) <- M.findWithDefault [] gen dispatcher_special]
        default_alt = Alt noSrcLoc PWildCard (UnGuardedAlt (con "Nothing")) (BDecls [])

    putter_decl = dispatcher_decl "Putters" dispatcher
      where
        dispatcher x = Case (var x) (alts x)
        alts x = [Alt noSrcLoc pat (UnGuardedAlt (App (Var puttername) (var x))) (BDecls []) | (_, _, pat, puttername) <- M.findWithDefault [] gen dispatcher_special]

    types_decl = dispatcher_decl "Types" dispatcher
      where
        dispatcher x = Case (var x) alts
        alts = [Alt noSrcLoc pat (UnGuardedAlt (Lit lit)) (BDecls []) | (lit, _, pat, _) <- M.findWithDefault [] gen dispatcher_special]

unParseChunk (_, datacon_special) (GenConstructorsChunk gen)
  = (emptySpecialInfo, ["         " ++ (if firstalt then "=" else "|") ++ LHEP.prettyPrint datacon | (firstalt, datacon) <- (exhaustive : repeat False) `zip` datacons])
  where exhaustive = gen == ShapeRecord
        datacons = M.findWithDefault [] gen datacon_special

codeBlock ls = "\\begin{code}" : map LHEP.prettyPrint ls ++ ["", "\\end{code}"]


data Generatable = Tag | Action | ShapeRecord
                 deriving (Eq, Ord, Show)

type SpecialInfo
  = (M.Map Generatable [(Literal, QName, Pat, QName)],
      -- Upon Lit, dispatch to getter with given QName, producing something
      -- matched by the Pat by the putter at QName
     M.Map Generatable [QualConDecl])
       -- Output specified data constructor

emptySpecialInfo = (M.empty, M.empty)

unionSpecialInfos :: [SpecialInfo] -> SpecialInfo
unionSpecialInfos sis = case unzip sis of
    (si1s, si2s) -> (M.unionsWith (++) si1s, M.unionsWith (++) si2s)


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
  } deriving (Show, Typeable, Data)

type FieldName = String

data Field = Field {
    field_name :: FieldName,
    field_type :: Type,
    field_comment :: String,
    field_excluded :: Maybe WhyExcluded
  } deriving (Show, Typeable, Data)

data WhyExcluded = IsReserved | IsPresenceFlag FieldName | IsSelectFlag FieldName | IsLength FieldName | IsBitLength BitTyConName FieldName
                 deriving (Show, Typeable, Data)

data BitTyConName = UB | SB | FB
                  deriving (Show, Typeable, Data)

data TyCon = TyCon String [FieldExpr]
           | BitsTyCon BitTyConName FieldExpr
           deriving (Show, Typeable, Data)

data Type = TyConType { type_tycon :: TyCon }
          | RepeatType { type_type :: Type, type_repeats :: Repeats }
          | IfThenType { type_cond :: FieldExpr, type_then :: Type }
          | IfThenElseType { type_cond :: FieldExpr, type_then :: Type, type_else :: Type }
          | CompositeType { type_fields :: [Field] } -- Inserted by analysis
          | TupleType { type_types :: [Type] }
          deriving (Show, Typeable, Data)

data Repeats = NumberOfTimes FieldExpr
             | OptionallyAtEnd
             | RepeatsUntilEnd
             deriving (Show, Typeable, Data)

data FieldExpr = LitE Int
               | FieldE FieldName
               | UnOpE FieldUnOp FieldExpr
               | BinOpE FieldBinOp FieldExpr FieldExpr
               deriving (Eq, Show, Typeable, Data)

data FieldUnOp = Not
               deriving (Eq, Show, Typeable, Data)

data FieldBinOp = Plus | Mult | Equals | NotEquals | Or | And
                deriving (Eq, Show, Typeable, Data)

type_cond_maybe :: Type -> Maybe (Bool, FieldExpr)
type_cond_maybe (IfThenType { type_cond })     = Just (True, type_cond)
type_cond_maybe (IfThenElseType { type_cond }) = Just (False, type_cond)
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
    cons <- tycontyp <|> tupletyp
    mb_repeats <- if optional then return (Just OptionallyAtEnd) else optionMaybe repeatspecifier
    return $ maybe cons (RepeatType cons) mb_repeats

repeatspecifier = between (char '[') (char ']' >> spaces) (fmap NumberOfTimes expr <|> return RepeatsUntilEnd)
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

tycon = do
    tc <- many1 alphaNum
    case lookup tc [("UB", UB), ("SB", SB), ("FB", FB)] of
      Just btc -> do
        e <- between (char '[') (char ']' >> spaces) expr
        return $ BitsTyCon btc e
      Nothing -> do
        mb_args <- optionMaybe arguments; spaces
        return $ TyCon tc (fromMaybe [] mb_args)

tycontyp = fmap TyConType tycon
         <?> "type constructor"

fieldname = do { x <- letter; xs <- many alphaNum; return (x:xs) }

arguments = between (char '(') (char ')' >> spaces) (sepBy expr (char ',' >> spaces))
        <?> "arguments"

parameters = between (char '(') (char ')' >> spaces) (sepBy fieldname (char ',' >> spaces))
         <?> "parameters"

tupletyp = fmap TupleType $ between (char '<') (char '>' >> spaces) $ sepBy typ (char ',' >> spaces)

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


-- The real purpose of this is to fix up the types of the conditional expressions (0 may be used instead of False, etc)
simplify :: [Field] -> [Field]
simplify = map simplifyOne
  where simplifyOne f = f { field_type = fmapFieldExpr (simplifyFieldExpr True) (field_type f) }
        fmapFieldExpr f ty@(IfThenType { type_cond, type_then }) = ty { type_cond = f type_cond, type_then = fmapFieldExpr f type_then }
        fmapFieldExpr f ty@(IfThenElseType { type_cond, type_then, type_else }) = ty { type_cond = f type_cond, type_then = fmapFieldExpr f type_then, type_else = fmapFieldExpr f type_else }
        fmapFieldExpr _ ty = ty

identifyComposites :: [Field] -> [Field]
identifyComposites [] = []
identifyComposites (f:fs)
  
  -- TODO: should really only do this combining if the fields are not in scope later on
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
  
  -- NB: we can only be said to control a field if the length count
  -- or condition is at the top level. If it is nested within another
  -- then when we write back we won't necessarily be able to materialise
  -- its value.
  
  -- However, it's totally OK to control more than one field. We can
  -- still extract a value in this case, and the consistency checks
  -- will ensure that everything is OK when writing back.
  
  | ((if_then, controls_field):_)
      <- [(if_then, other_f)
         | other_f <- fs
         , Just (if_then, FieldE cond_field_name) <- [type_cond_maybe (field_type other_f)]
         , cond_field_name == field_name f]
  = f { field_excluded = Just ((if if_then then IsPresenceFlag else IsSelectFlag) $ field_name controls_field) } : identifyExclusions fs
  
  | (controls_field:_)
      <- [other_f
         | other_f <- fs
         , RepeatType { type_type=typ, type_repeats=NumberOfTimes (FieldE len_field_name) } <- [field_type other_f]
         , len_field_name == field_name f]
  = f { field_excluded = Just (IsLength $ field_name controls_field) } : identifyExclusions fs
  
  | otherwise
  = f : identifyExclusions fs


recordToDecls :: Record -> (SpecialInfo, [Decl])
recordToDecls (Record { record_name, record_params, record_fields })
  | (Field "Header" (TyConType (TyCon "RECORDHEADER" [])) comment Nothing):record_fields <- record_fields
  , let tag_type = read $ drop (length "Tag type = ") comment
  , (datacon, datacon_name, getter, getter_name, putter, putter_name) <- process record_fields
  = ((M.singleton Tag [(Int tag_type, getter_name, PRec datacon_name [PFieldWildcard], putter_name)],
      M.singleton Tag [datacon]),
     [getter, putter])
  
  | (Field field_name (TyConType (TyCon "ACTIONRECORDHEADER" [])) comment Nothing):record_fields <- record_fields
  , field_name == record_name
  , [(action_code, _)] <- readHex $ drop (length "ActionCode = 0x") comment
  , (datacon, datacon_name, getter, getter_name, putter, putter_name) <- process record_fields
  = ((M.singleton Action [(Int action_code, getter_name, PRec datacon_name [PFieldWildcard], putter_name)],
      M.singleton Action [datacon]),
     [getter, putter])
  
  | record_name `elem` ["STYLECHANGERECORD", "STRAIGHTEDGERECORD", "CURVEDEDGERECORD"]
  , (datacon, _, getter, _, putter, _) <- process record_fields
  = ((M.empty, M.singleton ShapeRecord [datacon]),
     [getter, putter])
  
  | (datacon, _, getter, _, putter, _) <- process record_fields
  = ((M.empty, M.empty),
     [DataDecl noSrcLoc DataType [] (Ident record_name) [] [datacon] derivng, getter, putter])
  where
    derivng = [(qname "Eq", []), (qname "Show", []), (qname "Typeable", []), (qname "Data", [])]
    
    process record_fields = (datacon, datacon_name, getter, UnQual getter_name, putter, UnQual putter_name)
      where
        datacon_name = qname record_name
        datacon = QualConDecl noSrcLoc [] [] (RecDecl (Ident record_name) recfields)
        recfields = [([bndr], UnBangedTy typ) | (bndr, typ) <- present_fields]
        
        defuser = defuseFieldName record_name
        params_bndrs = map (PVar . defuser) record_params
        
        getter_name = Ident $ "get" ++ record_name
        getter = FunBind [Match noSrcLoc getter_name params_bndrs Nothing (UnGuardedRhs getexpr) (BDecls [])]
        
        putter_name = Ident $ "put" ++ record_name
        putter = FunBind [Match noSrcLoc putter_name (params_bndrs ++ [PRec datacon_name [PFieldWildcard]]) Nothing (UnGuardedRhs putexpr) (BDecls [])]
        
        (present_fields, getexpr, putexpr) = fieldsToSyntax defuser record_fields (\_ -> RecConstr (qname record_name) [FieldWildcard])

fieldsToSyntax :: (FieldName -> Name) -> [Field] -> ([Name] -> Exp) -> ([(Name, LHE.Type)], Exp, Exp)
fieldsToSyntax defuser fields finish
  = (present_fields,
     Do $ getter_stmts ++ [Qualifier $ App (var "return") $ finish (map fst present_fields)],
     Do $ putter_stmts ++ [Qualifier $ App (var "return") (Tuple [])])
  where
    present_fields = catMaybes mb_present_fieldss
    putter_stmts = concat putter_stmtss
    (mb_present_fieldss, getter_stmts, putter_stmtss) = unzip3 $ map (fieldToSyntax defuser) fields

fieldToSyntax :: (FieldName -> Name) -> Field -> (Maybe (Name, LHE.Type), Stmt, [Stmt])
fieldToSyntax defuser field = case field_excluded field of
    Just we | IsReserved <- we -> (Nothing, Qualifier (App (var "discardReserved") getexp), putter_stmts)
            | otherwise        -> (Nothing, Generator noSrcLoc (PVar bndr) getexp, putter_stmts)
            where putter_stmts = [LetStmt (BDecls [PatBind noSrcLoc (PVar bndr) Nothing (UnGuardedRhs $ whyExcludedToSyntax defuser we) (BDecls [])]),
                                  Qualifier $ putexp (Var (UnQual bndr))]
    Nothing -> (Just (bndr, ty), Generator noSrcLoc (PVar bndr) getexp, [Qualifier $ putexp (Var (UnQual bndr))])
  where
    bndr = defuser (field_name field)
    (getexp, putexp, ty) = typeToSyntax defuser (field_type field)

typeToSyntax :: (FieldName -> Name) -> Type -> (Exp, Exp -> Exp, LHE.Type)
typeToSyntax defuser typ = case typ of
    TyConType (TyCon "PADDING8" [])
      -> (var "byteAlign",
           -- Rather nasty hack here to deal with PADDING8. If we don't provide the expression
           -- to "put" into this field with a type, GHC will complain about ambiguous type variables,
           -- but flushBits doesn't need any value at all. Solution: force it to have a particular type.
          \e -> App (App (var "const") (var "flushBits")) (ExpTypeSig noSrcLoc e $ TyTuple Boxed []),
          TyTuple Boxed [])
    
    TyConType (TyCon tycon args)
      -> (apps (var $ "get" ++ tycon) args_syns,
          App $ apps (var $ "put" ++ tycon) args_syns,
          LHE.TyCon (qname tycon))
      where args_syns = map (fieldExprToSyntax defuser) args

    TyConType (BitsTyCon UB (LitE 1))
      -> (var "getFlag",
          App $ var "putFlag",
          LHE.TyCon (qname "Bool"))

    TyConType (BitsTyCon btc lenexpr)
      -> (App (var $ "get" ++ show btc) lenexpr_syn,
          App $ App (var $ "put" ++ show btc) lenexpr_syn,
          LHE.TyCon (qname $ show btc))
      where lenexpr_syn = fieldExprToSyntax defuser lenexpr

    TupleType typs
      -> (lift_ (Con con : getters),
          \e -> caseTuple_ nelems e $ \xs -> Do $ zipWith (\putter x -> Qualifier $ putter (Var $ UnQual x)) putters xs,
          TyTuple Boxed tys)
      where (getters, putters, tys) = unzip3 $ map (typeToSyntax defuser) typs
            nelems = length typs
            lift_  = apps (var $ "liftM" ++ show nelems)
            con    = qname $ "(" ++ replicate (nelems - 1) ',' ++ ")"

    CompositeType fields
      -> (getter,
          \e -> caseTupleKnownNames_ present_bndrs e putter,
          TyTuple Boxed present_typs)
      where (present_bndrs, present_typs) = unzip present_fields
            (present_fields, getter, putter) = fieldsToSyntax defuser fields (Tuple . map (Var . UnQual))

    IfThenType condexpr typ
      -> (App (App (var "maybeHas") condexprsyn) getexp,
          \e -> caseMaybe_ e (checkConsistency_ (con "True")  condexprsyn . putexp)
                             (checkConsistency_ (con "False") condexprsyn $ App (var "return") (Tuple [])),
          maybeTy_ ty)
      where condexprsyn = fieldExprToSyntax defuser condexpr
            (getexp, putexp, ty) = typeToSyntax defuser typ
    
    IfThenElseType condexpr typt typf
      -> (If condexprsyn (fmap_ (con "Left") getexpt) (fmap_ (con "Right") getexpf),
          \e -> caseEither_ e (checkConsistency_ (con "True")  condexprsyn . putexpt)
                              (checkConsistency_ (con "False") condexprsyn . putexpf),
          eitherTy_ tyt tyf)
      where condexprsyn = fieldExprToSyntax defuser condexpr
            (getexpt, putexpt, tyt) = typeToSyntax defuser typt
            (getexpf, putexpf, tyf) = typeToSyntax defuser typf
    
    RepeatType (TyConType (TyCon "BYTE" [])) RepeatsUntilEnd
      -> (var "getRemainingLazyByteString",
          App $ var "putLazyByteString",
          LHE.TyCon (qname "ByteString"))
    
    RepeatType typ repeats -> case repeats of
        NumberOfTimes lenexpr
          -> (App (App (var "genericReplicateM") lenexprsyn) onegenexp,
              \e -> checkConsistency_ (App (var "genericLength") e) lenexprsyn $ mapM__ (reifyLambda oneputexp) e, -- TODO: check consistency
              TyList onety)
          where lenexprsyn = fieldExprToSyntax defuser lenexpr
        OptionallyAtEnd
          -> (App (App (var "maybeHasM") (App (App (var "fmap") (var "not")) (var "isEmpty"))) onegenexp,
              \e -> caseMaybe_ e oneputexp (App (var "return") (Tuple [])),
              maybeTy_ onety)
        RepeatsUntilEnd
          -> (App (var "getToEnd") onegenexp,
              mapM__ (reifyLambda oneputexp),
              TyList onety)
      where (onegenexp, oneputexp, onety) = typeToSyntax defuser typ

whyExcludedToSyntax _       IsReserved          = var "reservedDefault"
whyExcludedToSyntax defuser (IsPresenceFlag fn) = App (var "isJust") (Var $ UnQual $ defuser fn)
whyExcludedToSyntax defuser (IsSelectFlag fn)   = App (var "isLeft") (Var $ UnQual $ defuser fn)
whyExcludedToSyntax defuser (IsLength fn)       = App (var "genericLength") (Var $ UnQual $ defuser fn)

fieldExprToSyntax _       (LitE i) = Lit (Int $ fromIntegral i)
fieldExprToSyntax defuser (FieldE x) = Var $ UnQual $ defuser x -- TODO: this is blocking us using short names for non-stored fields, since we don't know what kind of name it will have
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


defuseFieldName record_name field_name = Ident $ toVarName record_name ++ '_':toVarName field_name
  where toVarName (c:s) = toLower c : s


maybeTy_ = TyApp (LHE.TyCon (qname "Maybe"))  
eitherTy_ ty1 ty2 = TyApp (TyApp (LHE.TyCon (qname "Either")) ty1) ty2

fmap_ efun efunctor = App (App (var "fmap") efun) efunctor
mapM__ ef exs = App (App (var "mapM_") ef) exs

reifyLambda oneputexp = Lambda noSrcLoc [PVar $ Ident "x"] (oneputexp $ var "x")

checkConsistency_ ehave ecomputed eresult
  = If (InfixApp ehave (QVarOp $ UnQual $ Symbol "/=") (Paren ecomputed)) (var "inconsistent") eresult

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

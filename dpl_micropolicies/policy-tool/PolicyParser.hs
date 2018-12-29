{-
 - Copyright © 2017-2018 The Charles Stark Draper Laboratory, Inc. and/or Dover Microsystems, Inc.
 - All rights reserved. 
 -
 - Use and disclosure subject to the following license. 
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 - 
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 - 
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 - LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 - OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 - WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 -}
{-# LANGUAGE TemplateHaskell #-}
module PolicyParser (polParse,moduleExists) where

import Data.Char
import Data.Text (unpack, pack, dropWhileEnd)
import Control.Monad.State.Lazy


import Text.Megaparsec

import Text.Megaparsec.Expr
import System.FilePath
import System.Directory

import CommonTypes
import ErrorMsg
import PolicyLexer
import Generator
import AST

type ModuleSym = ModuleDecl QSym
type SectSym   = SectDecl QSym
--type TagSetSym = TagSet QSym
type TagSym = Tag QSym
type PolicyExSym = PolicyEx QSym

moduleExists :: Options -> [FilePath] -> IO Bool
moduleExists opts qmn = doesFileExist $ (optModuleDir opts) </> (moduleFile qmn)

moduleFile :: [FilePath] -> FilePath
moduleFile qmn = joinPath qmn <.> "dpl"

polParse :: Options -> ModName -> IO (Either ErrMsg (ModuleDecl QSym))
polParse opts qmn =
  let file = (optModuleDir opts) </> (moduleFile qmn) in do
    putStrLn ("Reading file: " ++ file)
    contents <- readFile file
--    putStrLn ("Contents: " ++ contents)
    case (parse ( runStateT pFile spaceConsumer) file contents)  of
      Left err -> return $ Left $ parseErrorPretty err
--      Left err -> return $ Left $ show err      
      Right (mn,_) -> return $ Right mn

---------------------------------------------    File     --------------------------------------------

pFile :: DPLParser ModuleSym
pFile = do
  put spaceConsumer
  lift spaceConsumer
  mn <- pMod
  lift eof
  return mn

---------------------------------------------   Module    --------------------------------------------
                     
pMod :: DPLParser ModuleSym
pMod = ModuleDecl <$> getPosition <*> moduleDecl <*> (many pSect)
  where
    moduleDecl = (reserved "module" *> pQName <* colon) <?> "Module declaration"

---------------------------------------------   Sections    --------------------------------------------
pSect :: DPLParser SectSym
pSect =  (pImportSect <?> "Imports Section")
     <|> (pTypeSect <?> "Types Section")
     <|> (pTagSect <?> "Metadata Section")
     <|> (pGroupSect <?> "Opgroups Section")
     <|> (pPolicySect <?> "Policy Section")
     <|> (pReqSect <?> "Requires Section")

---------------------------------------------   Import     --------------------------------------------
pImportSect :: DPLParser SectSym
pImportSect = do
  put lineConsumer
  section <- nonIndented (indentBlock p)
  put spaceConsumer
  return section
    where
      p = do
        reserved "import" <* colon;
        return (indMany Nothing (return.Imports) pImport)

pImport :: DPLParser (ImportDecl QSym)
pImport = (ImportDecl <$> getPosition <*> pQName) <?> "import statement"

---------------------------------------------   Type       --------------------------------------------
pTypeSect :: DPLParser SectSym
pTypeSect = do
  put lineConsumer
  section <- nonIndented (indentBlock p)
  put spaceConsumer
  return section
    where
      p = do
        reserved "type" <* colon
        return (indMany Nothing (return.Types) pTypeDecl)

pTypeDecl :: DPLParser (TypeDecl QSym)
pTypeDecl =
  (TypeDecl <$> getPosition
            <*> (reserved "data" *> pTypeQName <* assignEq)
            <*> pTagDataType)
  <?> "Data type declaration"

pTagDataType :: DPLParser TagDataType
pTagDataType = TDTInt <$> getPosition <*>
  (reserved "Int" *> (try (Just . fromIntegral <$> parens integer) <|> return Nothing))

---------------------------------------------   Tag        --------------------------------------------
pTagSect :: DPLParser SectSym
pTagSect = do
  put lineConsumer
  section <- nonIndented (indentBlock p)
  put spaceConsumer
  return section
    where
      p = do
        reserved "metadata" <* colon
        return (indMany Nothing (return.Tags) pTagDecl)


pTagDecl :: DPLParser (TagDecl QSym)
pTagDecl = (TagDecl <$> getPosition <*> pTagQName <*> (many pTypeQName))
       <?> "tag declaration"

pTFTag :: DPLParser (TagField QSym)
pTFTag = TFTag <$> getPosition <*> pTag

pTFVar :: DPLParser (TagField QSym)
pTFVar = TFVar <$> getPosition <*> pVarQName

pTFNew :: DPLParser (TagField QSym)
pTFNew = TFNew <$> getPosition <* reserved "new"

pTFAny :: DPLParser (TagField QSym)
pTFAny = TFAny <$> getPosition <* tok "_"

pTagEx :: DPLParser (TagEx QSym)
pTagEx = do
  srcPos <- getPosition
  prefix <- (option ' ' pPlusMinus) <?> "Optional metadata prefix"
  tag <- pTag
  case prefix of
    ' ' -> return $ TagEx srcPos tag
    '+' -> return $ TagPlusEx srcPos tag
    '-' -> return $ TagMinusEx srcPos tag
    _   -> codingError "pTagEx impossible"

pTag :: DPLParser TagSym
pTag = (parens pComplexTag <|> pSimpleTag) <?> "metadata"

pComplexTag :: DPLParser (Tag QSym)
pComplexTag = Tag <$> getPosition <*> pTagQName <*> (many pField)

pSimpleTag :: DPLParser (Tag QSym)
pSimpleTag = Tag <$> getPosition <*> pTagQName <*> (return [])

pField :: DPLParser (TagField QSym)
pField = ((try pTFNew) <|> (try pTFVar) <|> pTFAny <|> pTFTag) <?> "metadata argument"

pPlusMinus :: DPLParser Char
pPlusMinus = char '+' <|> char '-'  

---------------------------------------------   Set        --------------------------------------------
pBoundGroupPat :: DPLParser (BoundGroupPat QSym)
pBoundGroupPat =
  (BoundGroupPat <$> getPosition <*> pVarQName <* matchEq <*> pTagSetPat)
  <?> "metadata pattern match \"metadataName == metadataPattern\""

pRuleResult :: DPLParser (RuleResult QSym)
pRuleResult = ((pRRFail <|> pRRUpdate) <* lift spaceConsumer) <?> "rule result"
  where
    pRRFail :: DPLParser (RuleResult QSym)
    pRRFail = RRFail <$> getPosition <* reserved "fail" <*> stringLiteral

    stringLiteral :: DPLParser String
    stringLiteral = char '"' >> manyTill anyChar (char '"')

    pRRUpdate :: DPLParser (RuleResult QSym)
    pRRUpdate = RRUpdate <$> getPosition <*> sepBy pBoundEx comma

    pBoundEx :: DPLParser (BoundGroupEx QSym)
    pBoundEx = (BoundGroupEx <$> getPosition <*> pVarQName <* assignEq <*> pTagSetEx)
      <?> "tag set update expression \"metadataName = metadataSet\""


-- Tag Set Patterns
pTagSetPat :: DPLParser (TagSetPat QSym)
pTagSetPat =
      pAnyPat
--  <|> try pAtPat -- comes before pVarPat to avoid partial parse
--  <|> pVarPat
  <|> pExactPat
  <|> pAtLeastPat

pAnyPat :: DPLParser (TagSetPat QSym)
pAnyPat = (TSPAny <$> getPosition <* tok "_") <?> "wildcard pattern \"_\""

{- Removing from the language
pVarPat :: DPLParser (TagSetPat QSym)
pVarPat = TSPVar <$> getPosition <*> pVarQName

pAtPat :: DPLParser (TagSetPat QSym)
pAtPat = TSPVarSet <$> getPosition <*> pVarQName <* symbol "@" <*> pTagSetPat

-}

pExactPat :: DPLParser (TagSetPat QSym)
pExactPat = (TSPExact <$> getPosition <*> braces (sepBy pTag comma))
  <?> "exact set pattern \"{...}\""

pAtLeastPat :: DPLParser (TagSetPat QSym)
pAtLeastPat = (TSPAtLeast <$> getPosition <*> brackets (sepBy pTagEx comma))
  <?> "partial set pattern \"[...]\""

-- Tag Set Expressions
pTagSetEx :: DPLParser (TagSetEx QSym)
pTagSetEx = (makeExprParser tagSetExTerm tagSetExOps)
        <?> "Metadata set expression"

tagSetExOps :: [[Operator DPLParser (TagSetEx QSym)]]
tagSetExOps =
  [  [ Postfix pModify ]
  ,  [ InfixL (TSEUnion <$> getPosition <* symbol "\\/")
     , InfixL (TSEIntersect <$> getPosition <* symbol "/\\")
     ]
  ]

pModify :: DPLParser (TagSetEx QSym -> TagSetEx QSym)
pModify = do
  loc <- getPosition
  updates <- brackets (sepBy pTagEx comma)
  return $ \tse -> TSEModify loc tse updates

tagSetExTerm :: DPLParser (TagSetEx QSym)
tagSetExTerm = pVarEx <|> pExactEx

pVarEx :: DPLParser (TagSetEx QSym)
pVarEx  = TSEVar <$> getPosition <*> pVarQName 

pExactEx :: DPLParser (TagSetEx QSym)
pExactEx  = TSEExact <$> getPosition <*> braces (sepBy pTag comma)

---------------------------------------------   Policy     --------------------------------------------

pPolicySect :: DPLParser SectSym
pPolicySect = Policies <$> (reserved "policy" *> colon *> many pPolicyDecl)

pPolicyDecl :: DPLParser (PolicyDecl QSym)
pPolicyDecl = (PolicyDecl <$> getPosition <*> locality
                          <*> pPolicyQN <* assignEq <*> pPolicyExpr)
           <?> "policy declaration \"policyName = policyExpression\""
  where
    locality :: DPLParser PolicyLocality
    locality = (reserved "global" >> return PLGlobal)
           <|> (return PLLocal)

polOps :: [[Operator DPLParser PolicyExSym]]
polOps =
  [ [ InfixL (PECompModule    <$> getPosition <* symbol "&")]
  , [ InfixL (PECompExclusive <$> getPosition <* symbol "|")]
  , [ InfixL (PECompPriority  <$> getPosition <* symbol "^")]
  ]

polTerm :: DPLParser PolicyExSym
polTerm  =  (parens pPolicyExpr)
        <|> pPERule
        <|> pPENoChecks
        <|> pPEVar



pPolicyExpr :: DPLParser (PolicyEx QSym)
pPolicyExpr = (makeExprParser polTerm polOps) <?> "policy expression"

pPENoChecks :: DPLParser (PolicyEx QSym)
pPENoChecks = reserved "__NO_CHECKS" >> PENoChecks <$> getPosition

pPEVar :: DPLParser (PolicyEx QSym)
pPEVar = PEVar <$> getPosition <*> pPolicyQN

pPERule :: DPLParser (PolicyEx QSym)
pPERule = (PERule <$> getPosition <*> pRuleClause) <?> "policy rule"

pRuleClause :: DPLParser (RuleClause QSym)
pRuleClause = do
  srcPos <- getPosition
  name <- try nameParen
  pInputs <- sepBy pBoundGroupPat comma
  _ <- tok "->"
  pOutputs <- pRuleResult
  _ <- tok ")"
  return (RuleClause srcPos name pInputs pOutputs)
  where
    -- here we separate out parsing up the first paren to localize the use of
    -- "try" from the caller of pRuleClause to inside of it, allowing for better
    -- errors.
    nameParen = do
      name <- pGroupQN
      _ <- tok "("
      return name

---------------------------------------------    Reqs      --------------------------------------------

pReqSect :: DPLParser SectSym
pReqSect = do
  put lineConsumer
  section <- nonIndented (indentBlock p)
  put spaceConsumer
  return section
  where
    p = do
      reserved "require" <* colon;
      return (indMany Nothing (return.Require) pReqDecl)

pReqDecl :: DPLParser (RequireDecl QSym)
pReqDecl =  pReq -- maybe some more requires key words later

pReq :: DPLParser (RequireDecl QSym)
pReq = Init <$> getPosition <*> (reserved "init" *> pEntName) <*> pReqSet

pEntName :: DPLParser [String]
pEntName = lexeme $ sepBy1 entity dot

{- Obsolete
-- parse everything including whitespace upto '{', then strip trailing whitespace
reqQName :: DPLParser String
reqQName = do
  lexeme $ do
    xp <- someTill anyChar (lookAhead $ char '{')
    return $ unpack . dropWhileEnd (==' ') . pack $ xp
-}

pReqSet :: DPLParser (InitSet QSym)
pReqSet = ISExact <$> getPosition <*> braces (sepBy pTag comma)


---------------------------------------------   Group      --------------------------------------------

pGroupSect :: DPLParser SectSym
pGroupSect = do
  put lineConsumer
  section <- nonIndented (indentBlock p)
  put spaceConsumer
  return section
    where
      p = do
        reserved "group" <* colon;
        return (indMany Nothing (return.Groups) pGroupDecl)

pGroupDecl :: DPLParser (GroupDecl [ISA] QSym)
pGroupDecl = indentBlock p
  where
    p = do
      og <- pGroupPat
      return (indentSome (mkAsm og) pIsaDecl)
    mkAsm (GroupDecl srcPos name ins outs _) asms = GroupDecl srcPos name ins outs asms

pGroupPat :: DPLParser (GroupDecl () QSym)
pGroupPat = do
  srcPos <- getPosition
  name <- pGroupQN
  _ <- tok "("
  ins <- sepBy pGroupParam comma
  _ <-tok "->"
  outs <- sepBy pGroupParam comma
  _ <- tok ")"
  return (GroupDecl srcPos name ins outs ())

pGroupParam :: DPLParser (GroupParam QSym)
pGroupParam = GroupParam <$> getPosition <*> pTS <* colon <*> pVarQName

pIsaDecl :: DPLParser ISA    
pIsaDecl = do
  srcPos <- getPosition
  inst <- identifierWithDots
  pOpSpecs <- optional (sepBy1 pOpSpec comma) -- require 1 to have optional fail properly
  return (Asm srcPos inst pOpSpecs)

--    AnyOp | Const Int | Reg RF
pOpSpec :: DPLParser OpSpec
pOpSpec = pAnyOp <|> pImm <|> pReg
  where
    pAnyOp = AnyOp <$ tok "*"
    pImm = Const <$> (try hexadecimal <|> integer)
    pReg = Reg <$> pRF

{-
  -- RD | RS1 | RS2 | RS3 | Imm | Off | Csr | Mem
pTagSpec' :: DPLParser (TagSpec, Param)
pTagSpec' = do
  tagSpec <- pTS
  colon
  param <- identifier
  return (tagSpec, param)
-}


pTS :: DPLParser TagSpec  
pTS =
  RD  <$ symbol "RD"  <|>
  RS1 <$ symbol "RS1" <|>
  RS2 <$ symbol "RS2" <|>
  RS3 <$ symbol "RS3" <|>
  Imm <$ symbol "IMM" <|>
  Off <$ symbol "OFF" <|>
  Csr <$ symbol "CSR" <|>
  Mem <$ symbol "MEM"


pRF :: DPLParser RF
pRF = 
        X0 <$ symbol "x0" <|>
        X1 <$ symbol "x1" <|>
        X2 <$ symbol "x2" <|>
        X3 <$ symbol "x3" <|>
        X4 <$ symbol "x4" <|>
        X5 <$ symbol "x5" <|>
        X6 <$ symbol "x6" <|>
        X7 <$ symbol "x7" <|>
        X8 <$ symbol "x8" <|>
        X9 <$ symbol "x9" <|>
        X10 <$ symbol "x10" <|>
        X11 <$ symbol "x11" <|>
        X12 <$ symbol "x12" <|>
        X13 <$ symbol "x13" <|>
        X14 <$ symbol "x14" <|>
        X15 <$ symbol "x15" <|>
        X16 <$ symbol "x16" <|>
        X17 <$ symbol "x17" <|>
        X18 <$ symbol "x18" <|>
        X19 <$ symbol "x19" <|>
        X20 <$ symbol "x20" <|>
        X21 <$ symbol "x21" <|>
        X22 <$ symbol "x22" <|>
        X23 <$ symbol "x23" <|>
        X24 <$ symbol "x24" <|>
        X25 <$ symbol "x25" <|>
        X26 <$ symbol "x26" <|>
        X27 <$ symbol "x27" <|>
        X28 <$ symbol "x28" <|>
        X29 <$ symbol "x29" <|>
        X30 <$ symbol "x30" <|>
        X31 <$ symbol "x31"

---------------------------------------------   helpers     --------------------------------------------

reserved :: String -> DPLParser ()
reserved w = lexeme $ string w *> notFollowedBy alphaNumChar

rws :: [String] -- list of reserved words
rws = ["module","import","tag","metadata","type","data"
      ,"Int","policy","group","bind","isa", "new", "fail"
      ,"if", "then", "else","require", "init", "__NO_CHECKS"]

-- bool is whether to allow dots in name
idGen :: Bool -> DPLParser String
idGen dots = (lexeme . try) (p >>= check)
  where
    idChars = if dots then "_-." else "_-"
    p       = (:) <$> letterChar <*> many (try alphaNumChar <|> oneOf idChars)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

identifier :: DPLParser String
identifier = idGen False

identifierWithDots :: DPLParser String
identifierWithDots = idGen True

entity :: DPLParser String
entity = lexeme $ many ((try alphaNumChar) <|> oneOf "_-")

pQName :: DPLParser [String]
pQName = lexeme $ sepBy1 identifier dot

pPolicyQN :: DPLParser QSym
pPolicyQN = QPolicy <$> pQName

pGroupQN :: DPLParser QSym
pGroupQN = QGroup <$> pQName

pTypeQName :: DPLParser QSym
pTypeQName = QType <$> pQName

pTagQName :: DPLParser QSym
pTagQName = tvName <$> pQName

pVarQName :: DPLParser QSym
pVarQName = tvName <$> pQName


tvName :: [String] -> QName [String]
tvName qns | isTag qns = QTag qns
tvName qns = QVar qns

isTag :: [String] -> Bool
isTag [] = False
isTag [""] = False
isTag qns = isUpper $ head $ last qns

-- for readability identifiers should be lower case
--pId = pVarId

tok :: String -> DPLParser String
tok t = symbol t

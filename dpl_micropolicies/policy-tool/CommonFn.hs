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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CommonFn where

import Data.Either

import Data.List
import Data.Char
import Data.String
import Numeric

import AST
-- -- import ErrorMsg

moduleSects :: ModuleDecl t -> [SectDecl t]
moduleSects (ModuleDecl _ _ s) = s

sect :: forall a b (t :: * -> *).
        Foldable t =>
        (a -> [b]) -> t a -> [b]
sect fn sects = concatMap fn sects

importS :: SectDecl t -> [ImportDecl t]
importS (Imports s) = s
importS _ = []

typeS :: SectDecl t -> [TypeDecl t]
typeS (Types s) = s
typeS _ = []

tagS :: SectDecl t -> [TagDecl t]
tagS (Tags s) = s
tagS _ = []

policieS :: SectDecl t -> [PolicyDecl t]
policieS (Policies s) = s
policieS _ = []

groupS :: SectDecl t -> [GroupDecl [ISA] t]
groupS (Groups s) = s
groupS _ = []

requireS :: SectDecl t -> [RequireDecl t]
requireS (Require s) = s
requireS _ = []

checkTag :: QName n -> Bool
checkTag (QTag _) = True
checkTag _ = False

checkVar :: QName n -> Bool
checkVar (QVar _) = True
checkVar _ = False

requireSet :: RequireDecl n -> InitSet n
requireSet (Init _ _ is) = is




hasErrors :: [a] -> Bool
hasErrors = not . null

getErrors :: forall a b. [Either a b] -> [a]
getErrors = lefts

nubSort :: forall a. (Eq a, Ord a) => [a] -> [a]
nubSort = nub.sort

nubWith :: forall a a1. Eq a1 => (a -> a1) -> [a] -> [a]
nubWith fn = nubBy (\a b -> fn a == fn b)

sortWith :: forall a a1. Ord a1 => (a -> a1) -> [a] -> [a]
sortWith fn = sortBy (\a b -> compare (fn a) (fn b))

groupWith :: forall a a1. Eq a1 => (a -> a1) -> [a] -> [[a]]
groupWith fn = groupBy (\a b -> fn a == fn b)

byGroup :: forall t. QName t -> Bool
byGroup (QGroup _) = True
byGroup _ = False

qualifyQSym :: ModName -> QSym -> QSym
qualifyQSym mn qs = fmap (mn++) qs

qualifiers :: forall a. [a] -> [a]
qualifiers = init

unqualified :: forall t. [t] -> [t]
unqualified qn = [last qn]

parseDotName :: [Char] -> [String]
parseDotName = words . map toWords
  where
    toWords '.' = ' '
    toWords c = c

parseDashName :: [Char] -> [String]
parseDashName = words . map toWords
  where
    toWords '-' = ' '
    toWords c = c

typeName :: QSym -> String
typeName = reqName . map toLower . intercalate "_" . qName

tagName :: QSym -> String
tagName = reqName . intercalate "_" . qName

tagString :: QSym -> String
tagString = intercalate "." . qName

reqFnName :: String -> String
reqFnName n = "pex_init" ++ reqName n ++ "_tag"

reqName :: String -> String
reqName n = map rep n
  where
    rep '/' = '_'
    rep '-' = '_'
    rep c = c

handleName :: QSym -> [Char]
handleName = ("TAGTYPE_" ++) . tagName

catName :: QSym -> String
catName = ("CATEGORY_" ++) . tagName

rankName :: [String] -> String
rankName = ("RANK_" ++) . intercalate "_"

groupPrefix :: QSym -> QSym
groupPrefix  = fmap ("og":)

importDeclName :: forall t. ImportDecl t -> ModName
importDeclName (ImportDecl _ qn) = qn

{-
tagDeclName :: forall t. TagDecl t -> t
tagDeclName (TagDecl _ qn _) = qn


policyDeclName (PolicyDecl _ qn _) = qn
groupDeclName (GroupDecl _ qn _ _ _) = qn
-}

qName :: forall t. QName t -> t
qName (QType ns) = ns
qName (QTag ns) = ns
qName (QVar ns) = ns
qName (QPolicy ns) = ns
qName (QGroup ns) = ns

dotName :: [[Char]] -> [Char]
dotName = intercalate "."

policyName :: [[Char]] -> [Char]
policyName = intercalate "."

policyDotName :: PolicyDecl QSym -> String
policyDotName = dotName . qName . qsym

moduleQName :: forall t. ModuleDecl t -> ModName
moduleQName (ModuleDecl _ mn _) = mn

moduleDotName :: forall t. ModuleDecl t -> [Char]
moduleDotName = dotName . moduleQName

moduleName :: forall t. ModuleDecl t -> String
moduleName = last . moduleQName

moduleDirs :: forall t. ModuleDecl t -> [String]
moduleDirs = qualifiers . moduleQName


unqualSymStr :: QSym -> String
unqualSymStr qSym = last $ qName qSym

qualSymStr :: QSym -> String
qualSymStr qSym = dotName $ qName qSym

modSymStr :: QSym -> String
modSymStr qSym = dotName $ qName qSym

eitherErrors :: forall a b. [Either a b] -> [a]
eitherErrors = lefts

modName :: QSym -> ModName
modName = qualifiers . qName


unqualQSym :: QSym -> QSym
unqualQSym = fmap unqualified

isQualified :: QSym -> Bool
isQualified qs = length (qName qs) /= 1


mkTag :: [Char] -> [Char]
mkTag [] = []
mkTag (c:cs) = toUpper c : cs

mkVar :: [Char] -> [Char]
mkVar [] = []
mkVar (c:cs) = toLower c : cs

tab :: Int -> [Char]
tab n = replicate (n*4) ' '

mkComma :: [String] -> String
mkComma = intercalate ", "

mkCurly :: String -> String
mkCurly eq = "{" ++ eq ++ "}"

mkBracket :: String -> String
mkBracket eq = "[" ++ eq ++ "]"

--mkParen [] = ""
mkParen :: forall a. Data.String.IsString [a] => [a] -> [a]
mkParen eq = "(" ++ eq ++ ")"

pad :: forall (t :: * -> *) a. Foldable t => Int -> t a -> [Char]
pad n l = replicate (n-length l) ' '

hex :: (Show n, Integral n) => n -> String
hex n = "0x" ++ (showHex n) ""

dash :: String
dash = replicate 32 '-'

camelToUnder :: [Char] -> [Char]
camelToUnder [] = []
camelToUnder (c:cs) | isUpper c = '_':(toLower c):(camelToUnder cs)
camelToUnder (c:cs) = c:(camelToUnder cs)

qualifiedShowRule :: PolicyDecl QSym -> RuleClause QSym -> String
qualifiedShowRule p rc = policyDotName p ++ ":" ++ compactShowRule rc

-- Compact rule pattern display for debugging
compactShowRule :: RuleClause QSym -> String
compactShowRule (RuleClause _ og pats _) =
  (last $ qName og) ++ "<"
     ++ (intercalate ", " $ map compactShowPat pats) ++ ">"

compactShowPat :: BoundGroupPat QSym -> String
compactShowPat (BoundGroupPat _ nm tsp) =
  (last $ qName nm) ++ "=" ++ compactShowTagSetPat tsp

compactShowTagSetPat :: TagSetPat QSym -> String
compactShowTagSetPat (TSPAny _) = "_"
--compactShowTagSetPat (TSPVar _ nm) = last $ qName nm
compactShowTagSetPat (TSPExact _ tgs) =
  "{" ++ (intercalate ", " $ map compactShowTag tgs) ++ "}"
compactShowTagSetPat (TSPAtLeast _ tes) = 
  "{" ++ (intercalate ", " $ map compactShowTagEx tes) ++ "}"
--compactShowTagSetPat (TSPVarSet _ nm tsp) = (last $ qName nm) ++ "@" ++ compactShowTagSetPat tsp
  
compactShowTag :: Tag QSym -> String
compactShowTag (Tag _ nm args) =
  intercalate " " $ (last $ qName nm) : map compactShowTagField args

compactShowTagField :: TagField QSym -> String
compactShowTagField (TFTag _ t) = compactShowTag t
compactShowTagField (TFVar _ n) = last $ qName n
compactShowTagField (TFNew _) = "new"
compactShowTagField (TFAny _) = "_"

compactShowTagEx :: TagEx QSym -> String
compactShowTagEx (TagEx _ t) = compactShowTag t
compactShowTagEx (TagPlusEx _ t) = "+" ++ compactShowTag t
compactShowTagEx (TagMinusEx _ t) = "-" ++ compactShowTag t

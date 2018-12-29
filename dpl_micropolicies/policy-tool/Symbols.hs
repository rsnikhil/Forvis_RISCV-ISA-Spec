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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Symbols where

import AST
import CommonFn
import Data.Either
import Data.List (intercalate, nub)
import Data.Maybe
import ErrorMsg

-- The Symbol module creates two data structures with symbols for the parsed dpl
-- files and provides a set of functions for querrying
-- Symbols Data Structures --
-- Module Symbols - An association list of full symbol tables for each parsed dpl module
-- Symbols could be a superset of what is used by a given policy
type ModSymbols = [(ModName, SymbolTable QSym)]

-- UsedSymbols gives the actual list of Symbols used by the policy being processed
-- This type is actually constructed in the Validate module see: validateMain Fn
type UsedSymbols = [(ModName, QSym)]

-- SymbolTable is split up by type of symbol
data SymbolTable n = SymbolTable
  { modDecl :: ModuleDecl QSym
  , importSyms :: [ModName]
  , typeSyms :: [(QSym, TypeDecl n)]
  , tagSyms :: [(QSym, TagDecl n)]
  , policySyms :: [(QSym, PolicyDecl n)]
  , groupSyms :: [(QSym, GroupDecl [ISA] n)]
  , requires :: [RequireDecl n]
  } deriving (Show)

-- Symbols API --
-- take a list of parsed module declarations and produce the symbol tables
buildSymbolTables :: [ModuleDecl QSym] -> Either [ErrMsg] ModSymbols
buildSymbolTables mods = symbolTables mods

-- Use these functions when the thing you are looking for may not exist
-- Lookup Fns return Either a Decl or an error message
type LookupResult a = Either ErrMsg (ModName, a)

-- Lookup Fns take
--   ModSymbols - the list of all module symbol tables
--   ModName - the module name where the qsym is refrenced from (to calculate the include graph)
--   QSym - the symbol to look for, may be qualified or unqualified
--
-- The lookup will traverse the include graph locating all symbols with the specified name.
-- If the qsym is unqualified only one match must be found otherwise its an error
-- If the qsym is qualified multiple matches in the include graph are allowd and the one matching the
--    qualified name is returned
lookupType :: ModSymbols -> ModName -> QSym -> LookupResult (TypeDecl QSym)
lookupType ms mn qs = lookupQSym ms typeSyms mn qs

lookupTag :: ModSymbols -> ModName -> QSym -> LookupResult (TagDecl QSym)
lookupTag ms mn qs = lookupQSym ms tagSyms mn qs

lookupPolicy :: ModSymbols -> ModName -> QSym -> LookupResult (PolicyDecl QSym)
lookupPolicy ms mn qs = lookupQSym ms policySyms mn qs

lookupGroup ::
     ModSymbols -> ModName -> QSym -> LookupResult (GroupDecl [ISA] QSym)
lookupGroup ms mn qs = lookupQSym ms groupSyms mn qs

-- Use these functions when you are sure the thing you are looking for exists
-- andthe symbol has the correct type.
-- If it doesn't we call error with a message to terminate the program
getType :: ModSymbols -> ModName -> QSym -> (ModName, TypeDecl QSym)
getType ms mn qs = mayErr "Failed Type Lookup" $ lookupQSym ms typeSyms mn qs

getTag :: ModSymbols -> ModName -> QSym -> (ModName, TagDecl QSym)
getTag ms mn qs = mayErr "Failed Tag Lookup" $ lookupQSym ms tagSyms mn qs

getPolicy :: ModSymbols -> ModName -> QSym -> (ModName, PolicyDecl QSym)
getPolicy ms mn qs =
  mayErr "Failed Policy Lookup" $ lookupQSym ms policySyms mn qs

getGroup :: ModSymbols -> ModName -> QSym -> (ModName, GroupDecl [ISA] QSym)
getGroup ms mn qs = mayErr "Failed Group Lookup" $ lookupQSym ms groupSyms mn qs

-- Use these Fns to filter the list of UsedSymbols down to a particular type
-- e.g. Tags, Types, Groups, Policies
usedTypes :: UsedSymbols -> UsedSymbols
usedTypes us = filter isType us
  where
    isType (_, QType _) = True
    isType _ = False

usedTags :: UsedSymbols -> UsedSymbols
usedTags us = filter isTag us
  where
    isTag (_, QTag _) = True
    isTag _ = False

usedGroups :: UsedSymbols -> UsedSymbols
usedGroups us = filter isGroup us
  where
    isGroup (_, QGroup _) = True
    isGroup _ = False

usedRequires :: ModSymbols -> UsedSymbols -> [(ModName, RequireDecl QSym)]
usedRequires ms us = concatMap reqs mods
  where
    mods :: [ModName]
    mods = nubSort $ map fst us
    reqs mn =
      case lookup mn ms of
        Nothing -> []
        Just st -> zip (repeat mn) (requires st)

-- Use these functions when you are sure the thing you are looking for exists
-- andthe symbol has the correct type.
-- If it doesn't we call error with a message to terminate the program
typeDecl :: ModSymbols -> (ModName, QSym) -> (ModName, TypeDecl QSym)
typeDecl ms (mn, qt@(QType _)) = getType ms mn qt

tagDecl :: ModSymbols -> (ModName, QSym) -> (ModName, TagDecl QSym)
tagDecl ms (mn, qt@(QTag _)) = getTag ms mn qt

groupDecl :: ModSymbols -> (ModName, QSym) -> (ModName, GroupDecl [ISA] QSym)
groupDecl ms (mn, qt@(QGroup _)) = getGroup ms mn qt

resolveQSym :: ModSymbols -> ModName -> QSym -> QSym
resolveQSym ms startMod qs = qualifyQSym (moduleForQSym ms startMod qs) qs

-- Lookup a qsym to find the module its declared in
moduleForQSym :: ModSymbols -> ModName -> QSym -> ModName
moduleForQSym ms mn qs@(QType _) =
  fst $ mayErr "Failed module lookup" $ lookupQSym ms typeSyms mn qs
moduleForQSym ms mn qs@(QTag _) =
  fst $ mayErr "Failed module lookup" $ lookupQSym ms tagSyms mn qs
moduleForQSym ms mn qs@(QPolicy _) =
  fst $ mayErr "Failed module lookup" $ lookupQSym ms policySyms mn qs
moduleForQSym ms mn qs@(QGroup _) =
  fst $ mayErr "Failed module lookup" $ lookupQSym ms groupSyms mn qs
moduleForQSym ms mn qs@(QVar _) = mn
  -- look up a qsym by finding the ST for its module and then using fn to select which Syms to lu

-- Internal helper functions....
lookupQSym ::
     Eq a
  => ModSymbols
  -> (SymbolTable QSym -> [(QSym, a)])
  -> ModName
  -> QSym
  -> LookupResult a
lookupQSym sts fn mn qs
  | isQualified qs =
    case lookup (modName qs) $ catMaybes $ lookupMod sts fn qs mn of
      Nothing -> Left $ "Unable to find " ++ (qualSymStr qs)
      Just a -> Right (modName qs, a)
lookupQSym sts fn mn qs =
  case nub $ catMaybes $ lookupMod sts fn qs mn of
    [] ->
      Left $
      "Unable to find symbol " ++
      (unqualSymStr qs) ++
      " in modules: " ++
      (intercalate ", " $ map (dotName . fst) sts) ++
      " reached from: " ++ (dotName mn)
    res:[] -> Right res
    reses ->
      Left $
      "Multiple symbols found: " ++
      (unqualSymStr qs) ++
      " reached from: " ++
      (dotName mn) ++
      " from modules: " ++ (intercalate ", " $ map (dotName . fst) reses)

type PartialResult a = Maybe (ModName, a)

lookupMod ::
     ModSymbols
  -> (SymbolTable QSym -> [(QSym, a)])
  -> QSym
  -> ModName
  -> [PartialResult a]
lookupMod sts fn qs mn =
  case lookup mn sts of
    Nothing -> []
    Just st -> lookupSym sts st fn mn qs

-- look up a qsym in a module using fn to select which type of Syms to lu
lookupSym ::
     ModSymbols
  -> SymbolTable QSym
  -> (SymbolTable QSym -> [(QSym, a)])
  -> ModName
  -> QSym
  -> [PartialResult a]
lookupSym sts st fn mn qs =
  case lookup qs $ fn st of
    Nothing -> importedSyms fn
    Just a -> [Just (mn, a)] ++ importedSyms fn
  where
    imports :: [ModName]
    imports = importSyms st
    importedSyms :: (SymbolTable QSym -> [(QSym, a)]) -> [PartialResult a]
    importedSyms fn = concatMap (lookupMod sts fn qs) imports

-- This computes all the tags declared in the "tags" section of a module.  In
-- the future world where we allow modules to examine but not modify tags from
-- other modules, it will include tags that the present module can't modify.
moduleTags :: ModSymbols -> UsedSymbols -> ModName -> [TagDecl QSym]
moduleTags ms allSyms mn = map toTagDecl $ filter byTagDecl allSyms
  where
    byTagDecl :: (ModName, QSym) -> Bool
    byTagDecl (mn', QTag _)
      | mn == mn' = True
    byTagDecl _ = False
    toTagDecl :: (ModName, QSym) -> TagDecl QSym
    toTagDecl (modN, qs) = snd $ getTag ms modN qs

{-
-- Returns all type declarations
allTypes :: ModSymbols -> [TypeDecl QSym]
allTypes ms = concatMap (\(_,st) -> map snd $ typeSyms st) ms

allTags :: ModSymbols -> [TagDecl QSym]
allTags ms = map snd tags
  where
    tags = concatMap (tagSyms . snd) ms

--lookupSym sts fn qpn mn | trace ("lu: " ++ (dotName mn) ++ (show qpn) ++ " : " ++ (tmp $ lookup mn sts)) True = lookup mn sts >>= lookup pn . fn
--lookupSym sts fn qpn mn = lookup mn sts >>= lookup pn . fn
--  where
--    pn = unqualQSym qpn
  -}
-- create the set of symbol tables for an include graph
symbolTables :: [ModuleDecl QSym] -> Either [ErrMsg] ModSymbols
symbolTables mods = Right $ fmap symbolTable mods

-- create symbol table for a module
symbolTable :: ModuleDecl QSym -> (ModName, SymbolTable QSym)
symbolTable m@(ModuleDecl _ mn sects) = (mn, mkST)
  where
    mkST =
      SymbolTable
        { modDecl = m
        , importSyms = map importDeclName (sect importS sects)
        , typeSyms = map (declareSym mn) (sect typeS sects)
        , tagSyms = map (declareSym mn) (sect tagS sects)
        , policySyms = map (declareSym mn) (sect policieS sects)
        , groupSyms = map (declareSym mn) (sect groupS sects)
        , requires = (sect requireS sects)
        }
    declareSym :: Symbol s => ModName -> s -> (QSym, s)
    declareSym _ s = (unqualQSym $ qsym s, s)
{-
-- check if the symbol table has errors
symbolTableErrors :: SymbolTable ErrQSym -> [ErrMsg]
symbolTableErrors = lefts . extractST

rightST :: (ModName, SymbolTable ErrQSym) -> (ModName, SymbolTable QSym)
rightST (mn,st) = (mn, st')
  where
    st'  = st { typeSyms      = map rightSym $ typeSyms st
              , tagSyms       = map rightSym $ tagSyms st
              , policySyms    = map rightSym $ policySyms st
              , groupSyms     = map rightSym $ groupSyms st
              , requires         = map rightInit $ requires st 
              }
    rightInit = fmap toRight
    rightSym (s, t) = (s, fmap toRight t)
    toRight (Right qn) = qn
    toRight (Left _) = unexpectedError

  
extractST :: SymbolTable ErrQSym -> [ErrQSym]
extractST st = (getErrs $ tagSyms st) ++
                  (getErrs $ policySyms st) ++
                  (getErrs $ groupSyms st) ++
                  (reqErrs $ requires st)
  where
    getErrs :: Foldable f => [(a, f (Either b b1))] -> [(Either b b1)]
    getErrs  = concatMap (foldr (:) [] . snd)
    reqErrs :: Foldable f => [f (Either b b1)] -> [(Either b b1)]
    reqErrs  = concatMap (foldr (:) [])
  
qualifyAllST :: ModSymbols -> Either [ErrMsg] [(ModName, SymbolTable ErrQSym)]
qualifyAllST sts = Right $ map (qualifyST sts) sts

-- qualify all the symbols in the symbol table entries
qualifyST :: ModSymbols -> (ModName, SymbolTable QSym) -> (ModName, SymbolTable ErrQSym)
qualifyST sts (mn,st) = (mn, st')
  where
    st'  = st { typeSyms      = map qualSym $ typeSyms st
              , tagSyms       = map qualSym $ tagSyms st
              , policySyms    = map qualSym $ policySyms st
              , groupSyms     = map qualSym $ groupSyms st
              , requires      = map qualInit $ requires st 
              }
    qualSym (s, t) = (s, fmap (qualifySym sts) t)
    qualInit t = fmap (qualifySym sts) t

-- figure out the module where a sym is declared and qualify the sym with
--  the module name. Symbols that are explicitly qualified are checked to be valid
--  and passed along unchanged. Syms that have multiple declarations are errors.
--  Variables don't get qualified.
qualifySym :: ModSymbols -> QSym -> Either ErrMsg QSym
-- don't qualify vars
qualifySym _ qs@(QVar _) = Right qs
-- handle case for explicit qualified symbol
qualifySym sts qs | isQualified qs =
                        let mn = modName qs in
                        case lookup mn sts of
                          Just st -> case resolveSym qs st of
                            Just sym -> Right $ qualifyName mn $ sym
                            Nothing -> Left $ "Symbol " ++ (unqualSymStr qs) ++ " not in module " ++ (dotName mn)
                          Nothing -> Left $ "Unknown module " ++ (dotName mn)
-- look up unqual symbol hoping to find only 1 match                          
qualifySym sts qs = unique matches
  where
    mods = map fst sts
    maybeMatch = map (resolveSym qs . snd) sts
    matches = map unjust $ filter (isJust.snd) $ zip mods maybeMatch
    unique [] = Left $ "Unknown name: " ++ (unqualSymStr qs)
    unique ((mn,sym):[]) = Right $ qualifyName mn sym
    unique syms = Left $ "multiple matches for " ++ (unqualSymStr qs) ++ ", found in: " ++ (mkComma $  map (dotName.fst) syms)

----------------------------------------------   helpers       --------------------------------------------

-- Check AST for errors and return either errors or AST with Right removed
symErrors :: (Functor f, Foldable f) => f (Either a b) -> Either [a] (f b)
symErrors sym = case foldr errs [] sym of
  [] -> Right $ fmap toRight sym
  es -> Left es
  where errs (Left a) acc = a:acc
        errs _ acc = acc
        toRight (Right qn) = qn
        toRight (Left _) = codingError "Unexpected error"

nolu :: QSym -> SymbolTable QSym -> Maybe QSym
nolu qs _ = Just qs

resolveImports :: ModSymbols -> [(ModName, ModSymbols)]
resolveImports sts = zip mods $ map result imps
  where
    mods = map fst sts
    imps = map (importSyms . snd) sts
    result imp = map unjust $ filter (isJust.snd) $ zip mods $ map (find sts) imp
    find st sym = lookup sym st


resolveSym :: QSym -> SymbolTable QSym -> Maybe QSym
resolveSym qs@(QType _)  = lookup qs . map toSym . typeSyms
resolveSym qs@(QTag _)  = lookup qs . map toSym . tagSyms
resolveSym qs@(QVar _)  = nolu qs
resolveSym qs@(QPolicy _)   = lookup qs . map toSym . policySyms  
resolveSym qs@(QGroup _)   = lookup qs . map toSym . groupSyms

toSym :: forall t n. Symbol n => (t, n) -> (t, QSym)
toSym (a,b) = (a, qsym b)

unjust :: forall t t1. (t, Maybe t1) -> (t, t1)
unjust (a, Just b) = (a,b)
unjust _ = codingError "Unexpected Nothing"

qualifyName :: ModName -> QSym -> QSym
qualifyName mn qs = fmap (mn ++) qs

-}

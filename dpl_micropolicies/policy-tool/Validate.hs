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
module Validate where

import Data.Either
import Debug.Trace

import AST

import CommonFn
import ErrorMsg
import Symbols

-- Validate module
-- Perform error checking on policies and DPL modules
-- eventually will include type checking, currently only handles resolving symbol references
-- Takes policy list from cmd line and tries to find the 1 policy specified
-- Multiple policies are currently not supported
locateMain ::
     [String]
  -> [(ModName, SymbolTable QSym)]
  -> Either [ErrMsg] (ModName, PolicyDecl QSym)
locateMain [] _ = codingError "No policy case should be handled"
locateMain (mainPolicy:[]) sts =
  case lookupPolicy sts mod pol of
    Left msg -> Left [msg]
    Right pd -> Right pd
  where
    policyQSym = QPolicy $ parseDotName mainPolicy
    pol = unqualQSym policyQSym
    mod = modName policyQSym
    mainModName = modSymStr policyQSym
    mainPolicyName = unqualSymStr policyQSym
locateMain _ _ = Left ["Too many policies on command line"]

-- Validate that all symbols are reachable from main policy
-- Starts from main policy decl and traverses the ast for the main policy looking up
-- each symbol in the ModSymbols and records errors or builds UsedSymbols list
validateMain ::
     [(ModName, SymbolTable QSym)]
  -> ModName
  -> PolicyDecl QSym
  -> Either [ErrMsg] UsedSymbols
validateMain ms mn pd =
  case lefts validationResults of
    [] -> Right $ nubSymbols $ rights validationResults
    ls -> Left ls
  where
    validationResults = validatePolicyDecl ms mn pd []

-- Helper Fns to do validation...
validatePolicyDecl ::
     ModSymbols
  -> ModName
  -> PolicyDecl QSym
  -> [(Either ErrMsg (ModName, QSym))]
  -> [(Either ErrMsg (ModName, QSym))]
validatePolicyDecl ms mn pd@(PolicyDecl sp pl n pex) es =
  validatePolicyEx ms mn pex (es ++ [Right (mn, qsym pd)])

validatePolicyEx ::
     ModSymbols
  -> ModName
  -> PolicyEx QSym
  -> [(Either ErrMsg (ModName, QSym))]
  -> [(Either ErrMsg (ModName, QSym))]
validatePolicyEx ms mn (PEVar sp x) es =
  case lookupPolicy ms mn x of
    Right (mn', pd) -> validatePolicyDecl ms mn' pd es
    Left err -> es ++ [Left err]
validatePolicyEx ms mn (PECompExclusive sp p1 p2) es = es2
  where
    es1 = validatePolicyEx ms mn p1 es
    es2 = validatePolicyEx ms mn p2 es1
validatePolicyEx ms mn (PECompPriority sp p1 p2) es = es2
  where
    es1 = validatePolicyEx ms mn p1 es
    es2 = validatePolicyEx ms mn p2 es1
validatePolicyEx ms mn (PECompModule _ p1 p2) es = es2
  where
    es1 = validatePolicyEx ms mn p1 es
    es2 = validatePolicyEx ms mn p2 es1
validatePolicyEx ms mn rule es = foldr (validateQSym ms mn) es rule

validateModuleRequires ::
     ModSymbols -> [ModName] -> Either [ErrMsg] [(ModName, QSym)]
validateModuleRequires ms mns =
  case lefts validationResults of
    [] -> Right $ nubSymbols $ rights validationResults
    ls -> Left ls
  where
    validationResults :: [Either ErrMsg (ModName, QSym)]
    validationResults = foldr vmr [] mns
    vmr ::
         ModName
      -> [Either ErrMsg (ModName, QSym)]
      -> [Either ErrMsg (ModName, QSym)]
    vmr mn es =
      case lookup mn ms of
        Nothing -> [Left $ "Unable to locate module: " ++ (dotName mn)]
        Just st -> foldr (validateRequires ms mn) es $ requires st

validateRequires ::
     ModSymbols
  -> ModName
  -> RequireDecl QSym
  -> [Either ErrMsg (ModName, QSym)]
  -> [Either ErrMsg (ModName, QSym)]
validateRequires ms mn (Init sp e (ISExact _ ts)) es = foldr tagLookup es ts
  where
    tagLookup ::
         Tag QSym
      -> [Either ErrMsg (ModName, QSym)]
      -> [Either ErrMsg (ModName, QSym)]
    tagLookup ts es = foldr (validateQSym ms mn) es ts

validateQSym ::
     ModSymbols
  -> ModName
  -> QSym
  -> [Either ErrMsg (ModName, QSym)]
  -> [Either ErrMsg (ModName, QSym)]
validateQSym ms mn qs@(QType _) es = es ++ [doLookup (lookupType ms mn) qs]
validateQSym ms mn qs@(QTag _) es =
  case doLookup (lookupTag ms mn) qs of
    Left err -> es ++ [Left err]
    Right tag@(mn', qs') -> validateType ms mn' qs' (es ++ [Right tag])
validateQSym ms mn qs@(QPolicy _) es = es ++ [doLookup (lookupPolicy ms mn) qs]
validateQSym ms mn qs@(QGroup _) es = es ++ [doLookup (lookupGroup ms mn) qs]
validateQSym _ mn qs es = es ++ [Right (mn, qs)]

validateType ::
     ModSymbols
  -> ModName
  -> QSym
  -> [Either ErrMsg (ModName, QSym)]
  -> [Either ErrMsg (ModName, QSym)]
validateType ms mn qs es = foldr (validateQSym ms mn) es typs
  where
    (_, TagDecl _ _ typs) = getTag ms mn qs

doLookup fn qs =
  case fn qs of
    Left err -> Left err
    Right (mn, _) -> Right (mn, qs)

nubSymbols :: [(ModName, QSym)] -> [(ModName, QSym)]
nubSymbols = nubSort . map unqualify
  where
    unqualify (mn, qs) = (mn, fmap unqualified qs)

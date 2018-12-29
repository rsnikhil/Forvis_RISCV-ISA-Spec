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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module SrcPrinter where


import Data.List

import AST
import GenUtils
import CommonFn


printModule :: ModuleDecl QSym -> [String]
printModule md@(ModuleDecl _ _ _) = [ "module " ++ (moduleDotName md) ++ ":", ""] -- <:> concatMap printSect sects

{-
printSect :: SectDecl QSym -> [String]

printSect (Imports imps) = [ "import:"] <:> map (fmt 1 . importQName) imps
printSect (Tags tags) = [ "tag:"] <:> map (fmt 1 . printTagDecl) tags
printSect (Policies pols) = [ "policy:"] <:> concatMap printPolicy pols
printSect (Groups grps) = [ "group:"] <:> concatMap printGroup grps
--printSect (Inits ints) = [ "init:"] <:> concatMap printInit ints

printTagDecl :: TagDecl QSym -> String
printTagDecl (TagDecl _ qn flds) = (intercalate " " $ [hierarchyQName qn] ) -- ++ (map (mkParen.printFieldDecl) flds))

printFieldDecl :: FieldDecl QSym -> String
printFieldDecl (TagField _ qn) = unqualSymStr qn
printFieldDecl (CountField _ qn) = unqualSymStr qn ++ "++"
-}
                                    
printPolicy :: PolicyDecl QSym -> [String]
printPolicy (PolicyDecl _ _ pqn ex) = [fmt 1 (unqualSymStr pqn) ++ " = "] ++ (map (fmt 2) (printPEx ex))
  where
    printPEx (PERule _ clause) = printRuleClause clause
    printPEx (PECompExclusive _ lhs rhs) = printPEx lhs ++ ["]["] ++ printPEx rhs
    printPEx (PECompPriority _ lhs rhs) = printPEx lhs ++ ["^"] ++ printPEx rhs
    printPEx (PECompModule _ lhs rhs) = printPEx lhs ++ ["&"] ++ printPEx rhs
    printPEx (PEVar _ qn) = [printQN qn]

    printRuleClause :: RuleClause QSym -> [String]
    printRuleClause (RuleClause _ qn pats rr) = [ (printQN qn) ++
                                                   (mkParen (mkComma (map printBoundGroupPat pats)) ++
                                                             " -> " ++ printRuleResult rr)]

    printBoundGroupPat (BoundGroupPat _ qn tsp) = printQN qn ++ " = " ++ printTagSetPat tsp

    printRuleResult (RRFail _ s) = "fail \"" ++ s ++ "\""
    printRuleResult (RRUpdate _ bgps) = mkComma (map printBoundGroupEx bgps)

    printBoundGroupEx (BoundGroupEx _ qn tse) = printQN qn ++ " = " ++ printTagSetEx tse
    
    printTagSetPat (TSPAny _) = "_"
--    printTagSetPat (TSPVar _ var) = printQN var
    printTagSetPat (TSPExact _ ts) = mkCurly $ mkComma $ map printTag ts
    printTagSetPat (TSPAtLeast _ txs) = mkBracket $ mkComma $ map printTagEx txs
--    printTagSetPat (TSPVarSet _ var tsp) = printQN var ++ "@" ++ printTagSetPat tsp

    printTagSetEx (TSEVar _ var) = printQN var
    printTagSetEx (TSEExact _ tags) = mkCurly $ mkComma $ map printTag tags
    printTagSetEx (TSEModify _ tse txs) =
      printTagSetEx tse ++ (mkBracket $ mkComma $ map printTagEx txs)
    printTagSetEx (TSEUnion _ tse1 tse2) =
      printTagSetEx tse1 ++ "\\/" ++ printTagSetEx tse2
    printTagSetEx (TSEIntersect _ tse1 tse2) =
      printTagSetEx tse1 ++ "/\\" ++ printTagSetEx tse2

    printTagEx (TagEx _ t) = printTag t
    printTagEx (TagPlusEx _ t) = "+" ++ printTag t
    printTagEx (TagMinusEx _ t) = "-" ++ printTag t

    printTag (Tag _ t []) = printQN t
    printTag (Tag _ _ fs) = mkParen $ intercalate " " $ map printTagField fs

    printTagField (TFTag _ t) = printTag t
    printTagField (TFVar _ var) = printQN var
    printTagField (TFNew _ ) = "new"
    printTagField (TFAny _ ) = "_"

    printQN = qualSymStr
{-
printRuleDecl :: RuleDecl QSym -> String
printRuleDecl (RuleDecl _ qn st dy ins outs dy') = fmt 1
  ( unqualSymStr qn ++
    contextIn st dy ++
--    (mkParen ((mkComma $ map showTP ins) ++ " -> " ++ (mkComma $ map showTP outs))) ++
    contextOut dy'
  )

--printGuardDecl :: GuardDecl QSym -> String
--printGuardDecl (GuardDecl _ qn ins) = fmt 1 $ mkParen (mkComma $ map showTP ins)

{-
-}


mkParams :: GroupParam QSym -> String
mkParams (None _) = "_"
--mkParams (GroupParam _ nm) = nm

contextIn st dy = "" --" <" ++ (showTP st) ++ " | " ++ (showTP dy) ++ "> "
contextOut dy = "" --" <" ++ (showTP dy) ++ ">"

printGroup :: GroupDecl [ISA] QSym -> [String]
printGroup grp@(GroupDecl _ qn ins outs asm) = [fmt 1 (unqualSymStr qn) ++
                                                (mkParen $ (mkComma $ map mkParams ins)) ++ " -> " ++ (mkComma $ map mkParams outs)] ++
                                                (map printAsm asm)

printAsm (Asm _ inst ops dests params) =
  fmt 2 inst ++ (tab 1) ++ opSpec ops ++ " = " ++ (mkParen $ mkComma (map paramSpec $ zip dests params))
  where
    opSpec = mkComma . map showSpec
    showOp (spec,dest) = showSpec spec ++ ":" ++ show dest  
    showSpec AnyOp = "*"
    showSpec (Reg rf) = map toLower (show rf)
    showSpec (Const n) = "0x" ++ (hex n)
    paramSpec (dest, param) = show dest ++ ":" ++ param

{-
printInit :: InitDecl QSym -> [String]
printInit (Default _ sv) = [fmt 1 $ "default = " ++ showTP sv]
printInit (Zero _ sv) =  [fmt 1 $ "default = " ++ showTP sv]
printInit (Range _ s1 s2 sv) = [fmt 1 $ "range ["++ s1 ++ " ," ++ s2 ++ "] = " ++ showTP sv]
printInit (Symbol _ s sv) = [fmt 1 $ "symbol " ++ s ++ " = " ++ showTP sv]
printInit (Section _ s sv) = [fmt 1 $ "section " ++ s ++ " = " ++ showTP sv]
printInit (Flag _ f sv) = [fmt 1 $ "flag " ++ (show f) ++ " = " ++ showTP sv]
printInit (Meta _ s sv) = [fmt 1 $ "meta " ++ s ++ " = " ++ showTP sv]
-}
infixr 5 <:>
(<:>) :: [String] -> [String] -> [String] 
lhs <:> rhs = lhs ++ rhs ++ [""] 
-}

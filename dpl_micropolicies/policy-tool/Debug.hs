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
module Debug where



import Data.Maybe

import AST

import CommonFn
import GenUtils
import Symbols
import SrcPrinter


printModuleSymbols :: (ModName, SymbolTable QSym) -> [String]
printModuleSymbols (qpn, st) = ["", dotName qpn] ++
  printTable "Metadata" tagSyms ++
  printTable "Policies" policySyms ++
  printTable "Groups" groupSyms
  where
    printTable tNm tFn =  header tNm $ map showSymbol $ tFn st

printAST :: (PolicyName, SymbolTable QSym) -> [String]
printAST (qpn, st) = 
  header "Policy AST" (printPol qpn)
  where
    printPol qn = printPolicy $ fromJust $ lookup (QPolicy qn) (policySyms st)

{-  
printTagTable :: SymbolTable -> [String]
printTagTable st = header "Tags" (map (showSymbol tagName) $ tagSyms st)

printRuleTable :: SymbolTable -> [String]
printRuleTable st = header "Rules" (map (showSymbol ruleName) $ ruleSyms st)

printGuardTable :: SymbolTable -> [String]
printGuardTable st = header "Guards" (map (showSymbol guardName) $ guardSyms st)

printPolicyTable :: SymbolTable -> [String]
printPolicyTable st = header "Policy" (map (showSymbol policyName) $ policySyms st)

printGroupTable :: SymbolTable -> [String]
printGroupTable st = header "Group" (map (showSymbol groupName) $ groupSyms st)
-}

--printBoundRulesTable = header "Bound Rules" . map hierarchyQName . boundRules
--printBoundGroupsTable = header "Bound Groups" . map hierarchyQName . boundGroups

showSymbol :: Symbol s => (QSym, s) -> String
showSymbol (nm, _sym) = column 32 (unqualSymStr nm)

header :: String -> [String] -> [String]
header _ [] = []
header nm strs = map (fmt 1) $ [dash ++ " " ++ nm ++ " " ++ dash] ++ strs

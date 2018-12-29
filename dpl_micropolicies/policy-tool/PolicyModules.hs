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
module PolicyModules where


import Data.Either

import Data.Foldable
{-
import HeapPolicy
import StackPolicy
import CFIPolicy
import RWXPolicy
import NopPolicy
import CptPolicy
import WkTPolicy
-}
import AST
import Generator
import CommonFn
import PolicyParser
import ErrorMsg


getAllModules :: Options -> [String] -> IO (Either [ErrMsg] [ModuleDecl QSym])
getAllModules opts mods = do
    parsedMods <- getModules opts mods
    case lefts parsedMods of
      [] -> return $ Right $ rights parsedMods
      errs -> return $ Left errs

getModules :: Options -> [String] -> IO [Either ErrMsg (ModuleDecl QSym)]
getModules _ [] = return []
getModules opts (mn:[]) = getModule [] $ init $ parseDotName mn
  where
    -- recursively search for all imported modules, ignoring cycles
    getModule :: [Either ErrMsg (ModuleDecl QSym)] ->
                 ModName ->
                 IO [Either ErrMsg (ModuleDecl QSym)]
    getModule ms qmn | alreadyFound qmn ms = return ms
    getModule ms qmn = do
      exists <- moduleExists opts qmn
      if exists
        then do
        result <- polParse opts qmn
        case result of
          m@(Right mn) -> let imports = getImports mn in
                            foldlM getModule (m:ms) imports
          Left e -> error ("Error parsing module: " ++ e)
        else
        error ("Module doesnt exist: " ++  dotName qmn)

    
getModules _ _ = return [Left "Unable to locate top level module" ]
                
getImports :: ModuleDecl QSym -> [ModName]
getImports (ModuleDecl _ _ sects) = map importName $ sect importS sects
  where
    importName (ImportDecl _ qn) = qn

alreadyFound :: ModName -> [Either a (ModuleDecl t)] -> Bool    
alreadyFound qn ms = isKnown qn $ rights ms

isKnown :: ModName -> [ModuleDecl t] -> Bool
isKnown qn ms = or $ map (isModule qn) ms

isModule :: ModName -> ModuleDecl t -> Bool
isModule qn (ModuleDecl _ mn _) = qn == mn


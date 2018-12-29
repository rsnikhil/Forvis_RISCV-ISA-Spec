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
module Generator where

--import Numeric
import System.FilePath
import Control.Monad
import System.IO
import System.Directory

import qualified Data.Set as S

import AST
import CommonFn
import SrcPrinter
import Symbols
import Debug
import Tags
import Validate

-- C code Templates for the generator:
import GenRuleC
import GenRuleH
import GenUtilsC
import GenUtilsH
import GenMetaH
import GenMetaSetC (writeMetaSetCFile)
import GenMetaSetH (writeMetaSetHFile)

-- Runtime encoding information
import GenMetaY
import GenModuleY
import GenInitY
import GenGroupY
import GenEntityY

data Options = Options  { optIR          :: Bool
                        , optDebug       :: Bool
                        , optRules       :: Bool
                        , optProfile     :: Bool
                        , optLogging     :: Bool
                        , optModuleDir   :: String
                        , optTargetDir   :: String
                        , optOutputDir   :: String
                        , optFileName    :: String
                        }

defaultOptions :: Options
defaultOptions = Options { optDebug    = False
                         , optRules = False
                         , optProfile = False
                         , optLogging = False
                         , optIR  = False
                         , optModuleDir     = ""
                         , optTargetDir     = ""
                         , optOutputDir     = ""
                         , optFileName     = "policy"
                         }
{-                 
genSrcFiles :: Options -> [ModuleDecl QSym] -> IO ()
genSrcFiles opts modules =
  let srcDir = optOutputDir opts in do
    hPutStrLn stderr $ "Root module dir: " ++ srcDir
    createDirectoryIfMissing True $ srcDir
    mapM_ (genModule opts) modules
-}
genSymbolsFile :: ModSymbols -> IO ()
genSymbolsFile modules =
  let file = "symbols.txt" in do
    hPutStrLn stderr $ "Debug: Symbols from all modules can be found in: " ++ file
    writeFile file $ unlines $ syms
      where
        syms = concatMap printModuleSymbols modules
        
genASTFile :: Maybe (PolicyDecl QSym) -> IO ()
genASTFile Nothing =
  let file = "ast.txt" in do
    writeFile file $ unlines []
genASTFile (Just policy) =
  let file = "ast.txt" in do
    hPutStrLn stderr $ "Debug: AST for " ++ (qualSymStr $ qsym policy) ++ " can be found in: " ++ file
    writeFile file $ unlines $ ast
      where
        ast = printPolicy policy
{-        
genModule :: Options -> ModuleDecl QSym -> IO ()
genModule opts md =
  let modDir = foldl (</>) (optOutputDir opts) (moduleDirs md)
      modFile = modDir </> (moduleName md) <.> "dpl" in do
    hPutStrLn stderr $ "Generating: " ++ modFile
    writeFile modFile $ unlines $ printModule md
-}

genFiles :: Options
         -> ModSymbols
         -> [(ModName, QSym)]
         -> [(ModName, QSym)]
         -> ModName
         -> Maybe (PolicyDecl QSym)
         -> IO ()
genFiles opts allModSymTables policySyms requireSyms topMod policy = let

  {- No longer needed, 
    relevantModules :: S.Set ModName
    relevantModules = S.fromList usedModules
  
    symbols :: ModSymbols
    symbols = filter (\(mn,_) -> S.member mn relevantModules) allModSymTables
    -}
-- Use this instead
    allUsedSyms = nubSymbols (policySyms ++ requireSyms)
    relevantModules = S.fromList $ map fst allUsedSyms
  
    tagSetHFile = path </> "include" </> (optFileName opts) ++ "_meta_set.h"
    tagSetCFile = path </> "src" </> (optFileName opts) ++ "_meta_set.c"

    ruleCFile = path </> "src" </> (optFileName opts) ++ "_rule.c"
    ruleHFile = path </> "include" </> (optFileName opts) ++ "_rule.h"
    utilsCFile = path </> "src" </> (optFileName opts) ++ "_utils.c"
    utilsHFile = path </> "include" </> (optFileName opts) ++ "_utils.h"

    metaHFile = path </> "include" </> (optFileName opts) ++ "_meta.h"
    modYFile = path </> (optFileName opts) ++ "_modules.yml"
    metaYFile = path </> (optFileName opts) ++ "_meta.yml"
    initYFile = path </> (optFileName opts) ++ "_init.yml"
    entityYFile = path </> (optFileName opts) ++ "_entities.yml"
    groupsYFile = path </> (optFileName opts) ++ "_group.yml"

    path = addTrailingPathSeparator (optOutputDir opts)

    debug  = optDebug opts

    profile  = optProfile opts
    logging  = optLogging opts

    tagInfo :: TagInfo
    tagInfo = buildTagInfo allModSymTables allUsedSyms

    targetPath = optTargetDir opts
    
  in do
  -- make directory
    createDirectoryIfMissing True $ path
    createDirectoryIfMissing True $ path </> "src"
    createDirectoryIfMissing True $ path </> "include"

    -- tag_set files
    hPutStrLn stderr $ "\nGenerating: " ++ tagSetCFile
    writeMetaSetCFile tagSetCFile
    hPutStrLn stderr $ "Generating: " ++ tagSetHFile
    writeMetaSetHFile tagSetHFile tagInfo

      -- policy_rule files
    hPutStrLn stderr $ "Generating: " ++ ruleCFile
    writeRuleCFile ruleCFile debug profile logging topMod policy allModSymTables allUsedSyms tagInfo

    hPutStrLn stderr $ "Generating: " ++ ruleHFile
    writeRuleHFile ruleHFile debug

      -- policy_utils files
    hPutStrLn stderr $ "Generating: " ++ utilsCFile
    writeUtilsCFile utilsCFile tagInfo
    
    hPutStrLn stderr $ "Generating: " ++ utilsHFile
    writeUtilsHFile utilsHFile
    
      -- policy_metadata files
    hPutStrLn stderr $ "Generating: " ++ metaHFile
    writeMetaHFile metaHFile tagInfo
    hPutStrLn stderr $ "Generating: " ++ metaYFile
    writeMetaYFile metaYFile tagInfo
    hPutStrLn stderr $ "Generating: " ++ modYFile
    writeModYFile modYFile allModSymTables
    hPutStrLn stderr $ "Generating: " ++ groupsYFile
    writeGroupYFile groupsYFile allModSymTables allUsedSyms
    hPutStrLn stderr $ "Generating: " ++ initYFile
    writeInitYFile initYFile allModSymTables allUsedSyms
    hPutStrLn stderr $ "Generating: " ++ entityYFile
    writeEntityYFile entityYFile targetPath


dump stuff = do
  hPutStrLn stderr "\nHere's some stuff:: " 
  hPutStrLn stderr $ unlines $ map show stuff
        

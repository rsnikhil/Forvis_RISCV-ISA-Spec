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
{-# LANGUAGE OverloadedStrings #-}
module GenGroupY where

import Data.List
import Data.Yaml
--import qualified Data.HashMap.Lazy as M

import Data.Text (Text, pack)

import AST
import Symbols
--import Tags
import CommonFn


-- --------------------------------------------------------------------------------------

--      .h header
writeGroupYFile
  :: FilePath
  -> ModSymbols
  -> UsedSymbols
  -> IO ()
writeGroupYFile yFile ms us = encodeFile yFile $ object [ "Groups"     .= groups]
  where
    groups = object $ map (uncurry (.=)) $ combineInst
    combineInst = concatMap merge groupByInstruction
    merge :: [(Text, String)] -> [(Text, [String])]
    merge [] = []
    merge ls = let (is, as) = unzip ls in [(head is, nub $ sort as)]
    groupByInstruction :: [[(Text, String)]]
    groupByInstruction = groupWith fst $ sortWith fst $ flatten $ groupList
    groupList :: [(ModName, GroupDecl [ISA] QSym)]
    groupList = map (groupDecl ms) $ usedGroups us
    flatten :: [(ModName, GroupDecl [ISA] QSym)] -> [(Text, String)]
    flatten = concatMap instGroups
    instGroups :: (ModName, GroupDecl [ISA] QSym) -> [(Text, String)]
    instGroups (mn, GroupDecl _ qs _ _ insts) = zip (map asm insts) (repeat $ qualSymStr $ qualifyQSym (moduleForQSym ms mn qs) $ groupPrefix qs)
    asm :: ISA -> Text
    asm (Asm _ i _) = pack i

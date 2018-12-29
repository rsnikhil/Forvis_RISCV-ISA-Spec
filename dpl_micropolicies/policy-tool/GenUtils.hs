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
module GenUtils where



import Data.Word

import Text.PrettyPrint.Mainland.Class(Pretty, ppr)
import Text.PrettyPrint.Mainland(pretty)

import CommonFn

poundDef :: String -> Word32 -> String
poundDef sym val = column 47 ("#define " ++ sym) ++ " " ++ (hex val)

column :: Int -> String -> String
column n l = l ++ (replicate (n-length l) ' ')


blank :: Int -> [String]
blank n = replicate n "\n"

-- simple indent formatting
fmt :: Int -> String -> String
fmt tabs str = tab tabs ++ str

evalFn :: (String -> String) -> String
evalFn = evalFnDecl id id

evalFnDecl :: (String -> String) -> (String -> String)
           -> (String -> String) -> String
evalFnDecl ttyp etyp atyp = mkParen $ mkComma $ (map ttyp matchTags) ++ (map etyp matchExtras) ++ (map atyp arrays)

matchTags, matchExtras, arrays :: [String]
matchTags = ["pc", "ci", "rs1", "rs2", "rs3", "memtag"]
matchExtras = ["opgroup", "funct12", "subinstr"]
arrays = [ruleID, flags, size]

flags, ruleID, size :: String
flags = "flags"
ruleID = "rule"
size = "size"

-- C pretty printing stuff
maxWidth :: Int
maxWidth = 100

renderC :: Pretty a => a -> String
renderC = pretty maxWidth . ppr



-- generate system & user includes
genIncludes :: [String] -> [String] -> [String]
genIncludes shf uhf = map system shf ++ (map user uhf)
  where
    user file = "#include \"" ++ file ++ ".h\""
    system file = "#include <" ++ file ++ ".h>"
      

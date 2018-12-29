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
module GenInitY where


import Data.Yaml
import qualified Data.HashMap.Lazy as M

import Data.Text (Text, pack, splitOn)

import AST
import Symbols
import Tags
import CommonFn


-- --------------------------------------------------------------------------------------

--      .h header
writeInitYFile
  :: FilePath
  -> ModSymbols
  -> UsedSymbols
  -> IO ()
writeInitYFile yFile ms us = encodeFile yFile $ reqsYml $ map pathify $ initSetFns ms us
  where
    pathify :: ([String], [Tag QSym]) -> ([Text], [Tag QSym])
    pathify (p, t) = (map pack p, t)

reqsYml :: [([Text], [Tag QSym])] -> Value
reqsYml reqs = object [ "Require" .= (foldl reqYml (object [])  reqs)]

reqYml :: Value -> ([Text], [Tag QSym]) -> Value
reqYml (Object _) ([], tags) = object [ "metadata" .=  (map mkTagVal tags)]
reqYml (Object obj) (e:es, tags) = case M.lookup e obj of
                            Just o -> Object $ M.insert e (reqYml o (es, tags)) obj
                            Nothing -> Object $ M.insert e (reqYml (object []) (es,tags)) obj
reqYml _ _ = error "Internal error processing requirements"

mkField :: TagField QSym -> Value
mkField (TFTag _ tag) = mkTagVal tag
mkField tf = error $ "Illegal field value" ++ (show tf)

mkTagVal :: Tag QSym -> Value
mkTagVal (Tag _ qSym fields) = object [ "name"     .= (qualSymStr qSym)
                                      , "fields"   .= (map mkField fields)
                                   ]

initSetFns :: ModSymbols -> UsedSymbols -> [([String], [Tag QSym])]
initSetFns ms us = map unique $ map combine everyTag
  where
    -- group together common Fn defs
    everyTag = groupWith fst $ sortWith fst $ policyInitFns
    -- extract Requires from all modules
    policyInitFns = concatMap reqFn $ usedRequires ms us
    -- convert a requirement into a Fn spec
    reqFn (mn, Init _ rqn is)   = [mkPolicyFn (setTags $ fmap (resolveQSym ms mn) is) rqn]
    mkPolicyFn tags rqn = (rqn, tags)
    mkSystemFn (rqn, _) = (rqn, [])
    -- helpers to merge common Fn defs for each unique entity
    unique (n, ts) = (n, nubWith qsym $ sortWith qsym ts)
    combine = foldr merge ([],[])
    merge (n, ts) (_, ts2) = (n, ts2 ++ ts)


{-
mkFields :: (Word32,TypeDecl QSym) -> Value
mkFields (id, typ) = object [ "index"     .= id
                            , "type" .= (fieldTyp typ)
                            ]
  where
    fieldTyp (TypeDecl _ _ tdt) = ts tdt
    ts :: TagDataType -> String
    ts (TDTInt _ _) = "Int"
-}

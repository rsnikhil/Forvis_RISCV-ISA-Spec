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
module GenMetaY where

import qualified Data.Map as M
import Data.Yaml
import Data.Word

import AST

import CommonFn
import Tags

-- --------------------------------------------------------------------------------------

--      .h header
writeMetaYFile
  :: FilePath
     -> TagInfo
     -> IO ()
writeMetaYFile yFile tagInfo = encodeFile yFile $ metaYml tagInfo

metaYml ti = object [ "MaxBit" .= (tiMaxTag ti)
                    , "NumberBits" .= (tiNumBitFields ti)
                    , "NumberFields" .= (tiNumDataArgs ti)
                    , "ArrayLength" .= (tiArrayLength ti)
                    , "Metadata" .= (map (mkVal ti) $ tiTagNames ti) ]

mkVal :: TagInfo -> QSym -> Value
mkVal ti qSym = object [ "name"     .= (qualSymStr qSym)
                       , "id"     .= (pos qSym)
                       , "group" .= isGrp qSym
                       , "fields"     .= (fieldCount qSym)
                       , "data"     .= (map mkFields $ fields qSym)
                       ]
  where
    isGrp (QGroup _) = True
    isGrp _ = False
    pos qs = case M.lookup qs $ tiTagBitPositions ti of
      Just id -> id
      Nothing -> error "Encoding error in TagInfo"
    fieldCount qs = case M.lookup qs $ tiTagArgInfo ti of
      Just fs -> length fs
      Nothing -> error "Encoding error in TagInfo"
    fields qs = case M.lookup qs $ tiTagArgInfo ti of
      Just fs -> fs
      Nothing -> error "Encoding error in TagInfo"

mkFields :: (Word32,TypeDecl QSym) -> Value
mkFields (id, typ) = object [ "index"     .= id
                            , "type" .= (fieldTyp typ)
                            ]
  where
    fieldTyp (TypeDecl _ _ tdt) = ts tdt
    ts :: TagDataType -> String
    ts (TDTInt _ _) = "Int"

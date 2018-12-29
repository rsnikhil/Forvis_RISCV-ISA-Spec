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
{-# LANGUAGE NamedFieldPuns #-}
module GenMetaH where

import qualified Data.Map as M


import GenUtils
import CommonFn
import Tags

-- --------------------------------------------------------------------------------------

--      .h header
writeMetaHFile
  :: FilePath
     -> TagInfo
     -> IO ()
writeMetaHFile hFile tagInfo =
  writeFile hFile $ unlines $
  hHeader ++ (blank 1) ++
  (genTags tagInfo) ++ (blank 1) ++
  hFooter

hHeader :: [String]
hHeader =
  [ "/*"
  , " * Generated header"
  , " */"
  , ""
  , "#ifndef POLICY_META_H"
  , "#define POLICY_META_H"
  , ""
--  , "#include \"policy_names.h\""
--    , "#define COMPOSITE_METADATA                      0x0000000000000101"
--    , "#define COMPOSITE_SIZE                          0x0000000000000101"
--    , "#define COMPOSITE_SLOTS                         0x0000000000000101"
--  , "#define TAG_BYTES " ++ show (tagBytes t)
  , ""
  ]


hFooter :: [String]
hFooter =
  [ "#endif // POLICY_META_H"
  ]

genTags :: TagInfo -> [String]
genTags (TagInfo {tiTagBitPositions}) =
  map (\(qn,intID) -> poundDef (tagName qn) intID ++ "    // " ++ (show qn))
      (M.toAscList tiTagBitPositions)


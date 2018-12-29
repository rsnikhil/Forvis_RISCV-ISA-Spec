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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
module GenUtilsC where

import GenUtils
import AST
import CommonFn
import Tags

import Language.C.Syntax
import Language.C.Quote.GCC

import Data.Word
import qualified Data.Map as M


-- --------------------------------------------------------------------------------------

--      policy_utils.c implementation

writeUtilsCFile
  :: FilePath
     -> TagInfo
     -> IO ()

writeUtilsCFile cFile (tinfo@(TagInfo {tiTagNames})) =
  writeFile cFile $ unlines $                          -- Write the impl file, consisting of:
  cHeader ++ (blank 1) ++                              -- top of file includes
  [renderC $ tagArgsToString tinfo] ++
  [renderC $ tagToString tiTagNames] ++
  [renderC tagSetToString] ++
  printTagSetFn ++ (blank 1) ++
  printTagSetDebug ++ (blank 1) ++
  printDebug ++ (blank 1) ++
  cFooter
  
-- top of policy_impl.c
cHeader,cFooter :: [String]
cHeader = [ "#include \"policy_utils.h\""
          , "#include \"policy_meta.h\""
          , "#include \"policy_meta_set.h\""
          , "#include <stdio.h>"
          , "#include <stdlib.h>"
          , "#include <stdarg.h>"
          , "#include <string.h>"
          , "#include <inttypes.h>"
          , ""
          , "extern void printm(const char* s, ...);"
          ]
cFooter = []

-- This function takes in a tag set, an index, a buffer, and the size of the
-- buffer in bytes.  If the tag at index i has arguments, they are printed to
-- the buffer.  The return value follows othe same rules as tagSetToString,
-- below.
tagArgsToString :: TagInfo -> Definition
tagArgsToString tinfo =
  [cedecl|
     int meta_args_to_string(const typename meta_set_t* ts, int i,
                            char* buf, typename size_t buf_len) {
       int printed = 0;
       switch(i) {
         $stms:(concatMap argCase argInfos)
       }
       return printed;
     }
  |]
  where
    lookupBit :: QSym -> Word32
    lookupBit qs =
      case M.lookup qs (tiTagBitPositions tinfo) of
        Nothing -> error $ "Missing tag bit position for tag " ++ tagString qs
                        ++ " in GenUtils/tagArgsToString."
        Just w -> w

    -- Pairs are (a) tag bit positions and (b) positiions of any associated args
    argInfos :: [(Word32,[Word32])]
    argInfos = map (\(qs,ais) -> (lookupBit qs, map fst ais))
                 $ M.assocs (tiTagArgInfo tinfo)

    argCase :: (Word32,[Word32]) -> [Stm]
    argCase (_,[]) = []
    argCase (w,ws) =
         [cstms|  case $exp:w :
                    printed = printed; // Parsing bug in quasiquoter requires this
                    $stms:(concatMap printArg ws)
                    break; |]

    printArg :: Word32 -> [Stm]
    printArg w = [cstms|
       printed += snprintf(buf, buf_len - printed, " 0x%x", (unsigned int)(ts -> tags[$exp:w]));
       if (printed >= buf_len) {
         return printed;
       }
    |]
      

-- tagSetToString takes in a tag set, a buffer, and the size of that buffer (in
-- bytes).  The return value is usually the number of ints consumed in the
-- buffer, excluding the terminating NULL.  If the output was too big for the
-- buffer, the return value will be greater than buf_len, but will not say
-- exactly how much size was needed (unlike snprintf and friends).
tagSetToString :: Definition
tagSetToString = [cedecl|
  int meta_set_to_string(const typename meta_set_t* ts,
                        char* buf,
                        typename size_t buf_len) {
    char* cursor = buf;
    int consumed = 0;
    // These variables keep track of where we are in the buffer and how many
    // bytes we've consumed.

    if(buf == NULL || buf_len <= 0) {
      // Check for NULL input or tiny buffers
      return 1;
    }
    if(buf_len <= 2) {
      buf[0] = '\0';
      return 3;
    }
    if(ts == 0) {
      strncpy(buf,"-0-",4);
      return 4;
    }

    cursor[0] = '{';
    
    cursor++;
    consumed++;

    // This keep track of whether we've printed any tags, so we can have
    // commas only where needed.
    typename bool one_printed = false;

    // This loops over all possible tags, checking if they are present.
    // If they are, they are added to the buffer (if room remains) and our
    // state us updated accordingly.
    //
    // Could be sped up by using fls to find high bit instead of checking each.
    for(int i = 0; i <= MAX_TAG; i++) {
      int index = i / 32;
      int bit = i % 32;
      int meta_chars = 0;
      if (ts->tags[index] & 1 << bit) {
      // inlining ts contains because of weird build problem.
      //if(ts_contains(ts,i)) {

        // this adds ", " after the last tag, if at least one has been printed
        if(one_printed) {
          if(buf_len - consumed < 2) {
            buf[buf_len-1]='\0';
            return (consumed+3);
          }
          cursor[0]=',';
          cursor[1]=' ';
          cursor += 2;
          consumed += 2;
        }

        // print tag name
        meta_chars = meta_to_string(i, cursor, buf_len-consumed);
        consumed += meta_chars;

        if(consumed > buf_len) {
          buf[buf_len-1]='\0';
          return consumed;
        }

        cursor += meta_chars;

        // print tag arg
        meta_chars = meta_args_to_string(ts,i,cursor,buf_len-consumed);
        consumed += meta_chars;
        
        if(consumed > buf_len) {
          buf[buf_len-1]='\0';
          return consumed;
        }

        cursor += meta_chars;

        one_printed = true;
      }
    }

    if(buf_len-consumed < 2) {
      buf[buf_len-1] = '\0';
      return (consumed+3);
    }
    
    cursor[0] = '}';
    cursor[1] = '\0';

    return(consumed+1);
  }
|]

tagToString :: [QSym] -> Definition
tagToString tags = [cedecl|
  int meta_to_string(typename meta_t tag, char *buf, typename size_t buf_len) {
    if(buf == NULL || buf_len <= 0) {
      return 0;
    }
    
    switch(tag) {
      $stms:(concatMap tagCase tags)
    }
    return 0;
  }
  |]
  where
    tagCase :: QSym -> [Stm]
    tagCase q = [cstms|
      case $id:tagIntId :
        if (buf_len <= $int:nameLength) {
          buf[0] = '\0';
          return (1 + $int:nameLength);
        } else {
          strncpy(buf,$string:tagPrettyName,1+$int:nameLength);
          return $int:nameLength;
        }
    |]
      where
        tagIntId :: String
        tagIntId = tagName q

        tagPrettyName :: String
        tagPrettyName = unqualSymStr q

        nameLength :: Int
        nameLength = length tagPrettyName

printTagSetDebug :: [String]
printTagSetDebug = [ "void print_meta_set_debug(meta_t tag){"
              , fmt 1 "print_meta_set((const meta_set_t*)tag);"
              , "}"
              ]

printTagSetFn :: [String]
printTagSetFn = [ "void print_meta_set(const meta_set_t *meta_set){"
               , tab 1 ++ "char name[256];"
               , tab 1 ++ "meta_set_to_string(meta_set, name, 256);"
               , tab 1 ++ "printm(\"%s\\n\", name);"
               , "}"
               ]
  
printDebug :: [String]
printDebug = [ tab 1 ++ "// print the contents of the input vector"
             , tab 0 ++ "void print_debug(const meta_set_t *pc, const meta_set_t *ci, const meta_set_t *rs1, const meta_set_t *rs2, const meta_set_t *rs3, const meta_set_t *mem){"
             , tab 1 ++ ""
--             , fmt 1 "printm(\"Sub-Instruction: %d\\n\", subinstr);"
             , fmt 1 "printm(\"PC: %x:\", pc);"
             , tab 1 ++ "print_meta_set(pc);"
             , fmt 1 "printm(\"CI: %x:\", ci);"
             , tab 1 ++ "print_meta_set(ci);"
             , fmt 1 "printm(\"R1: %x:\", rs1);"
             , tab 1 ++ "print_meta_set(rs1);"
             , fmt 1 "printm(\"R2: %x:\", rs2);"
             , tab 1 ++ "print_meta_set(rs2);"
             , fmt 1 "printm(\"MEM: %x:\", mem);"
             , tab 1 ++ "print_meta_set(mem);"
             , tab 0 ++ "}"
             ]



logger :: [String]
logger = ["void printm(const char *fmt, ...){"
           , tab 1 ++ "va_list argp;"
           , tab 1 ++ "va_start(argp, fmt);"
           , tab 1 ++ "printm(fmt, argp);"
           , tab 1 ++ "va_end(argp);"
           , "}"
           ]

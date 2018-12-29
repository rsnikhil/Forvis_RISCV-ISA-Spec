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
module GenUtilsH where

writeUtilsHFile :: FilePath -> IO ()
writeUtilsHFile chFile =
  writeFile chFile $ unlines chBody                                            -- write the header, consisting of:

-- Text for policy_utils.h
chBody :: [String]
chBody = [ "#ifndef POLICY_UTILS_H"
         , "#define POLICY_UTILS_H"
         , "#include <stdlib.h>"
         , "#include \"policy_meta_set.h\""
         , ""
         , " #ifdef __cplusplus"
         , "extern \"C\" {"
         , "#endif"
         , ""
         , "void print_meta_set(const meta_set_t *meta_set);"
         , "int meta_set_to_string(const meta_set_t *meta_set, char *buf, size_t buf_len);"
         , "void print_debug(const meta_set_t *ci, const meta_set_t *rs1, const meta_set_t *rs2, const meta_set_t *rs3, const meta_set_t *mem, const meta_set_t *pc);"
         , ""
--         , "void logmsg(const char *fmt, ...);"
         , "#ifdef __cplusplus"
         , "}"
         , "#endif"
         , "#endif // POLICY_UTILS_H"
         ]


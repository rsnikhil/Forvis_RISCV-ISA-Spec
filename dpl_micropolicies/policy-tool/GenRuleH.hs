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
module GenRuleH where

writeRuleHFile :: FilePath -> Bool -> IO ()
writeRuleHFile chFile debug =
  writeFile chFile $ unlines $
  prHdr debug ++ prFtr

-- Text for policy_rule.h
prHdr :: Bool -> [String]
prHdr debug = [ "#ifndef POLICY_RULE_H"
        , "#define POLICY_RULE_H"
        , ""
        , "#include \"policy_eval.h\""
        , ""
        , "#define POLICY_EXP_FAILURE 0"
        , "#define POLICY_IMP_FAILURE -1"
        , "#define POLICY_SUCCESS 1"
        , ""
        , "#define DEBUG_STUFF " ++ if debug then "1" else "0"
        , "#ifdef __cplusplus"
        , "extern \"C\" {"
        , "#endif"
        , ""
        , "void logRuleInit();"
        , "void logRuleEval(const char* ruleDescription);"
        , "const char* nextLogRule(int* idx);"
        , ""
        , "#ifdef __cplusplus"
        , "}"
        , "#endif"
        ]

prFtr :: [String]  
prFtr = [ ""
        , "#endif // POLICY_RULE_H"
        ]

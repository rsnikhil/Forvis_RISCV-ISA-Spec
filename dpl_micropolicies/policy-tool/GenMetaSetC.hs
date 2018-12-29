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
{-# LANGUAGE QuasiQuotes #-}
module GenMetaSetC (writeMetaSetCFile) where

import GenUtils(renderC)

import Language.C.Syntax
import Language.C.Quote.GCC

{- The generated policy_meta_set.c file contains an implementation of sets of tags,
   and some associated memory mangement.

   The generated C is !!! NOT THREAD SAFE !!!
-}

writeMetaSetCFile :: FilePath -> IO ()
writeMetaSetCFile chFile = writeFile chFile $
  tagSetHeader ++ renderC tagSetBody

tagSetHeader :: String
tagSetHeader = unlines
  [""
  ,"#include <stdint.h>"
  ,"#include <stdbool.h>"
  ,"#include <inttypes.h>"
  ,"#include <stdlib.h>"
  ,"#include <string.h>"
  ,""
  ,"#include \"policy_eval.h\""
  ,""
  ,"#define ALLOC_BUMP_MAX  (KERNEL_MEM_END - (sizeof(meta_set_t)))"
  ,""
  ]

tagSetBody :: [Definition]
tagSetBody =
  [msContainsDef,
   msAddDef,
   msRemoveDef,
   msEqDef,
   msUnionDef]

msEqDef :: Definition
msEqDef = [cedecl|
  typename bool ms_eq(const typename meta_set_t* ms1, const typename meta_set_t* ms2) {
    if (ms1 == ms2) {
      return true;
    } else if (ms1 == 0 || ms2 == 0) {
      return false;
    } else {
      for(int i = 0; i < META_SET_WORDS; i++) {
        if(ms1->tags[i] != ms2 -> tags[i]) {
          return false;
        }
      }
    }
    return true;
  }
|]

{- The next few functions do a bit of computation to find the right place to look
   for the tag based on an integer representing the tag.  In many cases where
   this function is called, we know at code generation time which tag is being
   looked for.  In those cases, it would be more efficient to call a
   special-case version of this function that can skip the computation, or just
   inline the check directly.
 -}
msContainsDef :: Definition
msContainsDef = [cedecl|
  typename bool ms_contains(const typename meta_set_t* ms, const typename meta_t m) {
    if(ms == NULL || m > MAX_TAG) {
      return false;
    }

    int index = m / 32;
    int bit = m % 32;
    typename uint32_t mask = 1 << bit;
    
    // It would be nice to just return the scrutinee of this if, but it
    // doesn't have the right type.
    if((ms->tags[index]) & mask) {
      return true;
    } else {
      return false;
    }
  }
|]


-- NOTE: ms_bit_add and ms_bit_remove only handle the bit in the bitfield for
-- the corresponding tag.  They do not handle tag arguments
msAddDef :: Definition
msAddDef = [cedecl|
  void ms_bit_add(typename meta_set_t* ms, typename meta_t m) {
    if(ms == NULL || m > MAX_TAG) {
      return;
    }

    int index = m / 32;
    int bit = m % 32;

    typename uint32_t mask = 1 << bit;
    ms->tags[index] |= mask;

    return;
  }
|]

msRemoveDef :: Definition
msRemoveDef = [cedecl|
  void ms_bit_remove(typename meta_set_t* ms, typename meta_t m) {
    if(ms == NULL || m > MAX_TAG) {
      return;
    }

    int index = m / 32;
    int bit = m % 32;

    typename uint32_t mask = ~(1 << bit);
    ms->tags[index] &= mask;

    return;
  }
|]

-- Computes the union of two tag sets, storing the result in the first argument.
-- DON'T CALL THIS WITH A CANONIZED TAG SET FOR FIRST ARG - CHANGING THEM CAUSES
-- A DISTURBANCE IN THE FORCE
--
-- Returns an int - 0 for success, anything else for error.  When we add tags
-- with arguments, trying to union two tag sets with the same tag but different
-- arguments will be a dynamic error.  This is awful, but I don't have a better
-- plan.
msUnionDef :: Definition
msUnionDef = [cedecl|
  int ms_union(typename meta_set_t* ms1, const typename meta_set_t* ms2) {
    if(ms1 == NULL || ms2 == NULL) {
      return -1;
    }

    for(int i = 0; i < META_SET_WORDS; i++) {
      if(   i >= META_SET_BITFIELDS && ms1->tags[i] && ms2->tags[i]
         && ((ms1->tags[i]) != (ms2->tags[i]))) {
        return i;
      } else {
        ms1->tags[i] |= ms2->tags[i];
      }
    }

    return 0;
  }
|]


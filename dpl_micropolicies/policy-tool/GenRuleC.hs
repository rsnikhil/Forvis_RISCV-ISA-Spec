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
{-# LANGUAGE TupleSections #-}
module GenRuleC (writeRuleCFile) where

import GenUtils(renderC, blank)
import AST
import Symbols
import Tags
import CommonFn

import Language.C.Syntax
import Language.C.Quote.GCC

import Data.Loc (noLoc)

import Data.List (foldl')
import Data.Word
import Data.Array.Unboxed (elems)
import Data.Array.ST
import Data.Bits ((.|.))
import Data.Maybe (mapMaybe)
import Control.Monad.ST

import qualified Data.Map as M

-- This function deterministically generates names for C helper functions
-- associated with result generation in policy rule evaluation.  To avoid
-- collision, these names refer to the source location of the relevant pattern
-- or rule result expression.

-- XXX names, should be moved elsewhere
--resultsSizeMacro, resultsPCMacro, resultsRDMacro, resultsCSRMacro :: String
--resultsSizeMacro = "RULE_DEST_COUNT"

policySuccessName, policyIFailName, policyEFailName, policyErrorName :: String
policySuccessName = "policySuccess"
policyIFailName   = "policyImpFailure"
policyEFailName   = "policyExpFailure"
policyErrorName   = "policyErrorFailure"

policySuccessVal, policyIFailVal, policyEFailVal, policyErrorVal :: Int
policySuccessVal = 1
policyIFailVal   = -1
policyEFailVal   = 0
policyErrorVal   = -2

-- --------------------------------------------------------------------------------------

--      .c implementation
writeRuleCFile
  :: FilePath
     -> Bool
     -> Bool
     -> Bool
     -> ModName
     -> Maybe (PolicyDecl QSym)
     -> ModSymbols
     -> UsedSymbols
     -> TagInfo
     -> IO ()

writeRuleCFile cFile debug profile _logging topMod policy modSyms usedSyms tinfo =
  writeFile cFile $ unlines $                                                  -- Write the impl file, consisting of:
  cHeader debug profile ++ (blank 1) ++
  [renderC $     ruleLogStructure modSyms topMod policy ++ policyResultConsts ++ policyTypeHelpers modSyms usedSyms
             ++ translateTopPolicy debug profile modSyms usedSyms tinfo topMod policy]

  {-
  installRule ++
  intHandlerHdr profile ++ intHandlerFtr target logging debug profile ++ blank 1 ++
  cFooter
-}

ruleLogStructure :: ModSymbols -> ModName -> Maybe (PolicyDecl QSym) -> [Definition]
ruleLogStructure _ _ Nothing = []
ruleLogStructure ms topMod (Just p) = 
  [cunit|
    const int ruleLogMax = $int:(polCount p);
    char* ruleLog[$int:(polCount p + 1)];
    int ruleLogIdx = 0;

        void logRuleEval(const char* ruleDescription) {
          if(ruleLogIdx < ruleLogMax){
            ruleLog[ruleLogIdx] = ruleDescription;
            if(ruleLogIdx <= ruleLogMax){
              ruleLogIdx++;
            }
          }
        }
        void logRuleInit() {
          ruleLogIdx = 0;
          ruleLog[ruleLogMax] = "additional rules omitted...";
        }
        const char* nextLogRule(int* idx) {
          if(*idx < ruleLogIdx)
            return ruleLog[(*idx)++];
          return 0;
        }
  |]
    where
      -- use count of policy names as proxy for the number of possible rule evals
      -- TODO: perform a more accurate count
      polCount :: PolicyDecl QSym -> Int
      polCount (PolicyDecl _ _ _ pex) = 1 + pexCount pex
      pexCount :: PolicyEx QSym -> Int
      pexCount (PEVar _ qn) = let (modN, p) = getPolicy ms topMod qn in polCount p

      pexCount (PECompExclusive _ lhs rhs) = pexCount lhs + pexCount rhs
      pexCount (PECompPriority _ lhs rhs) =  pexCount lhs + pexCount rhs
      pexCount (PECompModule _ lhs rhs) =  pexCount lhs + pexCount rhs
      pexCount (PENoChecks _) = 0
      pexCount (PERule _ _) = 0

  -- Constant definitions for policy evaluation results
policyResultConsts :: [Definition]
policyResultConsts =
  [cunit|
    const int $id:policyErrorName = $int:policyErrorVal;
    const int $id:policyEFailName = $int:policyEFailVal;
    const int $id:policyIFailName = $int:policyIFailVal;
    const int $id:policySuccessName = $int:policySuccessVal;

  |]

-- Most policy evaluation functions need access to the operands tag sets.  For
-- convenience, we fix one set of names used as arguments to functions.  
pcArgName,ciArgName,op1ArgName,op2ArgName,op3ArgName :: String
memArgName,opsSetsArgName, resSetsArgName, contextArgName, operandsArgName, resultsArgName :: String
pcArgName = opsSetsArgName ++ "pc"
ciArgName = opsSetsArgName ++ "ci"
op1ArgName = opsSetsArgName ++ "op1"
op2ArgName = opsSetsArgName ++ "op2"
op3ArgName = opsSetsArgName ++ "op3"
memArgName = opsSetsArgName ++ "mem"
resultsPC, resultsRD, resultsCSR :: String
resultsPC = resSetsArgName ++ "pc"
resultsRD = resSetsArgName ++ "rd"
resultsCSR = resSetsArgName ++ "csr"

opsSetsArgName = operandsArgName ++ "->"
resSetsArgName = resultsArgName ++ "->"
contextArgName = "ctx"
operandsArgName = "ops"
resultsArgName = "res"

-- For convenience, we define a few commonly used types and parameter/argument
-- lists. 
tagSetType :: Type
tagSetType = [cty| const typename meta_set_t |]

contextType :: Type
contextType = [cty| typename context_t |]

operandsType :: Type
operandsType = [cty| typename operands_t |]

resultsType :: Type
resultsType = [cty| typename results_t |]

policyInputParams :: [Param]
policyInputParams =
  [cparams|$ty:contextType* $id:contextArgName,
           $ty:operandsType* $id:operandsArgName,
           $ty:resultsType* $id:resultsArgName|]

policyInputArgs :: [Exp]
policyInputArgs =
  map (\x -> [cexp|$id:x|])
      [contextArgName, operandsArgName, resultsArgName]


-- Here we define some globals and helper functions that are used to manipulate
-- user-defined datatypes.
--
-- Currently the only supported type is ints, possibly with a fixed range.  We
-- define two things for each declared type: The next unused value (as a global)
-- and a function to get a new one (respecting the defined range).
policyTypeHelpers :: ModSymbols -> UsedSymbols -> [Definition]
policyTypeHelpers ms us = concatMap (typeHelpers.typeDecl ms) $ usedTypes us
  where
    typeHelpers :: (ModName, TypeDecl QSym) -> [Definition]
    typeHelpers (mn, TypeDecl _ nm (TDTInt _ mrange)) =
      [cunit|
        typename uint32_t $id:globalNm = 0;

        typename uint32_t $id:(typeGenFuncName qualifiedName)($ty:contextType* $id:contextArgName) {
          $id:contextArgName->cached = false;
          typename uint32_t newval = $id:globalNm;
          $id:globalNm = $exp:updateVal;
          return newval;
        }|]
      where
        qualifiedName = resolveQSym ms mn nm
        globalNm :: String
        globalNm = typeUsedGlobalName qualifiedName

        updateVal :: Exp
        updateVal = case mrange of
                      Nothing -> [cexp|$id:globalNm + 1|]
                      Just count -> [cexp|($id:globalNm + 1) % $exp:count|]

typeUsedGlobalName, typeGenFuncName :: QSym -> String
typeUsedGlobalName qs = typeName qs ++ "_next_fresh"
typeGenFuncName qs = typeName qs ++ "_generator"

-- Policy rules contain clauses like:
--
--    g (foo = {X,Y}, bar = z -> baz = z)
--
-- Here, foo, bar and baz are either: "code", "env", or a name specified in the
-- opgroup declaration for g.  That opgroup declaration explains which tag set,
-- during policy evaluation, corresponds to the particular name.
--
-- We process the opgroup declarations into a convenient data structure for
-- looking up this information.  For each opgroup, we build two association
-- lists mapping QSyms (the names used in the policy) to a "TagSpec", which are
-- fixed names for the inputs of policy evaluation.  One list is for the LHS of
-- rules in this opgroup, and the other is for the RHS.
--
-- Note that "code" and "env" don't appear in these lists as qsyms - this
-- structure stores only the names that appear in opgroup declarations.
data OpGroupNames = OGNames {ognPats :: [(QSym,TagSpec)],
                             ognExps :: [(QSym,TagSpec)]}

-- This maps the name of an opgroup to its OpGroupNames structure.
type OpGroupMap = M.Map QSym OpGroupNames

buildOGMap :: ModSymbols -> UsedSymbols -> OpGroupMap
buildOGMap ms us = foldl' (flip $ uncurry M.insert)
                            M.empty (map declNames opGroupDecls)
  where
    opGroupDecls :: [(ModName, GroupDecl [ISA] QSym)]
    opGroupDecls = map (groupDecl ms) $ usedGroups us

    declNames :: (ModName, GroupDecl [ISA] QSym) -> (QSym, OpGroupNames)
    declNames (mn, GroupDecl _ groupNm leftParams rightParams _) =
      (qualifyQSym mn groupNm,
       OGNames {ognPats = map paramNames leftParams,
                ognExps = map paramNames rightParams})

    paramNames :: GroupParam QSym -> (QSym,TagSpec)
    paramNames (GroupParam _ ts qs) = (qs,ts)

-- patOperandLookup and expOperandLookup do the actual work of taking a QSym
-- that appears in a rule on the left hand of an equality and turning it into a
-- C identifier.  For patterns, this will be the name of a pointer to the tag
-- set being examined.  For rules, it is the index into the array (NOTE: this is
-- a macro, not a variable name).
--
-- Args:
--  - The opGroupMap (computed by buildOGMap)
--  - The opgroup's name
--  - the operand's name
patOperandLookup :: OpGroupMap -> QSym -> QSym -> Maybe String
patOperandLookup _ _ (QVar ["env"])  = Just pcArgName
patOperandLookup _ _ (QVar ["code"]) = Just ciArgName
patOperandLookup ogMap og operand = do
  ogNames <- M.lookup og ogMap
  tspec <- lookup operand $ ognPats ogNames
  case tspec of
    RS1 -> return op1ArgName
    RS2 -> return op2ArgName
    RS3 -> return op3ArgName
    Mem -> return memArgName
    Csr -> return op2ArgName
    _ -> error $ "Internal error: " ++ show tspec
              ++ " in patOperandLookup (CJC didn't know what to do here)."

expOperandLookup :: OpGroupMap -> QSym -> QSym -> Maybe String
expOperandLookup _ _ (QVar ["env"])  = Just resultsPC
expOperandLookup _ _ (QVar ["code"]) = Nothing
expOperandLookup ogMap og operand    = do
  ogNames <- M.lookup og ogMap
  tspec <- lookup operand $ ognExps ogNames
  case tspec of
    RD -> Just resultsRD
    Mem -> Just resultsRD
    Csr -> Just resultsCSR
    _ -> Nothing

-- This function checks that the top-level declaration has the form
--    gp_1 ^ ... ^ gp_n ^ (lp_1 & ... & lp_k)
-- Where each gp_i is a "global" policy (like the loader policy) and each lp_i
-- is a normal policy like (like rwx or cfi).
--
-- It takes a PolicyEx and returns two lists - the global policies
-- and the local policies, in that order.
--
-- This should really return a maybe, instead of erroring
topPolicyPieces :: ModSymbols -> ModName -> PolicyEx QSym
                -> ([(ModName, PolicyDecl QSym)],[(ModName,PolicyDecl QSym)])
topPolicyPieces ms topMod pEx =
  case topPolicyVars pEx of
    Nothing -> error tppError
    Just pr -> pr
  where
    topPolicyVars :: PolicyEx QSym
                  -> Maybe ([(ModName, PolicyDecl QSym)],[(ModName,PolicyDecl QSym)])
    topPolicyVars (PEVar _ x) =
      case getPolicy ms topMod x of
        (modN, p@(PolicyDecl _ PLGlobal _ _)) -> Just ([(modN,p)],[])
        (modN, p@(PolicyDecl _ PLLocal _ _))  -> Just ([],[(modN,p)])
        _ -> Nothing
    topPolicyVars (PECompPriority _ p1 p2) =
      case (gTop p1,topPolicyVars p2) of
        (Just gp1, Just (gp2,lp2)) -> Just (gp1 ++ gp2,lp2)
        _ -> Nothing
    topPolicyVars (PECompModule _ p1 p2) =
      case (lTop p1,lTop p2) of
        (Just lp1,Just lp2) -> Just ([],lp1 ++ lp2)
        _ -> Nothing
    topPolicyVars _ = Nothing

    gTop :: PolicyEx QSym -> Maybe [(ModName, PolicyDecl QSym)]
    gTop (PEVar _ x) =
      case getPolicy ms topMod x of
        (modN, p@(PolicyDecl _ PLGlobal _ _)) -> Just [(modN,p)]
        (modN, (PolicyDecl _ PLLocal _ _)) -> Nothing
        _ -> Nothing
    gTop (PECompPriority _ p1 p2) =
       case (gTop p1, gTop p2) of
         (Just gp1, Just gp2) -> Just (gp1 ++ gp2)
         _ -> Nothing
    gTop _ = Nothing

    lTop :: PolicyEx QSym -> Maybe [(ModName, PolicyDecl QSym)]
    lTop (PEVar _ x) =
      case getPolicy ms topMod x of
        (modN, p@(PolicyDecl _ PLLocal _ _)) -> Just [(modN,p)]
        (modN, (PolicyDecl _ PLGlobal _ _)) -> Nothing
        _ -> Nothing
    lTop (PECompModule _ p1 p2) =
       case (lTop p1, lTop p2) of
         (Just lp1, Just lp2) -> Just (lp1 ++ lp2)
         _ -> Nothing
    lTop _ = Nothing

    tppError :: String
    tppError = "Error: top-level policy must have the form:\n\n"
            ++ "   gp_1 ^ ... ^ gp_n ^ (lp_1 & ... & lp_k)\n\n"
            ++ "Where each gp_i is a \"global\" policy name (like loader) and "
            ++ "each lp_i is a \"normal\" policy name (like rwx or cfi)"


-- The policy mask is used to "hide" the irrelevant parts of a tag set, which is
-- sometimes useful for efficient tag set operations.
--
-- The mask has the same type as a tag set's array.  If it is bitwise "or"ed
-- with that array, only the tags declared in the relevant module remain.  This
-- includes data arguments.
policyMaskName :: PolicyDecl QSym -> String
policyMaskName (PolicyDecl _ _ n _) =
  "policy_mask_" ++ (unqualSymStr n)

-- The op-group mask is used to "hide" the op group bits in a tag set, which is
-- sometimes needed when copying tags within a rule
ogMaskName :: String
ogMaskName = "og_mask"

policyMask :: ModSymbols -> UsedSymbols -> TagInfo -> (ModName, PolicyDecl QSym) -> Definition
policyMask _ _ (TagInfo {tiArrayLength}) (mn, pd@(PolicyDecl _ PLGlobal _ _)) =
  [cedecl|const typename uint32_t $id:(policyMaskName pd)[META_SET_WORDS] = $init:initializer;|]
  where
    initializer :: Initializer
    initializer = CompoundInitializer (replicate (fromIntegral tiArrayLength) allOnes) noLoc

    allOnes :: (Maybe Designation,Initializer)
    allOnes = (Nothing, ExpInitializer [cexp|0xFFFFFFFF|] noLoc)
policyMask ms us tinfo (mn, pd@(PolicyDecl _ PLLocal pnm _)) =
  [cedecl|const typename uint32_t $id:(policyMaskName pd)[META_SET_WORDS] = $init:initializer;|]
  where
    initializer :: Initializer
    initializer = CompoundInitializer (map bi fieldMasks) noLoc
      where
        bi :: Exp -> (Maybe Designation,Initializer)
        bi e = (Nothing,ExpInitializer e noLoc)

    -- We construct fieldMasks from the actual tags
    -- declared in this module (declaredTags)
    fieldMasks :: [Exp]
    fieldMasks = fixedTagSetFields tinfo $
         (map (\(TagDecl _ nm args) ->
                     (nm, replicate (length args) [cexp|0xFFFFFFFF|]))
              qualifiedTags)

    qualifiedTags :: [TagDecl QSym]
    qualifiedTags = map (fmap (resolveQSym ms mn)) declaredTags

    declaredTags :: [TagDecl QSym]
    declaredTags = moduleTags ms us mn

    -- We construct fieldMasks from
    -- opgroups, since they count as "relevant" to this policy.
ogMasks :: TagInfo -> [Definition]
ogMasks tinfo =  [cedecl|const typename uint32_t $id:(ogMaskName)[META_SET_WORDS] = $init:initializer;|]:[]
  where
    initializer :: Initializer
    initializer = CompoundInitializer (map bi ogMask) noLoc
      where
        bi :: Exp -> (Maybe Designation,Initializer)
        bi e = (Nothing,ExpInitializer e noLoc)
        ogMask :: [Exp]
        ogMask = fixedTagSetFields tinfo (map (,[]) $ tiGroupNames tinfo)
    



-- Given a collection of tags, this computes the corresponding tag set array, as
-- a lit of C expressions.  Tags are provided as a pair (QSym,[Exp]), with the
-- QSym identifying the tag and the [Exp] carrying the arguments, if any.  The
-- arguments might be fixed values or might be variables that are in-scope in
-- the context where this is called (which is why we don't just use Word32
-- here).
fixedTagSetFields :: TagInfo -> [(QSym,[Exp])] -> [Exp]
fixedTagSetFields (TagInfo {tiTagBitPositions,tiTagArgInfo,tiArrayLength}) tags =
  map eToExp $ elems $ runSTArray $ do
    array <- newArray (0,tiArrayLength-1) (Left 0)
    mapM_ (addTag array) tags
    return array
  where
    eToExp :: Either Word32 Exp -> Exp
    eToExp e = case e of
                 Left w -> [cexp|$int:w|]
                 Right c -> c
    
    addTag :: STArray s Word32 (Either Word32 Exp) -> (QSym,[Exp]) -> ST s ()
    addTag array (tName,tArgs) = do 
      ew <- readArray array bitWordIndex
      case ew of
        Left w -> writeArray array bitWordIndex (Left $ w .|. (2 ^ bitWordBit))
        Right _ -> error $ "Internal error: conflict between bit tag and "
                        ++ "tag arg in fixedTagSetFields."
      mapM_ (\(p,val) -> writeArray array p (Right val)) argPairs
      where
        overallBitPosition :: Word32
        overallBitPosition =
          case M.lookup tName tiTagBitPositions of
            Nothing -> error $ "Unknown tag " ++ tagString tName
                            ++ " in fixed tag set bit computation."
            Just i -> i

        tagArgPositions :: [Word32]
        tagArgPositions =
          case M.lookup tName tiTagArgInfo of
            Nothing -> error $ "Unknown tag " ++ tagString tName
                          ++ " in fixed tag set arg computation."
            Just ws -> map fst ws

        bitWordIndex, bitWordBit :: Word32
        bitWordIndex = div overallBitPosition 32
        bitWordBit = mod overallBitPosition 32

        argPairs :: [(Word32,Exp)]
        argPairs =
          if (length tArgs) /= (length tagArgPositions) then
            error $ "Invalid number of arguments to " ++ tagString tName
          else
            zip tagArgPositions tArgs
    
-- Translates the "main" policy.
--
-- It returns several definitions, which must be included in the C file in the
-- order they are returned.  There will be one top-level "eval_policy" function,
-- which calls a bunch of helper functions.  There will one helper function per
-- named policy in the top-level composition.
--
-- Each generated function takes the arguments defined in "policyInputParams".
-- The first 6 of these are pointers to tag sets that describe the current state
-- of the system.
--
-- The last two inputs are output arguments: one is a tag set array, where
-- result tag sets are stored.  The other is a bool array, where we track
-- whether the rule chosen provided an updated tag set for each output position
-- (i.e., whether this function has modified each tag set in the previous
-- array).  eval_policy assumes that the provided meta_set array begins with
-- empty tag sets, and the individual policy evaluation functions assume that
-- these sets contain no tags from the policy in question.
--
-- The eval_policy functions do not check if the computed tag sets
-- already exist or do any canonization - that is the responsibility of the
-- caller.
translateTopPolicy :: Bool -> Bool -> ModSymbols -> UsedSymbols -> TagInfo -> ModName
                   -> Maybe (PolicyDecl QSym) -> [Definition]
translateTopPolicy _debug _profile _ms _us _ _ Nothing =
   [ [cedecl|
        int eval_policy ($params:policyInputParams) {
          return $id:policySuccessName;
        }|] ]
translateTopPolicy debug profile ms us tinfo topMod (Just (PolicyDecl _ _ _ p)) =
  ogMasks tinfo ++ policyMasks ++ evalHelpers ++ 
    [ [cedecl|
        int eval_policy ($params:policyInputParams) {
          int $id:resultVar = $id:policyIFailName;

          $stms:topDebugStms

          $stms:globalChecks
          $stms:localChecks

          // any policy failure will result in an early return.
          return $id:policySuccessName;
        }|] ]
  where
    globalPolicies, localPolicies :: [(ModName, PolicyDecl QSym)]
    (globalPolicies,localPolicies) = topPolicyPieces ms topMod p

    policyMasks :: [Definition]
    policyMasks = map (policyMask ms us tinfo) (globalPolicies ++ localPolicies)

    resultVar :: String
    resultVar = "evalResult"

    ogMap :: OpGroupMap
    ogMap = buildOGMap ms us

    evalHelpers :: [Definition]
    evalHelpers =
      map (policyEval debug profile ms ogMap tinfo) (globalPolicies ++ localPolicies)

    -- The results from global and local policy evaluation are handled
    -- differently, implementing the different semantics of ^ and &.
    --
    -- Explicit failure: In both cases, an explicit failure is counted as an
    -- explicit failure of the whole policy.
    --
    -- Success: For a global policy, success ends policy evaluation successfully
    -- (e.g., we do not run the normal policies on the special bits of the
    -- loader).  For a local policy, success adds to the computed result tag
    -- sets, but execution continues with the next composed local policy.
    --
    -- Implicit failure: For a global policy, implicit failure is the usual case
    -- and policy execution continues.  This occurs whenever we aren't in
    -- "special" code like the loader.  For a local policy, implicit failure
    -- causes failure of the top-level policy: each policy composed with & must
    -- have a rule for every instruction.
    globalChecks :: [Stm]
    globalChecks = concatMap globalCheck globalPolicies
      where
        globalCheck :: (ModName, PolicyDecl QSym) -> [Stm]
        globalCheck (mn, pol) = [cstms|
          $stms:(policyDebugStmsPre pol);
          $id:resultVar = $id:(singlePolicyEvalName pol)($args:policyInputArgs);
          $stms:(policyDebugStmsPost pol);
          if($id:resultVar == $id:policySuccessName) {
            return $id:policySuccessName;
          } else if($id:resultVar == $id:policyEFailName) {
            return $id:policyEFailName;
          }
        |]


    localChecks :: [Stm]
    localChecks = concatMap localCheck localPolicies
      where
        localCheck :: (ModName, PolicyDecl QSym) -> [Stm]
        localCheck (mn, pol) = [cstms|
          $stms:(policyDebugStmsPre pol);
          $id:resultVar = $id:(singlePolicyEvalName pol)($args:policyInputArgs);
          $stms:(policyDebugStmsPost pol);
          if($id:resultVar != $id:policySuccessName) {
            return $id:resultVar;
          }
        |]

    topDebugStms :: [Stm]
    topDebugStms =
       if debug then
         [cstms|
           debug_msg($id:contextArgName, "Policy input:\n");
           debug_operands($id:contextArgName, $id:operandsArgName);
           debug_msg($id:contextArgName, "\n");
         |]
       else []

    policyDebugStmsPre :: PolicyDecl QSym -> [Stm]
    policyDebugStmsPre pol =
      if debug then
        [cstms|debug_msg($id:contextArgName, $string:("\nEvaluating: " ++ (policyDotName pol) ++ "\n"));|]
      else []
    
    policyDebugStmsPost :: PolicyDecl QSym -> [Stm]
    policyDebugStmsPost pol =
      if debug then
        [cstms|debug_msg($id:contextArgName, $string:("Result " ++ (policyDotName pol) ++ ": "));
               debug_status($id:contextArgName, $id:resultVar);
               debug_msg($id:contextArgName, "\n");
               debug_results($id:contextArgName, $id:resultsArgName);|]
      else []

    
singlePolicyEvalName :: PolicyDecl QSym -> String
singlePolicyEvalName (PolicyDecl _ _ n _) =
  "policy_eval_" ++ (unqualSymStr n)

-- Builds a function that evaluates a specific policy.  Intended to be called
-- individually on the named, composed components of the top-level policy.
--
-- The final two parameter to the generated C function are "output" arguments.
--
-- The first is an array of tag sets, where the results of policy evaluation may
-- be stored.  The generated function assumes these sets do not yet contain any
-- of this policy's tags.  If policy evaluation is successful, the sets will be
-- modified.
--
-- The second is an array of bools, of the same length as the previous array.
-- Each bool records whether this policy provided an updated version of the
-- corresponding tag set.
--
-- If this is a normal policy (like RWX) this function will add only tags from
-- the RWX module to the output sets.  Policies are evaluated sequentially, so
-- there may already be tags from other policies on the lists, but they don't
-- affect this policy and won't be modified.  If it's a global policy, the
-- sets might be completely reworked.
policyEval :: Bool -> Bool -> ModSymbols -> OpGroupMap
           -> TagInfo -> (ModName, PolicyDecl QSym)
           -> Definition
policyEval debug _profile ms ogMap tagInfo (modN, pd@(PolicyDecl _ _ _ pEx)) =
  [cedecl|
       int $id:(singlePolicyEvalName pd) ($params:policyInputParams) {
         int $id:resultVar = $id:policyIFailName;

         $stm:body

         // Each result clause returns, so if we reach here it is an implicit failure.
         return $id:policyIFailName;
       } |]
  where
   resultVar :: String
   resultVar = "policyResult"

   body :: Stm
   body = Block p' noLoc
     where
       p' = translatePolicy debug ms ogMap pd tagInfo resultVar modN pEx

-- Takes as arguments:
--  - whether to print debug info
--  - The global symbol table
--  - Info about opgroups
--  - the policy declaration (used for policy name)
--  - the tag encoding list
--  - a variable name x where the result should be stored
--  - The enclosing module name
--  - the policy itself.
--
-- Results: A [BlockItem].  This is a list of statements that implements the
-- policy and is intended to be part of the eval_policy function.  These
-- statements will set x to one of policySuccessName, policyIFailName, or
-- policyEFailName.  In the first case, the function will add the appropriate
-- tags from this policy to the array of result tag lists.
translatePolicy :: Bool -> ModSymbols -> OpGroupMap ->  PolicyDecl QSym
                -> TagInfo -> String -> ModName
                -> PolicyEx QSym -> [BlockItem]
translatePolicy dbg ms ogMap pd tagInfo pass modN (PEVar _ x) = let (modN, (PolicyDecl _ _ _ p)) = getPolicy ms modN x in
  translatePolicy dbg ms ogMap pd tagInfo pass modN p
translatePolicy dbg ms ogMap pd tagInfo pass modN (PECompExclusive _ p1 p2) =
  [citems|$items:p1';
          if ($id:pass == $id:policyIFailName) {
            $items:p2'
          }|]
  where
     p1' = translatePolicy dbg ms ogMap pd tagInfo pass modN p1
     p2' = translatePolicy dbg ms ogMap pd tagInfo pass modN p2
translatePolicy dbg ms ogMap pd tagInfo pass modN (PECompPriority l p1 p2) =
  translatePolicy dbg ms ogMap pd tagInfo pass modN (PECompExclusive l p1 p2)
translatePolicy _ _ _ _ _ pass _ (PENoChecks _) =
  [citems|$id:pass = $id:policySuccessName;
          return $id:pass;|]
translatePolicy _ _ _ _ _ _ _ (PECompModule _ _p1 _p2) =
  error "Unsupported: PECompModule in translatePolicy"
translatePolicy dbg ms ogMap pd tagInfo pass modN (PERule _ rc@(RuleClause _ ogrp rpat rres)) =
  [citems|
       if(ms_contains($id:ciArgName,$id:(tagName (qualifiedOpGrpMacro)))) {
         $id:pass = $exp:patExp;
         if ($id:pass) {
           $stms:debugPrints
           $stms:ruleEvalLog
           $items:ruleResult
         } else {
           $id:pass = $id:policyIFailName;
         }
       }
   |]
  where
    qualifiedOpGrp :: QSym
    qualifiedOpGrp = resolveQSym ms modN ogrp 
    qualifiedOpGrpMacro :: QSym
    qualifiedOpGrpMacro = qualifyQSym (moduleForQSym ms modN ogrp) $ groupPrefix ogrp
    mask = policyMaskName pd
    patExp :: Exp
    boundNames :: [(QSym,Exp)]
    (patExp,boundNames) = translatePatterns ms modN mask tagInfo oprLookup rpat
      where
        oprLookup :: QSym -> Maybe String
        oprLookup = patOperandLookup ogMap qualifiedOpGrp

    ruleResult :: [BlockItem]
    ruleResult = translateRuleResult ms modN mask oprLookup boundNames tagInfo pass rres
      where
        oprLookup :: QSym -> Maybe String
        oprLookup = expOperandLookup ogMap qualifiedOpGrp

    debugPrints :: [Stm]
    debugPrints =
      if dbg then
        [cstms|
          debug_msg($id:contextArgName, $string:("rule match: " ++ (compactShowRule rc) ++ "\n"));
        |]
      else []
    ruleEvalLog :: [Stm]
    ruleEvalLog =
        [cstms|
          logRuleEval($string:(qualifiedShowRule pd rc));
        |]

-- Args:
--   - The policy mask
--   - The tag encoding list
--   - A function that takes operand names to C identifiers
--   - patterns
-- Results:
--   - A C expression that evaluates to "1" if the pattern
--     matches and "0" otherwise
--   - A map from policy variable names to the corresponding C expression.
--
-- XXX This function has become a bit crufty as features have been added, and
-- could do with a re-write.
translatePatterns :: ModSymbols
                  -> ModName
                  -> String
                  -> TagInfo
                  -> (QSym -> Maybe String)
                  -> [BoundGroupPat QSym]
                  -> (Exp,[(QSym,Exp)])
translatePatterns ms mn mask tagInfo ogmap pats = foldl' patternAcc ([cexp|1|],defaultEnv) pats
  where
    -- default binding for the env var, syntax sugar to allow it to be used in result
    -- without having been defined
    defaultEnv :: [(QSym,Exp)]
    defaultEnv = [(QVar ["env"],[cexp|$id:pcArgName|])]
    patternAcc :: (Exp,[(QSym,Exp)]) -> BoundGroupPat QSym
    
               -> (Exp,[(QSym,Exp)])
    patternAcc (e,ids) pat =
      foldl' addBindings ([cexp|$exp:e' && $exp:e|],ids) ids'
      where
        (e',ids') = translateBGPat pat

    -- This adds a pattern bindings to an existing set.  We allow non-linear
    -- bindings for tag arguments, so we check names as they are added to see if
    -- a binding already exists, and compute an expresion that accumulates all
    -- the implied equality constraints.
    addBindings :: (Exp,[(QSym,Exp)]) -> (QSym,Exp) -> (Exp,[(QSym,Exp)])
    addBindings (es,bnds) (qs,e) =
      case lookup qs bnds of
        Nothing -> (es,(qs,e):bnds)
        Just e' -> ([cexp|($exp:e == $exp:e') && $exp:es|],bnds)
    
    -- Give this a BoundGroupPat, and it generates (a) an expression that is
    -- true iff the pattern matches*, and (b) a list associating the names bound
    -- in this pattern with the C name for the relevant tag set.
    --
    --  *This does NOT account for non-linear pattern matching constraints in
    -- tag arguments, which are handled by the caller.
    translateBGPat :: BoundGroupPat QSym -> (Exp,[(QSym,Exp)])
    translateBGPat (BoundGroupPat loc tsID pat) =
      case ogmap tsID of
        Nothing -> error $ "No opgroup binding for operand " ++ show tsID
                        ++ " at " ++ show loc
        Just tsName -> translateTSPat tsID tsName (fmap (resolveQSym ms mn) pat)
        
    -- Give this the name of a pointer to a meta_set_t, and a TagSetPattern, and
    -- it will return (a) an expression that is true iff the pattern matches,
    -- and (b) a list of names that the pattern binds to the input tag set.
    translateTSPat :: QSym -> String -> TagSetPat QSym -> (Exp,[(QSym,Exp)])
    translateTSPat tsID ts  (TSPAny _) = ([cexp|1|],[(tsID,[cexp|$id:ts|])])
--    translateTSPat ts (TSPVar _ nm) = ([cexp|1|],[(nm,[cexp|$id:ts|])])
--    translateTSPat ts (TSPVarSet _ nm pat) =
--      let (e,nms) = translateTSPat ts pat in (e,(nm,[cexp|$id:ts|]):nms)
    translateTSPat tsID ts (TSPAtLeast _ tes) =
      foldl' (\(e1,bnds1) (e2,bnds2) -> (BinOp Land e1 e2 noLoc,bnds1++bnds2))
             ([cexp|1|],[(tsID,[cexp|$id:ts|])])
             (map checkAtLeast tes)
      where
        checkAtLeast :: TagEx QSym -> (Exp,[(QSym,Exp)])
        checkAtLeast (TagEx _ t) = checkContains ts t
        checkAtLeast (TagPlusEx _ t) = checkContains ts t
        checkAtLeast (TagMinusEx _ t) = checkAbsent ts t        
    translateTSPat tsID ts (TSPExact _ tags) =
      (foldl' (\e1 e2 -> [cexp|$exp:e1 && $exp:e2|])
              [cexp|1|]
              (map checkField exactFields),
              [(tsID,[cexp|$id:ts|])] ++ argBindings)
      where
        -- Here we cheat a bit!  We call "fixedTagSetFields", but we only care
        -- about the bitfield parts of its output, not the tag arguments.  We
        -- pass in 0 for the arguments, just to satisfy the preconditions of
        -- that function.
        --
        -- Separately, we use the "argBinding" function (which is also used in
        -- the at least case) to capture any argument bindings and any equality
        -- contraints created by non-linear pattern matching.
        
        tagQSyms :: [(QSym,[Exp])]
        tagQSyms = map (\(Tag _ qs args) -> (qs,map (\_ -> [cexp|0|]) args)) tags

        argBindings :: [(QSym,Exp)]
        argBindings = concatMap tagBindings tags
          where
            tagBindings :: Tag QSym -> [(QSym,Exp)]
            tagBindings (Tag _ qs args) =
              case M.lookup qs (tiTagArgInfo tagInfo) of
                Nothing -> error $ "Internal error: unknown tag " ++ tagString qs
                                ++ " in translateTSPat's arg lookup."
                Just argInfo ->
                  mapMaybe (argBinding ts) $
                    zipWith (\f (w,_) -> (w,f)) args argInfo

        exactFields :: [(Word32,Exp)]
        exactFields = zip [0..(tiNumBitFields tagInfo - 1)]
                          (fixedTagSetFields tagInfo tagQSyms)

        checkField :: (Word32,Exp) -> Exp
        checkField (idx,val) =
          [cexp|((($id:ts->tags)[$exp:idx]) & $id:mask[$idx]) == $exp:val|]


    -- These build a C boolean expression that checks whether a tag set (first
    -- argument) contains or does not contain a particular tag (second
    -- argument).  They also record any tag argument bindings.  Tag argument in
    -- patterns must be either "_" or a variable name.
    checkContains, checkAbsent :: String -> Tag QSym -> (Exp,[(QSym,Exp)])
    checkContains ts (Tag _ qn args) =
      case M.lookup qn (tiTagArgInfo tagInfo) of
        Nothing -> error $ "Internal error: tag " ++ tagString qn
                        ++ " missing from tagArgInfo map in checkContains."
        Just argInfo ->
           ([cexp|ms_contains($id:ts,$id:(tagName qn))|],
            mapMaybe (argBinding ts) $
              zipWith (\(idx,_) bnd -> (idx,bnd)) argInfo args)
    checkAbsent ts (Tag _ qn _) =
      ([cexp|(!(ms_contains($id:ts,$id:(tagName qn))))|],[])

    argBinding :: String -> (Word32,TagField QSym) -> Maybe (QSym,Exp)
    argBinding _ (_,TFNew p) =
      error $ "Illegal: Attempt to create \"new\" tag data in pattern "
           ++ "at " ++ show p
    argBinding ts (idx,TFVar _ v) = Just (v,[cexp|($id:ts -> tags)[$int:idx]|])
    argBinding _ (_,TFAny _) = Nothing
    argBinding _ (_,TFTag p _) =
      error $ "Unsupported: complex tag argument at " ++ show p

-- Arguments:
--   - The name of the policy mask
--   - A function that associates operands to C macros based on the opgroup.
--   - A mapping from policy variables to C expressions.
--   - The name of the result variable
--   - The result to be translated.
-- Results:
--   - A series of statements.  These will have the result of returning
--     policyEFail from the current function if the rule result is failure.  If
--     the rule result is to generate new tags, they'll assign new tags into the
--     result array and return policySuccess from the current function.
translateRuleResult :: ModSymbols -> ModName -> String -> (QSym -> Maybe String) -> [(QSym,Exp)]
                    -> TagInfo -> String -> RuleResult QSym -> [BlockItem]
-- handle the explicit failure case by printing a message and return failure                    
translateRuleResult _ _ _ _ _ _ _ (RRFail _ msg) = [citems|
                                                  $id:contextArgName->fail_msg = $string:msg;
                                                  return $id:policyEFailName;|]
translateRuleResult ms topMod mask ogMap varMap tagInfo pass (RRUpdate _ updates) =
     (concatMap (translateBoundGroupEx ms topMod mask ogMap varMap tagInfo pass) updates)
  ++ [ [citem|return $id:pass;|] ]

-- Arguments:
--   - The name of the policy mask
--   - A function that associates operands to C macros based on the opgroup.
--   - A mapping from policy tag set variables to C tag set variables.
--   - The name of the result variable     
--   - The BoundGroupEx to be translated.
-- Results:
--  - A series of statements that compute a revised tag set and store it into
--    the place in the results array indicated by the LHS of the BoundGroupEx
--
-- This relies on a recursive helper function that descends through the tag set
-- expression and roughly implements the judgment from the pdf.
translateBoundGroupEx :: ModSymbols -> ModName -> String -> (QSym -> Maybe String) -> [(QSym,Exp)]
                      -> TagInfo -> String -> BoundGroupEx QSym -> [BlockItem]
translateBoundGroupEx ms mn mask ogMap varMap tagInfo pass (BoundGroupEx loc opr tse) =
  case ogMap opr of
    Nothing -> error $ "Rule result uses invalid operand " ++ show opr
                        ++ "(" ++ show loc ++ ")"
    Just resPositionName ->
      -- Note that here we remove any opgroup tags from the computed tag sets.
      -- This kind of makes sense - if you write to code memory, you want to
      -- wipe the opgroups because the new value may not be the same kind of
      -- instruction, or any instruction at all.  And in general users don't
      -- know that opgroups are in tag sets, so they shouldn't expect them to be
      -- preserved.  This may be wrong, though, for "global" policies like the
      -- loader.
      [citems|
        { typename meta_set_t $id:topVar;
          $items:evalItems;
          for(int i = 0; i < META_SET_BITFIELDS; i++) {
              ($id:resPositionName)->tags[i] |= ($id:topVar.tags[i] & $id:mask[i]);
          }
          for(int i = META_SET_BITFIELDS; i < META_SET_WORDS; i++) {
            if($id:mask[i]) {
              $id:resPositionName->tags[i] =
                $id:topVar.tags[i];
            }
          }
          $id:resHasResult = true;
          
        }
      |]
      where
        evalItems :: [BlockItem]
        (evalItems,_) = translateTagSetEx ms mn vars topVar varMap tagInfo tse

        resHasResult :: String
        resHasResult = resPositionName ++ "Result"
        
        topVar :: String
        topVar = "tseEvalVar0"

        vars :: [String]
        vars = zipWith (++) (repeat "tseEvalVar") (map show ([1..] :: [Int]))


-- translateTagSetEx does the work of actually evaluating a TagSetEx.  It
-- recursively descends down the structure of a TagSetEx, accumulating the
-- result into a stack-allocated tag set.
--
-- Arguments:
--
--   - a supply of fresh variables.
--   - a variable name x, of type meta_set_t.  This is the stack allocated
--     meta_set into which we will accumulate the result.  The contents of x in
--     positions not relevant to this policy may be garbage, and can not be
--     relied on.
--   - A mapping from policy tag set variables to C tag set variables.
--   - The TagSetEx to evaluate
--
-- Results:
--   - A [BlockItem].  These statements compute the result of the input
--     TagSetEx, and store it in x.  Only the tags that are relevant to this
--     policy, according to the policy mask, should be used from x.
--   - The remaining variables from the fresh variable supply.
--
translateTagSetEx :: ModSymbols -> ModName -> [String] -> String -> [(QSym,Exp)] -> TagInfo
                  -> TagSetEx QSym -> ([BlockItem],[String])
translateTagSetEx _ _ [] _ _ _ _ =
  error "Internal error: translateTagSetEx exhausted its fresh name supply."
translateTagSetEx _ _ (_:[]) _ _ _ _ =
  error "Internal error: translateTagSetEx exhausted its fresh name supply."
translateTagSetEx _ _ vars resVar varMap _ (TSEVar loc y) =
  case lookup y varMap of
    Nothing -> error $ "Rule result uses unbound variable " ++ show y
                    ++ "(" ++ show loc ++ ")"
    Just ts -> ([citems|memcpy(&$id:resVar,$exp:ts,sizeof(typename meta_set_t));|],
                vars)
translateTagSetEx ms mn vars resVar varMap tagInfo (TSEExact _ tags) =
  (map (\(idx,val) -> [citem|$id:resVar.tags[$exp:idx] = $exp:val;|]) exactFields,
   vars)
  where
    exactFields :: [(Word32,Exp)]
    exactFields = zip [0..] (fixedTagSetFields tagInfo tagQSyms)

    tagQSyms :: [(QSym,[Exp])]
    tagQSyms = map (\(Tag _ qs tfs) -> (qs,map (buildArgField ms mn varMap)
                                             $ zip tfs (argInfo qs)))
                   qualifiedTags
      where
        argInfo :: QSym -> [(Word32,TypeDecl QSym)]
        argInfo qs =
          case M.lookup qs (tiTagArgInfo tagInfo) of
            Nothing -> error $ "Internal error: unknown tag " ++ tagString qs
                            ++ " in translateTagSetEx's arg lookup."
            Just ai -> ai
    qualifiedTags :: [Tag QSym]
    qualifiedTags = map (fmap (resolveQSym ms mn)) tags

        
translateTagSetEx ms mn vars resVar varMap tagInfo (TSEModify _ tse mods) =
  ([citems|
     $items:tseStmts;
     $items:modStmts;
   |],
   vars')
  where
    tseStmts :: [BlockItem]
    vars' :: [String]
    (tseStmts,vars') = translateTagSetEx ms mn vars resVar varMap tagInfo tse

    qualifiedMods :: [TagEx QSym]
    qualifiedMods = map (fmap (resolveQSym ms mn)) mods
    
    modStmts :: [BlockItem]
    modStmts = concatMap modStmt qualifiedMods

    modStmt :: TagEx QSym -> [BlockItem]
    modStmt (TagEx _ t) = tsAdd t
    modStmt (TagPlusEx _ t) = tsAdd t
    modStmt (TagMinusEx _ t) = tsRemove t

    tsAdd :: Tag QSym -> [BlockItem]
    tsAdd (Tag _ qn args) =
      case M.lookup qn (tiTagArgInfo tagInfo) of
        Nothing -> error $ "Internal error: unknown tag " ++ tagString qn
                ++ " in translateTagSetEx's arg lookup."
        Just ainfo ->
            [citem|ms_bit_add(&$id:resVar,$id:(tagName qn));|]
          : map (\(tf,(loc,typ)) ->
                   [citem|$id:resVar.tags[$exp:loc] =
                      $exp:(buildArgField ms mn varMap (tf,(loc,typ)));|])
                (zip args ainfo)

    tsRemove :: Tag QSym -> [BlockItem]
    tsRemove (Tag _ qn _) =
      case M.lookup qn (tiTagArgInfo tagInfo) of
        Nothing -> error $ "Internal error: unknown tag " ++ tagString qn
                ++ " in translateTagSetEx's arg lookup."
        Just ainfo ->
            [citem|ms_bit_remove(&$id:resVar,$id:(tagName qn));|]
          : map (\(loc,_) -> [citem|$id:resVar.tags[$exp:loc] = 0;|])
                ainfo
translateTagSetEx ms mn (v1:v2:vars) resVar varMap tagInfo (TSEUnion _ tse1 tse2) =
  ([citems|
     $items:tseStmts1;
     typename meta_set_t $id:v1;
     $items:tseStmts2;
     int $id:v2 = ms_union(&$id:resVar, &$id:v1);
     if($id:v2) {handle_panic("Invalid union in translateTagSetEx!\n");}
    |],
   vars'')
  where
    tseStmts1, tseStmts2 :: [BlockItem]
    vars', vars'' :: [String]
    (tseStmts1,vars') = translateTagSetEx ms mn vars resVar varMap tagInfo tse1
    (tseStmts2,vars'') = translateTagSetEx ms mn vars' v1 varMap tagInfo tse2
translateTagSetEx ms mn (v1:vars) resVar varMap tagInfo (TSEIntersect _ tse1 tse2) =
  ([citems|
     $items:tseStmts1;
     typename meta_set_t $id:v1;
     $items:tseStmts2;
     ms_intersection(&$id:resVar, &$id:v1);
    |],
   vars'')
  where
    tseStmts1, tseStmts2 :: [BlockItem]
    vars', vars'' :: [String]
    (tseStmts1,vars') = translateTagSetEx ms mn vars resVar varMap tagInfo tse1
    (tseStmts2,vars'') = translateTagSetEx ms mn vars' v1 varMap tagInfo tse2

--This builds a C expression corresponding to a tag argument field
buildArgField :: ModSymbols -> ModName -> [(QSym,Exp)] -> (TagField QSym,(Word32,TypeDecl QSym)) -> Exp
buildArgField _ _ _ (TFTag sp _,_) = error $
  "Unsupported: complex tag field at " ++ show sp
buildArgField _ _ varMap (TFVar sp v,_) =
  case lookup v varMap of
    Nothing -> error $
      "Undefined variable " ++ tagString v ++ " at " ++ show sp
    Just e -> e
buildArgField ms mn _ (TFNew _,(_,typ)) =
  [cexp|$id:(typeGenFuncName qualifiedType)(ctx)|]
    where
      qualifiedType = resolveQSym ms mn $ qsym typ
      
buildArgField _ _ _ (TFAny sp,_) = error $
  "Illegal: wildcard in tag argument definition at " ++ show sp    

-- top of policy_rule.c
cHeader :: Bool -> Bool -> [String]
cHeader _debug _profile = [ "#include \"policy_meta.h\""
                         , "#include \"policy_rule.h\""
                         , "#include \"policy_meta_set.h\""
                         , "#include <stdbool.h>"
                         , "#include <stdint.h>"
                         , "#include <inttypes.h>"
                         , "#include <limits.h>"
                         , "#include <string.h>"
                         , ""
                         ]


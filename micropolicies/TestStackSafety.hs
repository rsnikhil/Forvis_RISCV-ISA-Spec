{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TupleSections, FlexibleInstances, MultiParamTypeClasses, MultiWayIf #-}

module TestStackSafety where

-- From Haskell libraries
import Control.Lens hiding (elements)

import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace

import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P

-- From /src
import Arch_Defs
import Bit_Utils
import CSR_File
import Encoder
import Forvis_Spec_I
import Forvis_Spec_Instr_Fetch
import GPR_File
import Machine_State
import Memory
import ALU

-- From .
import Gen
import Run_Program_PIPE
import PIPE
import Printing
import Terminal
import MachineLenses
import TestState

-- | Policy Specific generation

noTag = fromExt []
boringTag = fromExt [("stack.Boring", Nothing)]
spTag = fromExt [("stack.SP", Nothing)]
tagH1 = fromExt [("stack.H1", Nothing)] 
tagH2 = fromExt [("stack.H2", Nothing)]  
tagH3 = fromExt [("stack.H3", Nothing)]  
tagR1 = fromExt [("stack.R1", Nothing)]
tagR2 = fromExt [("stack.R2", Nothing)]  
tagR3 = fromExt [("stack.R3", Nothing)]  
stackTag n = fromExt [("stack.Stack"  , Just n)]
pcTag n = fromExt [("stack.PC"  , Just n)]

{-
New memory layout:

Instr Memory

Stack

Data Memory
-}

load_policy = do
  ppol <- load_pipe_policy "stack.main"
  let pplus = PolicyPlus
        { policy = ppol
        , initGPR = noTag
        , initMem = stackTag 0
            -- TODO: Might be better to make it some separate
            -- "Uninitialized" tag?
        , initPC = pcTag 0
        , initNextColor = 1
        , instrLow = 0
        , instrHigh = 100
        , emptyInstTag = noTag
        , dataMemLow = 1000
        , dataMemHigh = 1020  -- Was 40, but that seems like a lot! (8 may be too little!)
        }
  return pplus

genMTag, genGPRTag :: PolicyPlus -> Gen TagSet 
genMTag pplus = frequency [(1, pure boringTag), (1, pure boringTag)]
genGPRTag = genMTag

dataP = const True
codeP = const True

genITag _ = return boringTag

isSecretMP :: Machine_State -> PIPE_State -> TagSet -> Bool
isSecretMP _ _ t = t == boringTag

mkInfo :: Machine_State -> PIPE_State -> ()
mkInfo _ _ = ()

-- | Main

-- The real one
main_test = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1000}
    $ forAllShrink (genVariationTestState pplus genMTag genGPRTag dataP codeP genITag isSecretMP mkInfo)
                   (\ts -> [] ) --shrinkMStatePair pplus mp 
--                   ++ concatMap (shrinkMStatePair pplus) (shrinkMStatePair pplus mp))
    $ \ts -> prop pplus ts

main = main_test

-- | Property

data DescTag = Stack Int
             | Instr
             deriving (Eq, Show)

data StateDesc = SD { pcdepth :: !Int
                    , memdepth :: Map Integer DescTag
                    , stack :: [(Integer, Integer)]
                    , callinstrs :: [Integer]
                    } 

initDesc :: Map Integer DescTag -> [Integer] -> StateDesc
initDesc memlayout calls = SD { pcdepth = 0
                              , memdepth = memlayout
                              , stack = []
                              , callinstrs = calls
                              } 

accessible :: Integer -> StateDesc -> Bool
accessible i sd =
  -- can error
  case memdepth sd Map.! i of
    (Stack n) -> n >= pcdepth sd
    _ -> False

-- TODO: Fix this
sp = 2
next_desc :: RichState -> StateDesc -> RichState -> StateDesc
next_desc s d s'
  | memdepth d Map.! (s ^. ms . fpc) == Instr =
    let isCall = elem (s ^. ms . fpc) (callinstrs d)
        isRet  = ((s' ^. ms . fpc) == 4 + fst (head $ stack d)) &&
                 (Just (snd (head $ stack d)) == (s ^. ms . fgpr . at sp))
        -- Should return Just (memory loc) if it is a write, Nothing otherwise
        isWrite =
          -- TODO: Common definitions from Forvis_Spec_I
          let writeAddr rs1 imm12 =
                let mstate = s ^. ms
                    -- rv   = mstate_rv_read    mstate
                    xlen = mstate_xlen_read  mstate
                    -- Compute effective address
                    rs1_val  = mstate_gpr_read  rs1  mstate    -- address base
                    s_imm12  = sign_extend  12  xlen  imm12
                    eaddr1   = alu_add  xlen  rs1_val  s_imm12
                    -- eaddr2   = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)
                    eaddr2  = eaddr1 -- TODO: Fix eaddr2 above
                in
                  eaddr2
          in
            case fst $ instr_fetch (s ^. ms) of
              Fetch u32 -> case decode_I RV32 u32 of
                Just instr -> case instr of
                  SB _ rs1 imm12 -> Just (writeAddr rs1 imm12)
                  SH _ rs1 imm12 -> Just (writeAddr rs1 imm12)
                  SW _ rs1 imm12 -> Just (writeAddr rs1 imm12)
                  _ -> Nothing
                _ -> Nothing
              -- TODO: SD support
              _ -> Nothing
    in
    d { pcdepth =
          if isCall then
            pcdepth d + 1
          else if isRet then 
            pcdepth d - 1
          else pcdepth d
      , memdepth =
          case isWrite of
            Just loc -> Map.insert loc (Stack (pcdepth d)) (memdepth d)
            _ -> memdepth d
      , stack =
          if isCall then
            (s ^. ms . fpc, fromJust (s ^. ms . fgpr . at sp)) : stack d
          else if isRet then 
            tail $ stack d
          else stack d
      } 

-- A scrambled version of S w.r.t. D is identical in the instruction memory and
-- accessible parts, and arbitrary in the inaccessible parts of the data memory.
scramble :: TestState () -> StateDesc -> TestState ()
scramble ts d = undefined

step_consistent :: PolicyPlus -> TestState () -> StateDesc -> Bool
step_consistent pplus ts d =
  let tt = scramble ts d in
  case (step pplus ts, step pplus tt) of
    (Right ts', Right tt') ->
      -- The instruction memory and the accessible_D parts of S’ and T’ agree
      -- and the inaccessible_D parts of T and T’ agree.
      undefined
      &&
      -- T’ is step consistent with D’.
      undefined
    _ -> True -- Vacuously

to_desc :: TagSet -> DescTag
to_desc ts =
  case toExt ts of
    [("stack.Stack", Just n)] -> Stack n
    _ -> Instr

-- TODO: Determine provenance of list of (allowed) call addresses. Currently all
-- addresses are allowed. Restrictions could be based on a combination of
-- machine and PIPE memory.
testInitDesc :: TestState a -> StateDesc
testInitDesc ts =
  let
    pmemMap = p_mem (ts ^. mp ^. ps) ^. pmem_map
    pmemDesc = Map.map to_desc pmemMap
    calls = Map.keys pmemMap
  in
    initDesc pmemDesc calls

prop_init :: PolicyPlus -> TestState () -> Property
prop_init pplus ts =
  let
    tsDesc = testInitDesc ts
  in
    whenFail (do putStrLn "Initial state is not step consistent!"
                 putStrLn "Original Test State:"
                 putStrLn $ printTestState pplus ts
                 -- TODO: Print state description
             ) $ (step_consistent pplus ts tsDesc)

-- Enrich a trace of test states with their associated state descriptions.
-- Currently, doing so after the run, so relatively inefficient.
trace_descs :: [TestState a] -> [(TestState a, StateDesc)]
trace_descs tss =
  let
    tsInit   = head tss
    descInit = testInitDesc tsInit
    accInit  = [(tsInit, descInit)]
    foldDesc tds ts' =
      let
        (ts, td) = head tds
        td' = next_desc (ts ^. mp) td (ts' ^. mp)
      in
        (ts', td') : tds
    revDescs = foldl foldDesc accInit (tail tss)
  in
    reverse revDescs

-- A direct encoding of the test of noninterference on stacks: if the current
-- instruction is a return, locate its matching call in the trace and compare
-- the two memory states.
--   In doing so, we can rely on state descriptions or on instruction decoding;
-- currently, for simplicity, the latter is used; well-bracketed control flow is
-- assumed to locate the matching call of a return.
isCall :: Machine_State -> Bool
isCall s =
  case fst $ instr_fetch s of
    Fetch u32 -> case decode_I RV32 u32 of
      Just instr -> case instr of
        JAL _ _ -> True
        _ -> False
      _ -> False
    _ -> False

isReturn :: Machine_State -> Bool
isReturn s =
  case fst $ instr_fetch s of
    Fetch u32 -> case decode_I RV32 u32 of
      Just instr -> case instr of
        JALR _ _ _ -> True
        _ -> False
      _ -> False
    _ -> False

find_call :: Integer -> [(TestState a, StateDesc)] -> Maybe (TestState a, StateDesc)
find_call level trace =
  case trace of
    [] -> Nothing
    (ts, td) : trace' ->
      if isReturn (ts ^. mp ^. ms) then
        find_call (level + 1) trace'
      else if isCall (ts ^. mp ^. ms) then
        if level == 0 then Just (ts, td)
        else find_call (level - 1) trace'
      else find_call level trace'

mem_NI :: TestState a -> TestState a -> Int -> Bool
mem_NI = undefined

test_NI :: TestState a -> [TestState a] -> Bool
test_NI ts_tl tss_rev =
  let
    -- Enriched trace and top (last step, under consideration)
    trace_rev = ts_tl : tss_rev & reverse & trace_descs & reverse
    (ts, td)  = head trace_rev
    -- Location of matching (potential) caller and depth
    -- ts' = step pplus ts
    Just (ts', td') = find_call 0 trace_rev
    depth = pcdepth td'
  in
    if isReturn (ts ^. mp ^. ms) then mem_NI ts ts' depth
    else True -- Holds vacuously: nothing to test here

-- TODO: Rephrase indistinguishability to only look at clean locs?

-- For now, to avoid modifying allWhenFail or enrich states with descriptions at
-- the type level (and construct those during execution), traces are enriched on
-- the fly: this is relatively simple but inefficient.
prop_NI :: PolicyPlus -> Int -> TestState () -> Property
prop_NI pplus maxCount ts =
  let (trace,err) = traceExec pplus ts maxCount in
    allWhenFail (\ts tss -> --tss is reversed here
                   (whenFail (do putStrLn "Error"
                             ) $ (test_NI ts tss))
--                 (whenFail (do putStrLn "Indistinguishable tags found!"
--                               putStrLn "Original Test State:"
--                               putStrLn $ printTestState pplus ts
--                               putStrLn " Trace:"
--                               putStrLn $ printTrace pplus $ reverse tss
--                           ) $ (indistinguishable (== taintTag) ts))
--                 .&&.
--                 (whenFail (do putStrLn $ "Clean tags set differs."
--                               putStrLn $ "Original: " ++ show clean
--                               putStrLn $ "Current:  " ++ show clean'
--                               putStrLn "Original Test State:"                               
--                               putStrLn $ printTestState pplus ts
--                               putStrLn " Trace:"                               
--                               putStrLn $ printTrace pplus $ reverse tss                               
--                           ) $ (clean == clean'))
                ) (takeWhile pcInSync trace)

prop :: PolicyPlus -> TestState () -> Property
prop pplus ts =
  prop_init pplus ts
  .&&.
  prop_NI pplus maxInstrsToGenerate ts



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
HEADER sequence:
           SW SP RA 1     @H1    - store RA
           ADDI SP SP 2  @H2    - increment SP (by frame size)

RETURN sequence:
           LW RA SP -1    @R1    - load return address  (offset by one less than frame size)
           ADDI SP SP -2 @R2    - decrement SP (by frame size)
           JALR RA RA     @R3    - jump back
-}

headerSeq offset =
            [ (JAL ra offset, boringTag)
            , (SW ra sp 1  , tagH1)
            , (ADDI sp sp 2, tagH2)
            ]

returnSeq = [ (LW ra sp (-1), tagR1)
            , (ADDI sp sp 2 , tagR2)
            , (JALR ra ra 0 , tagR3)
            ]

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
        , instrHigh = 400
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
callP t = t == tagH1

genITag _ = return boringTag

isSecretMP :: Machine_State -> PIPE_State -> TagSet -> Bool
isSecretMP ms ps t = t /= boringTag -- TODO: This should actually look at the accessible relationship

mkInfo :: Machine_State -> PIPE_State -> ()
mkInfo _ _ = ()

-- | Main

-- The real one
main_test = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1000}
    $ forAllShrink (genVariationTestState pplus genMTag genGPRTag dataP codeP callP headerSeq returnSeq genITag spTag isSecretMP mkInfo)
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
                    , stack :: [((Integer, Integer), RichState)]
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

isInstruction :: Integer -> StateDesc -> Bool
isInstruction i sd =
  -- can error
  case memdepth sd Map.! i of
    Instr -> True
    _ -> False

-- TODO: Fix this
next_desc :: RichState -> StateDesc -> RichState -> StateDesc
next_desc s d s'
  | memdepth d Map.! (s ^. ms . fpc) == Instr =
    let isCall = elem (s ^. ms . fpc) (callinstrs d)
        isRet  = ((s' ^. ms . fpc) == 4 + fst (fst $ head $ stack d)) &&
                 (Just (snd (fst $ head $ stack d)) == (s ^. ms . fgpr . at sp))
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
            ((s ^. ms . fpc, fromJust (s ^. ms . fgpr . at sp)), s) : stack d
          else if isRet then 
            tail $ stack d
          else stack d
      } 

-- A scrambled version of S w.r.t. D is identical in the instruction memory and
-- accessible parts, and arbitrary in the inaccessible parts of the data memory.
-- TODO: Better scrambling, link to TestStack functionality (cf. variants).
scramble :: TestState a -> StateDesc -> TestState a
scramble ts d =
  let
    scramble_mem m = Map.mapWithKey
      (\i v -> if accessible i d || isInstruction i d then v else 0) m
  in
    (%~) (mp . ms . fmem) scramble_mem ts

-- TODO: Possibly use trace instead of explicit steps. Explain relation between
-- property on traces and paper definition.
-- Alternative forms of this property: between a pair of consecutive states
-- (here), other alternatives.
step_consistent :: PolicyPlus -> TestState a -> StateDesc -> Bool
step_consistent pplus ts d =
  let
    tt = scramble ts d
  in
    case (step pplus ts, step pplus tt) of
      (Right ts', Right tt') ->
        -- The instruction memory and the accessible_D parts of S’ and T’ agree
        -- and the inaccessible_D parts of T and T’ agree.
        let
          memS'              = ts' ^. mp ^. ms ^. fmem
          memT               = tt  ^. mp ^. ms ^. fmem
          memT'              = tt' ^. mp ^. ms ^. fmem
          filterInstrAcc i _ = accessible i d || isInstruction i d
          filterInacc    i _ = not $ accessible i d
          instrAccS'         = Map.mapWithKey filterInstrAcc memS'
          instrAccT'         = Map.mapWithKey filterInstrAcc memT'
          inaccT             = Map.mapWithKey filterInacc memT
          inaccT'            = Map.mapWithKey filterInacc memT'
          eqMaps m1 m2       = Map.isSubmapOf m1 m2 && Map.isSubmapOf m2 m1
        in
          eqMaps instrAccS' instrAccT' && eqMaps inaccT inaccT'
        -- &&
        -- -- T’ is step consistent with D’.
        -- undefined
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

test_init :: PolicyPlus -> (TestState a, StateDesc) -> Bool
test_init pplus (ts, d) = step_consistent pplus ts d

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
-- TODO: Alternatively, check correct tagging in PIPE state: R3 for returns, a
-- new tag for call sites (or blessed call sites).
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

-- TODO: Is it feasible to discharge the well-bracketedness assumption?
-- Consider adding a stack of whole machine states to the state description to
-- simplify this process (how does scrambling interact with it?).
find_call :: [(TestState a, StateDesc)] -> Maybe (TestState a, StateDesc)
find_call trace_rev =
  let
    (_, callee_td) = head trace_rev
    caller_ts = snd $ head $ stack callee_td
    find_aux t = case t of
      [] -> Nothing
      (ts, td) : t' -> if (ts ^. mp) == caller_ts then Just (ts, td) else find_aux t'
  in
    find_aux trace_rev

-- TODO: Refactor definitions (see above).
-- Clarify: are instructions accessible?
mem_NI :: TestState a -> TestState a -> StateDesc -> Bool
mem_NI ts ts' d =
  let
    mem                = ts  ^. mp ^. ms ^. fmem
    mem'               = ts' ^. mp ^. ms ^. fmem
    filterInstrAcc i _ = accessible i d || isInstruction i d
    instrAcc           = Map.mapWithKey filterInstrAcc mem
    instrAcc'          = Map.mapWithKey filterInstrAcc mem'
    eqMaps m1 m2       = Map.isSubmapOf m1 m2 && Map.isSubmapOf m2 m1
  in
    eqMaps instrAcc instrAcc'

test_NI :: [(TestState a, StateDesc)] -> Bool
test_NI trace_rev =
  let
    -- Enriched trace and top (last step, under consideration)
    (ts, td)  = head trace_rev
    -- Location of matching (potential) caller and depth
    Just (ts', td') = find_call trace_rev
    -- depth = pcdepth td'
  in
    if isReturn (ts ^. mp ^. ms) then mem_NI ts ts' td'
    else True -- Holds vacuously: nothing to test here

-- TODO: Rephrase indistinguishability to only look at clean locs?
prop_NI :: PolicyPlus -> Int -> TestState () -> Property
prop_NI pplus maxCount ts =
--  whenFail (putStrLn $ printTestState pplus ts)
--           False
  let (trace,err) = traceExec pplus ts maxCount in
  whenFail (do putStrLn "Trace:"
               putStrLn $ printTrace pplus trace
               putStrLn "Terminatin error:"
               putStrLn $ show err
           ) $ length trace <= 5
--  allWhenFail (\ts tss -> --tss is reversed here
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
--              ) (takeWhile pcInSync trace)

-- For now, to avoid modifying allWhenFail or enrich states with descriptions at
-- the type level (and construct those during execution), traces are enriched on
-- the fly: this is relatively simple but inefficient.
prop_NI' :: PolicyPlus -> Int -> TestState () -> Property
prop_NI' pplus maxCount ts =
  let (trace,err) = traceExec pplus ts maxCount in
    allWhenFail (\ts tss -> --tss is reversed here
                  let trace' = ts : tss & reverse & trace_descs & reverse in
                    (whenFail (do putStrLn "Initial state does not preserve step consistency!"
                                  putStrLn "Original Test State:"
                                  putStrLn $ printTestState pplus ts
                                  -- TODO: Print state description
                             ) $ (test_init pplus (head trace')))
                  .&&.
                   (whenFail (do putStrLn "Stack state at call not preserved at return!"
                                 putStrLn "Original Test State:"
                                 putStrLn $ printTestState pplus ts
                                 -- TODO: Print state description
                             ) $ (test_NI trace'))
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
prop pplus ts = prop_NI' pplus maxInstrsToGenerate ts

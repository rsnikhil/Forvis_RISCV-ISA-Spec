{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TupleSections, FlexibleInstances, MultiParamTypeClasses, MultiWayIf #-}

module TestStackSafety where

import Prelude hiding (head)
import qualified Prelude as Prelude

-- From Haskell libraries
import Control.Lens hiding (elements)
import Control.Monad

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
import Terminal
import MachineLenses
import TestState

-- Printing
import Text.PrettyPrint (Doc, (<+>), ($$), fcat)
import qualified Text.PrettyPrint as P

head :: String -> [a] -> a
head s [] = error $ "head called with empty list: " ++ s
head _ (h:t) = h

-- | Policy Specific generation

-- TODO: H1, H2... should include the instr tag
noTag = fromExt []
-- boringTag = fromExt [("stack.Boring", Nothing)]
spTag = fromExt [("stack.SP", Nothing)]
tagH1 = fromExt [("stack.H1", Nothing), ("stack.Instr", Nothing)] 
tagH2 = fromExt [("stack.H2", Nothing), ("stack.Instr", Nothing)]  
tagH3 = fromExt [("stack.H3", Nothing), ("stack.Instr", Nothing)]  
tagR1 = fromExt [("stack.Instr", Nothing), ("stack.R1", Nothing)]
tagR2 = fromExt [("stack.Instr", Nothing), ("stack.R2", Nothing)]  
tagR3 = fromExt [("stack.Instr", Nothing), ("stack.R3", Nothing)]
callTag = fromExt [("stack.Call", Nothing), ("stack.Instr", Nothing)]
instrTag = fromExt [("stack.Instr", Nothing)]  
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
            [ (JAL ra offset, callTag)
            , (SW sp ra 4  , tagH1)
            , (ADDI sp sp 8, tagH2)
            ]

returnSeq = [ (LW ra sp (-4), tagR1)
            , (ADDI sp sp 8 , tagR2)
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
        , emptyInstTag = instrTag
        , initNextColor = 1
        , instrLow = 0
        , instrHigh = 400
        , dataMemLow = 1000
        , dataMemHigh = 1020  -- Was 40, but that seems like a lot! (8 may be too little!)
        }
  return pplus

genMTag, genGPRTag :: PolicyPlus -> Gen TagSet 
genMTag pplus = pure (initMem pplus)
genGPRTag pplus = pure (initGPR pplus)

dataP = const True
codeP = const True
callP t = t == tagH1

genITag _ = return instrTag

isSecretMP :: Machine_State -> PIPE_State -> TagSet -> Bool
isSecretMP ms ps t =
  -- TODO: Better way to do this?
  -- Get qualified symbol name
  -- Look it up in the map
  -- Get the int out
  -- Also TODO: Instructions?
  let depthOf :: TagSet -> Maybe Int
      depthOf t = join (snd <$> (listToMaybe $ toExt t)) in
  case (depthOf t, depthOf (ps ^. ppc) ) of
    (Just n, Just m) -> n < m
    (Nothing, _) -> False
    (Just n, Nothing) -> error "No depth in pc"

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
             | Other 
             deriving (Eq, Show)

data StateDesc = SD { pcdepth :: !Int
                    , memdepth :: Map Integer DescTag
                    , stack :: [((Integer, Integer), RichState)]
                    , callinstrs :: [Integer]
                    } 

instance Pretty DescTag where
  pretty _ t = P.text $ show t

instance Pretty (Integer, DescTag) where
  pretty pplus (i, t) = P.integer i <@> pretty pplus t

instance Pretty (Integer, Integer) where
  pretty pplus x = P.text $ show x

docStateDesc :: PolicyPlus -> StateDesc -> Doc
docStateDesc pplus sd =
  P.vcat [ P.text "State Description:"
         , P.text "PC depth:" <+> P.int (pcdepth sd)
         , P.text "Mem Depths:" $$ P.vcat (map (pretty pplus) $ Map.assocs $ memdepth sd)
         , P.text "Saved sp/pcs:" <+> P.hcat (List.intersperse (P.text ",") (map (pretty pplus . fst) (stack sd)))
         , P.text "Call Instrs:" <+> P.text (show $ callinstrs sd)
         ] 
         
docTestState pplus ts =
  docRichStates pplus (ts ^. mp) (map (\x -> calcDiff pplus (ts ^. mp) (x ^. mp_state)) (ts ^. variants))


initDesc :: Map Integer DescTag -> [Integer] -> StateDesc
initDesc memlayout calls = SD { pcdepth = 0
                              , memdepth = memlayout
                              , stack = []
                              , callinstrs = calls
                              } 

tagOf :: DescTag -> Integer -> StateDesc -> DescTag
tagOf def i sd =
  -- Lookup i, return the default memory tag if not found
  Map.findWithDefault def i (memdepth sd)

accessibleTag :: DescTag -> Int -> Bool
accessibleTag t depth =
  case t of
    (Stack n) -> n >= depth
    Instr -> False -- Instructions not accessible
    Other -> False -- Shouldn't happen. Error instead?

accessible :: DescTag -> Integer -> StateDesc -> Bool
accessible def i sd =
  accessibleTag (tagOf def i sd) (pcdepth sd)

isInstructionTag :: DescTag -> Bool
isInstructionTag t =
  case t of
    Instr -> True
    _ -> False

isInstruction :: DescTag -> Integer -> StateDesc -> Bool
isInstruction def i sd =
  isInstructionTag (tagOf def i sd)

next_desc :: PolicyPlus -> DescTag -> RichState -> StateDesc -> RichState -> StateDesc
next_desc pplus def s d s'
  | tagOf def (s ^. ms . fpc) d == Instr =
    let isCall = elem (s ^. ms . fpc) (callinstrs d)
        isRet  = case stack d of
                   [] -> False -- Nothing on the stack means you can't return
                   ((spc, ssp),_):_ -> ((s' ^. ms . fpc) == 4 + spc) &&
                                       (Just ssp == (s ^. ms . fgpr . at sp))
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
                  SW _ rs1 imm12 ->
                    traceShow ("Writing..", rs1, imm12, writeAddr rs1 imm12) $
                    Just (writeAddr rs1 imm12)
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
  | otherwise = error $ "Tag mismatch. Expected Instr. Found " ++ show (tagOf def (s ^. ms . fpc) d) ++ ".\n"
                         ++ "Other info:\n" 
                         ++ "PC: " ++ show (s ^. ms . fpc) ++ "\n"
                         ++ "State Desc:\n"
                         ++ P.render (docStateDesc pplus d) ++ "\n"
                         ++ "Rich State:\n"
                         ++ P.render (docRichStates pplus s [])
                       
                         


-- A scrambled version of S w.r.t. D is identical in the instruction memory and
-- accessible parts, and arbitrary in the inaccessible parts of the data memory.
-- TODO: Better scrambling, link to TestStack functionality (cf. variants).
scramble :: DescTag -> TestState a -> StateDesc -> TestState a
scramble def ts d =
  let
    scramble_mem m = Map.mapWithKey
      (\i v -> if accessible def i d || isInstruction def i d then v else 0) m
  in
    (%~) (mp . ms . fmem) scramble_mem ts

eqMapsWithDefault :: (Ord k, Eq a) => Map k a -> Map k a -> a -> Bool
eqMapsWithDefault m1 m2 adef =
  let
    ks = Map.keys m1 `List.union` Map.keys m2
    checkKey i =
      let
        a1 = Map.findWithDefault adef i m1
        a2 = Map.findWithDefault adef i m2
      in
        a1 == a2
  in
    all checkKey ks

-- TODO: Possibly use trace instead of explicit steps. Explain relation between
-- property on traces and paper definition.
-- Alternative forms of this property: between a pair of consecutive states
-- (here), other alternatives.
step_consistent :: PolicyPlus -> TestState a -> StateDesc -> Bool
step_consistent pplus ts d =
  let
    def = to_desc (initMem pplus)
    tt = scramble def ts d
  in
    case (step pplus ts, step pplus tt) of
      (Right ts', Right tt') ->
        -- The instruction memory and the accessible_D parts of S’ and T’ agree
        -- and the inaccessible_D parts of T and T’ agree.
        let
          memS'              = ts' ^. mp ^. ms ^. fmem
          memT               = tt  ^. mp ^. ms ^. fmem
          memT'              = tt' ^. mp ^. ms ^. fmem
          filterInstrAcc i _ = accessible def i d || isInstruction def i d
          filterInacc    i _ = not $ accessible def i d
          instrAccS'         = Map.mapWithKey filterInstrAcc memS'
          instrAccT'         = Map.mapWithKey filterInstrAcc memT'
          inaccT             = Map.mapWithKey filterInacc memT
          inaccT'            = Map.mapWithKey filterInacc memT'
          defInstrAcc        = accessibleTag def (pcdepth d) ||
                               isInstructionTag def
          defInacc           = not $ accessibleTag def (pcdepth d)
        in
          trace ("Result: " ++ show (eqMapsWithDefault instrAccS' instrAccT' defInstrAcc &&
                                     eqMapsWithDefault inaccT inaccT' defInacc))
          trace ("memS': " ++ show memS')
          trace ("memT: " ++ show memT)
          trace ("memT': " ++ show memT')
          trace ("instrAccS': " ++ show instrAccS')
          trace ("instrAccT': " ++ show instrAccT')
          trace ("inaccT: " ++ show inaccT)
          trace ("inaccT': " ++ show inaccT')
          eqMapsWithDefault instrAccS' instrAccT' defInstrAcc &&
          eqMapsWithDefault inaccT inaccT' defInacc
        -- &&
        -- -- T’ is step consistent with D’.
        -- undefined
      _ -> True -- Vacuously

to_desc :: TagSet -> DescTag
to_desc ts =
  case toExt ts of
    [("stack.Stack", Just n)] -> Stack n
    [("stack.PC", _)] -> Other
    [("stack.SP", _)] -> Other
    -- The rest are instructions    
    _ -> Instr

-- TODO: Are blessed calls also tagged with Instr? Anything else?
testInitDesc :: TestState a -> StateDesc
testInitDesc ts =
  let
    pmemMap = p_mem (ts ^. mp ^. ps) ^. pmem_map
    pmemDesc = Map.map to_desc pmemMap
    calls = Map.filter ((==) callTag) pmemMap & Map.keys
  in
    initDesc pmemDesc calls

test_init :: PolicyPlus -> (TestState a, StateDesc) -> Bool
test_init pplus (ts, d) = step_consistent pplus ts d

-- Enrich a trace of test states with their associated state descriptions.
-- Currently, doing so after the run, so relatively inefficient.
trace_descs :: PolicyPlus -> DescTag -> [TestState a] -> [(TestState a, StateDesc)]
trace_descs pplus def tss =
  let
    tsInit   = head "tsInit" tss
    descInit = testInitDesc tsInit

    accInit  = [(tsInit, descInit)]
    foldDesc tds ts' =
      let
        (ts, td) = head "foldDesc" tds
        td' = next_desc pplus def (ts ^. mp) td (ts' ^. mp)
      in
        (ts', td') : tds
    revDescs = foldl foldDesc accInit (tail tss)
  in
    reverse revDescs

-- A direct encoding of the test of noninterference on stacks: if the current
-- instruction is a return, locate its matching call in the trace and compare
-- the two memory states.
--   In doing so, we can rely on state descriptions or on instruction decoding.
-- The former is used to locate matching calls, whereas without relying on
-- well-bracketedness.
-- TODO: How does scrambling interact with this process?
find_call :: [(TestState a, StateDesc)] -> Maybe (TestState a, StateDesc)
find_call trace_rev =
  let
    (_, callee_td) = head "find_call/callee" trace_rev
    caller_ts = snd $ head "find_call/caller" $ stack callee_td
    find_aux t = case t of
      [] -> Nothing
      (ts, td) : t' -> if (ts ^. mp) == caller_ts then Just (ts, td) else find_aux t'
  in
    find_aux trace_rev

-- TODO: Refactor definitions (see above).
-- Clarify: are instructions accessible?
mem_NI :: DescTag -> TestState a -> TestState a -> StateDesc -> Bool
mem_NI def ts ts' d =
  let
    mem                = ts  ^. mp ^. ms ^. fmem
    mem'               = ts' ^. mp ^. ms ^. fmem
    filterInstrAcc i _ = accessible def i d || isInstruction def i d
    instrAcc           = Map.mapWithKey filterInstrAcc mem
    instrAcc'          = Map.mapWithKey filterInstrAcc mem'
    -- TODO: Refactor these definitions, also in step_consistent
    defInstrAcc        = accessibleTag def (pcdepth d) ||
                         isInstructionTag def
  in
    eqMapsWithDefault instrAcc instrAcc' defInstrAcc

-- Find return addresses based on the PIPE state.
-- TODO: Possibly in combination with other tags?
isReturn :: TestState a -> Bool
isReturn s =
  let
    pmemMap = p_mem (s ^. mp ^. ps) ^. pmem_map
    pc = s ^. mp ^.  ms . fpc
  in
    pmemMap Map.! pc == tagR3

test_NI :: DescTag -> [(TestState a, StateDesc)] -> Bool
test_NI def trace_rev =
  let
    -- Enriched trace and top (last step, under consideration)
    (ts, td)  = head "test_NI" trace_rev
    -- Location of matching (potential) caller and depth
    Just (ts', td') = find_call trace_rev
    -- depth = pcdepth td'
  in
    if isReturn ts then mem_NI def ts ts' td'
    else True -- Holds vacuously: nothing to test here

-- TODO: Rephrase indistinguishability to only look at clean locs?
prop_NI :: PolicyPlus -> Int -> TestState () -> Property
prop_NI pplus maxCount ts =
--  whenFail (putStrLn $ printTestState pplus ts)
--           False
  let (trace,err) = traceExec pplus ts maxCount in
  whenFail (do putStrLn $ "Trace: (len = " ++ show (length trace) ++ ")"
               putStrLn $ printTrace pplus trace
               putStrLn "Termination error:"
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
  let (trace,err) = traceExec pplus ts maxCount
      def = to_desc (initMem pplus)
  in 
    collect (length $ takeWhile pcInSync trace, length trace) $ 
    allWhenFail (\ts tss -> --tss is reversed here
                  let trace' = ts : tss & reverse & trace_descs pplus def & reverse in
                    (whenFail (do putStrLn "Initial state does not preserve step consistency!"
                                  putStrLn "Original Test State:"
                                  putStrLn $ printTestState pplus ts
                                  putStrLn " Trace:"
                                  putStrLn $ printTrace pplus $ reverse tss
                                  putStrLn "Final State Desc"
                                  let descInit = testInitDesc ts
                                  putStrLn $ P.render $ docStateDesc pplus descInit
                                  -- TODO: Print state description
                             ) $ (test_init pplus (head "prop_NI'/init state" trace')))
                  .&&.
                   (whenFail (do putStrLn "Stack state at call not preserved at return!"
                                 putStrLn "Original Test State:"
                                 putStrLn $ printTestState pplus ts
                                 putStrLn " Trace:"
                                 putStrLn $ printTrace pplus $ reverse tss
                                 -- TODO: Print state description
                             ) $ (test_NI def trace'))
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

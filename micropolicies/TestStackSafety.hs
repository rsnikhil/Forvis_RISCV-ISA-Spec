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
import qualified Data.Bits as Bits

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


-- For extra info
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
calleeTag = tagH1
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
  -- Also TODO: Instructions?
  let depthOf :: TagSet -> Maybe Int
      depthOf t = 
        let al = toExt t in
        case (List.lookup "stack.PC" al, List.lookup "stack.Stack" al) of
          (Just n, _) -> n -- n is already a just (the optional tag argument)
          (_, Just n) -> n
          _ -> Nothing
  in 
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
  quickCheckWith stdArgs{maxSuccess=1000} (prop_stack_safety_full pplus)

main_const instrs = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1} (prop_stack_safety_constant instrs pplus)

instr1 =
  [ (ADDI 3 3 40, instrTag)
  , (SW 3 3 1000, instrTag)
  , (JAL ra 4, callTag)
  , (SW sp ra 4  , tagH1)
  , (ADDI sp sp 8, tagH2)
  , (LW 3 3 1000, instrTag)
  ]

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
                    , callsites :: [Integer]
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
         , P.text "Call Sites:" <+> P.text (show $ callsites sd)
         ]

printStateDesc :: PolicyPlus -> StateDesc -> String
printStateDesc pplus d = P.render $ docStateDesc pplus d
         
-- docTestState pplus ts =
--   docRichStates pplus (ts ^. mp) (map (\x -> calcDiff pplus (ts ^. mp) (x ^. mp_state)) (ts ^. variants))


initDesc :: Map Integer DescTag -> [Integer] -> [Integer] -> StateDesc
initDesc memlayout callers callees = SD { pcdepth = 0
                              , memdepth = memlayout
                              , stack = []
                              , callinstrs = callers
                              , callsites = callees
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
  let res = accessibleTag (tagOf def i sd) (pcdepth sd) in
--  traceShow ("Accessible", def, i, tagOf def i sd, pcdepth sd, res) res
  res

isInstructionTag :: DescTag -> Bool
isInstructionTag t =
  case t of
    Instr -> True
    _ -> False

isInstruction :: DescTag -> Integer -> StateDesc -> Bool
isInstruction def i sd =
  isInstructionTag (tagOf def i sd)

next_desc :: PolicyPlus -> DescTag -> RichState -> StateDesc -> RichState -> Maybe StateDesc
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
                    rv   = mstate_rv_read    mstate
                    xlen = mstate_xlen_read  mstate
                    -- Compute effective address
                    rs1_val  = mstate_gpr_read  rs1  mstate    -- address base
                    s_imm12  = sign_extend  12  xlen  imm12
                    eaddr1   = alu_add  xlen  rs1_val  s_imm12
                    eaddr2   = if (rv == RV64) then eaddr1 else (eaddr1 Bits..&. 0xffffFFFF)
                in
                  eaddr2
          in
            case fst $ instr_fetch (s ^. ms) of
              Fetch u32 -> case decode_I RV32 u32 of
                Just instr -> case instr of
                  SB rs1 _ imm12 -> Just (writeAddr rs1 imm12)
                  SH rs1 _ imm12 -> Just (writeAddr rs1 imm12)
                  SW rs1 _ imm12 ->
--                    traceShow ("Writing..", rs1, mstate_gpr_read rs1 (s ^. ms), imm12, writeAddr rs1 imm12) $
                    Just (writeAddr rs1 imm12)
                  _ -> Nothing
                _ -> Nothing
              -- TODO: SD support
              _ -> Nothing
    in
    Just $ d { pcdepth =
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
    -- TODO: This shouldn't be an error. Cut off execution?
  | otherwise = Nothing
    --error $ "Tag mismatch. Expected Instr. Found " ++ show (tagOf def (s ^. ms . fpc) d) ++ ".\n"
    --                     ++ "Other info:\n" 
    --                     ++ "PC: " ++ show (s ^. ms . fpc) ++ "\n"
    --                     ++ "State Desc:\n"
    --                     ++ P.render (docStateDesc pplus d) ++ "\n"
    --                     ++ "Rich State:\n"
    --                     ++ P.render (docRichStates pplus s [])

-- -- A scrambled version of S w.r.t. D is identical in the instruction memory and
-- -- accessible parts, and arbitrary in the inaccessible parts of the data memory.
-- -- TODO: Better scrambling, link to TestStack functionality (cf. variants).
-- scramble :: DescTag -> TestState a -> StateDesc -> TestState a
-- scramble def ts d =
--   let
--     scramble_mem m = Map.mapWithKey
--       (\i v -> if accessible def i d || isInstruction def i d then v else 0) m
--   in
--     (%~) (mp . ms . fmem) scramble_mem ts

-- eqMapsWithDefault :: (Ord k, Eq a) => Map k a -> Map k a -> a -> Bool
-- eqMapsWithDefault m1 m2 adef =
--   let
--     ks = Map.keys m1 `List.union` Map.keys m2
--     checkKey i =
--       let
--         a1 = Map.findWithDefault adef i m1
--         a2 = Map.findWithDefault adef i m2
--       in a1 == a2 
--   in
--     all checkKey ks

eqMapProperty :: (Pretty k, Pretty a, Ord k, Eq a) =>
                 PolicyPlus -> Map k a -> Map k a -> a -> Property
eqMapProperty pplus m1 m2 adef =
  let
    ks = Map.keys m1 `List.union` Map.keys m2
    checkKey i =
      let
        a1 = Map.findWithDefault adef i m1
        a2 = Map.findWithDefault adef i m2

        -- Debug information
        doc = P.vcat [ P.text "Mapping inequality at key:" <+> pretty pplus i
                     , P.text "Map 1 binding:" <+> pretty pplus (m1 Map.!? i)
                     , P.text "Map 2 binding:" <+> pretty pplus (m2 Map.!? i)
                     , P.text "Default element:" <+> pretty pplus adef
                     ]
      in                  
        whenFail (do putStrLn $ P.render doc) (a1 == a2)
  in 
    conjoin (map checkKey ks)

-- stepOrFail :: PolicyPlus -> TestState a -> TestState a
-- stepOrFail pplus ts =
--   case step pplus ts of
--     Right ts' -> ts'
--     _ -> error $ "Failed to step through!"

-- -- TODO: Possibly use trace instead of explicit steps. Explain relation between
-- -- property on traces and paper definition.
-- -- Alternative forms of this property: between a pair of consecutive states
-- -- (here), other alternatives.
-- step_consistent :: PolicyPlus ->
--                    (StateDesc, TestState a, TestState a, TestState a, TestState a) ->
--                    Bool
-- step_consistent pplus (d, ts, ts', tt, tt') =
--   let
--     def                = to_desc (initMem pplus)
--     -- The instruction memory and the accessible_D parts of S’ and T’ agree
--     -- and the inaccessible_D parts of T and T’ agree.
--     memS'              = ts' ^. mp ^. ms ^. fmem
--     memT               = tt  ^. mp ^. ms ^. fmem
--     memT'              = tt' ^. mp ^. ms ^. fmem
--     filterInstrAcc i _ = accessible def i d || isInstruction def i d
--     filterInacc    i _ = not $ accessible def i d
--     instrAccS'         = Map.mapWithKey filterInstrAcc memS'
--     instrAccT'         = Map.mapWithKey filterInstrAcc memT'
--     inaccT             = Map.mapWithKey filterInacc memT
--     inaccT'            = Map.mapWithKey filterInacc memT'
--     defInstrAcc        = accessibleTag def (pcdepth d) ||
--                          isInstructionTag def
--     defInacc           = not $ accessibleTag def (pcdepth d)
--   in
-- --          trace ("Result: " ++ show (eqMapsWithDefault instrAccS' instrAccT' defInstrAcc &&
-- --                                     eqMapsWithDefault inaccT inaccT' defInacc))
-- --          trace ("memS': " ++ show memS')
-- --          trace ("memT: " ++ show memT)
-- --          trace ("memT': " ++ show memT')
-- --          trace ("instrAccS': " ++ show instrAccS')
-- --          trace ("instrAccT': " ++ show instrAccT')
-- --          trace ("inaccT: " ++ show inaccT)
-- --          trace ("inaccT': " ++ show inaccT')
--     eqMapsWithDefault instrAccS' instrAccT' defInstrAcc &&
--     eqMapsWithDefault inaccT inaccT' defInacc
--     -- &&
--     -- -- T’ is step consistent with D’.
--     -- undefined

to_desc :: TagSet -> DescTag
to_desc ts =
  case toExt ts of
    [("stack.Stack", Just n)] -> Stack n
    [("stack.PC", _)] -> Other
    [("stack.SP", _)] -> Other
    -- The rest are instructions    
    _ -> Instr

-- -- TODO: Are blessed calls also tagged with Instr? Anything else?
-- testInitDesc :: TestState a -> StateDesc
-- testInitDesc ts =
--   let
--     pmemMap = p_mem (ts ^. mp ^. ps) ^. pmem_map
--     pmemDesc = Map.map to_desc pmemMap
--     callers = Map.filter ((==) callTag) pmemMap & Map.keys
--     callees = Map.filter ((==) calleeTag) pmemMap & Map.keys
--   in
--     initDesc pmemDesc callers callees

-- -- Enrich a trace of test states with their associated state descriptions.
-- -- Currently, doing so after the run, so relatively inefficient.
-- trace_descs :: PolicyPlus -> DescTag -> [TestState a] -> [(TestState a, StateDesc)]
-- trace_descs pplus def tss =
--   let
--     tsInit   = head "tsInit" tss
--     descInit = testInitDesc tsInit

--     accInit  = [(tsInit, descInit)]
--     foldDesc tds ts' =
--       let
--         (ts, td) = head "foldDesc" tds
--         -- NOW: fix fromJust
--         td' = fromJust $ next_desc pplus def (ts ^. mp) td (ts' ^. mp)
--       in
--         (ts', td') : tds
--     revDescs = foldl foldDesc accInit (tail tss)
--   in
--     reverse revDescs

-- -- A direct encoding of the test of noninterference on stacks: if the current
-- -- instruction is a return, locate its matching call in the trace and compare
-- -- the two memory states.
-- --   In doing so, we can rely on state descriptions or on instruction decoding.
-- -- The former is used to locate matching calls, whereas without relying on
-- -- well-bracketedness.
-- -- TODO: How does scrambling interact with this process?
-- find_call :: [(TestState a, StateDesc)] -> Maybe (TestState a, StateDesc)
-- find_call trace_rev = do 
--   (_, callee_td) <- listToMaybe trace_rev
--   caller_ts <- snd <$> listToMaybe (stack callee_td)
--   let find_aux t = case t of
--                      [] -> Nothing
--                      (ts, td) : t' -> if (ts ^. mp) == caller_ts then Just (ts, td) else find_aux t'
--   find_aux trace_rev
-- --    (_, callee_td) = head "find_call/callee" trace_rev
-- --    caller_ts = snd $ head "find_call/caller" $ stack callee_td

-- -- TODO: Refactor definitions (see above).
-- -- Clarify: are instructions accessible?
-- mem_NI :: DescTag -> TestState a -> TestState a -> StateDesc -> Bool
-- mem_NI def ts ts' d =
--   let
--     mem                = ts  ^. mp ^. ms ^. fmem
--     mem'               = ts' ^. mp ^. ms ^. fmem
--     filterInstrAcc i _ = accessible def i d || isInstruction def i d
--     instrAcc           = Map.mapWithKey filterInstrAcc mem
--     instrAcc'          = Map.mapWithKey filterInstrAcc mem'
--     -- TODO: Refactor these definitions, also in step_consistent
--     defInstrAcc        = accessibleTag def (pcdepth d) ||
--                          isInstructionTag def
--   in
--     eqMapsWithDefault instrAcc instrAcc' defInstrAcc

-- -- Find return addresses based on the PIPE state.
-- -- TODO: Possibly in combination with other tags?
-- isReturn :: TestState a -> Bool
-- isReturn s =
--   let
--     pmemMap = s ^. mp . ps . pmem
--     pc = s ^. mp ^.  ms . fpc
--   in
--     pmemMap Map.!? pc == Just tagR3

-- test_NI :: DescTag -> [(TestState a, StateDesc)] -> Bool
-- test_NI def trace_rev =
--   let
--     -- Enriched trace and top (last step, under consideration)
--     (ts, td)  = head "test_NI" trace_rev
--     -- Location of matching (potential) caller and depth
--     Just (ts', td') = find_call trace_rev
--     -- depth = pcdepth td'
--   in
--     if isReturn ts then mem_NI def ts ts' td'
--     else True -- Holds vacuously: nothing to test here

-- -- TODO: Rephrase indistinguishability to only look at clean locs?
-- prop_NI :: PolicyPlus -> Int -> TestState () -> Property
-- prop_NI pplus maxCount ts =
-- --  whenFail (putStrLn $ printTestState pplus ts)
-- --           False
--   let (trace,err) = traceExec pplus ts maxCount in
--   whenFail (do putStrLn $ "Trace: (len = " ++ show (length trace) ++ ")"
--                putStrLn $ printTrace pplus trace
--                putStrLn "Termination error:"
--                putStrLn $ show err
--            ) $ length trace > 1
-- --  allWhenFail (\ts tss -> --tss is reversed here
-- --                 (whenFail (do putStrLn "Indistinguishable tags found!"
-- --                               putStrLn "Original Test State:"
-- --                               putStrLn $ printTestState pplus ts
-- --                               putStrLn " Trace:"
-- --                               putStrLn $ printTrace pplus $ reverse tss
-- --                           ) $ (indistinguishable (== taintTag) ts))
-- --                 .&&.
-- --                 (whenFail (do putStrLn $ "Clean tags set differs."
-- --                               putStrLn $ "Original: " ++ show clean
-- --                               putStrLn $ "Current:  " ++ show clean'
-- --                               putStrLn "Original Test State:"                               
-- --                               putStrLn $ printTestState pplus ts
-- --                               putStrLn " Trace:"                               
-- --                               putStrLn $ printTrace pplus $ reverse tss                               
-- --                           ) $ (clean == clean'))
-- --              ) (takeWhile pcInSync trace)

-------------------
-- The Property: --
-------------------

{-| The step-wise property operates on a 6-tuple:
  * A state S
  * A state description D
  * A state T that is a variant of S w.r.t D
  * A state S' such that S -> S'
  * A state description D' of S'
  * A state T' such that T -> T'

  For each such quintuple we check:
  (1) Instruction memory and accessible parts of S' and T' w.r.t. D agree
  (2) Inaccessible parts of T and T' w.r.t D agree 
  \LEO: Google doc says (1) and (2) w.r.t D. Is that correct? Or should it be D'?
  \LEO: We can also check that S and S' inaccessible parts agree. It is implied by the identity scrambling, but couldn't hurt to check, right?
-}
single_step_stack_safety :: PolicyPlus ->
                            (RichState, StateDesc, RichState, RichState, StateDesc, RichState)->
                            Property
single_step_stack_safety pplus (s, d, t, s', d', t') =
  let -- The default memory tag
      def = to_desc (initMem pplus)
      -- Default memory cell is: 0
      defMem = 0
      -- Default instruction is: N/A
      defInstr = error "Default instruction should never be accessed"

      -- | Instruction Memory comparison
      -- Calculate instruction memory (of s' and t' w.r.t D)
      isInstr i _ = isInstruction def i d
      instrS' = Map.filterWithKey isInstr (s' ^. ms . fmem)
      instrT' = Map.filterWithKey isInstr (t' ^. ms . fmem)
      -- Compare instruction memories for equality
      instrEq = eqMapProperty pplus instrS' instrT' defInstr

      -- Calculate accessible memory (of s' and t' w.r.t D)
      isAccessible i _ = accessible def i d
      accS' = Map.filterWithKey isAccessible (s' ^. ms . fmem)
      accT' = Map.filterWithKey isAccessible (t' ^. ms . fmem)
      accEq = eqMapProperty pplus accS' accT' defMem

      -- Calculate inaccessible memory (of t and t' w.r.t D)
      isInaccessible i x = not $ accessible def i d
      inaccT  = Map.filterWithKey isInaccessible (t  ^. ms .fmem)
      inaccT' = Map.filterWithKey isInaccessible (t' ^. ms .fmem)
      inaccEq = eqMapProperty pplus inaccT inaccT' defMem
  in
  let debug_info msg = do
        putStrLn msg
        putStrLn "From test state:"
        putStrLn $ printTestState pplus (TS s [SE t ()])
        putStrLn "with state description:"
        putStrLn $ printStateDesc pplus d
        putStrLn "Arrive at test state:"
        putStrLn $ printTestState pplus (TS s' [SE t' ()])
        putStrLn ""
  in

  whenFail (debug_info "Instruction  memory mismatch.") instrEq .&&.
  whenFail (debug_info "Accessible   memory mismatch.") accEq .&&.
  whenFail (debug_info "Inaccessible memory mismatch.") inaccEq

-- {-| Calculate a list of state descriptions based on a trace of rich states -}
-- trace_desc_rich :: PolicyPlus -> DescTag -> [RichState] -> StateDesc -> [StateDesc]
-- -- Regular case: calculate d' based on s -> s' and d
-- trace_desc_rich pplus def (s:s':ss) d =
--   case next_desc pplus def s d s' of
--     Just d' -> d : trace_desc_rich pplus def (s':ss) d'
--     Nothing -> error "trace_desc_rich/can't calculate the next description"
-- -- Termination: End of execution, yield the final d
-- trace_desc_rich pplus def [s] d = [d]
-- -- Error: Empty trace
-- trace_desc_rich pplus def [] d = error "trace_desc_rich/input can't be empty"

-- {-| The full stack-safety property repeatedly calls single-step
--     on the trace produced by a single generated state S. -}
-- genStackSafetyTrace :: PolicyPlus -> Int -> Gen [(RichState, StateDesc, RichState, RichState, StateDesc, RichState)]
-- genStackSafetyTrace pplus max_steps = do
--   -- Default memory tag
--   let def = to_desc $ initMem pplus
  
--   -- Generate a single machine state
--   s <- genMachine pplus genMTag genGPRTag dataP codeP callP headerSeq returnSeq genITag spTag
--   -- Produce its execution trace
--   let (ss,err) = traceRich pplus s max_steps

--   -- Produce the initial state description
--   let d = let pm = s ^. ps . pmem
--               layout  = Map.map to_desc pm
--               callers = Map.keys $ Map.filter ((==) callTag)   pm
--               callees = Map.keys $ Map.filter ((==) calleeTag) pm
--           in initDesc layout callers callees
--   -- Annotate the trace with state descriptions
--       ds = trace_desc_rich pplus def ss d
      
--   -- Scramble each machine state
--   -- TO DISCUSS: Our variation function currently operates based on tags (to be generic). Should we take descriptions?
--   ts <- mapM (varySecretState pplus isSecretMP) ss
  
--   -- Step all of the scrambled states. These might fail, so result is an Either String
--   let ts' = map (stepRich pplus) ts

--   -- Group all of the different parts together
--       -- Regular case, at least two elements in each
--       group (s:s':ss) (d:d':ds) (t:ts) (et':ts') =
--         case et' of
--           Right t' -> (s,d,t,s',d',t') : group (s':ss) (d':ds) ts ts'
--           -- TO DISCUSS: What should we do if it fails to execute?
--           -- For now, drop.
--           -- TODO: Stats on how many?
--           Left _   -> group (s':ss) (d':ds) ts ts'
--       -- Base case, everything should have the same length
--       group [s] [d] [t] [t'] = [] -- Base
--       group _ _ _ _ = error "group arguments don't have the same length or are empty."

--   -- Pack and return
--   return $ group ss ds ts ts'

-- prop_stack_safety :: PolicyPlus -> Property 
-- prop_stack_safety pplus =
--   forAllShrinkShow (genStackSafetyTrace pplus maxInstrsToGenerate)
--                    (\tr -> []) -- TODO: Shrinking
--                    (\tr -> "") -- Empty printing. All inside the single_step
--                    (\tr -> conjoin $ map (single_step_stack_safety pplus) tr)

{-| The full stack property operates on traces of TestStates (Main + Variants)

    + Attempt 1: Make it recursive

-}
stack_safety_full :: PolicyPlus -> Int -> StateDesc -> TestState () -> Property
stack_safety_full pplus 0 d ts = collect "Out of fuel" $ property True
stack_safety_full pplus n d ts = 
--  trace ("Entering stack safety full with fuel: " ++ show n) $
--  traceShow (callsites d) $
  let def = to_desc $ initMem pplus in
    
  -- First, check if the currently executed instruction is an entry point
  let isCallee = elem (ts ^. mp. ms . fpc) (callsites d) in

  -- Create a scrambler for the current step:
  -- LEO-TODO: only scramble before a call. Shouldn't this be right after a call though?
  let scramble =
        if isCallee then do
          -- Vary the main state and add it to the stack of variants
          s <- varySecretState pplus isSecretMP (ts ^. mp)
          return (ts & variants %~ ((SE s ()):))
        else
          return ts
  in

  -- Actually do the scrambling and continue:
  forAllShrinkShow scramble (const []) (const "") $ \ts' ->

  -- Take a step in ALL machine variants in ts'
  case step pplus ts' of
    Left err -> collect err $ property True -- Execution terminated
    Right ts'' ->
      -- Calculate the next state description:
      let d' = case next_desc pplus def (ts ^. mp) d (ts'' ^. mp) of
                 Just d' -> d'
                 Nothing -> error "stack_safety_null/can't calculate the next description"
      in

      -- Call single_step stack_safety for each variant
      (conjoin $ map (\(t, t') -> single_step_stack_safety pplus
                                   (ts'  ^. mp, d , t  ^. mp_state,
                                    ts'' ^. mp, d', t' ^. mp_state))
                    (zip (ts' ^. variants) (ts'' ^. variants)))
      .&&.
      -- Recursively call stack safety
      (stack_safety_full pplus (n-1) d' ts'')

prop_stack_safety_full :: PolicyPlus -> Property
prop_stack_safety_full pplus =
  forAllShrinkShow
       (genVariationTestState pplus genMTag genGPRTag dataP codeP callP headerSeq returnSeq genITag spTag isSecretMP mkInfo)
       (\_ -> []) -- no shrink
       (\_ -> "") -- no show
       (\ts ->
         -- Produce the initial state description
         let d = let pm = ts ^. mp . ps . pmem
                     layout  = Map.map to_desc pm
                     callers = Map.keys $ Map.filter ((==) callTag) pm
                     callees = Map.keys $ Map.filter ((==) calleeTag) pm
                 in initDesc layout callers callees
         in stack_safety_full pplus maxInstrsToGenerate d ts)

prop_stack_safety_constant :: [(Instr_I, TagSet)] -> PolicyPlus -> Property
prop_stack_safety_constant instrs pplus =
  forAllShrinkShow
       (genVariationTestStateFromInstrs pplus genMTag genGPRTag dataP codeP callP headerSeq returnSeq genITag spTag instrs isSecretMP mkInfo)
       (\_ -> []) -- no shrink
       (\_ -> "") -- no show
       (\ts ->
         -- Produce the initial state description
--         trace (P.render $ docTestState pplus ts) $
         let d = let pm = ts ^. mp . ps . pmem
                     layout  = Map.map to_desc pm
                     callers = Map.keys $ Map.filter ((==) callTag) pm
                     callees = Map.keys $ Map.filter ((==) calleeTag) pm
                 in initDesc layout callers callees
         in stack_safety_full pplus (length instrs + 1) d ts)

  
-- -- Given a reverse execution trace and the latest test state in an execution,
-- -- compute the following:
-- --  * The full trace in execution order;
-- --  * The enrichment of the full trace with state descriptions;
-- --  * The scrambling of each of the states in the trace w.r.t. its description.
-- -- For each of the two resulting traces of tests states (original and
-- -- scrambled), compute the successor of each test state:
-- --  * For the original states, simply shift the trace one position;
-- --  * For the scrambled trace, apply the step function manually.
--   -- In both cases, ignore the last state of the two traces, given that a next
-- -- state may not exist. Finally, pack all five components together (dropping
-- -- the last element in those lists where it still appears) and test the
-- -- property on the most recent tuple.
-- prop_NI' :: PolicyPlus -> Int -> TestState () -> Property
-- prop_NI' pplus maxCount ts =
--   let (trace,err) = traceExec pplus ts maxCount
--       def = to_desc (initMem pplus)
--   in 
--     collect (length $ takeWhile pcInSync trace, length trace, err) $ 
--     allWhenFail (\ts tss -> --tss is reversed here
--                   let
--                     tssS = ts : tss & reverse
--                     tssS_sds = tssS & trace_descs pplus def
--                     (_, sds) = List.unzip tssS_sds
--                     tssT = List.map (\(ts, sd) -> scramble def ts sd) tssS_sds
--                     -- dropLast l = take (List.length l - 1) l
--                     tssS' = List.drop 1 tssS
--                     tssT' = List.map (stepOrFail pplus) (take (List.length tssT - 1) tssT)
--                     tuples = List.zip5 sds tssS tssS' tssT tssT'
--                     tuple = tuples & reverse & List.take 1
--                     testInitTuple ts = case ts of
--                       [t] -> step_consistent pplus t
--                       [] -> True
--                       _ -> error $ "testInitTuple"
--                   in
--                     -- TODO: At the moment, only the last element is tested at
--                     -- each step, incurring much redundant computation.
--                     (whenFail (do putStrLn "Initial state does not preserve step consistency!"
--                                   putStrLn "Original Test State:"
--                                   putStrLn $ printTestState pplus ts
--                                   putStrLn " Trace:"
--                                   putStrLn $ printTrace pplus $ reverse tss
--                                   putStrLn "Final State Desc"
--                                   let descInit = testInitDesc ts
--                                   putStrLn $ P.render $ docStateDesc pplus descInit
--                                   -- TODO: Print state description
--                              ) $ (testInitTuple tuple))
--                   .&&.
--                    (whenFail (do putStrLn "Stack state at call not preserved at return!"
--                                  putStrLn "Original Test State:"
--                                  putStrLn $ printTestState pplus ts
--                                  putStrLn " Trace:"
--                                  putStrLn $ printTrace pplus $ reverse tss
--                                  -- TODO: Print state description
--                              ) $ (test_NI def (tssS_sds & List.reverse)))
-- --                 (whenFail (do putStrLn "Indistinguishable tags found!"
-- --                               putStrLn "Original Test State:"
-- --                               putStrLn $ printTestState pplus ts
-- --                               putStrLn " Trace:"
-- --                               putStrLn $ printTrace pplus $ reverse tss
-- --                           ) $ (indistinguishable (== taintTag) ts))
-- --                 .&&.
-- --                 (whenFail (do putStrLn $ "Clean tags set differs."
-- --                               putStrLn $ "Original: " ++ show clean
-- --                               putStrLn $ "Current:  " ++ show clean'
-- --                               putStrLn "Original Test State:"                               
-- --                               putStrLn $ printTestState pplus ts
-- --                               putStrLn " Trace:"                               
-- --                               putStrLn $ printTrace pplus $ reverse tss                               
-- --                           ) $ (clean == clean'))
--                 ) (takeWhile pcInSync trace)

-- prop :: PolicyPlus -> TestState () -> Property
-- prop pplus ts = prop_NI' pplus maxInstrsToGenerate ts

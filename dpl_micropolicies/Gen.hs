module Gen where

import Arch_Defs
import GPR_File
import Forvis_Spec_I
import Memory

import Data.Bits

import Encoder
import PIPE

import qualified Data.Map.Strict as Data_Map
import qualified Data.Set as Data_Set
import Data.Set (Set)
import Data.Maybe (isJust, fromJust)

import Machine_State

import Control.Arrow (second)
import Test.QuickCheck
import TestHeapSafety

import Debug.Trace
import Run_Program_PIPE

initMachine = 
  let initial_PC     = 0
      misa           = ((    shiftL  1  misa_A_bitpos)
                        .|. (shiftL  1  misa_I_bitpos)
                        .|. (shiftL  1  misa_M_bitpos)
                        .|. (shiftL  1  misa_S_bitpos)
                        .|. (shiftL  1  misa_U_bitpos)
                        .|. (shiftL  xl_rv32  misa_MXL_bitpos_RV32))
      mem_base       = 0
      mem_size       = 0xFFFFFFFFFFFFFFFF
      addr_ranges    = [(mem_base, mem_base + mem_size)]
      addr_byte_list = []
  in mkMachine_State  RV32  misa  initial_PC  addr_ranges  addr_byte_list

{-
Put heap_base r1 @ default
Mov r1 r1 @ (fresh color)
Store r1 r1 @ default
-}
{- ACCEPT:
Load r1 r2 @ default
-}
{- Reject:
Put heap_base r2 @ default
Load r2 r3 @ default
-}
exampleMachines =
  let ms = initMachine
      heap_base = 100
      base_code =
        [ (0*4, ((encode_I RV32 (ADDI 1 0 heap_base), Alloc)))
        , (1*4, ((encode_I RV32 (ADD  1 1 0), NoAlloc)))    -- Useless
        , (2*4, ((encode_I RV32 (SW   1 1 0), NoAlloc))) 
        , (3*4, ((encode_I RV32 (ADDI 4 0 heap_base), NoAlloc)))
        , (4*4, ((encode_I RV32 (ADDI 5 0 heap_base), NoAlloc)))
        ]
      accept_code =
        [ (5*4, ((encode_I RV32 (LW 2 1 0)), NoAlloc)) ]
      reject_code =
        [ (5*4, ((encode_I RV32 (ADDI 2 0 heap_base)), NoAlloc))
        , (6*4, ((encode_I RV32 (LW 3 2 0)), NoAlloc)) ]
      mem_acc = (f_mem ms) { f_dm = Data_Map.fromList (map (second fst) (base_code ++ accept_code)) }
      mem_rej = (f_mem ms) { f_dm = Data_Map.fromList (map (second fst) (base_code ++ reject_code)) }
      p_macc = MemT $ Data_Map.fromList (map (second $ MTagI . snd) (base_code ++ accept_code)) 
      p_mrej = MemT $ Data_Map.fromList (map (second $ MTagI . snd) (base_code ++ reject_code)) 
      ms_acc = ms { f_mem = mem_acc }
      ms_rej = ms { f_mem = mem_rej }
      p_acc = init_pipe_state { p_mem = p_macc }
      p_rej = init_pipe_state { p_mem = p_mrej }
  in ((ms_acc,p_acc), (ms_rej,p_rej))
  
--- Generate input program + tags
-- Tags in call/ret f1-2-3-4-5
-- Memory colors in movs
-- add heap_base

-- Invariants: r0 is always zero

-- Generate a random register for source
-- TODO: add types?
genSourceReg :: Machine_State -> Gen GPR_Addr
genSourceReg ms =
  -- GPR's are hard coded to be [0..31].
  choose (0, 31)

-- Generate a target register GPR
-- For now, just avoid R0
genTargetReg :: Machine_State -> Gen GPR_Addr
genTargetReg ms =
  choose (1, 31)

-- Generate an immediate up to number
-- Multiple of 4
genImm :: Integer -> Gen InstrField
genImm n = (4*) <$> choose (1, n `div` 4)   -- BCP: Why do we not generate 0?

dataMemLow  = 4
dataMemHigh = 40
instrLow = 1000

-- Generate an instruction that is valid in the current context
-- For load and store to make sense, we need to go through the register file
-- and pick "valid" current addresses.

-- Picks out valid (data registers + max immediate), (jump registers + min immediate), integer registers
groupRegisters :: GPR_File -> ([(GPR_Addr, Integer)], [(GPR_Addr, Integer)], [GPR_Addr])
groupRegisters (GPR_File rs) =
  let regs = Data_Map.assocs rs
      validData n
        | n >= dataMemLow && n <= dataMemHigh =
            Just (dataMemHigh - n)
        | otherwise = Nothing
      validJump n = Just (instrLow - n)
      dataRegs    = map (second fromJust) $ filter (isJust . snd) $ map (second validData) regs
      controlRegs = map (second fromJust) $ filter (isJust . snd) $ map (second validJump) regs
      arithRegs   = map fst regs
  in (dataRegs, controlRegs, arithRegs)       

genInstr :: Machine_State -> Gen (Instr_I, Tag)
genInstr ms =
  let (dataRegs, ctrlRegs, arithRegs) = groupRegisters (f_gprs ms)
      onNonEmpty [] _= 0
      onNonEmpty _ n = n
  in 
  frequency [ (onNonEmpty arithRegs 1,
               do -- ADDI
                  rs <- elements arithRegs
                  rd <- genTargetReg ms
                  imm <- genImm 40
                  -- TODO: Figure out what to do with Malloc
                  alloc <- frequency [(1, pure $ MTagI Alloc), (4, pure $ MTagI NoAlloc)]
                  return (ADDI rd rs imm, alloc))
            , (onNonEmpty dataRegs 3,
               do -- LOAD
                  (rs,max_imm) <- elements dataRegs
                  rd <- genTargetReg ms
                  imm <- genImm max_imm
                  return (LW rd rs imm, MTagI NoAlloc))
            , (onNonEmpty dataRegs 3 * onNonEmpty arithRegs 1,
               do -- STORE
                  (rd,max_imm) <- elements dataRegs
                  rs <- genTargetReg ms
                  imm <- genImm max_imm
                  return (SW rd rs imm, MTagI NoAlloc))
            , (onNonEmpty arithRegs 1,
               do -- ADD
                  rs1 <- elements arithRegs
                  rs2 <- elements arithRegs
                  rd <- genTargetReg ms
                  return (ADD rd rs1 rs2, MTagI NoAlloc))
            ]

randInstr :: Machine_State -> Gen (Instr_I, Tag)
randInstr ms =          
  frequency [ (1, do -- ADDI
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 4
                  alloc <- frequency [(1, pure $ MTagI Alloc), (4, pure $ MTagI NoAlloc)]
                  return (ADDI rd rs imm, alloc))
            , (1, do -- LOAD
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 4
                  return (LW rd rs imm, MTagI NoAlloc))
            , (1, do -- STORE
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 4
                  return (SW rd rs imm, MTagI NoAlloc))
            , (1, do -- ADD
                  rs1 <- genSourceReg ms
                  rs2 <- genSourceReg ms
                  rd <- genTargetReg ms
                  return (ADD rd rs1 rs2, MTagI NoAlloc))
            ]


-- | Instruction 0 is JAL 1000
--   Instructions are put in loc 1000+

-- setInstructions :: Machine_State -> [Instr_I] -> Machine_State
-- setInstructions ms instrs =
--   ms {f_mem = (f_mem ms) { f_dm = Data_Map.union (f_dm $ f_mem ms) (Data_Map.fromList ((0, encode_I RV32 (JAL 0 1000)) : (zip [1000,1004..] (map (encode_I RV32) instrs)))) }}
-- 
-- -- | Overwrites any memory tags in the instr memory
-- setInstrTags :: PIPE_State -> [Tag] -> PIPE_State
-- setInstrTags ps its =
--   ps {p_mem = MemT $ Data_Map.union (Data_Map.fromList ((0, MTagI NoAlloc) : (zip [1000,1004..] its))) (unMemT $ p_mem ps)}
-- 
genColor :: Gen Color
genColor =
  frequency [ (9, pure $ C 0)
            , (1, C <$> choose (0, 4)) ]

genMTagM :: Gen Tag
genMTagM = MTagM <$> genColor <*> genColor

genDataMemory :: Gen (Mem, MemT)
genDataMemory = do
  let idx = [dataMemLow,dataMemLow+4..dataMemHigh]
  combined <- mapM (\i -> do d <- genImm 4    -- BCP: This always puts 4 in every location!
                             t <- genMTagM
                             return ((i, d),(i,t))) idx
  let (m,pm) = unzip combined
  return (Mem (Data_Map.fromList m) Nothing, MemT $ Data_Map.fromList pm)

setInstrI :: Machine_State -> Instr_I -> Machine_State
setInstrI ms i =
  ms {f_mem = (f_mem ms) { f_dm = Data_Map.insert (f_pc ms) (encode_I RV32 i) (f_dm $ f_mem ms) } }

setInstrTagI :: Machine_State -> PIPE_State -> Tag -> PIPE_State
setInstrTagI ms ps it =
  ps {p_mem = ( MemT $ Data_Map.insert (f_pc ms) (it) (unMemT $ p_mem ps) ) }

genByExec :: Int -> Machine_State -> PIPE_State -> Set GPR_Addr -> Gen (Machine_State, PIPE_State, Set GPR_Addr)
genByExec 0 ms ps instrlocs = return (ms, ps, instrlocs)
genByExec n ms ps instrlocs
  -- Check if an instruction already exists
  | Data_Map.member (f_pc ms) (f_dm $ f_mem ms) =
    case fetch_and_execute ps ms of
      Right (ps'', ms'') ->
        genByExec (n-1) ms'' ps'' instrlocs
      Left _ ->
  --      trace ("Warning: Fetch and execute failed with steps remaining:" ++ show n) $
        return (ms, ps, instrlocs)
  | otherwise = do
    (is, it) <- genInstr ms
    let ms' = setInstrI ms is
        ps' = setInstrTagI ms ps it
    case fetch_and_execute ps' ms' of
      Right (ps'', ms'') ->
        genByExec (n-1) ms'' ps'' (Data_Set.insert (f_pc ms') instrlocs)
      Left _ ->
  --      trace ("Warning: Fetch and execute failed with steps remaining:" ++ show n) $
        return (ms', ps', instrlocs)

genMachine :: Gen (Machine_State, PIPE_State)
genMachine = do
  -- TODO: this is random, not generation by execution   (BCP: Really??)
  -- registers
  (mem,pmem) <- genDataMemory
  let ms = initMachine {f_mem = mem}
      ps = init_pipe_state {p_mem = pmem}
      ms' = setInstrI ms (JAL 0 1000)
      ps' = setInstrTagI ms ps (MTagI NoAlloc)  -- BCP: Needed??
  (ms_fin, ps_fin, instrlocs) <- genByExec 10 ms' ps' Data_Set.empty

  let final_mem = f_dm $ f_mem ms_fin
      res_mem = foldr (\a mem -> Data_Map.insert a (fromJust $ Data_Map.lookup a final_mem) mem) (f_dm $ f_mem ms') instrlocs
      ms_fin' = 
        ms' {f_mem = (f_mem ms') { f_dm = res_mem } }  

      final_pmem = unMemT $ p_mem ps_fin
      res_pmem = foldr (\a pmem -> Data_Map.insert a (fromJust $ Data_Map.lookup a final_pmem) pmem) (unMemT $ p_mem ps') instrlocs
      ps_fin' =
        ps' {p_mem = MemT res_pmem}

--  let ms_fin' = 
--        ms' {f_mem = (f_mem ms') { f_dm = Data_Map.union (f_dm $ f_mem ms') (f_dm $ f_mem ms_fin) } }
--      ps_fin' =
--        ps' {p_mem = MemT $ Data_Map.union (unMemT $ p_mem ps') (unMemT $ p_mem ps_fin) }
  return (ms_fin', ps_fin')

--  (is,its) <- unzip <$> vectorOf 20 (genInstr ms)
--  return (setInstructions ms is, setInstrTags ps its)

varyUnreachableMem :: Set Color -> Mem -> MemT -> Gen (Mem, MemT)
varyUnreachableMem r (Mem m ra) (MemT pm) = do
  combined <- mapM (\((i,d),(j,t)) ->
                       case t of
                         MTagM v l
                           | Data_Set.member l r -> return ((i,d),(j,t))
                           | otherwise -> do d' <- genImm 12 -- TODO: This makes no sense
                                             return ((i,d'),(j,t)) -- TODO: Here we could scramble v
                         _ -> return ((i,d),(j,t))
                    ) $ zip (Data_Map.assocs m) (Data_Map.assocs pm)
  let (m', pm') = unzip combined
  return (Mem (Data_Map.fromList m') ra, MemT (Data_Map.fromList pm'))

varyUnreachable :: (Machine_State, PIPE_State) -> Gen MStatePair
varyUnreachable (m, p) = do
  let r = reachable p
  (mem', pmem') <- varyUnreachableMem r (f_mem m) (p_mem p)
  return $ M (m,p) (m {f_mem = mem'}, p {p_mem = pmem'})

genMStatePair :: Gen MStatePair
genMStatePair = 
  genMachine >>= varyUnreachable

{-


Global Instance shrinkMStatePair : Shrink MStatePair := {|
  shrink p := 
    let candidates := 
      (r <- shrinkRegistersPair (p.(s1).(regs), p.(s2).(regs)) ;;
      ret {| s1 := {| regs   := (fst r); 
                      memory := p.(s1).(memory); 
                      pc     := p.(s1).(pc); 
                      pstate := p.(s1).(pstate) |};
             s2 := {| regs   := (snd r); 
                      memory := p.(s2).(memory); 
                      pc     := p.(s2).(pc); 
                      pstate := p.(s2).(pstate) |};
           |} )
     ++
      (h <- shrinkHeapPair (getReachable p.(s1))
                           (p.(s1).(memory), p.(s2).(memory)) ;;
      ret {| s1 := {| regs   := p.(s1).(regs); 
                      memory := (fst h); 
                      pc     := p.(s1).(pc); 
                      pstate := p.(s1).(pstate) |};
             s2 := {| regs   := p.(s2).(regs); 
                      memory := (snd h); 
                      pc     := p.(s2).(pc); 
                      pstate := p.(s2).(pstate) |};
           |} ) in
    filter sameReachablePart candidates
  |}%list.


Definition prop_shrinkingPreservesReachability : Checker :=
  forAll arbitrary $ fun states : MStatePair => 
    let shrunk := shrink states in
    (* collect (show (List.length shrunk)) $ *)
    List.forallb sameReachablePart shrunk.
(* QuickCheck prop_shrinkingPreservesReachability. *)

-}

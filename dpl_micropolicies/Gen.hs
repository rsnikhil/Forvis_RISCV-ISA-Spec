{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.List as Data_List

import Control.Monad.Reader

import Machine_State

import Control.Arrow (second)
import Test.QuickCheck
import TestHeapSafety

import Control.Monad.Reader

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

initPC :: PIPE_Policy -> TagSet
initPC ppol = mkTagSet ppol ["test", "Env"] [Nothing]

initGPR :: PIPE_Policy -> TagSet
initGPR ppol = mkTagSet ppol ["test", "Pointer"] [Just 0]

initNextColor :: Int
initNextColor = 5

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

-- TODO : Fix example machine tags
--exampleMachines =
--  let ms = initMachine
--      heap_base = 100
--      base_code =
--        [ (0*4, ((encode_I RV32 (ADDI 1 0 heap_base), Alloc)))
--        , (1*4, ((encode_I RV32 (ADD  1 1 0), NoAlloc)))    -- Useless
--        , (2*4, ((encode_I RV32 (SW   1 1 0), NoAlloc))) 
--        , (3*4, ((encode_I RV32 (ADDI 4 0 heap_base), NoAlloc)))
--        , (4*4, ((encode_I RV32 (ADDI 5 0 heap_base), NoAlloc)))
--        ]
--      accept_code =
--        [ (5*4, ((encode_I RV32 (LW 2 1 0)), NoAlloc)) ]
--      reject_code =
--        [ (5*4, ((encode_I RV32 (ADDI 2 0 heap_base)), NoAlloc))
--        , (6*4, ((encode_I RV32 (LW 3 2 0)), NoAlloc)) ]
--      mem_acc = (f_mem ms) { f_dm = Data_Map.fromList (map (second fst) (base_code ++ accept_code)) }
--      mem_rej = (f_mem ms) { f_dm = Data_Map.fromList (map (second fst) (base_code ++ reject_code)) }
--      p_macc = MemT $ Data_Map.fromList (map (second $ MTagI . snd) (base_code ++ accept_code)) 
--      p_mrej = MemT $ Data_Map.fromList (map (second $ MTagI . snd) (base_code ++ reject_code)) 
--      ms_acc = ms { f_mem = mem_acc }
--      ms_rej = ms { f_mem = mem_rej }
--      p_acc = init_pipe_state { p_mem = p_macc }
--      p_rej = init_pipe_state { p_mem = p_mrej }
--  in ((ms_acc,p_acc), (ms_rej,p_rej))
  
--- Generate input program + tags
-- Tags in call/ret f1-2-3-4-5
-- Memory colors in movs
-- add heap_base

-- Invariants: r0 is always zero

-- GPR's are hard coded to be [0..31], but we only use a couple of them
maxReg = 3

-- Generate a random register for source
-- TODO: add types?
genSourceReg :: Machine_State -> Gen GPR_Addr
genSourceReg ms =
  choose (0, maxReg)

-- Generate a target register GPR
-- For now, just avoid R0
genTargetReg :: Machine_State -> Gen GPR_Addr
genTargetReg ms =
  choose (1, maxReg)

-- Generate an immediate up to number
-- Multiple of 4
genImm :: Integer -> Gen InstrField
-- genImm n = (4*) <$> choose (1, n `div` 4)   -- BCP: Why do we never generate 0?
genImm n = (4*) <$> choose (0, n `div` 4)  

dataMemLow  = 4
dataMemHigh = 20  -- Was 40, but that seems like a lot! (8 may be too little!)
instrLow = 1000

-- Generate an instruction that is valid in the current context
-- For load and store to make sense, we need to go through the register file
-- and pick "valid" current addresses.

-- If not only using multiples of 4, add a modulus to data 
--groupRegisters :: GPR_File -> GPR_FileT ->
--                  ([(GPR_Addr, Integer, Integer, Integer, Tag)],
--                   [(GPR_Addr, Integer)],
--                   [GPR_Addr])
--groupRegisters (GPR_File rs) (GPR_FileT ts) =
--  let regs' = Data_Map.assocs rs
--      regs = take 4 regs'  -- Just use the first four registers!
--      validData n
--        | n >= dataMemLow && n <= dataMemHigh =
--            -- TODO: If we allow non multiple of 4 values this needs to be fixed
--            let mod = 0 in
--            Just (n, mod, dataMemHigh - n)
--        | n == 0 =
--            -- We can allow a 0 register by adding at least 4
--            Just (0, 4, 40)
--        | otherwise = Nothing
--      validJump n = Just (instrLow - n)
--      dataRegs    = map (second fromJust) $ filter (isJust . snd) $ map (second validData) regs
--      dataRegs'   = map (\(i,(content, minimm,maximm)) -> (i, content, minimm, maximm, fromJust $ Data_Map.lookup i ts)) dataRegs
--      controlRegs = map (second fromJust) $ filter (isJust . snd) $ map (second validJump) regs
--      arithRegs   = map fst regs
--  in (dataRegs', controlRegs, arithRegs)       


-- Picks out valid (data registers + content + min immediate + max immediate + tag),
--                 (jump registers + min immediate),
--                 integer registers
groupRegisters :: PIPE_Policy -> GPR_File -> GPR_FileT ->
                  ([(GPR_Addr, Integer, Integer, Integer, TagSet)],
                   [(GPR_Addr, Integer)],
                   [GPR_Addr])
groupRegisters ppol (GPR_File rs) (GPR_FileT ts) =
  -- Assuming that the register files are same length and they have no holes
  let regs = Data_Map.assocs rs
      tags = Data_Map.assocs ts
      rts = zip regs tags

      validData ((reg_id,reg_content),(_reg_id, reg_tag)) 
        | reg_content >= dataMemLow &&
          reg_content <= dataMemHigh
          && isJust (runReader (pointerColorOf reg_tag) ppol) =
           Just (reg_id, reg_content, 0, dataMemHigh - reg_content, reg_tag)
        | reg_content == 0 && isJust (runReader (pointerColorOf reg_tag) ppol) =
        -- We can allow a 0 register by adding at least 4
           Just (reg_id, 0, dataMemLow, dataMemHigh, reg_tag)
        | otherwise =
           Nothing
        
      validJump ((reg_id,reg_content),(_, reg_tag))
        | reg_content < instrLow && isJust (runReader (envColorOf reg_tag) ppol) =
          Just (reg_id, instrLow - reg_content)
        | otherwise =
          Nothing

      dataRegs    = map (fromJust) $ filter (isJust) $ map validData rts
      controlRegs = map (fromJust) $ filter (isJust) $ map validJump rts
      arithRegs   = map fst regs
  in (dataRegs, controlRegs, arithRegs)

-- All locations that can be accessed using color 'c' between 'lo' and 'hi'
reachableLocsBetween :: PIPE_Policy -> Mem -> MemT -> Integer -> Integer -> TagSet -> [Integer]
reachableLocsBetween ppol (Mem m _) (MemT pm) lo hi t =
  case runReader (pointerColorOf t) ppol of
    Just c -> 
      map fst $ filter (\(i,t) ->
                          case runReader (cellColorOf t) ppol of
                            Just c' -> c == c' && i >= lo && i <= hi
                            _ -> False
                       ) (Data_Map.assocs pm)
    _ -> []


genInstr :: PIPE_Policy -> Machine_State -> PIPE_State -> Gen (Instr_I, TagSet)
genInstr ppol ms ps =
  let (dataRegs, ctrlRegs, arithRegs) = groupRegisters ppol (f_gprs ms) (p_gprs ps)
      onNonEmpty [] _= 0
      onNonEmpty _ n = n
  in 
  frequency [ (onNonEmpty arithRegs 1,
               do -- ADDI
                  rs <- elements arithRegs
                  rd <- genTargetReg ms
                  imm <- genImm dataMemHigh
                  -- TODO: Figure out what to do with Malloc
                  alloc <- frequency [(2, pure $ emptyInstTag ppol),
                                      (3, pure $ allocInstTag ppol)]
                  return (ADDI rd rs imm, alloc))
            , (onNonEmpty dataRegs 3,
               do -- LOAD
                  (rs,content,min_imm,max_imm,tag) <- elements dataRegs
                  let locs = --traceShow (content, min_imm, max_imm, tag) $
                             reachableLocsBetween ppol (f_mem ms) (p_mem ps) (content+min_imm) (content+max_imm) tag
                  rd <- genTargetReg ms
                  imm <- frequency [ -- Generate a reachable location)
                                     (--traceShow locs $
                                       onNonEmpty locs 1,
                                      do addr <- elements locs
                                         return $ addr - content)
                                   , (1, (min_imm+) <$> genImm (max_imm - min_imm))
                                   ]
                  let tag = emptyInstTag ppol                         
                  return (LW rd rs imm, tag)
              )
            , (onNonEmpty dataRegs 3 * onNonEmpty arithRegs 1,
               do -- STORE
                  (rd,content, min_imm,max_imm,tag) <- elements dataRegs
                  rs <- genTargetReg ms
                  imm <- (min_imm+) <$> genImm (max_imm - min_imm)
                  let tag = emptyInstTag ppol
                  return (SW rd rs imm, tag))
            , (onNonEmpty arithRegs 1,
               do -- ADD
                  rs1 <- elements arithRegs
                  rs2 <- elements arithRegs
                  rd <- genTargetReg ms
                  let tag = emptyInstTag ppol
                  return (ADD rd rs1 rs2, tag))
            ]

randInstr :: PIPE_Policy -> Machine_State -> Gen (Instr_I, TagSet)
randInstr ppol ms =          
  frequency [ (1, do -- ADDI
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 4
                  alloc <- frequency [(1, pure $ emptyInstTag ppol), (4, pure $ allocInstTag ppol)]
                  return (ADDI rd rs imm, alloc))
            , (1, do -- LOAD
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 4
                  let tag = emptyInstTag ppol                  
                  return (LW rd rs imm, tag))
            , (1, do -- STORE
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 4
                  let tag = emptyInstTag ppol
                  return (SW rd rs imm, tag))
            , (1, do -- ADD
                  rs1 <- genSourceReg ms
                  rs2 <- genSourceReg ms
                  rd <- genTargetReg ms
                  let tag = emptyInstTag ppol
                  return (ADD rd rs1 rs2, tag))
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
  frequency [ (1, pure $ 0)
            , (4, choose (0, 4)) ]

-- Only colors up to 2 (for registers)
genColorLow :: Gen Color
genColorLow = frequency [ (1, pure $ 0)
                        , (2, choose (0,2)) ]

-- Focus on unreachable colors
genColorHigh :: Gen Color
genColorHigh =
  frequency [ (1, pure $ 0)
            , (1, choose (1,2) )
            , (3, choose (3,4) )
            ]


genMTagM :: PIPE_Policy -> Gen TagSet
genMTagM ppol = do
  c1 <- genColor
  c2 <- genColor
  return $ mkTagSet ppol ["test","CP"] [Just c1, Just c2]

genDataMemory :: PIPE_Policy -> Gen (Mem, MemT)
genDataMemory ppol = do
  let idx = [dataMemLow,dataMemLow+4..dataMemHigh]
  combined <- mapM (\i -> do d <- genImm dataMemHigh    -- BCP: This always puts 4 in every location!
                             t <- genMTagM ppol
                             return ((i, d),(i,t))) idx
  let (m,pm) = unzip combined
  return (Mem (Data_Map.fromList m) Nothing, MemT $ Data_Map.fromList pm)

setInstrI :: Machine_State -> Instr_I -> Machine_State
setInstrI ms i =
  ms {f_mem = (f_mem ms) { f_dm = Data_Map.insert (f_pc ms) (encode_I RV32 i) (f_dm $ f_mem ms) } }

setInstrTagI :: Machine_State -> PIPE_State -> TagSet -> PIPE_State
setInstrTagI ms ps it =
  ps {p_mem = ( MemT $ Data_Map.insert (f_pc ms) (it) (unMemT $ p_mem ps) ) }

genByExec :: PIPE_Policy -> Int -> Machine_State -> PIPE_State -> Set GPR_Addr -> Gen (Machine_State, PIPE_State, Set GPR_Addr)
genByExec ppol 0 ms ps instrlocs = return (ms, ps, instrlocs)
genByExec ppol n ms ps instrlocs
  -- Check if an instruction already exists
  | Data_Map.member (f_pc ms) (f_dm $ f_mem ms) =
    case trace "Is it here?" $ fetch_and_execute ppol ps ms of
      Right (ps'', ms'') ->
        genByExec ppol (n-1) ms'' ps'' instrlocs
      Left err ->
        trace ("Warning: Fetch and execute failed with steps remaining:" ++ show n ++ " and error: " ++ show err) $
        return (ms, ps, instrlocs)
  | otherwise = do
    (is, it) <- genInstr ppol ms ps
    let ms' = setInstrI ms is
        ps' = setInstrTagI ms ps it
    case traceShow ("Instruction generated...", is) $ fetch_and_execute ppol ps' ms' of
      Right (ps'', ms'') ->
        trace "Successfull execution" $
        genByExec ppol (n-1) ms'' ps'' (Data_Set.insert (f_pc ms') instrlocs)
      Left err ->
        trace ("Warning: Fetch and execute failed with steps remaining:" ++ show n ++ " and error: " ++ show err) $
        return (ms', ps', instrlocs)

updRegs :: GPR_File -> Gen GPR_File
updRegs (GPR_File rs) = do
  [d1, d2, d3] <- replicateM 3 $ genImm 40
  let rs' :: Data_Map.Map Integer Integer = Data_Map.insert 1 d1 $ Data_Map.insert 2 d2 $ Data_Map.insert 3 d3 rs
  return $ GPR_File rs'

mkPointerTagSet ppol c = mkTagSet ppol ["test", "Pointer"] [Just c]

updTags :: PIPE_Policy -> GPR_FileT -> Gen GPR_FileT
updTags ppol (GPR_FileT rs) = do
  [c1, c2, c3] <- (map $ mkPointerTagSet ppol) <$> (replicateM 3 genColorLow)
  
  let rs' :: Data_Map.Map Integer TagSet = Data_Map.insert 1 c1 $ Data_Map.insert 2 c2 $ Data_Map.insert 3 c3 rs
  return $ GPR_FileT rs'

maxInstrsToGenerate = 10

genMachine :: PIPE_Policy -> Gen (Machine_State, PIPE_State)
genMachine ppol = do
  -- registers
  (mem,pmem) <- genDataMemory ppol
  let ms = trace "This has to be called..." $ initMachine {f_mem = mem}
      ps = (init_pipe_state (initPC ppol) (initGPR ppol) initNextColor){p_mem = pmem}
      ms2 = setInstrI ms (JAL 0 1000)
      ps2 = setInstrTagI ms ps (emptyInstTag ppol)  -- BCP: Needed??

  rs' <- updRegs $ f_gprs ms2
  ts' <- updTags ppol $ p_gprs ps2
  let ms' = ms2 {f_gprs = rs'}
      ps' = ps2 {p_gprs = ts'}
  
  (ms_fin, ps_fin, instrlocs) <- trace "Calling genbyExec" $ genByExec ppol maxInstrsToGenerate ms' ps' Data_Set.empty

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

varyUnreachableMem :: PIPE_Policy -> Set Color -> Mem -> MemT -> Gen (Mem, MemT)
varyUnreachableMem ppol r (Mem m ra) (MemT pm) = do
  combined <- mapM (\((i,d),(j,t)) -> do
                       -- Inlined here. Need to refactor
                       let l = rdTagSet ppol t
                       -- Ughly:
                       let c = case (Data_List.lookup ["test","CP"] l, Data_List.lookup ["test","Cell"] l) of
                                 (Just [c,_], _) -> c
                                 (_, Just [c]) -> c
                                 _ -> Nothing
                       case c of
                         Just c'
                           | Data_Set.member c' r -> return ((i,d),(j,t))
                           | otherwise -> do d' <- genImm 12 -- TODO: This makes no sense
                                             return ((i,d'),(j,t)) -- TODO: Here we could scramble v
                         _ -> return ((i,d),(j,t))
                    ) $ zip (Data_Map.assocs m) (Data_Map.assocs pm)
  let (m', pm') = unzip combined
  return (Mem (Data_Map.fromList m') ra, MemT (Data_Map.fromList pm'))

varyUnreachable :: PIPE_Policy -> (Machine_State, PIPE_State) -> Gen MStatePair
varyUnreachable ppol (m, p) = do
  let r = runReader (reachable p) ppol
  (mem', pmem') <- varyUnreachableMem ppol r (f_mem m) (p_mem p)
  return $ M (m,p) (m {f_mem = mem'}, p {p_mem = pmem'})

genMStatePair :: PIPE_Policy -> Gen MStatePair
genMStatePair ppol = 
  genMachine ppol >>= varyUnreachable ppol

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

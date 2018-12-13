module Gen where

import Arch_Defs
import Forvis_Spec_I
import PIPE
import Memory
import Data.Bits

import qualified Data.Map.Strict as Data_Map
import Machine_State

import Control.Arrow (second)
import Test.QuickCheck

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

-- Generate an immediate up to n bits
genImm :: Int -> Gen InstrField
genImm n = choose (0, shiftL 1 n)
  
-- Generate an instruction that is valid in the current context
-- TODO: For now, random
-- For load and store to make sense, we need to go through the register file
-- and pick "valid" current addresses.
genInstr :: Machine_State -> Gen Instr_I
genInstr ms =          
  frequency [ (1, do -- ADDI
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 12
                  return (ADDI rd rs imm))
            , (1, do -- LOAD
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 12
                  return (LW rd rs imm))
            , (1, do -- STORE
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm 12
                  return (SW rd rs imm))
            , (1, do -- ADD
                  rs1 <- genSourceReg ms
                  rs2 <- genSourceReg ms
                  rd <- genTargetReg ms
                  return (ADD rd rs1 rs2))
            ]

setInstructions :: Machine_State -> [Instr_I] -> Machine_State
setInstructions ms instrs =
  ms {f_mem = (f_mem ms) { f_dm = Data_Map.fromList (zip [0..] (map (encode_I RV32) instrs)) }}

genMachine :: Gen Machine_State
genMachine = do
  -- TODO: this is random, not generation by execution
  is <- vectorOf 20 (genInstr initMachine)
  return $ setInstructions initMachine is

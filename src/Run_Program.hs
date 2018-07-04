-- See LICENSE for license details

module Run_Program where

-- ================================================================
-- This module has a 'run loop' for a RISV-V program:
-- It repeatedly calls 'instr_fetch', 'exec_instr', and 'exec_instr_C'
-- from the ISA spec.

-- This module is not part of the ISA specification, it's just a
-- wrapper representing one possible way to invoke the spec functions.
-- Other execution models are possible modeling more concurrency
-- (pipelineing, weak memory models, multiple harts, etc.)

-- ================================================================
-- Standard Haskell imports

import Control.Monad
import System.IO
import Data.Int
import Data.List
import Data.Word
import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

import Bit_Manipulation
import Arch_Defs
import Machine_State
import CSR_File

import Forvis_Spec

-- ================================================================
-- The 'run loop': runs a program until:
--   - the given maximum # of instructions have executed
--   - something is written into the 'tohost_addr' mem location
--   - stopped for some other reason

-- Takes an architecture state and returns the new architecture state.
-- Fetches and executes one instruction; and repeats.

run_program :: Int -> (Maybe Word64) -> Machine_State -> IO (Int, Machine_State)
run_program  maxinstrs  m_tohost_addr  mstate = do
  let instret               = mstate_csr_read  mstate  csr_addr_minstret
      (tohost_u64, mstate1) = mstate_mem_read_tohost  mstate  m_tohost_addr

  if (tohost_u64 /= 0)
    -- Simulation/Testing aid: Stop due to value written to <tohost>
    then (do
             putStrLn ("<tohost> = " ++ (show  tohost_u64) ++ " (instret = " ++ show  instret ++ "); exiting")
             let exit_value = (fromIntegral  (shiftR  tohost_u64  1)) :: Int
             return (exit_value, mstate1))

    -- Simulation aid: Stop due to instruction limit
    else if instret >= fromIntegral maxinstrs
    then (do
             putStrLn ("Reached instret limit (" ++ show maxinstrs ++ "); exiting")
             return (0, mstate_run_state_write  mstate1  Run_State_Instr_Limit))

    -- Simulation aid: Stop due to any other reason
    else if (mstate_run_state_read  mstate1 /= Run_State_Running)
    then (do
             putStrLn ("Stopped; instret = " ++ show instret ++ "; exiting")
             return (0, mstate1))

    -- Fetch-and-execute instruction and continue
    else (do
             -- Fetch and execute one instruction (may be 0 instrs if
             -- recognize interrupt and just set up exception handler)
             mstate2 <- fetch_and_execute  mstate1

             -- Consume and print out new console output, if any
             let (console_output, mstate3) = mstate_mem_consume_console_output  mstate2
             when (console_output /= "") (putStr  console_output)

             -- Continue
             let pc1 = mstate_pc_read  mstate1
                 pc2 = mstate_pc_read  mstate2
             if (pc1 == pc2)
               then (do
                        -- For simulation debugging only: stop on self loop
                        putStrLn ("Self-loop at PC " ++ (showHex pc1 "") ++
                                  "; instret = " ++ show instret ++ "; exiting")
                        return (0, mstate3))
               else (do
                        -- Loop (tail recursive) for the next instr
                        run_program  maxinstrs  m_tohost_addr  mstate3))

-- Read the word in mem [tohost_addr], if possible

mstate_mem_read_tohost :: Machine_State -> Maybe Word64 -> (Word64, Machine_State)
mstate_mem_read_tohost  mstate  Nothing            = (0, mstate)
mstate_mem_read_tohost  mstate  (Just tohost_addr) =
  let
    (load_result, mstate') = mstate_mem_read  mstate  exc_code_load_access_fault  funct3_LW  tohost_addr
  in
    case load_result of
      Mem_Result_Err  exc_code -> (  0, mstate')
      Mem_Result_Ok   u64      -> (u64, mstate')

-- ================================================================
-- Fetch and execute an instruction (RV32 32b instr or RV32C 16b compressed instr)

-- First tick mtime (which may register a timer interrupt).
-- Then check if any interrupt is pending, and update the state to
--     the trap vector if so (so, fetched instr will be first in trap
--     vector).

fetch_and_execute :: Machine_State -> IO  Machine_State
fetch_and_execute  mstate = do
  let verbosity            = mstate_verbosity_read  mstate
      mstate1              = mstate_mem_tick_mtime  mstate
      (interrupt, mstate2) = take_interrupt_if_any  mstate1

  -- Debug-print when we take an interrupt
  when (interrupt)
    (do
        when (verbosity >= 1) (putStrLn  "Taking interrupt")
        when (verbosity >  1) (mstate_print  "  "  mstate2))

  -- Fetch an instruction
  let pc                      = mstate_pc_read  mstate2
      instret                 = mstate_csr_read  mstate2  csr_addr_minstret
      (fetch_result, mstate3) = instr_fetch  mstate2
      priv                    = mstate_priv_read  mstate3

  case fetch_result of
    Fetch_Trap  ec -> (do
                          putStrLn ("Fetch Trap:" ++ show_trap_exc_code  ec)
                          return mstate3)
    Fetch_C  u16 -> (do
                        -- Exec 'C' instruction
                        let (mstate4, spec_name) = (exec_instr_C  mstate3  u16)
                        when (verbosity >= 1)
                          (do
                              putStr  ("inum:" ++ show (instret + 1))
                              putStr  ("  pc 0x" ++ (showHex pc ""))
                              putStr  ("  instr.C 0x" ++ showHex u16 "")
                              putStr  ("  priv " ++ show (priv))
                              putStrLn ("  " ++ spec_name))
                        when (verbosity > 1) (mstate_print  "  "  mstate4)
                        return  mstate4)
    Fetch    u32 -> (do
                        -- Exec 32b instruction
                        let (mstate4, spec_name) = (exec_instr  mstate3  u32)
                        when (verbosity >= 1)
                          (do
                              putStr  ("inum:" ++ show (instret + 1))
                              putStr  ("  pc 0x" ++ (showHex pc ""))
                              putStr  ("  instr 0x" ++ showHex u32 "")
                              putStr  ("  priv " ++ show (priv))
                              putStrLn ("  " ++ spec_name))
                        when (verbosity > 1) (mstate_print  "  "  mstate4)
                        return  mstate4)

-- ================================================================

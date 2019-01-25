-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Run_Program_Sequential where

-- ================================================================
-- This module has a 'run loop' for a purely sequential execution of a
-- RISV-V program: it repeatedly calls 'instr_fetch', 'exec_instr',
-- and 'exec_instr_C' from the ISA spec.

-- This module is OUTSIDE the ISA specification and is NOT part of the
-- ISA specification.  It's just a wrapper representing one possible
-- way to invoke the spec functions.  Other execution models are
-- possible modeling more concurrency (pipelineing, weak memory
-- models, multiple harts, etc.).  The run_loop function here has
-- several simulation aids (not part of the ISA specification).

-- ================================================================
-- Standard Haskell imports

import Control.Monad
import System.IO
import Data.Int
import Data.Char
import Data.List
import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

import Bit_Utils
import Arch_Defs
import Machine_State
import CSR_File

import Forvis_Spec_Instr_Fetch
import Forvis_Spec_Execute
import Forvis_Spec_Interrupts

-- ================================================================
-- The 'run loop's main function is to repeatedly fetch and execute an
-- instruction, with the following simulation-friendly nuances:
--   - stop at a given maximum # of instructions executed
--   - stop if something interesting is written into the 'tohost_addr' mem location
--   - stop due to any other reason (e.g., breakpoint into debugger)

-- Takes an architecture state and returns the new architecture state.
-- Fetches and executes one instruction; and repeats.

run_loop :: Int     -> (Maybe Integer) -> Machine_State -> IO (Int, Machine_State)
run_loop    maxinstrs  m_tohost_addr      mstate = do
  let instret   = mstate_csr_read        mstate  csr_addr_minstret
      verbosity = mstate_verbosity_read  mstate
      run_state = mstate_run_state_read  mstate

      -- Tick: advance independent processes in IO devices
      -- (increment cycle count, real-time timer, propagate interrupts, etc.)
      mstate1 = mstate_io_tick  mstate

      -- Simulation/testing: read <tohost> location, if any
      (tohost_u64, mstate2) = mstate_mem_read_tohost  mstate1  m_tohost_addr

  if (tohost_u64 /= 0)
    -- Simulation/Testing aid: Stop due to value written to <tohost>
    then (do
             putStrLn ("Stopping due to write to <tohost> = " ++ (show  tohost_u64) ++
                       "; instret = " ++ show  instret)
             let exit_value = (fromIntegral  (shiftR  tohost_u64  1)) :: Int
             return (exit_value, mstate2))

    -- Simulation aid: Stop due to instruction limit
    else if instret >= fromIntegral maxinstrs
    then (do
             putStrLn ("Stopping due to instret limit (" ++ show maxinstrs ++ ")")
             return (0, mstate2))

    -- Simulation aid: Stop due to any other reason
    else if ((run_state /= Run_State_Running) && (run_state /= Run_State_WFI))
    then (do
             putStrLn ("Stopping due to runstate " ++ show run_state ++ "; instret = " ++ show instret)
             return (0, mstate2))

    -- Fetch-and-execute instruction or WFI-resume, and continue run-loop
    else (do
             -- Debugging: print progress-report every 10M instrs
             when ((instret `mod` 10000000) == 0) (
               do
                 let mtime = mstate_mem_read_mtime  mstate2
                 putStrLn  ("[Forvis: instret = " ++ show instret ++
                            "; MTIME = " ++ show mtime ++
                            "]")
                 hFlush  stdout)

             -- Check for tty console input and, if any, buffer it into the UART
             mstate3 <- get_tty_input  mstate2

             -- If running, fetch-and-execute; if in WFI pause, check resumption
             mstate4 <- if (run_state == Run_State_Running) then
                          fetch_and_execute  mstate3

                        else
                          -- run_state == Run_State_WFI
                          do
                            let resume    = mstate_wfi_resume  mstate3
                                mstate3_a = if (resume) then
                                              mstate_run_state_write  mstate3  Run_State_Running
                                            else
                                              mstate3
                            return mstate3_a
 
             -- Debugging: print message if last instruction trapped
             when ((verbosity /= 0)
                   && (mstate_last_instr_trapped_read  mstate4)) (
               putStrLn "Last instruction trapped")

             -- If the UART has buffered output, consume and print out to the tty console
             mstate5 <- put_tty_output  mstate4

             -- Continue the run-loop
             -- (except, as simulation heuristic aid, stop if PC did not change around fetch_and_execute)
             let self_loop_halt = True        -- True/False: enable/disable the heuristic
                 pc3            = mstate_pc_read         mstate3
                 run_state3     = mstate_run_state_read  mstate3
                 pc5            = mstate_pc_read         mstate5
                 run_state5     = mstate_run_state_read  mstate5
             if (self_loop_halt
                 && (pc3 == pc5)
                 && (run_state3 == Run_State_Running)
                 && (run_state5 == Run_State_Running))
               then (do
                        putStrLn ("Stopping due to self-loop at PC " ++ (showHex pc5 "") ++
                                  "; instret = " ++ show instret)
                        return (0, mstate5))
               else (do
                        -- Continue run-loop (tail recursive)
                        run_loop  maxinstrs  m_tohost_addr  mstate5))

-- ================================================================
-- Fetch and execute an instruction (RV32 32b instr or RV32C 16b compressed instr)
-- First check if any interrupt is pending and, if so, update the
--     state to the trap vector (so, the fetched instr will be the
--     first instruction in the trap vector).

fetch_and_execute :: Machine_State -> IO  Machine_State
fetch_and_execute    mstate = do
  let verbosity               = mstate_verbosity_read  mstate
      mstate1                 = mstate_last_instr_trapped_write  mstate  False
      (intr_pending, mstate2) = mstate_take_interrupt_if_any  mstate1

  -- Debug-print when we take an interrupt
  case intr_pending of
    Nothing       -> return ()
    Just exc_code -> do
      let instret = mstate_csr_read  mstate2  csr_addr_minstret
      when (verbosity >= 1) (
        putStrLn  ("Taking interrupt; instret = " ++ show instret ++
                   "; exc_code = " ++ (showHex  exc_code  "") ++
                   "\n"))
      when (verbosity >= 2) (
        do
          putStrLn "---------------- State before interrupt trap setup"
          mstate_print  "  "  mstate
          putStrLn "---------------- State after interrupt trap setup"
          mstate_print  "  "  mstate2)
  -- End debug-print

  -- Fetch an instruction
  let pc                      = mstate_pc_read  mstate2
      instret                 = mstate_csr_read  mstate2  csr_addr_minstret
      (fetch_result, mstate3) = instr_fetch  mstate2
      priv                    = mstate_priv_read  mstate3

  -- Handle fetch-exception or execute
  case fetch_result of
    Fetch_Trap  ec -> (do
                          when (verbosity >= 1) (putStrLn ("Fetch Trap:" ++ show_trap_exc_code  ec))
                          return mstate3)
    Fetch_C  u16 -> (do
                        -- Exec 'C' instruction
                        let (mstate4, spec_name) = (exec_instr_16b  u16  mstate3)
                        when (verbosity >= 1)
                          (do
                              putStr  ("inum:" ++ show (instret + 1))
                              putStr  ("  pc 0x" ++ (showHex pc ""))
                              putStr  ("  instr.C 0x_" ++ show_wordXL  16  '0'  u16)
                              putStr  ("  priv " ++ show (priv))
                              putStrLn ("  " ++ spec_name))
                        when (verbosity > 1) (mstate_print  "  "  mstate4)
                        return  mstate4)
    Fetch    u32 -> (do
                        -- Exec 32b instruction
                        let (mstate4, spec_name) = (exec_instr_32b  u32  mstate3)
                        when (verbosity >= 1)
                          (do
                              putStr  ("inum:" ++ show (instret + 1))
                              putStr  ("  pc 0x" ++ (showHex pc ""))
                              putStr  ("  instr 0x_" ++ show_wordXL  32  '0'  u32)
                              putStr  ("  priv " ++ show (priv))
                              putStrLn ("  " ++ spec_name))
                        when (verbosity > 1) (mstate_print  "  "  mstate4)
                        return  mstate4)

-- ================================================================
-- Read the word in mem [tohost_addr], if such an addr is given,
-- and if no read exception.

mstate_mem_read_tohost :: Machine_State -> Maybe Integer -> (Integer, Machine_State)
mstate_mem_read_tohost  mstate  Nothing            = (0, mstate)
mstate_mem_read_tohost  mstate  (Just tohost_addr) =
  let
    (load_result, mstate') = mstate_mem_read  mstate  exc_code_load_access_fault  funct3_LW  tohost_addr
  in
    case load_result of
      Mem_Result_Err  exc_code -> (  0, mstate')
      Mem_Result_Ok   u64      -> (u64, mstate')

-- ================================================================
-- Check for tty console input and, if any, buffer it in the UART.

get_tty_input  :: Machine_State -> IO (Machine_State)
get_tty_input  mstate = do
  let mtime = mstate_mem_read_mtime  mstate

  -- Check for console input (for efficiency, only check at certain mtime intervals)
  console_input <- if (mtime .&. 0x3FF == 0) then hGetLine_polled  stdin
                   else return ""

  {- Debugging
  when (console_input /= "") (do
                                 putStrLn ("Providing input to UART: [" ++ console_input ++ "]")
                                   hFlush  stdout)
  -}

  -- Buffer console input, if any, into Machine_State.Mem.MMIO.UART
  let mstate' = if (console_input == "") then
                  mstate
                else
                  mstate_mem_enq_console_input  mstate  console_input

  return mstate'

-- ================
-- Get a line from stdin if any input is available.
-- Return empty string if none available.
-- We loop using hGetChar, instead of just calling hGetLine, to also
--     get the '\n' at the end if it exists  (hGetLine drops '\n').
-- Note: with normal stdin buffering, this won't actually return
-- anything until a complete line has been typed in, ending in \n or
-- EOF.

hGetLine_polled  :: Handle -> IO (String)
hGetLine_polled h = do
  input_available <- hWaitForInput  h  0
  if not input_available
    then return ""
    else (do
             ch  <- hGetChar  h
             chs <- hGetLine_polled  h
             return  (ch:chs))

-- ================================================================
-- If the UART has any buffered output, consume it and write it to the terminal

put_tty_output :: Machine_State -> IO (Machine_State)
put_tty_output  mstate = do
  let (console_output, mstate') = mstate_mem_deq_console_output  mstate
  when (console_output /= "") (do
                                  putStr  console_output
                                  hFlush  stdout)
  return mstate'

-- ================================================================

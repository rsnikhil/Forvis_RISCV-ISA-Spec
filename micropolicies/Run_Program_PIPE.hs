-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Run_Program_PIPE where

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
import Data.Bits

-- Project imports

import Arch_Defs
import Machine_State
--import CSR_File

import Forvis_Spec
import Forvis_Spec_I

import PIPE

import Debug.Trace

-- ================================================================
-- Simplified compared to the one in /src

data Reason = Halted String
            | OutOfGas 
            | PIPEError String
            deriving (Show)

run_loop :: Int -> PIPE_State -> Machine_State -> (Reason, PIPE_State, Machine_State) 
run_loop  maxinstrs pipe_state mstate =
  let instret   = mstate_csr_read        mstate  csr_addr_minstret
      run_state = mstate_run_state_read  mstate

      -- Tick: regular maintenance (increment cycle count, real-time
      -- timer, propagate interrupts, etc.)
      mstate1 = mstate_mem_tick  mstate

    -- Simulation aid: Stop due to instruction limit
    in if traceShow instret (instret >= fromIntegral maxinstrs)
    then  (OutOfGas, pipe_state, mstate1)

    -- Simulation aid: Stop due to any other reason
    else if ((run_state /= Run_State_Running) && (run_state /= Run_State_WFI))
    then (Halted $ "Stopping due to runstate " ++ show run_state ++ "; instret = " ++ show instret,
          pipe_state, mstate1)

    -- Fetch-and-execute instruction or WFI-resume, and continue run-loop
    else -- (Someday, we may want to check for user input and buffer it into the uart; 
         --  ditto uart output stuff)
         -- If running, fetch-and-execute; if in WFI pause, check resumption
         if (run_state == Run_State_Running) then
            case fetch_and_execute pipe_state mstate1 of
              Right (ps, ms) -> run_loop maxinstrs ps ms
              Left s -> (PIPEError s, pipe_state, mstate1)
         else error "Unimplemented WFI stuff"
{-
                        else
                          -- run_state == Run_State_WFI
                          do
                            let resume    = mstate_wfi_resume  mstate3
                                mstate3_a = if (resume) then
                                              mstate_run_state_write  mstate3  Run_State_Running
                                            else
                                              mstate3
                            return (pipe_state, mstate3_a)   -- WRONG!
 
                        run_loop  maxinstrs  m_tohost_addr  pipe_state1 mstate5))
-}

-- ================================================================
-- Fetch and execute an instruction (RV32 32b instr or RV32C 16b compressed instr)
-- First check if any interrupt is pending and, if so, update the
--     state to the trap vector (so, the fetched instr will be the
--     first instruction in the trap vector).

fetch_and_execute :: PIPE_State -> Machine_State -> Either String (PIPE_State, Machine_State)
fetch_and_execute pipe_state mstate = 
  let _verbosity               = mstate_verbosity_read  mstate
      (intr_pending, mstate2) = take_interrupt_if_any  mstate

  -- Fetch an instruction
      _pc                      = mstate_pc_read  mstate2
      _instret                 = mstate_csr_read  mstate2  csr_addr_minstret
      (fetch_result, mstate3) = instr_fetch  mstate2
      _priv                    = mstate_priv_read  mstate3

  -- Handle fetch-exception of execute
  in case fetch_result of
    Fetch_Trap  _ec -> Right (pipe_state, mstate3)  -- WRONG?
    Fetch_C  u16 -> let (mstate4, _spec_name) = (exec_instr_16b u16 mstate3)
                    in Right (pipe_state, mstate4)  --WRONG?

    Fetch    u32 ->
      traceShow ("Executing...", decode_I RV32 u32) $
      let (mstate4, _spec_name) = (exec_instr_32b  u32   mstate3)
          (pipe_state1, trap) = exec_pipe pipe_state mstate3 u32 
      in case trap of 
           PIPE_Trap s -> Left s
           PIPE_Success -> Right (pipe_state1, mstate4)

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

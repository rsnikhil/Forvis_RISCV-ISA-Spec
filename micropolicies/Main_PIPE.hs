-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Main where

-- ================================================================
-- This is the 'main' function of the program, which just dispatches
-- to one of several possible 'main' functions for different use-cases
-- of Forvis.

-- ================================================================
-- Standard Haskell imports

-- none

-- Project imports

import Main_Run_Program_PIPE

--
import Arch_Defs
import Forvis_Spec_I
import PIPE
import Memory
import Data.Bits

import qualified Data.Map.Strict as Data_Map
import Machine_State

import Run_Program_PIPE

-- ================================================================

main :: IO ()

-- Uncomment just one of the following 'main = ' use-cases

-- ================================================================
-- Use this for a standalone RISC-V simulator that loads and runs an
-- ELF file

-- main = main_run_program

-- ================================================================
-- Use this section for a Tandem Verifier server

-- main = main_tandem_verifier

-- ================================================================
-- Use this section to test Virtual Memory translation

-- main = main_test_virtual_mem

-- ================================================================

--

main =
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
      ms = mkMachine_State  RV32  misa  initial_PC  addr_ranges  addr_byte_list

      --- Generate input program + tags
      -- Tags in call/ret f1-2-3-4-5
      -- Memory colors in movs
      -- add heap_base
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
      heap_base = 100
      base_code =
        [ (0, encode_I RV32 (ADDI 1 0 heap_base))
        , (1, encode_I RV32 (ADD  1 0 1))
        , (2, encode_I RV32 (SW   1 1 0))
        ]
      accept_code =
        [ (3, encode_I RV32 (LW 2 1 0)) ]
      reject_code =
        [ (3, encode_I RV32 (ADDI 2 0 heap_base))
        , (4, encode_I RV32 (LW 3 2 0)) ]
      mem_acc = (f_mem ms) { f_dm = Data_Map.fromList (base_code ++ accept_code) }
      mem_rej = (f_mem ms) { f_dm = Data_Map.fromList (base_code ++ reject_code) }
      ms_acc = ms { f_mem = mem_acc }
      ms_rej = ms { f_mem = mem_rej }
  in do
    -- Here: pass the tags to pipe
    (n, ps, ms) <- run_loop 100 Nothing init_pipe_state ms_acc
    putStrLn $ show (n, ps)
    mstate_print "acc" ms

    (n, ps, ms) <- run_loop 100 Nothing init_pipe_state ms_rej
    putStrLn $ show (n, ps)
    mstate_print "rej" ms


-----------

--  putStrLn $ show $ decode_I RV32 (encode_I RV32 (ADDI 2 3 42))


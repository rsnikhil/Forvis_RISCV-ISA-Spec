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

      mem' = (f_mem ms) { f_dm = Data_Map.fromList [(0, encode_I RV32 (ADDI 2 3 42))] }
      ms' = ms { f_mem = mem'}
  in do
    (n, ps, ms) <- run_loop 100 Nothing init_pipe_state ms' 
    putStrLn $ show (n, ps)
    mstate_print "foo" ms

-----------

--  putStrLn $ show $ decode_I RV32 (encode_I RV32 (ADDI 2 3 42))


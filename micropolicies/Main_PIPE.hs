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

import Gen
import Printing
import Test.QuickCheck

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

main = do
  ms <- head <$> sample' genMachine
  (n, ps, ms) <- run_loop 100 Nothing init_pipe_state ms
  print_pipe ps
  print_mstate "gen" ms
  
  let (ms_acc, ms_rej) = exampleMachines

  -- Here: pass the tags to pipe
  (n, ps, ms) <- run_loop 100 Nothing init_pipe_state ms_acc
  print_pipe ps
  print_mstate "acc" ms

  (n, ps, ms) <- run_loop 100 Nothing init_pipe_state ms_rej
  print_pipe ps
  print_mstate "rej" ms


-----------

--  putStrLn $ show $ decode_I RV32 (encode_I RV32 (ADDI 2 3 42))


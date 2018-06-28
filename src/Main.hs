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

import Main_Run_Program
import Main_Tandem_Verifier
import Main_Test_Virtual_Mem

-- ================================================================

main :: IO ()

-- Uncomment just one of the following 'main = ' use-cases

-- ================================================================
-- Use this for a standalone RISC-V simulator that loads and runs an
-- ELF file

main = main_run_program

-- ================================================================
-- Use this section for a Tandem Verifier server

-- main = main_tandem_verifier

-- ================================================================
-- Use this section to test Virtual Memory translation

-- main = main_test_virtual_mem

-- ================================================================

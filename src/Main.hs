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

-- ================================================================

main :: IO ()

-- Uncomment just one of the following use-cases

-- As standalone execution engine: ELF filename(s) on command line: load ELF and run
main = main_run_program

-- As Tandem Verifier
-- main = main_tandem_verifier

-- ================================================================

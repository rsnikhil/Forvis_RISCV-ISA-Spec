module Main where

-- ================================================================
-- This is the 'main' function of the program, which just dispatches
-- to one of several possible 'main' functions for different use-cases
-- of the RISC-V ISA Formal Semantics.

-- ================================================================
-- Standard Haskell imports

-- none

-- Project imports

import Main_RunProgram
import Main_TandemVerifier

-- ================================================================

main :: IO ()

-- Uncomment just one of the following use-cases

-- As standalone execution engine: ELF filename(s) on command line: load ELF and run
main = main_RunProgram

-- As Tandem Verifier
-- main = main_TandemVerifier

-- ================================================================

module RunProgram where

-- ================================================================
-- This module is the 'run loop' for a RISV-V program

-- ================================================================
-- Standard Haskell imports

import Control.Monad
import System.IO
import Data.Int
import Data.List
import Data.Word
import Numeric (showHex, readHex)

-- Project imports

import ArchDefs64
import ArchState64
import CSRFile

import Decode
import ExecuteInstr

-- ================================================================
-- The 'run loop': runs a program up to a given maximum # of instructions executed.
-- Takes an architecture state and returns the new architecture state.
-- Fetches, decodes and executes one instruction; and repeats.

runProgram :: Int -> ArchState64 -> IO ArchState64
runProgram  maxinstrs  astate = do
  let instret = get_ArchState64_csr  astate  CSR_minstret
  if instret >= fromIntegral maxinstrs
    then do
      putStrLn ("Reached instret limit (" ++ show maxinstrs ++ "); exiting")
      set_ArchState64_stop  astate  Stop_Limit

    else if (get_ArchState64_stop  astate /= Stop_Running) then do
        putStrLn ("Reached stop; instret = " ++ show instret ++ "; exiting")
        return astate

         else do
           when (get_ArchState64_verbosity astate > 1)
             (do
                 putStrLn "Executing instr ================"
                 print_ArchState64 "  "  astate)

           let instr_word = ifetch astate                  -- FETCH
               instr      = decode xlen instr_word         -- DECODE
               pc         = get_ArchState64_PC  astate

           when (get_ArchState64_verbosity astate == 1)
             (do
                 putStr (show (instret + 1))
                 putStr ("  pc 0x" ++ (showHex pc ""))
                 putStr ("  instr 0x" ++ showHex instr_word "")
                 putStrLn ("  " ++ show instr))

           astate' <- executeInstr  astate  instr          -- EXECUTE

           if pc /= get_ArchState64_PC  astate' then

             runProgram  maxinstrs  astate'                -- LOOP (tail-recursive call)

           else do
             let instret = get_ArchState64_csr  astate  CSR_minstret
             putStrLn ("Reached jump-to-self infinite loop; instret = " ++ show instret ++ "; exiting")
             return astate'

-- ================================================================

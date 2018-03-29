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

import ArchDefs
import ArchState
import CSRFile

import Decode
import ExecuteInstr

-- ================================================================
-- The 'run loop': runs a program up to a given maximum # of instructions executed.
-- Takes an architecture state and returns the new architecture state.
-- Fetches, decodes and executes one instruction; and repeats.

runProgram :: Int -> ArchState -> IO ArchState
runProgram  maxinstrs  astate = do
  let instret = get_ArchState_csr  astate  csr_addr_minstret
      rv      = get_ArchState_rv  astate
      misa    = get_ArchState_csr  astate  csr_addr_misa
  if instret >= fromIntegral maxinstrs
    then (do
             putStrLn ("Reached instret limit (" ++ show maxinstrs ++ "); exiting")
             set_ArchState_stop  astate  Stop_Limit)

    else if (get_ArchState_stop  astate /= Stop_Running)
         then (do
                  putStrLn ("Reached stop; instret = " ++ show instret ++ "; exiting")
                  return astate)
         else (do
                  when (get_ArchState_verbosity astate > 1)
                    (do
                        putStrLn "Executing instr ================"
                        print_ArchState "  "  astate)

                  let (result_instr, astate1) = ifetch astate         -- FETCH
                  case (result_instr) of
                    LoadResult_Err cause -> (do
                                                putStrLn ("Instruction-fetch fault: " ++ show (cause))
                                                putStrLn ("    instret = " ++ show instret ++ "; exiting")
                                                return astate1)
                    LoadResult_Ok instr_word ->
                      (do
                          let instr = decode  rv  misa  instr_word         -- DECODE
                              pc    = get_ArchState_PC  astate1

                          when (get_ArchState_verbosity astate1 >= 1)
                            (do
                                putStr (show (instret + 1))
                                putStr ("  pc 0x" ++ (showHex pc ""))
                                putStr ("  instr 0x" ++ showHex instr_word "")
                                putStrLn ("  " ++ show instr))

                          astate2 <- executeInstr  astate1  instr          -- EXECUTE

                          if (pc /= get_ArchState_PC  astate2) then
                            runProgram  maxinstrs  astate2                -- LOOP (tail-recursive call)
                          else (do
                                   let instret = get_ArchState_csr  astate2  csr_addr_minstret
                                   putStrLn ("Reached jump-to-self infinite loop; instret = " ++ show instret ++ "; exiting")
                                   return astate2)))

-- ================================================================

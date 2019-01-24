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

--
import Arch_Defs
import Forvis_Spec_I
import PIPE
import Memory
import Data.Bits

import qualified Data.Map.Strict as Data_Map
import Machine_State 

import Run_Program_PIPE
import Generator (genASTFile,genSymbolsFile)

import Test.QuickCheck

import Gen
import TestHeapSafety
import Shrinking

main_example = do
  ppol@(name,pol,symbols) <- load_pipe_policy "heap.main"
  putStrLn $ "module name = " ++ (show name)
  genASTFile (Just pol)
  genSymbolsFile symbols
  let x = mkTagSet ppol ["test","CP"] [Just 42,Just 99]
  putStrLn $ show (rdTagSet ppol x)

main_test = do
  ppol@(name,pol,symbols) <- load_pipe_policy "heap.main"  
  quickCheckWith stdArgs{maxSuccess=1000} $ forAllShrink (genMStatePair ppol) (shrinkMStatePair ppol) $ \m ->
    prop_noninterference ppol m

main = main_test

instance Show Machine_State where
  show _ = ""

instance Show MStatePair where
  show _ = ""

{-main2 = do
  let ((ms,ps),_) = exampleMachines
  pol <- load_pipe_policy "heap"
  let (res, tr) = run_loop pol 10 ps ms
      (ps', ms') : _ = tr
  putStrLn ""
  putStrLn "Initial state:"
  print_coupled ms ps
  putStrLn "_______________________________________________________________________"
  putStrLn "Final state:"
  print_coupled ms' ps'
  putStrLn "_______________________________________________________________________"
  putStrLn "Trace:"
  putStrLn $ showTrace (reverse tr)
  putStrLn (show res)

main = do
  (ms,ps) <- head <$> sample' genMachine
  pol <- load_pipe_policy "heap"
  let (res, tr) = run_loop pol 100 ps ms
      (ps', ms') : _ = tr
  putStrLn ""
  putStrLn "Initial state:"
  print_coupled ms ps
  putStrLn "_______________________________________________________________________"
  putStrLn "Final state:"
  print_coupled ms' ps'
  putStrLn "_______________________________________________________________________"
  putStrLn "Trace:"
  putStrLn $ showTrace (reverse tr)
  putStrLn (show res)
  putStrLn $ "Instructions executed: " ++ show (mstate_csr_read ms' csr_addr_minstret)
  
main1 = do
  quickCheckWith stdArgs{maxSuccess=1000} testHeapSafety
-}
--  (ms,ps) <- head <$> sample' genMachine
--  let (res, (ps', ms') : _ ) = run_loop 5 ps ms
--  putStrLn (show res)
--  print_coupled ms ps
--  print_coupled ms' ps'

--  let ((ms_acc,p_acc),(ms_rej,p_rej)) = exampleMachines
--  
--  -- Accept
--  let (res, ps, ms') = run_loop 100 p_acc ms_acc
--  putStrLn (show res)
--  print_coupled ms' ps
-- 
--  -- Reject
--  let (res, ps, ms') = run_loop 100 p_rej ms_rej
--  putStrLn (show res)
--  print_coupled ms' ps

-- main = testHeapSafety
  
  -- let (ms_acc, ms_rej) = exampleMachines

{-
  -- Here: pass the tags to pipe
  (n, ps, ms) <- run_loop 100 init_pipe_state ms_acc
  print_pipe ps
  print_mstate "acc" ms

  (n, ps, ms) <- run_loop 100 init_pipe_state ms_rej
  print_pipe ps
  print_mstate "rej" ms
-}

--  putStrLn $ show $ decode_I RV32 (encode_I RV32 (ADDI 2 3 42))


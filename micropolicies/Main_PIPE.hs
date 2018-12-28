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
import TestHeapSafety

import Gen
import Printing
import Test.QuickCheck
import Shrinking

instance Show Machine_State where
  show _ = ""

instance Show MStatePair where
  show _ = ""

testHeapSafety =
  forAllShrink genMStatePair shrinkMStatePair $ \m ->
--  sameReachablePart m
  prop_noninterference m

--main2 = do
--  let ((ms,ps),_) = exampleMachines
--  let (res, tr) = run_loop 10 ps ms
--      (ps', ms') : _ = tr
--      ss = zip (reverse ss1') (reverse ss2') 
--  uncurry printTrace (unzip ss)
--  putStrLn ""
--  putStrLn "Initial state:"
--  print_coupled ms ps
--  putStrLn "_______________________________________________________________________"
--  putStrLn "Final state:"
--  print_coupled ms' ps'
--  putStrLn "_______________________________________________________________________"
--  putStrLn "Trace:"
--  putStrLn $ showTrace (reverse tr)
--  putStrLn (show res)

main4 = do
  (ms,ps) <- head <$> sample' genMachine
  let (res, tr) = run_loop 100 ps ms
      (ps', ms') : _ = tr
  putStrLn ""
  putStrLn "Initial state:"
  print_coupled ms ps
  putStrLn "_______________________________________________________________________"
  putStrLn "Final state:"
  print_coupled ms' ps'
--  putStrLn "_______________________________________________________________________"
--  putStrLn "Trace:"
--  putStrLn $ showTrace (reverse tr)
--  putStrLn (show res)
--  putStrLn $ "Instructions executed: " ++ show (mstate_csr_read ms' csr_addr_minstret)

main5 = do
  M (m1,p1) (m2,p2) <- head <$> sample' (genMachine >>= varyUnreachable)
  let (r1,ss1') = run_loop 10 p1 m1
      (r2,ss2') = run_loop 10 p2 m2
      ss = zip (reverse ss1') (reverse ss2') 
  uncurry printTrace (unzip ss)
  
mainHeap = do
  quickCheckWith stdArgs{maxSuccess=10000} testHeapSafety

main3 = do
  let ((ms_acc,p_acc),(ms_rej,p_rej)) = exampleMachines
  
  -- Accept
  let (res, tr) = run_loop 100 p_acc ms_acc
      (ps', ms') : _ = tr
  putStrLn (show res)
  print_coupled ms' ps'
 
  -- Reject
  let (res, tr) = run_loop 100 p_rej ms_rej
      (ps', ms') : _ = tr
  putStrLn (show res)
  print_coupled ms' ps'

main_mangled =
  quickCheck $ prop_noninterference bug_mangled_store_color

--main = main_mangled  
main = mainHeap
  
{-
  -- let (ms_acc, ms_rej) = exampleMachines

  -- Here: pass the tags to pipe
  (n, ps, ms) <- run_loop 100 init_pipe_state ms_acc
  print_pipe ps
  print_mstate "acc" ms

  (n, ps, ms) <- run_loop 100 init_pipe_state ms_rej
  print_pipe ps
  print_mstate "rej" ms
-}

--  putStrLn $ show $ decode_I RV32 (encode_I RV32 (ADDI 2 3 42))


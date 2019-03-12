module Main where

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

import qualified TestHeapSafety
import Printing

import Control.Monad

-- Null "show" functions, for things that we don't want QuickCheck trying to print
instance Show Machine_State where
  show _ = ""
instance Show MStatePair where
  show _ = ""

pol = "heap"

load_policy :: IO PolicyPlus
load_policy = do
  case pol of
    "heap" -> TestHeapSafety.load_policy 
    otherwise -> error $ "unknown policy '" ++ pol ++ "'"

main_trace = do
  pplus <- load_policy
  (M (ms1,ps1) (ms2,ps2)) <- head <$> sample' (genMStatePair pplus pplus)
  let (res, tr) = run_loop pplus 10 ps1 ms1
      (ps', ms') : _ = tr
  putStrLn ""
--  putStrLn "Initial state:"
--  print_coupled pplus ms1 ps1
--  putStrLn "_______________________________________________________________________"
--  putStrLn "Final state:"
--  print_coupled pplus ms' ps'
--  putStrLn "_______________________________________________________________________"
--  putStrLn "Trace:"
  let finalTrace = {- map flipboth $ -} reverse $ zip tr tr
  uncurry (printTrace pplus) (unzip finalTrace)
--  printTrace pplus (reverse tr)
  putStrLn (show res)

-- The real one
main_test = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1000}
    $ forAllShrink (genMStatePair pplus pplus)
           (\mp -> (shrinkMStatePair pplus) pplus mp 
                   ++ concatMap (shrinkMStatePair pplus pplus) (shrinkMStatePair pplus pplus mp))
    $ \m -> prop pplus pplus m

main = main_test


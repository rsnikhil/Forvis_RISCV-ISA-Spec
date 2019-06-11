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
import qualified TestStackSafety
import qualified TestWriteOnce
import Printing

import Control.Monad

pol = "heap"

main :: IO ()
main = do
  case pol of
    "heap"    -> TestHeapSafety.main 
    "stack"   -> TestStackSafety.main
    "writeonce"   -> TestWriteOnce.main
    otherwise -> error $ "unknown policy '" ++ pol ++ "'"



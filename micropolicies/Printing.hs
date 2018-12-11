module Printing where

import Arch_Defs
import Forvis_Spec_I
import PIPE
import Memory
import Data.Bits

import qualified Data.Map.Strict as Data_Map
import Machine_State

import Numeric (showHex, readHex)

import GPR_File
import FPR_File
import CSR_File

import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P

class PP a where
  pp :: a -> Doc

instance PP Tag where
  pp (Tag ()) = P.text "_"


instance PP GPR_FileT where
  pp (GPR_FileT m) =
    P.vcat $ map (\(i,r) -> P.integer i <+> P.char ':' <+> pp r)
           $ Data_Map.assocs m


instance PP PIPE_State where
  pp ps = 
    P.vcat [ P.text "PC Tag:" <+> pp (p_pc ps)
           , P.text "Register Tags:" $$ P.nest 2 (pp $ p_gprs ps)
           -- p_mem
           ]

print_pipe :: PIPE_State -> IO ()
print_pipe ps =
  putStrLn $ P.render $ pp ps
  
print_mstate :: String -> Machine_State -> IO ()
print_mstate  indent  mstate = do
  let pc   = f_pc    mstate
      gprs = f_gprs  mstate
      fprs = f_fprs  mstate
      csrs = f_csrs  mstate
      priv = f_priv  mstate

      rv        = f_rv    mstate
      run_state = f_run_state  mstate

      xlen      = if (rv == RV32) then 32 else 64
  
  putStrLn (indent ++ show rv ++ " pc:" ++ showHex pc " priv:" ++ show priv)
  print_GPR_File  indent  xlen  gprs

  -- We do not care a bout the floating point registers
  -- print_FPR_File  indent  64    fprs    -- FPR always stored as 64-bit
  
  print_CSR_File  indent  rv  csrs
  -- We do not print memory or MMIO
  putStrLn (indent ++ (show run_state))

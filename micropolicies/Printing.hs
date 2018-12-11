{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
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

import Data.List.Split(chunksOf)

class PP a where
  pp :: a -> Doc

instance PP Tag where
  pp (Tag ()) = P.text "_"

instance PP Integer where
  pp n = P.sizedText 2 $ show n

instance PP GPR_FileT where
  pp (GPR_FileT m) =
    P.vcat $ map (\(i,r) -> pp i <+> P.char ':' <+> pp r)
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

class CoupledPP a b | a -> b where
  pretty :: a -> b -> Doc

instance CoupledPP Integer Tag where
  pretty d t = pp d P.<> P.char '@' P.<> pp t

instance CoupledPP GPR_File GPR_FileT where
  pretty (GPR_File m) (GPR_FileT mt) =
    P.vcat $ map P.hcat
           $ chunksOf 4
           $ map (\((i,d),(i', t)) -> P.integer i <+> P.char ':' <+> pretty d t)
           $ zip (Data_Map.assocs m) (Data_Map.assocs mt)

instance CoupledPP Machine_State PIPE_State where
  pretty ms ps =
    P.vcat [ P.text "PC Tag:" <+> pretty (f_pc ms) (p_pc ps)
           , P.text "Register Tags:" $$ P.nest 2 (pretty (f_gprs ms) (p_gprs ps))
           -- p_mem
           ]

print_coupled :: Machine_State -> PIPE_State -> IO ()
print_coupled ms ps =
  putStrLn $ P.render $ pretty ms ps
  
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

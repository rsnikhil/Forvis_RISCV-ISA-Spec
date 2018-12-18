{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Printing where

import Arch_Defs
import Forvis_Spec_I
import PIPE
import Memory
import Data.Bits
import Data.Maybe (isJust)

import qualified Data.Map.Strict as Data_Map
import Machine_State

import Numeric (showHex, readHex)

import GPR_File
import FPR_File
import CSR_File

import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P

import Control.Arrow (second)
import Data.List.Split(chunksOf)

class PP a where
  pp :: a -> Doc

instance PP Color where
  pp (C n) = P.text $ (["","A","B","C","D","E","X","Y","Z","W"] ++ (repeat "BIGCOLOR")) !! n

instance PP AllocOrNot where
  pp NoAlloc = P.text ""
  pp Alloc = P.text "Alloc"

instance PP Tag where
  pp MTagP = P.text ""
  pp (MTagI a) = pp a
  pp (MTagR c) = pp c
  pp (MTagM (C 0) (C 0)) = P.text ""
  pp (MTagM cv cl) = pp cv P.<> P.text "/" P.<> pp cl

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

-- Helpers
x <|> y = x P.<> P.text "\t|\t" P.<> y
x <:> y = x P.<> P.text ": " P.<> y

pr_register :: InstrField -> Doc
pr_register n = P.char 'r' P.<> P.integer n  

pr_instr_I_type :: String -> InstrField -> InstrField -> InstrField -> Doc
pr_instr_I_type label rd rs imm =
  P.text label <+> pr_register rd <+> pr_register rs <+> P.integer imm

pr_instr_R_type :: String -> InstrField -> InstrField -> InstrField -> Doc
pr_instr_R_type label rd rs1 rs2  =
  P.text label <+> pr_register rd <+> pr_register rs1 <+> pr_register rs2

pr_instr_J_type :: String -> InstrField -> InstrField -> Doc
pr_instr_J_type label rs imm =
  P.text label <+> pr_register rs <+> P.integer imm

instance PP Instr_I where
  pp (ADDI rd rs imm) = pr_instr_I_type "ADDI" rd rs imm
  pp (LW rd rs imm) = pr_instr_I_type "LW" rd rs imm
  pp (SW rd rs imm) = pr_instr_I_type "SW" rd rs imm
  pp (ADD rd rs1 rs2) = pr_instr_R_type "ADD" rd rs1 rs2
  pp (JAL rs imm) = pr_instr_J_type "JAL" rs imm

pr_imem :: Mem -> Doc
pr_imem m =
  let contents = Data_Map.assocs $ f_dm m 
      decoded  = filter (isJust . snd) $ map (second $ decode_I RV32) contents
  in P.vcat $ map (\(i, Just instr) -> P.integer i <:> pp instr) decoded

-- IDEAS: only show non-trivial registers?

-- TODO: Align better, tabs don't work well
instance CoupledPP GPR_File GPR_FileT where
  pretty (GPR_File m) (GPR_FileT mt) =
    P.vcat $ map (foldl1 (<|>))
           $ chunksOf 4
           $ map (\((i,d),(i', t)) -> P.integer i <+> P.char ':' <+> pretty d t)
           $ zip (Data_Map.assocs m) (Data_Map.assocs mt)

instance CoupledPP Machine_State PIPE_State where
  pretty ms ps =
    P.vcat [ P.text "PC:" <+> pretty (f_pc ms) (p_pc ps)
           , P.text "Registers:" $$ P.nest 2 (pretty (f_gprs ms) (p_gprs ps))
           , P.text "IMem:" $$ P.nest 2 (pr_imem (f_mem ms))
           , P.text "Mem:" $$ P.text (show (Data_Map.assocs (f_dm (f_mem ms))))
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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Printing where

import Arch_Defs
import Forvis_Spec_I
import PIPE
import Memory
import Data.Bits
import Data.Maybe (isJust)

import qualified Data.Map.Strict as Data_Map
import qualified Data.Set as Data_Set
import Data.Set (Set)
import Machine_State

import Numeric (showHex, readHex)

import GPR_File
import FPR_File
import CSR_File

import Text.PrettyPrint (Doc, (<+>), ($$), fcat)
import qualified Text.PrettyPrint as P

import Control.Arrow (second)
import Data.List.Split (chunksOf)

import Data.List (intercalate)

import Terminal

-- TODO: This doesn't need pplus any more, so we could remove it from
-- all the printing stuff!!
class PP a where
  pp :: PolicyPlus -> a -> Doc

instance PP TagSet where
  -- pp t = P.text (show t)
  pp pplus t = P.text (showTagSet t)

instance PP Integer where
  pp _ n = P.sizedText 2 $ show n

instance PP GPR_FileT where
  pp pplus (GPR_FileT m) =
    P.vcat $ map (\(i,r) -> pp pplus i <+> P.char ':' <+> pp pplus r)
           $ Data_Map.assocs m

instance PP PIPE_State where
  pp pplus ps = 
    P.vcat [ P.text "PC Tag:" <+> pp pplus (p_pc ps)
           , P.text "Register Tags:" $$ P.nest 2 (pp pplus $ p_gprs ps)
           -- p_mem
           ]

print_pipe :: PolicyPlus -> PIPE_State -> IO ()
print_pipe pplus ps =
  putStrLn $ P.render $ pp pplus ps

class CoupledPP a b | a -> b where
  pretty :: PolicyPlus -> a -> b -> Doc

instance CoupledPP Integer TagSet where
  pretty pplus d t = pp pplus d P.<> P.char ' ' P.<> pp pplus t

-- Helpers
x <|> y = x P.<> P.text "\t|\t" P.<> y
x <:> y = x P.<> P.text ": " P.<> y
x <@> y = x P.<> P.text " " P.<> y
x <||> y = x P.<> P.text " || " P.<> y

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
  pp _ (ADD 0 0 0) = P.text "<NOP>"
  pp _ (ADDI rd rs imm) = pr_instr_I_type "ADDI" rd rs imm
  pp _ (LW rd rs imm) = pr_instr_I_type "LW" rd rs imm
  pp _ (SW rd rs imm) = pr_instr_I_type "SW" rd rs imm
  pp _ (ADD rd rs1 rs2) = pr_instr_R_type "ADD" rd rs1 rs2
  pp _ (JAL rs imm) = pr_instr_J_type "JAL" rs imm
  pp _ i = error $ show i

pr_imem :: Mem -> PolicyPlus -> Doc
pr_imem m pplus =
  let contents = Data_Map.assocs $ f_dm m 
      decoded  = filter (isJust . snd) $ map (second $ decode_I RV32) contents
  in P.vcat $ map (\(i, Just instr) -> pad 4 (P.integer i) <:> pp pplus instr) decoded

pr_mem :: Mem -> PolicyPlus -> Doc
pr_mem m pplus = 
  let contents = Data_Map.assocs $ f_dm m 
      decoded  = filter (not . isJust . decode_I RV32 . snd) contents
  in P.vcat $ map (\(i, d) -> P.integer i <:> P.integer d) decoded

-- instance CoupledPP GPR_File GPR_FileT where
--   pretty pplus (GPR_File m) (GPR_FileT mt) =
--     P.vcat $ map (foldl1 (<|>))
--            $ chunksOf 4
--            $ map (\((i,d),(i', t)) -> P.integer i <+> P.char ':'
--                                       <+> pretty pplus d t)
--            $ zip (Data_Map.assocs m) (Data_Map.assocs mt)

instance CoupledPP GPR_File GPR_FileT where
  pretty pplus (GPR_File m) (GPR_FileT mt) =
    fcat $
    map (\((i,d),(i', t)) ->
            P.integer i <+> P.char ':' <+> pretty pplus d t <+> P.text "  ")
      $ filter (\((_,d),(_,t)) -> (d /= 0) || (t /= initGPR pplus))
      $ zip (Data_Map.assocs m) (Data_Map.assocs mt)

instance CoupledPP Mem MemT where
  pretty pplus (Mem m _) (MemT pm) =
    let contents = zip (Data_Map.assocs $ m) (Data_Map.assocs pm)
    in P.vcat $ map (\((i,d),(j,t)) ->
                        case decode_I RV32 d of
                          Just instr -> P.integer i <:> pp pplus instr <@> pp pplus t
                          Nothing -> P.integer i <:> P.integer d <@> pp pplus t
                    ) contents

instance CoupledPP Machine_State PIPE_State where
  pretty pplus ms ps =
    P.vcat [ P.text "PC:" <+> pretty pplus (f_pc ms) (p_pc ps)
           , P.text "Registers:" $$ P.nest 2 (pretty pplus (f_gprs ms) (p_gprs ps))
           , P.text "Memories:" $$ pretty pplus (f_mem ms) (p_mem ps)
           ]

instance CoupledPP (Integer, TagSet) (Integer, TagSet) where
  pretty pplus (i1, t1) (i2, t2) =
    if i1 == i2 && t1 == t2 then
      pretty pplus i1 t1 
    else
      ppStrong (pretty pplus i1 t1 <||> pretty pplus i2 t2)

instance CoupledPP (GPR_File, GPR_FileT) (GPR_File, GPR_FileT) where
  pretty pplus (GPR_File r1, GPR_FileT t1) (GPR_File r2, GPR_FileT t2) =
    if r1 == r2 && t1 == t2 then 
      pretty pplus (GPR_File r1) (GPR_FileT t1)
    else -- TODO: Fix printing
      P.fcat $ map (\(((i,d1),(_,t1)),((_,d2),(_,t2))) ->
                if d1 == d2 && t1 == t2 then 
                  P.integer i <+> P.char ':' <+> pretty pplus d1 t1
                else
                  P.integer i <+> P.char ':' <+> ppStrong (pretty pplus d1 t1 <||> pretty pplus d2 t2)
                   )
             $ filter (\ (((_,d1),(_,t1)),((_,d2),(_,t2))) ->
                            (d1 /= 0) || (t1 /= initGPR pplus) ||
                            (d2 /= 0) || (t2 /= initGPR pplus))
             $ zip (zip (Data_Map.assocs $ r1) (Data_Map.assocs $ t1))
                   (zip (Data_Map.assocs $ r2) (Data_Map.assocs $ t2))
    
instance CoupledPP (Mem, MemT) (Mem, MemT) where
  pretty pplus (Mem m1 _, MemT p1) (Mem m2 _, MemT p2) =
    let c1 = zip (Data_Map.assocs $ m1) (Data_Map.assocs p1)
        c2 = zip (Data_Map.assocs $ m2) (Data_Map.assocs p2)

        pr_loc ((i,d),(j,t)) =
          case decode_I RV32 d of
            Just instr
              | i == 0 || i >= 1000 -> pad 6 (P.integer i) <+> pp pplus instr <@> pp pplus t
              | otherwise -> pad 6 (P.integer i) <+> P.integer d <@> pp pplus t
            Nothing -> pad 6 (P.integer i) <+> P.integer d <@> pp pplus t
          
        pr_aux acc [] [] = reverse acc
        pr_aux acc [] (loc:locs) = pr_aux ((P.text "R:" <+> pr_loc loc) : acc) [] locs
        pr_aux acc (loc:locs) [] = pr_aux ((P.text "L:" <+> pr_loc loc) : acc) locs []
        pr_aux acc (((i1,d1),(_,t1)):loc1) (((i2,d2),(_,t2)):loc2)
          | i1 == i2 && d1 == d2 && t1 == t2 =
            pr_aux (pr_loc ((i1,d1),(i1,t1)) : acc) loc1 loc2
          | i1 == i2 =
            pr_aux ((ppStrong ((pr_loc ((i1,d1),(i2,t1)) <||> pr_loc ((i2,d2),(i2,t2)))) : acc)) loc1 loc2
          | i1 < i2 =
            pr_aux (P.text "L:" <+> pr_loc ((i1,d1),(i1,t1)) : acc) loc1 (((i2,d2),(i2,t2)):loc2)
          | i1 > i2 = 
            pr_aux (P.text "R:" <+> pr_loc ((i2,d2),(i2,t2)) : acc) (((i1,d1),(i1,t1)):loc1) loc2

    in P.vcat $ pr_aux [] c1 c2

instance PP Color where
  -- LATER: Pretty printing of colors according to the policy??
  pp pplus n = P.int n
  
instance CoupledPP (Set Color) (Set Color) where
  pretty pplus s1 s2 =
    if s1 == s2 then foldl1 (<+>) (map (pp pplus) $ Data_Set.elems s1)
    else
      ppStrong (foldl1 (<+>) (map (pp pplus) $ Data_Set.elems s1) <||> foldl1 (<+>) (map (pp pplus) $ Data_Set.elems s2))

print_coupled :: PolicyPlus -> Machine_State -> PIPE_State -> IO ()
print_coupled pplus ms ps =
  putStrLn $ P.render $ pretty pplus ms ps
  
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

pad :: Int -> Doc -> Doc
pad i p = let s = show p in
          P.text (s ++ take (i - (length s)) (repeat ' '))

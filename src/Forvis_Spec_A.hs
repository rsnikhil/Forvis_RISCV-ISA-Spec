-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_A where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'A' Extension
-- i.e., Atomic Memory Ops (AMO)

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import Arch_Defs
import Machine_State
import Virtual_Mem

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- 'A' Extension

-- Note: the following are defined in module Arch_Defs
--     opcode_AMO, funct3_AMO_W/D, msbs5_AMO_LR/SC/ADD/SWAP/XOR/AND/OR/MIN/MAX/MINU/MAXU

-- ================================================================
-- The following is a list of all the specification functions defined below.

instr_specs_A :: [(Instr_Spec, String)]
instr_specs_A = [ (spec_AMO, "AMO")
                ]

-- ================================================================

spec_AMO :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_AMO    mstate           instr    is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr
    (msbs5, aq, rl) = r_funct7_fields_for_AMO  funct7

    -- Decode check
    rv       = mstate_rv_read  mstate
    is_legal = ((opcode == opcode_AMO)
                && ((funct3 == funct3_AMO_W)
                    || ((rv == RV64) && (funct3 == funct3_AMO_D)))
                && ((  (msbs5 == msbs5_AMO_LR) && (rs2 == 0))
                    || (msbs5 == msbs5_AMO_SC)
                    || (msbs5 == msbs5_AMO_ADD)
                    || (msbs5 == msbs5_AMO_SWAP)
                    || (msbs5 == msbs5_AMO_XOR)
                    || (msbs5 == msbs5_AMO_AND)
                    || (msbs5 == msbs5_AMO_OR)
                    || (msbs5 == msbs5_AMO_MIN)
                    || (msbs5 == msbs5_AMO_MAX)
                    || (msbs5 == msbs5_AMO_MINU)
                    || (msbs5 == msbs5_AMO_MAXU)))

    -- Semantics
    rs2_val = mstate_gpr_read  mstate  rs2

    --     Compute effective address
    eaddr1  = mstate_gpr_read  mstate  rs1
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    --     If Virtual Mem is active, translate to a physical addr
    is_instr = False
    is_read  = False
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  eaddr2
                         else
                           (Mem_Result_Ok  eaddr2, mstate)

    --     If no trap due to Virtual Mem translation, do AMO op in memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   eaddr2_pa ->
                             mstate_mem_amo  mstate1  eaddr2_pa  funct3  msbs5  aq  rl  rs2_val

    --     Finally: finish with trap, or finish with loading Rd with AMO result
    mstate3 = case result2 of
                Mem_Result_Err exc_code ->
                  finish_trap  mstate2  exc_code  eaddr2

                Mem_Result_Ok  x        ->
                  finish_rd_and_pc_incr  mstate2  rd  x  is_C
  in
    (is_legal, mstate3)

-- ================================================================

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
import CSR_File
import Virtual_Mem

import Forvis_Spec_Finish_Instr     -- Canonical ways for finish an instruction

-- ================================================================
-- 'A' Extension

opcode_AMO     = 0x2F :: InstrField    -- 7'b_01_011_11

-- Note: these are duplicates of defs in Mem_Ops.hs
funct3_AMO_W   = 0x2 :: InstrField     -- 3'b010
funct3_AMO_D   = 0x3 :: InstrField     -- 3'b011

msbs5_AMO_LR   = 0x02 :: InstrField    -- 5'b00010;
msbs5_AMO_SC   = 0x03 :: InstrField    -- 5'b00011;
msbs5_AMO_ADD  = 0x00 :: InstrField    -- 5'b00000;
msbs5_AMO_SWAP = 0x01 :: InstrField    -- 5'b00001;
msbs5_AMO_XOR  = 0x04 :: InstrField    -- 5'b00100;
msbs5_AMO_AND  = 0x0C :: InstrField    -- 5'b01100;
msbs5_AMO_OR   = 0x08 :: InstrField    -- 5'b01000;
msbs5_AMO_MIN  = 0x10 :: InstrField    -- 5'b10000;
msbs5_AMO_MAX  = 0x14 :: InstrField    -- 5'b10100;
msbs5_AMO_MINU = 0x18 :: InstrField    -- 5'b11000;
msbs5_AMO_MAXU = 0x1C :: InstrField    -- 5'b11100;

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

-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_I64 where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'I' (Base) instructions for RV64

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import ALU
import Arch_Defs
import Machine_State

import Forvis_Spec_Common    -- Canonical ways for finish an instruction
import Forvis_Spec_I

-- ================================================================
-- 'I64' Base instruction set (RV64 only)

-- NOTE: opcode_XXX, funct3_XXX are defined in module Arch_Defs

-- ================================================================

data Instr_I64 = LWU    GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
               | LD     GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
               | SD     GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm12
               | ADDIW  GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
               | SLLIW  GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, shamt
               | SRLIW  GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, shamt
               | SRAIW  GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, shamt
               | ADDW   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
               | SUBW   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
               | SLLW   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
               | SRLW   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
               | SRAW   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
  deriving (Eq, Show)

-- ================
-- Decode from 32b representation to Instr_I64 data structure

decode_I64 :: RV -> Instr_32b -> Maybe Instr_I64
decode_I64    rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode  = bitSlice  instr_32b   6   0
    rd      = bitSlice  instr_32b  11   7
    funct3  = bitSlice  instr_32b  14  12
    rs1     = bitSlice  instr_32b  19  15
    rs2     = bitSlice  instr_32b  24  20
    funct7  = bitSlice  instr_32b  31  25

    imm12_I = bitSlice  instr_32b  31  20
    shamt5  = bitSlice  instr_32b  24  20

    imm12_S = (shiftL  (bitSlice  instr_32b  31  25) 5) .|. (bitSlice  instr_32b  11 7)

    instr_I64
      | opcode==opcode_LOAD,   funct3==funct3_LWU   = Just  (LWU  rd  rs1  imm12_I)
      | opcode==opcode_LOAD,   funct3==funct3_LD    = Just  (LD   rd  rs1  imm12_I)

      | opcode==opcode_STORE,  funct3==funct3_SD    = Just  (SD  rs1  rs2  imm12_S)

      | opcode==opcode_OP_IMM_32, funct3==funct3_ADDIW = Just  (ADDIW  rd  rs1  imm12_I)
      | opcode==opcode_OP_IMM_32, funct3==funct3_SLLIW, funct7==funct7_SLLIW = Just  (SLLIW  rd  rs1  shamt5)
      | opcode==opcode_OP_IMM_32, funct3==funct3_SRLIW, funct7==funct7_SRLIW = Just  (SRLIW  rd  rs1  shamt5)
      | opcode==opcode_OP_IMM_32, funct3==funct3_SRAIW, funct7==funct7_SRAIW = Just  (SRAIW  rd  rs1  shamt5)

      | opcode==opcode_OP_32, funct3==funct3_ADDW, funct7==funct7_ADDW = Just  (ADDW   rd  rs1  rs2)
      | opcode==opcode_OP_32, funct3==funct3_SUBW, funct7==funct7_SUBW = Just  (SUBW   rd  rs1  rs2)
      | opcode==opcode_OP_32, funct3==funct3_SLLW, funct7==funct7_SLLW = Just  (SLLW   rd  rs1  rs2)
      | opcode==opcode_OP_32, funct3==funct3_SRLW, funct7==funct7_SRLW = Just  (SRLW   rd  rs1  rs2)
      | opcode==opcode_OP_32, funct3==funct3_SRAW, funct7==funct7_SRAW = Just  (SRAW   rd  rs1  rs2)

      | True = Nothing
  in
    instr_I64

-- ================================================================
-- Execution of Instr_I64

type Spec_Instr_I64 = Bool -> Instr_I64 -> Machine_State -> Machine_State
--                    is_C    instr_I64    mstate           mstate'

exec_instr_I64 :: Spec_Instr_I64
exec_instr_I64  is_C  instr_I64  mstate =
  case instr_I64 of
    LWU    rd   rs1  imm12 -> exec_LWU    is_C  instr_I64  mstate
    LD     rd   rs1  imm12 -> exec_LD     is_C  instr_I64  mstate
    SD     rs1  rs2  imm12 -> exec_SD     is_C  instr_I64  mstate
    ADDIW  rd   rs1  imm12 -> exec_ADDIW  is_C  instr_I64  mstate
    SLLIW  rd   rs1  shamt -> exec_SLLIW  is_C  instr_I64  mstate
    SRLIW  rd   rs1  shamt -> exec_SRLIW  is_C  instr_I64  mstate
    SRAIW  rd   rs1  shamt -> exec_SRAIW  is_C  instr_I64  mstate
    ADDW   rd   rs1  rs2   -> exec_ADDW   is_C  instr_I64  mstate
    SUBW   rd   rs1  rs2   -> exec_SUBW   is_C  instr_I64  mstate
    SLLW   rd   rs1  rs2   -> exec_SLLW   is_C  instr_I64  mstate
    SRLW   rd   rs1  rs2   -> exec_SRLW   is_C  instr_I64  mstate
    SRAW   rd   rs1  rs2   -> exec_SRAW   is_C  instr_I64  mstate

-- ================================================================
-- LOAD: LWU, LD

exec_LWU :: Spec_Instr_I64
exec_LWU  is_C  (LWU  rd  rs1  imm12)  mstate = exec_LOAD  is_C  rd  rs1  imm12  funct3_LWU  mstate

exec_LD  :: Spec_Instr_I64
exec_LD   is_C  (LD   rd  rs1  imm12)  mstate = exec_LOAD  is_C  rd  rs1  imm12  funct3_LD  mstate

-- ================================================================
-- STORE: SD

exec_SD :: Spec_Instr_I64
exec_SD  is_C  (SD  rs1  rs2  imm12)  mstate = exec_STORE  is_C  rs1  rs2  imm12  funct3_SD  mstate

-- ================================================================
-- OP-IMM-32: ADDIW, SLLIW, SRLIW, SRAIW

exec_ADDIW :: Spec_Instr_I64
exec_ADDIW  is_C  (ADDIW  rd  rs1  imm12)  mstate =
  let
    xlen     = mstate_xlen_read  mstate
  in
    exec_OP_IMM_32  alu_addw  is_C  rd  rs1  (sign_extend  12  xlen  imm12)  mstate

exec_SLLIW :: Spec_Instr_I64
exec_SLLIW  is_C  (SLLIW  rd  rs1  shamt)  mstate =
  exec_OP_IMM_32  alu_sllw  is_C  rd  rs1  shamt  mstate

exec_SRLIW :: Spec_Instr_I64
exec_SRLIW  is_C  (SRLIW  rd  rs1  shamt)  mstate =
  exec_OP_IMM_32  alu_srlw  is_C  rd  rs1  shamt  mstate

exec_SRAIW :: Spec_Instr_I64
exec_SRAIW  is_C  (SRAIW  rd  rs1  shamt)  mstate =
  exec_OP_IMM_32  alu_sraw  is_C  rd  rs1  shamt  mstate

exec_OP_IMM_32 :: (Integer -> Integer -> Integer) ->
                  Bool ->
                  GPR_Addr ->
                  GPR_Addr ->
                  InstrField ->
                  Machine_State -> Machine_State
exec_OP_IMM_32  alu_op  is_C  rd  rs1  v2  mstate =
  let
    rs1_val = mstate_gpr_read  mstate  rs1
    rd_val  = alu_op  rs1_val  v2
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1

-- ================================================================
-- OP-32: for RV64: ADDW, SUBW, SLLW, SRLW, SRAW

exec_ADDW :: Spec_Instr_I64
exec_ADDW  is_C  (ADDW  rd  rs1  rs2)  mstate =
  exec_OP_32  alu_addw  is_C  rd  rs1  rs2  mstate

exec_SUBW :: Spec_Instr_I64
exec_SUBW  is_C  (SUBW  rd  rs1  rs2)  mstate =
  exec_OP_32  alu_subw  is_C  rd  rs1  rs2  mstate

exec_SLLW :: Spec_Instr_I64
exec_SLLW  is_C  (SLLW  rd  rs1  rs2)  mstate =
  exec_OP_32  alu_sllw  is_C  rd  rs1  rs2  mstate

exec_SRLW :: Spec_Instr_I64
exec_SRLW  is_C  (SRLW  rd  rs1  rs2)  mstate =
  exec_OP_32  alu_srlw  is_C  rd  rs1  rs2  mstate

exec_SRAW :: Spec_Instr_I64
exec_SRAW  is_C  (SRAW  rd  rs1  rs2)  mstate =
  exec_OP_32  alu_sraw  is_C  rd  rs1  rs2  mstate

exec_OP_32 :: (Integer -> Integer -> Integer) ->
              Bool ->
              GPR_Addr ->
              GPR_Addr ->
              GPR_Addr ->
              Machine_State -> Machine_State
exec_OP_32  alu_op  is_C  rd  rs1  rs2  mstate =
  let
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2
    rd_val  = alu_op  rs1_val  rs2_val
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1

-- ================================================================

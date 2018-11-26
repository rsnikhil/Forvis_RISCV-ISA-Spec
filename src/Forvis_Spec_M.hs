-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_M where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'M' Extension
-- i.e., Integer Multiply/Divide

-- ================================================================
-- Haskell lib imports

-- None

-- Local imports

import Bit_Utils
import ALU
import Arch_Defs
import Machine_State

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- 'M' Extension (Integer Multiply/Divide)

-- NOTE: opcode_OP, opcode_OP_32, funct3_XXX, funct7_XXX
-- are defined in module Arch_Defs

-- ================================================================
-- Data structure for instructions in 'I' (base instruction set)

data Instr_M = MUL     GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | MULH    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | MULHSU  GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | MULHU   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2

             | DIV     GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | DIVU    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | REM     GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | REMU    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2

             | MULW    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | DIVW    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | DIVUW   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | REMW    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | REMUW   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
  deriving (Eq, Show)

-- ================================================================
-- Decode from 32b representation to Instr_M data structure

decode_M :: RV -> Instr_32b -> Maybe Instr_M
decode_M    rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode  = bitSlice  instr_32b   6   0
    rd      = bitSlice  instr_32b  11   7
    funct3  = bitSlice  instr_32b  14  12
    rs1     = bitSlice  instr_32b  19  15
    rs2     = bitSlice  instr_32b  24  20
    funct7  = bitSlice  instr_32b  31  25

    instr_M
      | opcode==opcode_OP, funct3==funct3_MUL,    funct7==funct7_MUL    = Just  (MUL  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_MULH,   funct7==funct7_MULH   = Just  (MULH  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_MULHSU, funct7==funct7_MULHSU = Just  (MULHSU  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_MULHU,  funct7==funct7_MULHU  = Just  (MULHU  rd rs1 rs2)

      | opcode==opcode_OP, funct3==funct3_DIV,    funct7==funct7_DIV   = Just  (DIV  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_DIVU,   funct7==funct7_DIVU  = Just  (DIVU rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_REM,    funct7==funct7_REM   = Just  (REM  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_REMU,   funct7==funct7_REMU  = Just  (REMU rd rs1 rs2)

      | opcode==opcode_OP_32, funct3==funct3_MULW,  funct7==funct7_MULW,  rv==RV64 = Just  (MULW  rd rs1 rs2)
      | opcode==opcode_OP_32, funct3==funct3_DIVW,  funct7==funct7_DIVW,  rv==RV64 = Just  (DIVW  rd rs1 rs2)
      | opcode==opcode_OP_32, funct3==funct3_DIVUW, funct7==funct7_DIVUW, rv==RV64 = Just  (DIVUW rd rs1 rs2)
      | opcode==opcode_OP_32, funct3==funct3_REMW,  funct7==funct7_REMW,  rv==RV64 = Just  (REMW  rd rs1 rs2)
      | opcode==opcode_OP_32, funct3==funct3_REMUW, funct7==funct7_REMUW, rv==RV64 = Just  (REMUW rd rs1 rs2)

      | True = Nothing
  in
    instr_M

-- ================================================================
-- Execution of Instr_M

type Spec_Instr_M = Bool -> Instr_M -> Machine_State -> Machine_State
--                  is_C    instr_M    mstate           mstate'

exec_instr_M :: Spec_Instr_M
exec_instr_M  is_C  instr_M  mstate =
  case instr_M of
    MUL     rd  rs1  rs2 -> exec_OP_M  alu_mul     is_C  rd  rs1  rs2  mstate
    MULH    rd  rs1  rs2 -> exec_OP_M  alu_mulh    is_C  rd  rs1  rs2  mstate
    MULHSU  rd  rs1  rs2 -> exec_OP_M  alu_mulhsu  is_C  rd  rs1  rs2  mstate
    MULHU   rd  rs1  rs2 -> exec_OP_M  alu_mulhu   is_C  rd  rs1  rs2  mstate
    DIV     rd  rs1  rs2 -> exec_OP_M  alu_div     is_C  rd  rs1  rs2  mstate
    DIVU    rd  rs1  rs2 -> exec_OP_M  alu_divu    is_C  rd  rs1  rs2  mstate
    REM     rd  rs1  rs2 -> exec_OP_M  alu_rem     is_C  rd  rs1  rs2  mstate
    REMU    rd  rs1  rs2 -> exec_OP_M  alu_remu    is_C  rd  rs1  rs2  mstate

    MULW    rd  rs1  rs2 -> exec_OP_M_32  alu_mulw    is_C  rd  rs1  rs2  mstate
    DIVW    rd  rs1  rs2 -> exec_OP_M_32  alu_divw    is_C  rd  rs1  rs2  mstate
    DIVUW   rd  rs1  rs2 -> exec_OP_M_32  alu_divuw   is_C  rd  rs1  rs2  mstate
    REMW    rd  rs1  rs2 -> exec_OP_M_32  alu_remw    is_C  rd  rs1  rs2  mstate
    REMUW   rd  rs1  rs2 -> exec_OP_M_32  alu_remuw   is_C  rd  rs1  rs2  mstate

-- ================================================================
-- MUL, MULH, MULHSU, MULHU
-- DIV, DIVU
-- REM, REMU

exec_OP_M :: (Int -> Integer -> Integer -> Integer) ->
             Bool ->
             GPR_Addr ->
             GPR_Addr ->
             GPR_Addr ->
             Machine_State -> Machine_State
exec_OP_M  alu_op  is_C  rd  rs1  rs2  mstate =
  let
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2
    rd_val  = alu_op  xlen  rs1_val  rs2_val
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1

-- ================================================================
-- MULW, DIVW, DIVUW, REMW, REMUW

exec_OP_M_32 :: (Integer -> Integer -> Integer) ->
                Bool ->
                GPR_Addr ->
                GPR_Addr ->
                GPR_Addr ->
                Machine_State -> Machine_State
exec_OP_M_32  alu_op  is_C  rd  rs1  rs2  mstate =
  let
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2
    rd_val  = alu_op  rs1_val  rs2_val
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1

-- ================================================================

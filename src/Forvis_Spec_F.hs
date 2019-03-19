-- Copyright (c) 2018-2019 Rishiyur S. Nikhil, Niraj N. Sharma
-- See LICENSE for license details

module Forvis_Spec_F where

#ifdef FLOAT
-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'F' Extension
-- i.e., single-precision floating point.

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import ALU
import FPU
import Arch_Defs
import Machine_State
import CSR_File
import Virtual_Mem

import Forvis_Spec_Common    -- Canonical ways to finish an instruction

-- ================================================================
-- Data structure for instructions in 'F'
-- (single-precision floating point instruction set)

data Instr_F = FLW        GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, imm12
             | FSW        GPR_Addr  GPR_Addr  InstrField              -- rs1, rs2, imm12

             | FMADD_S    GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm
             | FMSUB_S    GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm
             | FNMSUB_S   GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm
             | FNMADD_S   GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm

             | FADD_S     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm
             | FSUB_S     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm
             | FMUL_S     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm
             | FDIV_S     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm

             | FSQRT_S    GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm

             | FSGNJ_S    GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FSGNJN_S   GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FSGNJX_S   GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FMIN_S     GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FMAX_S     GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2

             | FCVT_W_S   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_WU_S  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm

             | FMV_X_W    GPR_Addr  GPR_Addr                          -- rd, rs1

             | FEQ_S      GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FLT_S      GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FLE_S      GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2

             | FCLASS_S   GPR_Addr  GPR_Addr                          -- rd, rs1

             | FCVT_S_W   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_S_WU  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm

             | FMV_W_X    GPR_Addr  GPR_Addr                          -- rd, rs1

             | FCVT_L_S   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_LU_S  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_S_L   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_S_LU  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
  deriving (Eq, Show)

-- ================================================================
-- Sub-opcodes for 'F' instructions
-- NOTE: opcode_XXX are defined in module Arch_Defs

funct3_FLW       = 0x2    :: InstrField  -- 3'b_010
funct3_FSW       = 0x2    :: InstrField  -- 3'b_010

funct2_FMADD_S   = 0x0    :: InstrField  -- 2'b_00    (instr [26:25])
funct2_FMSUB_S   = 0x0    :: InstrField  -- 2'b_00    (instr [26:25])
funct2_FNMSUB_S  = 0x0    :: InstrField  -- 2'b_00    (instr [26:25])
funct2_FNMADD_S  = 0x0    :: InstrField  -- 2'b_00    (instr [26:25])

funct7_FADD_S    = 0x0    :: InstrField  -- 7'b_000_0000
funct7_FSUB_S    = 0x4    :: InstrField  -- 7'b_000_0100
funct7_FMUL_S    = 0x8    :: InstrField  -- 7'b_000_1000
funct7_FDIV_S    = 0xC    :: InstrField  -- 7'b_000_1100

funct12_FSQRT_S  = 0x580  :: InstrField  -- 7'b_0101_1000_0000

funct7_FSGNJ_S   = 0x10   :: InstrField  -- 7'b_001_0000
funct3_FSGNJ_S   = 0x0    :: InstrField  -- 3'b_000

funct7_FSGNJN_S  = 0x10   :: InstrField  -- 7'b_001_0000
funct3_FSGNJN_S  = 0x1    :: InstrField  -- 3'b_001

funct7_FSGNJX_S  = 0x10   :: InstrField  -- 7'b_001_0000
funct3_FSGNJX_S  = 0x2    :: InstrField  -- 3'b_010

funct7_FMIN_S    = 0x14   :: InstrField  -- 7'b_001_01000
funct3_FMIN_S    = 0x0    :: InstrField  -- 3'b_000

funct7_FMAX_S    = 0x14   :: InstrField  -- 7'b_001_01000
funct3_FMAX_S    = 0x1    :: InstrField  -- 3'b_000

funct12_FCVT_W_S  = 0xC00  :: InstrField  -- 12'b_1100_0000_0000
funct12_FCVT_WU_S = 0xC01  :: InstrField  -- 12'b_1100_0000_0001

funct12_FMV_X_W   = 0xE00  :: InstrField  -- 12'b_1110_0000_0000
funct3_FMV_X_W    = 0x0    :: InstrField  -- 3'b_000

funct7_FEQ_S      = 0x50   :: InstrField  -- 7'b_101_0000
funct3_FEQ_S      = 0x2    :: InstrField  -- 3'b_010
funct7_FLT_S      = 0x50   :: InstrField  -- 7'b_101_0000
funct3_FLT_S      = 0x1    :: InstrField  -- 3'b_001
funct7_FLE_S      = 0x50   :: InstrField  -- 7'b_101_0000
funct3_FLE_S      = 0x0    :: InstrField  -- 3'b_000

funct12_FCLASS_S  = 0xE00  :: InstrField  -- 12'b_1110_0000_0000
funct3_FCLASS_S   = 0x1    :: InstrField  -- 3'b_001

funct12_FCVT_S_W  = 0xD00  :: InstrField  -- 12'b_1101_0000_0000
funct12_FCVT_S_WU = 0xD01  :: InstrField  -- 12'b_1101_0000_0001

funct12_FMV_W_X   = 0xF00  :: InstrField  -- 12'b_1111_0000_0000
funct3_FMV_W_X    = 0x0    :: InstrField  -- 3'b_000

-- RV64F

funct12_FCVT_L_S  = 0xC02  :: InstrField  -- 12'b_1100_0000_0010
funct12_FCVT_LU_S = 0xC03  :: InstrField  -- 12'b_1100_0000_0011
funct12_FCVT_S_L  = 0xD02  :: InstrField  -- 12'b_1101_0000_0010
funct12_FCVT_S_LU = 0xD03  :: InstrField  -- 12'b_1101_0000_0011

-- ================================================================
-- Decode from 32b representation to Instr_F data structure
-- 'frm' is the value from CSR FRM (rounding mode)

decode_F :: Integer -> RV -> Instr_32b -> Maybe Instr_F
decode_F    frm        rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode       = bitSlice  instr_32b   6   0
    rd           = bitSlice  instr_32b  11   7
    funct3       = bitSlice  instr_32b  14  12
    rm           = bitSlice  instr_32b  14  12    -- Rounding Mode
    rs1          = bitSlice  instr_32b  19  15
    rs2          = bitSlice  instr_32b  24  20
    funct7       = bitSlice  instr_32b  31  25
    funct12      = bitSlice  instr_32b  31  20
    funct2_fmadd = bitSlice  instr_32b  26  25
    rs3          = bitSlice  instr_32b  31  27

    imm12_I = bitSlice  instr_32b  31  20

    imm12_S = (shiftL  (bitSlice  instr_32b  31  25) 5) .|. (bitSlice  instr_32b  11 7)

    -- Pick rounding mode from CSR or from instr, and check if legal
    (rm1, rmLegal) = rounding_mode_check  rm  frm

    instr_F
      | opcode==opcode_LOAD_FP,  funct3==funct3_FLW              = Just  (FLW  rd   rs1  imm12_I)
      | opcode==opcode_STORE_FP, funct3==funct3_FSW              = Just  (FSW  rs1  rs2  imm12_S)

      | opcode==opcode_FMADD,   funct2_fmadd==funct2_FMADD_S,   rmLegal = Just  (FMADD_S   rd  rs1  rs2  rs3  rm1)
      | opcode==opcode_FMSUB,   funct2_fmadd==funct2_FMSUB_S,   rmLegal = Just  (FMSUB_S   rd  rs1  rs2  rs3  rm1)
      | opcode==opcode_FNMSUB,  funct2_fmadd==funct2_FNMSUB_S,  rmLegal = Just  (FNMSUB_S  rd  rs1  rs2  rs3  rm1)
      | opcode==opcode_FNMADD,  funct2_fmadd==funct2_FNMADD_S,  rmLegal = Just  (FNMADD_S  rd  rs1  rs2  rs3  rm1)

      | opcode==opcode_OP_FP,  funct7==funct7_FADD_S,     rmLegal = Just  (FADD_S   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct7==funct7_FSUB_S,     rmLegal = Just  (FSUB_S   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct7==funct7_FMUL_S,     rmLegal = Just  (FMUL_S   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct7==funct7_FDIV_S,     rmLegal = Just  (FDIV_S   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FSQRT_S,  rmLegal = Just  (FSQRT_S  rd  rs1  rm1)

      | opcode==opcode_OP_FP,  funct7==funct7_FSGNJ_S,   funct3==funct3_FSGNJ_S  = Just  (FSGNJ_S   rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FSGNJN_S,  funct3==funct3_FSGNJN_S = Just  (FSGNJN_S  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FSGNJX_S,  funct3==funct3_FSGNJX_S = Just  (FSGNJX_S  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FMIN_S,    funct3==funct3_FMIN_S   = Just  (FMIN_S    rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FMAX_S,    funct3==funct3_FMAX_S   = Just  (FMAX_S    rd  rs1  rs2)

      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_W_S,   rmLegal = Just  (FCVT_W_S   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_WU_S,  rmLegal = Just  (FCVT_WU_S  rd  rs1  rm1)

      | opcode==opcode_OP_FP,  funct12==funct12_FMV_X_W, funct3==funct3_FMV_X_W = Just  (FMV_X_W  rd  rs1)

      | opcode==opcode_OP_FP,  funct7==funct7_FEQ_S,  funct3==funct3_FEQ_S = Just  (FEQ_S  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FLT_S,  funct3==funct3_FLT_S = Just  (FLT_S  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FLE_S,  funct3==funct3_FLE_S = Just  (FLE_S  rd  rs1  rs2)

      | opcode==opcode_OP_FP,  funct12==funct12_FCLASS_S, funct3==funct3_FCLASS_S = Just  (FCLASS_S  rd  rs1)

      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_S_W,   rmLegal = Just  (FCVT_S_W   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_S_WU,  rmLegal = Just  (FCVT_S_WU  rd  rs1  rm1)

      | opcode==opcode_OP_FP,  funct12==funct12_FMV_W_X, funct3==funct3_FMV_W_X = Just  (FMV_W_X  rd  rs1)

      -- RV64 'F' instructions
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_L_S,   rmLegal,  rv==RV64 = Just  (FCVT_L_S   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_LU_S,  rmLegal,  rv==RV64 = Just  (FCVT_LU_S  rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_S_L,   rmLegal,  rv==RV64 = Just  (FCVT_S_L   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_S_LU,  rmLegal,  rv==RV64 = Just  (FCVT_S_LU  rd  rs1  rm1)

      | True = Nothing
  in
    instr_F

-- ================================================================
-- Execution of Instr_F

type Spec_Instr_F = Bool -> Instr_F -> Machine_State -> Machine_State
--                  is_C    instr_F    mstate           mstate'

exec_instr_F :: Spec_Instr_F
exec_instr_F  is_C  instr_F  mstate =
  case instr_F of
    FLW        rd   rs1  imm12 -> exec_FLW  is_C  instr_F  mstate
    FSW        rs1  rs2  imm12 -> exec_FSW  is_C  instr_F  mstate

    FMADD_S    rd  rs1  rs2  rs3  rm -> exec_FMADD_S  is_C  instr_F  mstate
    FMSUB_S    rd  rs1  rs2  rs3  rm -> exec_FMSUB_S  is_C  instr_F  mstate
    FNMSUB_S   rd  rs1  rs2  rs3  rm -> exec_FNMSUB_S  is_C  instr_F  mstate
    FNMADD_S   rd  rs1  rs2  rs3  rm -> exec_FNMADD_S  is_C  instr_F  mstate

    FADD_S     rd  rs1  rs2  rm -> exec_FADD_S  is_C  instr_F  mstate
    FSUB_S     rd  rs1  rs2  rm -> exec_FSUB_S  is_C  instr_F  mstate
    FMUL_S     rd  rs1  rs2  rm -> exec_FMUL_S  is_C  instr_F  mstate
    FDIV_S     rd  rs1  rs2  rm -> exec_FDIV_S  is_C  instr_F  mstate

    FSQRT_S    rd  rs1  rm -> exec_FSQRT_S  is_C  instr_F  mstate

    FSGNJ_S    rd  rs1  rs2 -> exec_FSGNJ_S   is_C  instr_F  mstate
    FSGNJN_S   rd  rs1  rs2 -> exec_FSGNJN_S  is_C  instr_F  mstate
    FSGNJX_S   rd  rs1  rs2 -> exec_FSGNJX_S  is_C  instr_F  mstate
    FMIN_S     rd  rs1  rs2 -> exec_FMIN_S    is_C  instr_F  mstate
    FMAX_S     rd  rs1  rs2 -> exec_FMAX_S    is_C  instr_F  mstate

    FCVT_W_S   rd  rs1  rm -> exec_FCVT_W_S   is_C  instr_F  mstate
    FCVT_WU_S  rd  rs1  rm -> exec_FCVT_WU_S  is_C  instr_F  mstate

    FMV_X_W    rd  rs1 -> exec_FMV_X_W   is_C  instr_F  mstate

    FEQ_S      rd  rs1  rs2 -> exec_FEQ_S  is_C  instr_F  mstate
    FLT_S      rd  rs1  rs2 -> exec_FLT_S  is_C  instr_F  mstate
    FLE_S      rd  rs1  rs2 -> exec_FLE_S  is_C  instr_F  mstate

    FCLASS_S   rd  rs1 -> exec_FCLASS_S  is_C  instr_F  mstate

    FCVT_S_W   rd  rs1  rm -> exec_FCVT_S_W   is_C  instr_F  mstate
    FCVT_S_WU  rd  rs1  rm -> exec_FCVT_S_WU  is_C  instr_F  mstate

    FMV_W_X    rd  rs1 -> exec_FMV_W_X   is_C  instr_F  mstate

    -- RV64F

    FCVT_L_S   rd  rs1  rm -> exec_FCVT_L_S   is_C  instr_F  mstate
    FCVT_LU_S  rd  rs1  rm -> exec_FCVT_LU_S  is_C  instr_F  mstate
    FCVT_S_L   rd  rs1  rm -> exec_FCVT_S_L   is_C  instr_F  mstate
    FCVT_S_LU  rd  rs1  rm -> exec_FCVT_S_LU  is_C  instr_F  mstate

-- ================================================================
-- FLW

exec_FLW :: Spec_Instr_F
exec_FLW  is_C  (FLW  rd  rs1  imm12)  mstate =
  let
    rv   = mstate_rv_read  mstate
    xlen = mstate_xlen_read  mstate

    -- Compute effective address
    rs1_val = mstate_gpr_read  rs1  mstate
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    -- Read mem, possibly with virtual mem translation
    is_instr = False
    (result1, mstate1) = mstate_vm_read  mstate  is_instr  exc_code_load_access_fault  funct3_FLW  eaddr2

    -- Finish with trap, or finish with loading Rd with load-value
    is_n_lt_FLEN = True
    mstate2 = case result1 of
                Mem_Result_Err exc_code ->
                  finish_trap  exc_code  eaddr2  mstate1

                Mem_Result_Ok  d_u64    ->
                  finish_frd_and_pc_plus_4  rd  d_u64  is_n_lt_FLEN  mstate1
  in
    mstate2

-- ================================================================
-- FSW

exec_FSW :: Spec_Instr_F
exec_FSW  is_C  (FSW  rs1  rs2  imm12)  mstate =
  let
    rv   = mstate_rv_read  mstate
    xlen = mstate_xlen_read  mstate

    rs2_val = mstate_fpr_read  rs2  mstate   -- store value

    -- Compute effective address
    rs1_val = mstate_gpr_read  rs1  mstate    -- address base
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    -- Write mem, possibly with virtual mem translation
    (result1, mstate1) = mstate_vm_write  mstate  funct3_FSW  eaddr2  rs2_val

    -- Finish with trap, or finish with fall-through
    mstate2 = case result1 of
                Mem_Result_Err exc_code -> finish_trap  exc_code  eaddr2  mstate1
                Mem_Result_Ok  _        -> finish_pc_incr  is_C  mstate1
  in
    mstate2

-- ================================================================
-- FMADD_S

exec_FMADD_S :: Spec_Instr_F
exec_FMADD_S  is_C  (FMADD_S  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = True
    rs1_val = unboxSP  (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP  (mstate_fpr_read  rs2  mstate)
    rs3_val = unboxSP  (mstate_fpr_read  rs3  mstate)

    (fflags, rdVal) = fpu_f32MulAdd  rm  rs1_val  rs2_val  rs3_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FMSUB_S

exec_FMSUB_S :: Spec_Instr_F
exec_FMSUB_S  is_C  (FMSUB_S  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = True
    rs1_val = unboxSP  (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP  (mstate_fpr_read  rs2  mstate)
    rs3_val = unboxSP  (mstate_fpr_read  rs3  mstate)

    (fflags, rdVal) = fpu_f32MulAdd  rm  rs1_val  rs2_val  (negateS rs3_val)

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FNMSUB_S

exec_FNMSUB_S :: Spec_Instr_F
exec_FNMSUB_S  is_C  (FNMSUB_S  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = True
    rs1_val = unboxSP  (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP  (mstate_fpr_read  rs2  mstate)
    rs3_val = unboxSP  (mstate_fpr_read  rs3  mstate)

    (fflags, rdVal) = fpu_f32MulAdd  rm  (negateS rs1_val)  rs2_val  rs3_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FNMADD_S

exec_FNMADD_S :: Spec_Instr_F
exec_FNMADD_S  is_C  (FNMADD_S  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = True
    rs1_val = unboxSP  (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP  (mstate_fpr_read  rs2  mstate)
    rs3_val = unboxSP  (mstate_fpr_read  rs3  mstate)

    (fflags, rdVal) = fpu_f32MulAdd  rm  (negateS rs1_val)  rs2_val  (negateS rs3_val)

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FADD_S

exec_FADD_S :: Spec_Instr_F
exec_FADD_S  is_C  (FADD_S  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs1  mstate))
    rs2_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs2  mstate))

    (fflags, rd_val) = fpu_f32Add  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FSUB_S

exec_FSUB_S :: Spec_Instr_F
exec_FSUB_S  is_C  (FSUB_S  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs1  mstate))
    rs2_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs2  mstate))

    (fflags, rd_val) = fpu_f32Sub  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FMUL_S

exec_FMUL_S :: Spec_Instr_F
exec_FMUL_S  is_C  (FMUL_S  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs1  mstate))
    rs2_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs2  mstate))

    (fflags, rd_val) = fpu_f32Mul  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FDIV_S

exec_FDIV_S :: Spec_Instr_F
exec_FDIV_S  is_C  (FDIV_S  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs1  mstate))
    rs2_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs2  mstate))

    (fflags, rd_val) = fpu_f32Div  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FSQRT_S

exec_FSQRT_S :: Spec_Instr_F
exec_FSQRT_S  is_C  (FSQRT_S  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  rs1  mstate))

    (fflags, rd_val) = fpu_f32Sqrt  rm  rs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FSGNJ_S

exec_FSGNJ_S :: Spec_Instr_F
exec_FSGNJ_S  is_C  (FSGNJ_S  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleSP  rs1_val
    (s2, e2, m2) = disassembleSP  rs2_val

    rd_val = assembleSP  s2  e1  m1

    -- No exceptions are signalled by this operation
    fflags  = 0
    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FSGNJN_S

exec_FSGNJN_S :: Spec_Instr_F
exec_FSGNJN_S  is_C  (FSGNJN_S  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleSP  rs1_val
    (s2, e2, m2) = disassembleSP  rs2_val

    rd_val = assembleSP  (xor  s2  0x1)  e1  m1

    -- No exceptions are signalled by this operation
    fflags  = 0
    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FSGNJX_S

exec_FSGNJX_S :: Spec_Instr_F
exec_FSGNJX_S  is_C  (FSGNJX_S  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleSP  rs1_val
    (s2, e2, m2) = disassembleSP  rs2_val

    rd_val = assembleSP  (xor  s2  s1)   e1  m1

    -- No exceptions are signalled by this operation
    fflags  = 0
    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FMIN_S

exec_FMIN_S :: Spec_Instr_F
exec_FMIN_S  is_C  (FMIN_S  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = True
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the result of the operation and the flags
    (rs1_lt_rs2, fflags) = fpu_f32LE  rs1_val  rs2_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f32IsSNaN  rs1_val
    rs2IsSNaN = fpu_f32IsSNaN  rs2_val

    rs1IsQNaN = fpu_f32IsQNaN  rs1_val
    rs2IsQNaN = fpu_f32IsQNaN  rs2_val

    rs1IsPos0 = fpu_f32IsPosZero  rs1_val
    rs2IsPos0 = fpu_f32IsPosZero  rs2_val

    rs1IsNeg0 = fpu_f32IsNegZero  rs1_val
    rs2IsNeg0 = fpu_f32IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN32
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN32
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs1_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs2_val
           | rs1_lt_rs2                = rs1_val
           | (not rs1_lt_rs2)          = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FMAX_S

exec_FMAX_S :: Spec_Instr_F
exec_FMAX_S  is_C  (FMAX_S  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = True

    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the result of the operation and the flags
    (rs2_lt_rs1, fflags) = fpu_f32LE  rs2_val  rs1_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f32IsSNaN  rs1_val
    rs2IsSNaN = fpu_f32IsSNaN  rs2_val

    rs1IsQNaN = fpu_f32IsQNaN  rs1_val
    rs2IsQNaN = fpu_f32IsQNaN  rs2_val

    rs1IsPos0 = fpu_f32IsPosZero  rs1_val
    rs2IsPos0 = fpu_f32IsPosZero  rs2_val

    rs1IsNeg0 = fpu_f32IsNegZero  rs1_val
    rs2IsNeg0 = fpu_f32IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN32
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN32
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs2_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs1_val
           | rs2_lt_rs1                = rs1_val
           | (not rs2_lt_rs1)          = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FCVT_W_S

exec_FCVT_W_S :: Spec_Instr_F
exec_FCVT_W_S  is_C  (FCVT_W_S  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    frs1_val    = unboxSP  (mstate_fpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_f32ToI32  rm   frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  rd  rdVal  fflags  mstate
  in
    mstate1

-- ================================================================
-- FCVT_WU_S

exec_FCVT_WU_S :: Spec_Instr_F
exec_FCVT_WU_S  is_C  (FCVT_WU_S  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    frs1_val    = unboxSP  (mstate_fpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_f32ToUi32  rm  frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  rd  rdVal  fflags  mstate
  in
    mstate1

-- ================================================================
-- FMV_X_W

exec_FMV_X_W :: Spec_Instr_F
exec_FMV_X_W  is_C  (FMV_X_W  rd  rs1)  mstate =
  let
    frs1_val = mstate_fpr_read  rs1  mstate

    -- GPR value is sign-extended version of lower 32-bits of FPR contents
    frs1_val' = sign_extend  32  64  (bitSlice frs1_val  31  0)

    mstate1 = finish_rd_and_pc_incr  rd  frs1_val'  is_C  mstate
  in
    mstate1

-- ================================================================
-- FEQ_S

exec_FEQ_S :: Spec_Instr_F
exec_FEQ_S  is_C  (FEQ_S  rd  rs1  rs2)  mstate =
  let
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) = fpu_f32EQQ  rs1_val  rs2_val

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f32IsSNaN  rs1_val
    rs2IsSNaN = fpu_f32IsSNaN  rs2_val

    rs1IsQNaN = fpu_f32IsQNaN  rs1_val
    rs2IsQNaN = fpu_f32IsQNaN  rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  rd  rd_val  fflags  mstate
  in
    mstate1

-- ================================================================
-- FLT_S

exec_FLT_S :: Spec_Instr_F
exec_FLT_S  is_C  (FLT_S  rd  rs1  rs2)  mstate =
  let
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) = fpu_f32LT   rs1_val  rs2_val  False

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f32IsSNaN  rs1_val
    rs2IsSNaN = fpu_f32IsSNaN  rs2_val

    rs1IsQNaN = fpu_f32IsQNaN  rs1_val
    rs2IsQNaN = fpu_f32IsQNaN  rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  rd  rd_val  fflags  mstate
  in
    mstate1

-- ================================================================
-- FLE_S

exec_FLE_S :: Spec_Instr_F
exec_FLE_S  is_C  (FLE_S  rd  rs1  rs2)  mstate =
  let
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  rs1  mstate)
    rs2_val = unboxSP (mstate_fpr_read  rs2  mstate)

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) = fpu_f32LE   rs1_val  rs2_val  False

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f32IsSNaN  rs1_val
    rs2IsSNaN = fpu_f32IsSNaN  rs2_val

    rs1IsQNaN = fpu_f32IsQNaN  rs1_val
    rs2IsQNaN = fpu_f32IsQNaN  rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  rd  rd_val  fflags  mstate
  in
    mstate1

-- ================================================================
-- FCLASS_S

exec_FCLASS_S :: Spec_Instr_F
exec_FCLASS_S  is_C  (FCLASS_S  rd  rs1)  mstate =
  let
    frs1_val = unboxSP  (mstate_fpr_read  rs1  mstate)
    
    -- Classify the frs1_val
    is_NegInf     = fpu_f32IsNegInf        frs1_val
    is_NegNorm    = fpu_f32IsNegNorm       frs1_val
    is_NegSubNorm = fpu_f32IsNegSubNorm    frs1_val
    is_NegZero    = fpu_f32IsNegZero       frs1_val
    is_PosZero    = fpu_f32IsPosZero       frs1_val
    is_PosSubNorm = fpu_f32IsPosSubNorm    frs1_val
    is_PosNorm    = fpu_f32IsPosNorm       frs1_val
    is_PosInf     = fpu_f32IsPosInf        frs1_val
    is_SNaN       = fpu_f32IsSNaN          frs1_val
    is_QNaN       = fpu_f32IsQNaN          frs1_val

    -- Form the rd based on the above clasification
    rd_val  = 0x0 :: Integer
    rd_val' | is_NegInf       = rd_val .|. shiftL  1  fclass_negInf_bitpos
            | is_NegNorm      = rd_val .|. shiftL  1  fclass_negNorm_bitpos
            | is_NegSubNorm   = rd_val .|. shiftL  1  fclass_negSubNorm_bitpos
            | is_NegZero      = rd_val .|. shiftL  1  fclass_negZero_bitpos
            | is_PosZero      = rd_val .|. shiftL  1  fclass_posZero_bitpos
            | is_PosSubNorm   = rd_val .|. shiftL  1  fclass_posSubNorm_bitpos
            | is_PosNorm      = rd_val .|. shiftL  1  fclass_posNorm_bitpos
            | is_PosInf       = rd_val .|. shiftL  1  fclass_posInf_bitpos
            | is_SNaN         = rd_val .|. shiftL  1  fclass_SNaN_bitpos
            | is_QNaN         = rd_val .|. shiftL  1  fclass_QNaN_bitpos
    
    -- No exceptions are signalled by this operation
    mstate1 = finish_rd_and_pc_incr  rd  rd_val'  is_C  mstate
  in
    mstate1

-- ================================================================
-- FCVT_S_W

exec_FCVT_S_W :: Spec_Instr_F
exec_FCVT_S_W  is_C  (FCVT_S_W  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    grs1_val = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_i32ToF32  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FCVT_S_WU

exec_FCVT_S_WU :: Spec_Instr_F
exec_FCVT_S_WU  is_C  (FCVT_S_WU  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    grs1_val    = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_ui32ToF32  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FMV_W_X

exec_FMV_W_X :: Spec_Instr_F
exec_FMV_W_X  is_C  (FMV_W_X  rd  rs1)  mstate =
  let
    is_n_lt_FLEN = True

    grs1_val = mstate_gpr_read  rs1  mstate

    mstate1 = finish_frd_and_pc_plus_4  rd  grs1_val  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FCVT_L_S

exec_FCVT_L_S :: Spec_Instr_F
exec_FCVT_L_S  is_C  (FCVT_L_S  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    frs1_val    = unboxSP  (mstate_fpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_f32ToI64   rm   frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  rd  rdVal  fflags  mstate
  in
    mstate1

-- ================================================================
-- FCVT_LU_S

exec_FCVT_LU_S :: Spec_Instr_F
exec_FCVT_LU_S  is_C  (FCVT_LU_S  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    frs1_val    = unboxSP  (mstate_fpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_f32ToUi64  rm  frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  rd  rdVal  fflags  mstate
  in
    mstate1

-- ================================================================
-- FCVT_S_L

exec_FCVT_S_L :: Spec_Instr_F
exec_FCVT_S_L  is_C  (FCVT_S_L  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    grs1_val = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_i64ToF32  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
-- FCVT_S_LU

exec_FCVT_S_LU :: Spec_Instr_F
exec_FCVT_S_LU  is_C  (FCVT_S_LU  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True
    xlen         = mstate_xlen_read  mstate

    grs1_val    = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  rs1  mstate)

    (fflags, rdVal) = fpu_ui64ToF32  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  rd  rdVal  fflags  is_n_lt_FLEN  mstate
  in
    mstate1

-- ================================================================
#endif

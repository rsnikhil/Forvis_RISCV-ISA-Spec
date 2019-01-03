-- Copyright (c) 2018-2019 Rishiyur S. Nikhil, Niraj N. Sharma
-- See LICENSE for license details

module Forvis_Spec_D where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'D' Extension
-- i.e., double-precision floating point.

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
-- 'D' extension (double-precision floating point)

-- NOTE: opcode_XXX, funct3_XXX are defined in module Arch_Defs

-- ================================================================
-- Data structure for instructions in 'D'
-- (double-precision floating point instruction set)

data Instr_D = FLD        GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, imm12
             | FSD        GPR_Addr  GPR_Addr  InstrField              -- rs1, rs2, imm12

             | FMADD_D    GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm
             | FMSUB_D    GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm
             | FNMSUB_D   GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm
             | FNMADD_D   GPR_Addr  GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rs3, rm

             | FADD_D     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm
             | FSUB_D     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm
             | FMUL_D     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm
             | FDIV_D     GPR_Addr  GPR_Addr  GPR_Addr  InstrField    -- rd, rs1, rs2, rm

             | FSQRT_D    GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm

             | FSGNJ_D    GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FSGNJN_D   GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FSGNJX_D   GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FMIN_D     GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FMAX_D     GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2

             | FCVT_S_D   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_D_S   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm

             | FEQ_D      GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FLT_D      GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2
             | FLE_D      GPR_Addr  GPR_Addr  GPR_Addr                -- rd, rs1, rs2

             | FCLASS_D   GPR_Addr  GPR_Addr                          -- rd, rs1

             | FCVT_W_D   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_WU_D  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_D_W   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_D_WU  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm

             -- RV64D
             | FCVT_L_D   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_LU_D  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FMV_X_D    GPR_Addr  GPR_Addr                          -- rd, rs1
             | FCVT_D_L   GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FCVT_D_LU  GPR_Addr  GPR_Addr  InstrField              -- rd, rs1, rm
             | FMV_D_X    GPR_Addr  GPR_Addr                          -- rd, rs1

  deriving (Eq, Show)

-- ================================================================
-- Decode from 32b representation to Instr_D data structure
-- 'frm' is the value from CSR FRM (rounding mode)

decode_D :: Integer -> RV -> Instr_32b -> Maybe Instr_D
decode_D    frm        rv    instr_32b =
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

    instr_D
      | opcode==opcode_LOAD_FP,  funct3==funct3_FLD  = Just  (FLD  rd   rs1  imm12_I)
      | opcode==opcode_STORE_FP, funct3==funct3_FSD  = Just  (FSD  rs1  rs2  imm12_S)

      | opcode==opcode_FMADD,   funct2_fmadd==funct2_FMADD_D,   rmLegal = Just  (FMADD_D   rd  rs1  rs2  rs3  rm1)
      | opcode==opcode_FMSUB,   funct2_fmadd==funct2_FMSUB_D,   rmLegal = Just  (FMSUB_D   rd  rs1  rs2  rs3  rm1)
      | opcode==opcode_FNMSUB,  funct2_fmadd==funct2_FNMSUB_D,  rmLegal = Just  (FNMSUB_D  rd  rs1  rs2  rs3  rm1)
      | opcode==opcode_FNMADD,  funct2_fmadd==funct2_FNMADD_D,  rmLegal = Just  (FNMADD_D  rd  rs1  rs2  rs3  rm1)

      | opcode==opcode_OP_FP,  funct7==funct7_FADD_D,     rmLegal = Just  (FADD_D   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct7==funct7_FSUB_D,     rmLegal = Just  (FSUB_D   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct7==funct7_FMUL_D,     rmLegal = Just  (FMUL_D   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct7==funct7_FDIV_D,     rmLegal = Just  (FDIV_D   rd  rs1  rs2  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FSQRT_D,  rmLegal = Just  (FSQRT_D  rd  rs1  rm1)

      | opcode==opcode_OP_FP,  funct7==funct7_FSGNJ_D,   funct3==funct3_FSGNJ_D  = Just  (FSGNJ_D   rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FSGNJN_D,  funct3==funct3_FSGNJN_D = Just  (FSGNJN_D  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FSGNJX_D,  funct3==funct3_FSGNJX_D = Just  (FSGNJX_D  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FMIN_D,    funct3==funct3_FMIN_D   = Just  (FMIN_D    rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FMAX_D,    funct3==funct3_FMAX_D   = Just  (FMAX_D    rd  rs1  rs2)

      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_S_D,  rmLegal = Just  (FCVT_S_D  rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_D_S,  rmLegal = Just  (FCVT_D_S  rd  rs1  rm1)

      | opcode==opcode_OP_FP,  funct7==funct7_FEQ_D,  funct3==funct3_FEQ_D = Just  (FEQ_D  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FLT_D,  funct3==funct3_FLT_D = Just  (FLT_D  rd  rs1  rs2)
      | opcode==opcode_OP_FP,  funct7==funct7_FLE_D,  funct3==funct3_FLE_D = Just  (FLE_D  rd  rs1  rs2)

      | opcode==opcode_OP_FP,  funct12==funct12_FCLASS_D, funct3==funct3_FCLASS_D = Just  (FCLASS_D  rd  rs1)

      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_W_D,   rmLegal = Just  (FCVT_W_D   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_WU_D,  rmLegal = Just  (FCVT_WU_D  rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_D_W,   rmLegal = Just  (FCVT_D_W   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_D_WU,  rmLegal = Just  (FCVT_D_WU  rd  rs1  rm1)

      -- RV64 'D' instructions

      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_L_D,   rmLegal,  rv==RV64  = Just  (FCVT_L_D   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_LU_D,  rmLegal,  rv==RV64  = Just  (FCVT_LU_D  rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FMV_X_D, funct3==funct3_FMV_X_D = Just  (FMV_X_D    rd  rs1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_D_L,   rmLegal,  rv==RV64  = Just  (FCVT_D_L   rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FCVT_D_LU,  rmLegal,  rv==RV64  = Just  (FCVT_D_LU  rd  rs1  rm1)
      | opcode==opcode_OP_FP,  funct12==funct12_FMV_D_X, funct3==funct3_FMV_D_X = Just  (FMV_D_X    rd  rs1)

      | True = Nothing
  in
    instr_D

-- ================================================================
-- Execution of Instr_D

type Spec_Instr_D = Bool -> Instr_D -> Machine_State -> Machine_State
--                  is_C    instr_D    mstate           mstate'

exec_instr_D :: Spec_Instr_D
exec_instr_D  is_C  instr_D  mstate =
  case instr_D of
    FLD        rd   rs1  imm12 -> exec_FLD  is_C  instr_D  mstate
    FSD        rs1  rs2  imm12 -> exec_FSD  is_C  instr_D  mstate

    FMADD_D    rd  rs1  rs2  rs3  rm -> exec_FMADD_D  is_C  instr_D  mstate
    FMSUB_D    rd  rs1  rs2  rs3  rm -> exec_FMSUB_D  is_C  instr_D  mstate
    FNMSUB_D   rd  rs1  rs2  rs3  rm -> exec_FNMSUB_D  is_C  instr_D  mstate
    FNMADD_D   rd  rs1  rs2  rs3  rm -> exec_FNMADD_D  is_C  instr_D  mstate

    FADD_D     rd  rs1  rs2  rm -> exec_FADD_D  is_C  instr_D  mstate
    FSUB_D     rd  rs1  rs2  rm -> exec_FSUB_D  is_C  instr_D  mstate
    FMUL_D     rd  rs1  rs2  rm -> exec_FMUL_D  is_C  instr_D  mstate
    FDIV_D     rd  rs1  rs2  rm -> exec_FDIV_D  is_C  instr_D  mstate

    FSQRT_D    rd  rs1  rm -> exec_FSQRT_D  is_C  instr_D  mstate

    FSGNJ_D    rd  rs1  rs2 -> exec_FSGNJ_D   is_C  instr_D  mstate
    FSGNJN_D   rd  rs1  rs2 -> exec_FSGNJN_D  is_C  instr_D  mstate
    FSGNJX_D   rd  rs1  rs2 -> exec_FSGNJX_D  is_C  instr_D  mstate
    FMIN_D     rd  rs1  rs2 -> exec_FMIN_D    is_C  instr_D  mstate
    FMAX_D     rd  rs1  rs2 -> exec_FMAX_D    is_C  instr_D  mstate

    FCVT_S_D   rd  rs1  rm -> exec_FCVT_S_D  is_C  instr_D  mstate
    FCVT_D_S   rd  rs1  rm -> exec_FCVT_D_S  is_C  instr_D  mstate

    FEQ_D      rd  rs1  rs2 -> exec_FEQ_D  is_C  instr_D  mstate
    FLT_D      rd  rs1  rs2 -> exec_FLT_D  is_C  instr_D  mstate
    FLE_D      rd  rs1  rs2 -> exec_FLE_D  is_C  instr_D  mstate

    FCLASS_D   rd  rs1 -> exec_FCLASS_D  is_C  instr_D  mstate

    FCVT_W_D   rd  rs1  rm -> exec_FCVT_W_D   is_C  instr_D  mstate
    FCVT_WU_D  rd  rs1  rm -> exec_FCVT_WU_D  is_C  instr_D  mstate
    FCVT_D_W   rd  rs1  rm -> exec_FCVT_D_W   is_C  instr_D  mstate
    FCVT_D_WU  rd  rs1  rm -> exec_FCVT_D_WU  is_C  instr_D  mstate

    -- RV64D

    FCVT_L_D   rd  rs1  rm -> exec_FCVT_L_D   is_C  instr_D  mstate
    FCVT_LU_D  rd  rs1  rm -> exec_FCVT_LU_D  is_C  instr_D  mstate
    FMV_X_D    rd  rs1     -> exec_FMV_X_D    is_C  instr_D  mstate
    FCVT_D_L   rd  rs1  rm -> exec_FCVT_D_L   is_C  instr_D  mstate
    FCVT_D_LU  rd  rs1  rm -> exec_FCVT_D_LU  is_C  instr_D  mstate
    FMV_D_X    rd  rs1     -> exec_FMV_D_X    is_C  instr_D  mstate

-- ================================================================
-- FLD

exec_FLD :: Spec_Instr_D
exec_FLD  is_C  (FLD  rd  rs1  imm12)  mstate =
  let
    rv   = mstate_rv_read  mstate
    xlen = mstate_xlen_read  mstate

    --     Compute effective address
    rs1_val = mstate_gpr_read  mstate  rs1
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    --     If Virtual Mem is active, translate to a physical addr
    is_instr     = False
    is_read      = True
    is_n_lt_FLEN = False
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  eaddr2
                         else
                           (Mem_Result_Ok  eaddr2, mstate)

    --     If no trap due to Virtual Mem translation, read from memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   eaddr2_pa ->
                             mstate_mem_read   mstate1  exc_code_load_access_fault  funct3_FLD  eaddr2_pa

    --     Finally: finish with trap, or finish with loading Rd with load-value
    mstate3 = case result2 of
                Mem_Result_Err exc_code ->
                  finish_trap  mstate2  exc_code  eaddr2

                Mem_Result_Ok  d_u64    ->
                  finish_frd_and_pc_plus_4  mstate2  rd  d_u64  is_n_lt_FLEN
  in
    mstate3

-- ================================================================
-- FSD

exec_FSD :: Spec_Instr_D
exec_FSD  is_C  (FSD  rs1  rs2  imm12)  mstate =
  let
    rv   = mstate_rv_read  mstate
    xlen = mstate_xlen_read  mstate

    rs2_val = mstate_fpr_read  mstate  rs2   -- store value

    --     Compute effective address
    rs1_val = mstate_gpr_read  mstate  rs1    -- address base
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    --     If Virtual Mem is active, translate to a physical addr
    is_instr = False
    is_read  = False
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  eaddr2
                         else
                           (Mem_Result_Ok  eaddr2, mstate)

    --     If no trap due to Virtual Mem translation, store to memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   eaddr2_pa ->
                             mstate_mem_write   mstate1  funct3_FSD  eaddr2_pa  rs2_val

    --     Finally: finish with trap, or finish with fall-through
    mstate3 = case result2 of
                Mem_Result_Err exc_code -> finish_trap  mstate2  exc_code  eaddr2
                Mem_Result_Ok  _        -> finish_pc_incr  mstate2  is_C
  in
    mstate3

-- ================================================================
-- FMADD_D

exec_FMADD_D :: Spec_Instr_D
exec_FMADD_D  is_C  (FMADD_D  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2
    rs3_val = mstate_fpr_read  mstate  rs3

    (fflags, rdVal) = fpu_f64MulAdd  rm  rs1_val  rs2_val  rs3_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FMSUB_D

exec_FMSUB_D :: Spec_Instr_D
exec_FMSUB_D  is_C  (FMSUB_D  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2
    rs3_val = mstate_fpr_read  mstate  rs3

    (fflags, rdVal) = fpu_f64MulAdd  rm  rs1_val  rs2_val  (negateD  rs3_val)

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FNMSUB_D

exec_FNMSUB_D :: Spec_Instr_D
exec_FNMSUB_D  is_C  (FNMSUB_D  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2
    rs3_val = mstate_fpr_read  mstate  rs3

    (fflags, rdVal) = fpu_f64MulAdd  rm  (negateD  rs1_val)  rs2_val  rs3_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FNMADD_D

exec_FNMADD_D :: Spec_Instr_D
exec_FNMADD_D  is_C  (FNMADD_D  rd  rs1  rs2  rs3  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2
    rs3_val = mstate_fpr_read  mstate  rs3

    (fflags, rdVal) = fpu_f64MulAdd  rm  (negateD  rs1_val)  rs2_val  (negateD  rs3_val)

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FADD_D

exec_FADD_D :: Spec_Instr_D
exec_FADD_D  is_C  (FADD_D  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)
    rs2_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs2)

    (fflags, rd_val) = fpu_f64Add  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FSUB_D

exec_FSUB_D :: Spec_Instr_D
exec_FSUB_D  is_C  (FSUB_D  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)
    rs2_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs2)

    (fflags, rd_val) = fpu_f64Sub  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FMUL_D

exec_FMUL_D :: Spec_Instr_D
exec_FMUL_D  is_C  (FMUL_D  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)
    rs2_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs2)

    (fflags, rd_val) = fpu_f64Mul  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FDIV_D

exec_FDIV_D :: Spec_Instr_D
exec_FDIV_D  is_C  (FDIV_D  rd  rs1  rs2  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)
    rs2_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs2)

    (fflags, rd_val) = fpu_f64Div  rm  rs1_val  rs2_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FSQRT_D

exec_FSQRT_D :: Spec_Instr_D
exec_FSQRT_D  is_C  (FSQRT_D  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)

    (fflags, rd_val) = fpu_f64Sqrt  rm  rs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FSGNJ_D

exec_FSGNJ_D :: Spec_Instr_D
exec_FSGNJ_D  is_C  (FSGNJ_D  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleDP  rs1_val
    (s2, e2, m2) = disassembleDP  rs2_val

    rd_val = assembleDP  s2  e1  m1

    -- No exceptions are signalled by this operation
    fflags  = 0
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FSGNJN_D

exec_FSGNJN_D :: Spec_Instr_D
exec_FSGNJN_D  is_C  (FSGNJN_D  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleDP  rs1_val
    (s2, e2, m2) = disassembleDP  rs2_val

    rd_val = assembleDP  (xor  s2  0x1)  e1  m1

    -- No exceptions are signalled by this operation
    fflags  = 0
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FSGNJX_D

exec_FSGNJX_D :: Spec_Instr_D
exec_FSGNJX_D  is_C  (FSGNJX_D  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleDP  rs1_val
    (s2, e2, m2) = disassembleDP  rs2_val

    rd_val = assembleDP  (xor  s2  s1)   e1  m1

    -- No exceptions are signalled by this operation
    fflags  = 0
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FMIN_D

exec_FMIN_D :: Spec_Instr_D
exec_FMIN_D  is_C  (FMIN_D  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs1_lt_rs2, fflags) = fpu_f64LE  rs1_val  rs2_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f64IsSNaN     rs1_val
    rs2IsSNaN = fpu_f64IsSNaN     rs2_val

    rs1IsQNaN = fpu_f64IsQNaN     rs1_val
    rs2IsQNaN = fpu_f64IsQNaN     rs2_val

    rs1IsPos0 = fpu_f64IsPosZero  rs1_val
    rs2IsPos0 = fpu_f64IsPosZero  rs2_val

    rs1IsNeg0 = fpu_f64IsNegZero  rs1_val
    rs2IsNeg0 = fpu_f64IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN64
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN64
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs1_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs2_val
           | rs1_lt_rs2                = rs1_val
           | (not rs1_lt_rs2)          = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FMAX_D

exec_FMAX_D :: Spec_Instr_D
exec_FMAX_D  is_C  (FMAX_D  rd  rs1  rs2)  mstate =
  let
    is_n_lt_FLEN = False

    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs2_lt_rs1, fflags) = fpu_f64LE  rs2_val  rs1_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f64IsSNaN     rs1_val
    rs2IsSNaN = fpu_f64IsSNaN     rs2_val

    rs1IsQNaN = fpu_f64IsQNaN     rs1_val
    rs2IsQNaN = fpu_f64IsQNaN     rs2_val

    rs1IsPos0 = fpu_f64IsPosZero  rs1_val
    rs2IsPos0 = fpu_f64IsPosZero  rs2_val

    rs1IsNeg0 = fpu_f64IsNegZero  rs1_val
    rs2IsNeg0 = fpu_f64IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN64
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN64
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs2_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs1_val
           | rs2_lt_rs1 = rs1_val
           | (not rs2_lt_rs1) = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FCVT_S_D

exec_FCVT_S_D :: Spec_Instr_D
exec_FCVT_S_D  is_C  (FCVT_S_D  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = True

    frs1_val    = mstate_fpr_read  mstate  rs1

    (fflags, rdVal) = fpu_f64ToF32  rm  frs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FCVT_D_S

exec_FCVT_D_S :: Spec_Instr_D
exec_FCVT_D_S  is_C  (FCVT_D_S  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False

    frs1_val_sp = unboxSP  (mstate_fpr_read  mstate  rs1)

    (fflags, rdVal) = fpu_f32ToF64   rm  frs1_val_sp

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FEQ_D

exec_FEQ_D :: Spec_Instr_D
exec_FEQ_D  is_C  (FEQ_D  rd  rs1  rs2)  mstate =
  let
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) = fpu_f64EQQ  rs1_val  rs2_val

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f64IsSNaN     rs1_val
    rs2IsSNaN = fpu_f64IsSNaN     rs2_val

    rs1IsQNaN = fpu_f64IsQNaN     rs1_val
    rs2IsQNaN = fpu_f64IsQNaN     rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags
  in
    mstate1

-- ================================================================
-- FLT_D

exec_FLT_D :: Spec_Instr_D
exec_FLT_D  is_C  (FLT_D  rd  rs1  rs2)  mstate =
  let
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) = fpu_f64LT   rs1_val  rs2_val  False

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f64IsSNaN     rs1_val
    rs2IsSNaN = fpu_f64IsSNaN     rs2_val

    rs1IsQNaN = fpu_f64IsQNaN     rs1_val
    rs2IsQNaN = fpu_f64IsQNaN     rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags
  in
    mstate1

-- ================================================================
-- FLE_D

exec_FLE_D :: Spec_Instr_D
exec_FLE_D  is_C  (FLE_D  rd  rs1  rs2)  mstate =
  let
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) = fpu_f64LE   rs1_val  rs2_val  False

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = fpu_f64IsSNaN     rs1_val
    rs2IsSNaN = fpu_f64IsSNaN     rs2_val

    rs1IsQNaN = fpu_f64IsQNaN     rs1_val
    rs2IsQNaN = fpu_f64IsQNaN     rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags
  in
    mstate1

-- ================================================================
-- FCLASS_D

exec_FCLASS_D :: Spec_Instr_D
exec_FCLASS_D  is_C  (FCLASS_D  rd  rs1)  mstate =
  let
    frs1_val = mstate_fpr_read  mstate  rs1
    
    -- Classify the frs1_val
    is_NegInf     = fpu_f64IsNegInf        frs1_val
    is_NegNorm    = fpu_f64IsNegNorm       frs1_val
    is_NegSubNorm = fpu_f64IsNegSubNorm    frs1_val
    is_NegZero    = fpu_f64IsNegZero       frs1_val
    is_PosZero    = fpu_f64IsPosZero       frs1_val
    is_PosSubNorm = fpu_f64IsPosSubNorm    frs1_val
    is_PosNorm    = fpu_f64IsPosNorm       frs1_val
    is_PosInf     = fpu_f64IsPosInf        frs1_val
    is_SNaN       = fpu_f64IsSNaN          frs1_val
    is_QNaN       = fpu_f64IsQNaN          frs1_val

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
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val'  is_C
  in
    mstate1

-- ================================================================
-- FCVT_W_D

exec_FCVT_W_D :: Spec_Instr_D
exec_FCVT_W_D  is_C  (FCVT_W_D  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False

    frs1_val    = mstate_fpr_read  mstate  rs1

    (fflags, rdVal) = fpu_f64ToI32  rm   frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags
  in
    mstate1

-- ================================================================
-- FCVT_WU_D

exec_FCVT_WU_D :: Spec_Instr_D
exec_FCVT_WU_D  is_C  (FCVT_WU_D  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False

    frs1_val    = mstate_fpr_read  mstate  rs1

    (fflags, rdVal) = fpu_f64ToUi32  rm  frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags
  in
    mstate1

-- ================================================================
-- FCVT_D_W

exec_FCVT_D_W :: Spec_Instr_D
exec_FCVT_D_W  is_C  (FCVT_D_W  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False
    xlen         = mstate_xlen_read  mstate

    grs1_val = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)

    (fflags, rdVal) = fpu_i32ToF64  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FCVT_D_WU

exec_FCVT_D_WU :: Spec_Instr_D
exec_FCVT_D_WU  is_C  (FCVT_D_WU  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False
    xlen         = mstate_xlen_read  mstate

    grs1_val = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)

    (fflags, rdVal) = fpu_ui32ToF64  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FCVT_L_D

exec_FCVT_L_D :: Spec_Instr_D
exec_FCVT_L_D  is_C  (FCVT_L_D  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False

    frs1_val = mstate_fpr_read  mstate  rs1

    (fflags, rdVal) = fpu_f64ToI64   rm  frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags
  in
    mstate1

-- ================================================================
-- FCVT_LU_D

exec_FCVT_LU_D :: Spec_Instr_D
exec_FCVT_LU_D  is_C  (FCVT_LU_D  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False

    frs1_val = mstate_fpr_read  mstate  rs1

    (fflags, rdVal) = fpu_f64ToUi64  rm  frs1_val

    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags
  in
    mstate1

-- ================================================================
-- FMV_X_D

exec_FMV_X_D :: Spec_Instr_D
exec_FMV_X_D  is_C  (FMV_X_D  rd  rs1)  mstate =
  let
    frs1_val = mstate_fpr_read  mstate  rs1

    mstate1 = finish_rd_and_pc_incr  mstate  rd  frs1_val  is_C
  in
    mstate1

-- ================================================================
-- FCVT_D_L

exec_FCVT_D_L :: Spec_Instr_D
exec_FCVT_D_L  is_C  (FCVT_D_L  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False
    xlen         = mstate_xlen_read  mstate

    grs1_val = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)

    (fflags, rdVal) = fpu_i64ToF64  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FCVT_D_LU

exec_FCVT_D_LU :: Spec_Instr_D
exec_FCVT_D_LU  is_C  (FCVT_D_LU  rd  rs1  rm)  mstate =
  let
    is_n_lt_FLEN = False
    xlen         = mstate_xlen_read  mstate

    grs1_val = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)

    (fflags, rdVal) = fpu_ui64ToF64  rm  grs1_val

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    mstate1

-- ================================================================
-- FMV_D_X

exec_FMV_D_X :: Spec_Instr_D
exec_FMV_D_X  is_C  (FMV_D_X  rd  rs1)  mstate =
  let
    is_n_lt_FLEN = False

    grs1_val = mstate_gpr_read  mstate  rs1

    mstate1 = finish_frd_and_pc_plus_4  mstate  rd  grs1_val  is_n_lt_FLEN
  in
    mstate1

-- ================================================================

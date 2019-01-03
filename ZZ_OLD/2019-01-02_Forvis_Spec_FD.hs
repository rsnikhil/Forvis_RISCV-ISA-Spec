-- Copyright (c) 2018-2019 Rishiyur S. Nikhil, Niraj N. Sharma
-- See LICENSE for license details

module Forvis_Spec_FD where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'F' and 'D' Extension
-- i.e., single- and double-precision floating point.

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

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- 'F' and 'D' extensions (floating point)

-- ================================================================
-- The following is a list of all the specification functions defined below.

instr_specs_FD :: [(Instr_Spec, String)]
instr_specs_FD = [(spec_FD_LOAD,           "FD_LOAD"),
                  (spec_FD_STORE,          "FD_STORE"),
                  (spec_D_OP,              "FPU_D_OP"),
                  (spec_D_FCVT,            "FPU_D_CONVERT"),
                  (spec_D_FCLASS,          "FPU_D_CLASS"),
                  (spec_D_FSGNJ,           "FPU_D_SIGN_CHANGE"),
                  (spec_D_CMP,             "FPU_D_COMPARE"),
                  (spec_D_MAX,             "FPU_D_MAX"),
                  (spec_D_MIN,             "FPU_D_MIN"),
                  (spec_D_FMOP,            "FPU_D_MULTIPLY_ACCUMULATE"),
                  (spec_D_FMV,             "FPU_D_REG_MOVE"),
                  (spec_F_OP,              "FPU_F_OP"),
                  (spec_F_FCVT,            "FPU_F_CONVERT"),
                  (spec_F_FCLASS,          "FPU_F_CLASS"),
                  (spec_F_FSGNJ,           "FPU_F_SIGN_CHANGE"),
                  (spec_F_CMP,             "FPU_F_COMPARE"),
                  (spec_F_MIN,             "FPU_F_MIN"),
                  (spec_F_MAX,             "FPU_F_MAX"),
                  (spec_F_FMOP,            "FPU_F_MULTIPLY_ACCUMULATE"),
                  (spec_F_FMV,             "FPU_F_REG_MOVE")
                 ]

-- ================================================================
-- FD_LOAD
--    SP: FLW
--    DP: FLD

opcode_FD_LOAD = 0x07   :: InstrField  -- 7'b_00_001_11
funct3_FD_LW   = 0x2    :: InstrField  -- 3'b_010
funct3_FD_LD   = 0x3    :: InstrField  -- 3'b_011

spec_FD_LOAD :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_FD_LOAD    mstate           instr        is_C =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    rv         = mstate_rv_read  mstate
    xlen       = mstate_xlen_read  mstate
    misa       = (mstate_csr_read mstate  csr_addr_misa)
    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_LW      = (funct3 == funct3_FD_LW)
    is_LD      = (funct3 == funct3_FD_LD)
    is_legal   = (   (opcode == opcode_FD_LOAD)
                  && (is_F)
                  && (is_LW || (is_LD && is_D)))

    -- Semantics
    --     Compute effective address
    rs1_val = mstate_gpr_read  mstate  rs1
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    --     If Virtual Mem is active, translate to a physical addr
    is_instr = False
    is_read  = True
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  eaddr2
                         else
                           (Mem_Result_Ok  eaddr2, mstate)

    --     If no trap due to Virtual Mem translation, read from memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   eaddr2_pa ->
                             mstate_mem_read   mstate1  exc_code_load_access_fault  funct3  eaddr2_pa

    --     Finally: finish with trap, or finish with loading Rd with load-value
    mstate3 = case result2 of
                Mem_Result_Err exc_code ->
                  finish_trap  mstate2  exc_code  eaddr2

                Mem_Result_Ok  d_u64    ->
                  finish_frd_and_pc_plus_4  mstate2  rd  d_u64  is_LW
  in
    (is_legal, mstate3)

-- ================================================================
-- FD_STORE
--    SP: FSW
--    DP: FSD

-- Note: these are duplicates of defs in Mem_Ops.hs
opcode_FD_STORE   = 0x27   :: InstrField  -- 7'b_01_001_11
funct3_FD_SW      = 0x2    :: InstrField  -- 3'b_010
funct3_FD_SD      = 0x3    :: InstrField  -- 3'b_011

spec_FD_STORE :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_FD_STORE    mstate           instr        is_C =
  let
    -- Instr fields: S-type
    (imm12, rs2, rs1, funct3, opcode) = ifields_S_type  instr

    -- Decode check
    rv         = mstate_rv_read  mstate
    xlen     = mstate_xlen_read  mstate
    misa       = (mstate_csr_read mstate  csr_addr_misa)
    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_SW      = (funct3 == funct3_FD_SW)
    is_SD      = (funct3 == funct3_FD_SD)
    is_legal   = (   (opcode == opcode_FD_STORE)
                  && (is_F)
                  && (is_SW || (is_SD && is_D)))

    -- Semantics
    -- For SW, the upper bits are to be ignored
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
                             mstate_mem_write   mstate1  funct3  eaddr2_pa  rs2_val

    --     Finally: finish with trap, or finish with fall-through
    mstate3 = case result2 of
                Mem_Result_Err exc_code -> finish_trap  mstate2  exc_code  eaddr2
                Mem_Result_Ok  _        -> finish_pc_incr  mstate2  is_C
  in
    (is_legal, mstate3)

-- ================================================================
-- FD Opcodes
-- Opcode (duplicate from Arch_Defs)
opcode_FD_OP      = 0x53   :: InstrField  -- 7'b_10_100_11

funct7_FADD_D     = 0x1    :: InstrField  -- 7'b_00_000_01
funct7_FSUB_D     = 0x5    :: InstrField  -- 7'b_00_001_01
funct7_FMUL_D     = 0x9    :: InstrField  -- 7'b_00_010_01
funct7_FDIV_D     = 0xD    :: InstrField  -- 7'b_01_011_01
funct7_FSQRT_D    = 0x2D   :: InstrField  -- 7'b_00_000_01
funct7_FCMP_D     = 0x51   :: InstrField  -- 7'b_10_100_01
funct7_FMIN_D     = 0x15   :: InstrField  -- 7'b_00_101_01
funct7_FMAX_D     = 0x15   :: InstrField  -- 7'b_00_101_01
funct7_FSGNJ_D    = 0x11   :: InstrField  -- 7'b_00_100_01

funct7_FADD_S     = 0x0    :: InstrField  -- 7'b_00_000_00
funct7_FSUB_S     = 0x4    :: InstrField  -- 7'b_00_001_00
funct7_FMUL_S     = 0x8    :: InstrField  -- 7'b_00_010_00
funct7_FDIV_S     = 0xC    :: InstrField  -- 7'b_01_011_00
funct7_FSQRT_S    = 0x2C   :: InstrField  -- 7'b_00_000_00
funct7_FCMP_S     = 0x50   :: InstrField  -- 7'b_10_100_00
funct7_FMIN_S     = 0x14   :: InstrField  -- 7'b_00_101_01
funct7_FMAX_S     = 0x14   :: InstrField  -- 7'b_00_101_01
funct7_FSGNJ_S    = 0x10   :: InstrField  -- 7'b_00_100_00

funct7_FCVT_W_S   = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_WU_S  = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_S_W   = 0x68   :: InstrField  -- 7'b_11_010_00
funct7_FCVT_S_WU  = 0x68   :: InstrField  -- 7'b_11_010_00

funct7_FCVT_L_S   = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_LU_S  = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_S_L   = 0x68   :: InstrField  -- 7'b_11_010_00
funct7_FCVT_S_LU  = 0x68   :: InstrField  -- 7'b_11_010_00

funct7_FCVT_S_D   = 0x20   :: InstrField  -- 7'b_01_000_00
funct7_FCVT_D_S   = 0x21   :: InstrField  -- 7'b_01_000_01
funct7_FCVT_W_D   = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_WU_D  = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_D_W   = 0x69   :: InstrField  -- 7'b_11_010_01
funct7_FCVT_D_WU  = 0x69   :: InstrField  -- 7'b_11_010_01

funct7_FCVT_L_D   = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_LU_D  = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_D_L   = 0x69   :: InstrField  -- 7'b_11_010_00
funct7_FCVT_D_LU  = 0x69   :: InstrField  -- 7'b_11_010_00


-- 'D' extensions OPs: FADD, FSUB, FMUL, FDIV, FMIN, FMAX, FSQRT
spec_D_OP :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_OP    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FADD_D  = (funct7 == funct7_FADD_D)
    is_FSUB_D  = (funct7 == funct7_FSUB_D)
    is_FMUL_D  = (funct7 == funct7_FMUL_D)
    is_FDIV_D  = (funct7 == funct7_FDIV_D)
    is_FSQRT_D = (funct7 == funct7_FSQRT_D)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FADD_D
                    || is_FSUB_D
                    || is_FMUL_D
                    || is_FDIV_D
                    || is_FSQRT_D)
                && (is_F && is_D)
                && rmIsLegal)

    -- Semantics
    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)
    rs2_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs2)

    -- Do the operations using the fpu functions
    (fflags, rd_val) | is_FADD_D  = fpu_f64Add  frmVal  rs1_val  rs2_val
                     | is_FSUB_D  = fpu_f64Sub  frmVal  rs1_val  rs2_val
                     | is_FMUL_D  = fpu_f64Mul  frmVal  rs1_val  rs2_val
                     | is_FDIV_D  = fpu_f64Div  frmVal  rs1_val  rs2_val
                     | is_FSQRT_D = fpu_f64Sqrt frmVal  rs1_val


    is_n_lt_FLEN = False
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FSGNJ, FSGNJN, FSGNJX
spec_D_FSGNJ :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_FSGNJ    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)

    is_F          = (misa_flag misa 'F')
    is_D          = (misa_flag misa 'D')

    is_FSGNJ_D    = (funct7 == funct7_FSGNJ_D) && (rm == 0x0)
    is_FSGNJN_D   = (funct7 == funct7_FSGNJ_D) && (rm == 0x1)
    is_FSGNJX_D   = (funct7 == funct7_FSGNJ_D) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FSGNJ_D
                    || is_FSGNJN_D
                    || is_FSGNJX_D)
                && (is_F && is_D))

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleDP  rs1_val
    (s2, e2, m2) = disassembleDP  rs2_val

    rd_val | is_FSGNJ_D    = assembleDP   s2             e1  m1
           | is_FSGNJN_D   = assembleDP  (xor  s2  0x1)  e1  m1
           | is_FSGNJX_D   = assembleDP  (xor  s2  s1)   e1  m1


    -- No exceptions are signalled by these operations
    is_n_lt_FLEN = False
    fflags       = 0x0
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FCVT
spec_D_FCVT :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_FCVT    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)
    rv            = mstate_rv_read  mstate

    is_F          = (misa_flag misa 'F')
    is_D          = (misa_flag misa 'D')
    
    is_FCVT_W_D   =    (funct7 == funct7_FCVT_W_D)
                    && (rs2 == 0)
    is_FCVT_WU_D  =    (funct7 == funct7_FCVT_WU_D)
                    && (rs2 == 1)
    is_FCVT_L_D   =    (funct7 == funct7_FCVT_L_D)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_LU_D  =    (funct7 == funct7_FCVT_LU_D)
                    && (rs2 == 3)
                    && (rv == RV64)
    is_FCVT_D_W   =    (funct7 == funct7_FCVT_D_W)
                    && (rs2 == 0)
    is_FCVT_D_WU  =    (funct7 == funct7_FCVT_D_WU)
                    && (rs2 == 1)
    is_FCVT_D_L   =    (funct7 == funct7_FCVT_D_L)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_D_LU  =    (funct7 == funct7_FCVT_D_LU)
                    && (rs2 == 3)
                    && (rv == RV64)
    is_FCVT_D_S   =    (funct7 == funct7_FCVT_D_S)
                    && (rs2 == 0)
    is_FCVT_S_D   =    (funct7 == funct7_FCVT_S_D)
                    && (rs2 == 1)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FCVT_W_D 
                    || is_FCVT_WU_D
                    || is_FCVT_L_D 
                    || is_FCVT_LU_D
                    || is_FCVT_D_W 
                    || is_FCVT_D_WU
                    || is_FCVT_D_L 
                    || is_FCVT_D_LU
                    || is_FCVT_D_S
                    || is_FCVT_S_D) 
                && (is_F && is_D)
                && rmIsLegal)

    destInGPR   =    is_FCVT_W_D
                  || is_FCVT_WU_D
                  || is_FCVT_L_D
                  || is_FCVT_LU_D

    -- Semantics
    xlen        | (rv == RV64) = 64
                | (rv == RV32) = 32

    frs1_val    = mstate_fpr_read  mstate  rs1
    frs1_val_sp = unboxSP  (mstate_fpr_read  mstate  rs1)
    grs1_val    = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)


    -- Do the operations using the FPU functions
    (fflags, rdVal)  | is_FCVT_D_L   = fpu_i64ToF64   frmVal   grs1_val
                     | is_FCVT_D_LU  = fpu_ui64ToF64  frmVal   grs1_val
                     | is_FCVT_D_W   = fpu_i32ToF64   frmVal   grs1_val
                     | is_FCVT_D_WU  = fpu_ui32ToF64  frmVal   grs1_val
                     | is_FCVT_D_S   = fpu_f32ToF64   frmVal   frs1_val_sp
                     | is_FCVT_S_D   = fpu_f64ToF32   frmVal   frs1_val
                     | is_FCVT_L_D   = fpu_f64ToI64   frmVal   frs1_val
                     | is_FCVT_LU_D  = fpu_f64ToUi64  frmVal   frs1_val
                     | is_FCVT_W_D   = fpu_f64ToI32   frmVal   frs1_val
                     | is_FCVT_WU_D  = fpu_f64ToUi32  frmVal   frs1_val


    is_n_lt_FLEN = is_FCVT_S_D
    mstate1 = if (destInGPR) then
                finish_grd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags
              else
                finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FMIN, FMAX
spec_D_MIN :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_MIN    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMIN_D  = (funct7 == funct7_FMIN_D) && (rm == 0x0)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMIN_D
                && (is_F && is_D))

    -- Semantics
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
    is_n_lt_FLEN = False
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FEQ, FLT, FLE
spec_D_CMP :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_CMP    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FLE_D   = (funct7 == funct7_FCMP_D) && (rm == 0x0)
    is_FLT_D   = (funct7 == funct7_FCMP_D) && (rm == 0x1)
    is_FEQ_D   = (funct7 == funct7_FCMP_D) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (is_FEQ_D || is_FLT_D || is_FLE_D)
                && (is_F && is_D))

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) | (is_FEQ_D) = fpu_f64EQQ  rs1_val  rs2_val
                          | (is_FLT_D) = fpu_f64LT   rs1_val  rs2_val  False
                          | (is_FLE_D) = fpu_f64LE   rs1_val  rs2_val  False

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
    (is_legal, mstate1)


-- 'D' extensions OPs:  FMAX

spec_D_MAX :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_MAX    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMIN_D  = (funct7 == funct7_FMIN_D) && (rm == 0x0)
    is_FMAX_D  = (funct7 == funct7_FMAX_D) && (rm == 0x1)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMAX_D
                && (is_F && is_D))

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs2_lt_rs1, fflags) = fpu_f64LE rs2_val  rs1_val  True

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
    is_n_lt_FLEN = False
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- ================================================================
-- 'D' extensions OPs: FMADD, FMSUB, FNMADD, FNMSUB, 
opcode_FMADD_OP   = 0x43   :: InstrField  -- 7'b_10_000_11
opcode_FMSUB_OP   = 0x47   :: InstrField  -- 7'b_10_001_11
opcode_FNMSUB_OP  = 0x4B   :: InstrField  -- 7'b_10_010_11
opcode_FNMADD_OP  = 0x4F   :: InstrField  -- 7'b_10_011_11

spec_D_FMOP :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_FMOP    mstate           instr        is_C =
  let
    -- Instr fields: R4-type
    (rs3, funct2, rs2, rs1, rm, rd, opcode) = ifields_R4_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMADD_D  = (opcode == opcode_FMADD_OP)  && (funct2 == 0x1)
    is_FMSUB_D  = (opcode == opcode_FMSUB_OP)  && (funct2 == 0x1)
    is_FNMADD_D = (opcode == opcode_FNMADD_OP) && (funct2 == 0x1)
    is_FNMSUB_D = (opcode == opcode_FNMSUB_OP) && (funct2 == 0x1)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (is_F && is_D)
                && (   is_FMADD_D
                    || is_FMSUB_D
                    || is_FNMADD_D
                    || is_FNMSUB_D)
                && rmIsLegal)

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2
    rs3_val = mstate_fpr_read  mstate  rs3

    -- Extract the result of the operation and the flags
    (fflags, rdVal) | is_FMADD_D    = fpu_f64MulAdd  frmVal  rs1_val             rs2_val  rs3_val
                    | is_FMSUB_D    = fpu_f64MulAdd  frmVal  rs1_val             rs2_val  (negateD  rs3_val)
                    | is_FNMSUB_D   = fpu_f64MulAdd  frmVal  (negateD  rs1_val)  rs2_val  rs3_val
                    | is_FNMADD_D   = fpu_f64MulAdd  frmVal  (negateD  rs1_val)  rs2_val  (negateD  rs3_val)

    is_n_lt_FLEN = False
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal   fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- RV64-'D' extension Ops: FMV.D.X and FMV.X.D
funct7_FMV_X_D    = 0x71   :: InstrField  -- 7'b_11_100_01
funct7_FMV_D_X    = 0x79   :: InstrField  -- 7'b_11_110_01

spec_D_FMV :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_FMV    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_FMV_X_D = (funct7 == funct7_FMV_X_D)
    is_FMV_D_X = (funct7 == funct7_FMV_D_X)
    rmIsLegal  = (rm == 0x0)
    rv         = mstate_rv_read  mstate

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FMV_X_D
                    || is_FMV_D_X)
                && (is_F && is_D)
                && (rv == RV64)
                && rmIsLegal)

    -- Semantics
    frs1_val = mstate_fpr_read  mstate  rs1
    grs1_val = mstate_gpr_read  mstate  rs1

    mstate1  = if (is_FMV_X_D) then
                 finish_rd_and_pc_incr  mstate  rd  frs1_val  is_C
               else
                 finish_frd_and_pc_plus_4  mstate  rd  grs1_val  False
  in
    (is_legal, mstate1)


-- 'D' extension Ops: FCLASS
funct7_FCLASS_D  = 0x71 :: InstrField  -- 7'b_11_100_01
spec_D_FCLASS :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_D_FCLASS    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_FCLASS  = (funct7 == funct7_FCLASS_D)
    rmIsLegal  = (rm == 0x1)
    is_legal   = (   (opcode == opcode_FD_OP)
                  && is_FCLASS
                  && (is_F && is_D)
                  && rmIsLegal)

    -- Semantics
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
    (is_legal, mstate1)


-- 'F' extensions OPs: FADD, FSUB, FMUL, FDIV, FSQRT
spec_F_OP :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_OP    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FADD_S  = (funct7 == funct7_FADD_S)
    is_FSUB_S  = (funct7 == funct7_FSUB_S)
    is_FMUL_S  = (funct7 == funct7_FMUL_S)
    is_FDIV_S  = (funct7 == funct7_FDIV_S)
    is_FSQRT_S = (funct7 == funct7_FSQRT_S)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FADD_S
                    || is_FSUB_S
                    || is_FMUL_S
                    || is_FDIV_S
                    || is_FSQRT_S)
                && is_F
                && rmIsLegal)

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  mstate  rs1))
    rs2_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  mstate  rs2))

    -- Do the operations using the FPU functions
    (fflags, rd_val) | is_FADD_S  = fpu_f32Add  frmVal  rs1_val  rs2_val
                     | is_FSUB_S  = fpu_f32Sub  frmVal  rs1_val  rs2_val
                     | is_FMUL_S  = fpu_f32Mul  frmVal  rs1_val  rs2_val
                     | is_FDIV_S  = fpu_f32Div  frmVal  rs1_val  rs2_val
                     | is_FSQRT_S = fpu_f32Sqrt frmVal  rs1_val

    is_n_lt_FLEN = True
    mstate1      = finish_frd_fflags_and_pc_plus_4 mstate rd rd_val fflags is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FSGNJ, FSGNJN, FSGNJX
spec_F_FSGNJ :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_FSGNJ    mstate           instr        is_C  =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)

    is_F          = (misa_flag misa 'F')

    is_FSGNJ_S    = (funct7 == funct7_FSGNJ_S) && (rm == 0x0)
    is_FSGNJN_S   = (funct7 == funct7_FSGNJ_S) && (rm == 0x1)
    is_FSGNJX_S   = (funct7 == funct7_FSGNJ_S) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FSGNJ_S
                    || is_FSGNJN_S
                    || is_FSGNJX_S)
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

    -- Extract the components of the source values
    (s1, e1, m1) = disassembleSP  rs1_val
    (s2, e2, m2) = disassembleSP  rs2_val

    rd_val | is_FSGNJ_S    = assembleSP   s2             e1  m1
           | is_FSGNJN_S   = assembleSP  (xor  s2  0x1)  e1  m1
           | is_FSGNJX_S   = assembleSP  (xor  s2  s1)   e1  m1


    -- No exceptions are signalled by these operations
    is_n_lt_FLEN = True
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  0x0  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FCVT
spec_F_FCVT :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_FCVT    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)
    rv            = mstate_rv_read  mstate

    is_F          = (misa_flag misa 'F')
    
    is_FCVT_W_S   =    (funct7 == funct7_FCVT_W_S)
                    && (rs2 == 0)
    is_FCVT_WU_S  =    (funct7 == funct7_FCVT_WU_S)
                    && (rs2 == 1)
    is_FCVT_L_S   =    (funct7 == funct7_FCVT_L_S)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_LU_S  =    (funct7 == funct7_FCVT_LU_S)
                    && (rs2 == 3)
                    && (rv == RV64)
    is_FCVT_S_W   =    (funct7 == funct7_FCVT_S_W)
                    && (rs2 == 0)
    is_FCVT_S_WU  =    (funct7 == funct7_FCVT_S_WU)
                    && (rs2 == 1)
    is_FCVT_S_L   =    (funct7 == funct7_FCVT_S_L)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_S_LU  =    (funct7 == funct7_FCVT_S_LU)
                    && (rs2 == 3)
                    && (rv == RV64)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FCVT_W_S 
                    || is_FCVT_WU_S
                    || is_FCVT_L_S 
                    || is_FCVT_LU_S
                    || is_FCVT_S_W 
                    || is_FCVT_S_WU
                    || is_FCVT_S_L 
                    || is_FCVT_S_LU)
                && (is_F)
                && rmIsLegal)

    destInGPR   =    is_FCVT_W_S
                  || is_FCVT_WU_S
                  || is_FCVT_L_S
                  || is_FCVT_LU_S

    -- Semantics
    xlen        | (rv == RV64) = 64
                | (rv == RV32) = 32

    frs1_val    = unboxSP  (mstate_fpr_read  mstate  rs1)
    grs1_val    = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)


    -- Do the operations using the FPU functions
    (fflags, rdVal)  | is_FCVT_S_L   = fpu_i64ToF32   frmVal   grs1_val
                     | is_FCVT_S_LU  = fpu_ui64ToF32  frmVal   grs1_val
                     | is_FCVT_S_W   = fpu_i32ToF32   frmVal   grs1_val
                     | is_FCVT_S_WU  = fpu_ui32ToF32  frmVal   grs1_val
                     | is_FCVT_L_S   = fpu_f32ToI64   frmVal   frs1_val
                     | is_FCVT_LU_S  = fpu_f32ToUi64  frmVal   frs1_val
                     | is_FCVT_W_S   = fpu_f32ToI32   frmVal   frs1_val
                     | is_FCVT_WU_S  = fpu_f32ToUi32  frmVal   frs1_val


    is_n_lt_FLEN = True
    mstate1 = if (destInGPR) then
                finish_grd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags
              else
                finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FMIN
spec_F_MIN :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_MIN    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')

    is_FMIN_S  = (funct7 == funct7_FMIN_S) && (rm == 0x0)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMIN_S
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

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
    is_n_lt_FLEN = True
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FEQ, FLT, FLE
spec_F_CMP :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_CMP    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')

    is_FLE_S   = (funct7 == funct7_FCMP_S) && (rm == 0x0)
    is_FLT_S   = (funct7 == funct7_FCMP_S) && (rm == 0x1)
    is_FEQ_S   = (funct7 == funct7_FCMP_S) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (is_FEQ_S || is_FLT_S || is_FLE_S)
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) | (is_FEQ_S) = fpu_f32EQQ  rs1_val  rs2_val
                          | (is_FLT_S) = fpu_f32LT   rs1_val  rs2_val  False
                          | (is_FLE_S) = fpu_f32LE   rs1_val  rs2_val  False

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
    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FMAX

spec_F_MAX :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_MAX    mstate           instr        is_C  =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')

    is_FMAX_S  = (funct7 == funct7_FMAX_S) && (rm == 0x1)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMAX_S
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

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
    is_n_lt_FLEN = True
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- ================================================================
-- 'F' extensions OPs: FMADD, FMSUB, FNMADD, FNMSUB, 

spec_F_FMOP :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_FMOP    mstate           instr        is_C =
  let
    -- Instr fields: R4-type
    (rs3, funct2, rs2, rs1, rm, rd, opcode) = ifields_R4_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMADD_S  = (opcode == opcode_FMADD_OP)  && (funct2 == 0)
    is_FMSUB_S  = (opcode == opcode_FMSUB_OP)  && (funct2 == 0)
    is_FNMADD_S = (opcode == opcode_FNMADD_OP) && (funct2 == 0)
    is_FNMSUB_S = (opcode == opcode_FNMSUB_OP) && (funct2 == 0)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (is_F)
                && (   is_FMADD_S
                    || is_FMSUB_S
                    || is_FNMADD_S
                    || is_FNMSUB_S)
                && rmIsLegal)

    -- Semantics
    rs1_val = unboxSP  (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP  (mstate_fpr_read  mstate  rs2)
    rs3_val = unboxSP  (mstate_fpr_read  mstate  rs3)


    -- Extract the result of the operation and the flags
    (fflags, rdVal)  | is_FMADD_S   = fpu_f32MulAdd  frmVal  rs1_val             rs2_val  rs3_val
                     | is_FMSUB_S   = fpu_f32MulAdd  frmVal  rs1_val             rs2_val  (negateS rs3_val)
                     | is_FNMSUB_S  = fpu_f32MulAdd  frmVal  (negateS rs1_val)   rs2_val  rs3_val
                     | is_FNMADD_S  = fpu_f32MulAdd  frmVal  (negateS rs1_val)   rs2_val  (negateS rs3_val)

    is_n_lt_FLEN = True
    mstate1      = finish_frd_fflags_and_pc_plus_4  mstate  rd  rdVal   fflags  is_n_lt_FLEN
  in
    (is_legal, mstate1)


-- RV32/64 - 'F' extension Ops: FMV.W.X and FMV.X.W
funct7_FMV_X_W    = 0x70   :: InstrField  -- 7'b_11_100_00
funct7_FMV_W_X    = 0x78   :: InstrField  -- 7'b_11_110_00

spec_F_FMV :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_FMV    mstate           instr        is_C = 
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_FMV_X_W = (funct7 == funct7_FMV_X_W)
    is_FMV_W_X = (funct7 == funct7_FMV_W_X)
    rmIsLegal  = (rm == 0x0)
    rv         = mstate_rv_read  mstate

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FMV_X_W
                    || is_FMV_W_X)
                && (is_F)
                && rmIsLegal)

    -- Semantics
    frs1_val = mstate_fpr_read  mstate  rs1
    grs1_val = mstate_gpr_read  mstate  rs1

    -- FMV_X_W
    -- GPR value is sign-extended version of lower 32-bits of FPR contents
    frs1_val' = sign_extend  32  64  (bitSlice frs1_val  31  0)

    mstate1  = if (is_FMV_X_W) then
                 finish_rd_and_pc_incr  mstate  rd  frs1_val'  is_C
               else
                 finish_frd_and_pc_plus_4  mstate  rd  grs1_val  True
  in
    (is_legal, mstate1)


-- 'F' extension Ops: FCLASS
funct7_FCLASS_F = 0x70  :: InstrField  -- 7'b_11_100_01
spec_F_FCLASS :: Machine_State -> Instr_32b -> Bool -> (Bool, Machine_State)
spec_F_FCLASS    mstate           instr        is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_FCLASS  = (funct7 == funct7_FCLASS_F)
    rmIsLegal  = (rm == 0x1)
    is_legal   = (   (opcode == opcode_FD_OP)
                  && is_FCLASS
                  && (is_F)
                  && rmIsLegal)

    -- Semantics
    frs1_val = unboxSP  (mstate_fpr_read  mstate  rs1)
    
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
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val'  is_C
  in
    (is_legal, mstate1)

-- ================================================================

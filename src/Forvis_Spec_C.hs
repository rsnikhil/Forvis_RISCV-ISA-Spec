-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_C where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module describes the specificatin of the RISC-V 'C' Extension
-- i.e., 16-bit 'compressed' instructions.

-- Each function mostly captures a decode and legality check.
-- Then, it is expanded into an existing 32-bit RV32I or RV64I instruction,
-- and we simply invoke the semantic function for that instruction.

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import Arch_Defs
import Machine_State
import CSR_File

import Forvis_Spec_I                -- Base instr set ('C' is defined in terms of Base)

import Forvis_Spec_Finish_Instr     -- Canonical ways for finish an instruction

-- ================================================================
-- 'C' Extension ("Compressed") major opcodes ('quadrants' 0, 1 and 2)

-- The following are defined in module Arch_Defs
-- opcode_C0 = 0x0 :: InstrField    -- 2'b00
-- opcode_C1 = 0x1 :: InstrField    -- 2'b01
-- opcode_C2 = 0x2 :: InstrField    -- 2'b10

-- ================================================================
-- 'C' Extension Stack-Pointer-Based Loads

funct3_C_LWSP  = 0x2 :: InstrField    -- 3'b_010
funct3_C_LDSP  = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128
funct3_C_LQSP  = 0x1 :: InstrField    -- 3'b_001     RV128
funct3_C_FLWSP = 0x3 :: InstrField    -- 3'b_011     RV32FC
funct3_C_FLDSP = 0x1 :: InstrField    -- 3'b_001     RV32DC, RV64DC

spec_C_LWSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LWSP    mstate           instr =
  let
    -- Instr fields: I-type
    (funct3, imm_at_12, rd, imm_at_6_2, op) = ifields_CI_type  instr
    offset = ((    shift  (bitSlice  imm_at_6_2  1  0)  6)
              .|. (shift  (bitSlice  imm_at_6_2  4  2)  2)
              .|. (shift  imm_at_12                     5))

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (rd /= 0)
                && (funct3 == funct3_C_LWSP))

    -- Semantics: same as LW
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_I_type  offset  rs1  funct3_LW  rd  opcode_LOAD
    is_C         = True
    (b, mstate1) = spec_LOAD  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LWSP constructed an illegal LW"
  in
    (is_legal, mstate2)

spec_C_LDSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LDSP    mstate           instr =
  let
    -- Instr fields: I-type
    (funct3, imm_at_12, rd, imm_at_6_2, op) = ifields_CI_type  instr
    offset = ((    shift  (bitSlice  imm_at_6_2  2  0)  6)
              .|. (shift  (bitSlice  imm_at_6_2  4  3)  3)
              .|. (shift  imm_at_12                     5))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C2)
                && (rd /= 0)
                && (funct3 == funct3_C_LDSP)
                && (rv == RV64))

    -- Semantics: same as LD
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_I_type  offset  rs1  funct3_LD  rd  opcode_LOAD
    is_C         = True
    (b, mstate1) = spec_LOAD  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LDSP constructed an illegal LD"
  in
    (is_legal, mstate2)

{- TODO: Uncomment when we do RV128
spec_C_LQSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LQSP    mstate           instr =
  let
    -- Instr fields: I-type
    (funct3, imm_at_12, rd, imm_at_6_2, op) = ifields_CI_type  instr
    offset = ((    shift  (bitSlice  imm_at_6_2  3  0)  6)
              .|. (shift  (bitSlice  imm_at_6_2  4  4)  4)
              .|. (shift  imm_at_12                     5))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C2)
                && (rd /= 0)
                && (funct3 == funct3_C_LQSP)
                && (rv == RV128))

    -- Semantics: same as LQ
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_I_type  offset  rs1  funct3_LQ  rd  opcode_LOAD
    is_C         = True
    (b, mstate1) = spec_LOAD  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LQSP constructed an illegal LQ"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'F' floating point
spec_C_FLWSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FLWSP    mstate           instr =
  let
    -- Instr fields: I-type
    (funct3, imm_at_12, rd, imm_at_6_2, op) = ifields_CI_type  instr
    offset = ((    shift  (bitSlice  imm_at_6_2  1  0)  6)
              .|. (shift  (bitSlice  imm_at_6_2  4  2)  2)
              .|. (shift  imm_at_12                     5))

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_FLWSP)
                && (rv == RV32))

    -- Semantics: same as FLW
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_I_type  offset  rs1  funct3_FLW  rd  opcode_STORE_FP
    is_C         = True
    (b, mstate1) = spec_STORE_FP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FLWSP constructed an illegal FLW"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'D' floating point
spec_C_FLDSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FLDSP    mstate           instr =
  let
    -- Instr fields: I-type
    (funct3, imm_at_12, rd, imm_at_6_2, op) = ifields_CI_type  instr
    offset = ((    shift  (bitSlice  imm_at_6_2  2  0)  6)
              .|. (shift  (bitSlice  imm_at_6_2  4  3)  3)
              .|. (shift  imm_at_12                     5))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_FLDSP))

    -- Semantics: same as FLD
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_I_type  offset  rs1  funct3_FLD  rd  opcode_STORE_FP
    is_C         = True
    (b, mstate1) = spec_STORE_FP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FLDSP constructed an illegal FLD"
  in
    (is_legal, mstate2)
-}

-- ================================================================
-- 'C' Extension Stack-Pointer-Based Stores

funct3_C_SWSP  = 0x6 :: InstrField    -- 3'b_110

funct3_C_SQSP  = 0x5 :: InstrField    -- 3'b_101     RV128
funct3_C_FSDSP = 0x5 :: InstrField    -- 3'b_101     RV32DC, RV64DC

funct3_C_SDSP  = 0x7 :: InstrField    -- 3'b_111     RV64 and RV128
funct3_C_FSWSP = 0x7 :: InstrField    -- 3'b_111     RV32FC

spec_C_SWSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SWSP    mstate           instr =
  let
    -- Instr fields: CSS-type
    (funct3, imm_at_12_7, rs2, op) = ifields_CSS_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_7  5  2)  2)
              .|. (shift  (bitSlice  imm_at_12_7  1  0)  6))

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_SWSP))

    -- Semantics: same as SW
    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero-extended
    instr32      = mkInstr_S_type  imm12  rs2  rs1  funct3_SW  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_STORE  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SWSP constructed an illegal SW"
  in
    (is_legal, mstate2)

spec_C_SDSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SDSP    mstate           instr =
  let
    -- Instr fields: CSS-type
    (funct3, imm_at_12_7, rs2, op) = ifields_CSS_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_7  5  3)  3))
              .|. (shift  (bitSlice  imm_at_12_7  2  0)  6)

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_SDSP)
                && (rv == RV64))

    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero-extended
    instr32      = mkInstr_S_type  imm12  rs2  rs1  funct3_SD  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_STORE  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SDSP constructed an illegal SD"
  in
    (is_legal, mstate2)

{- TODO: Uncomment when we do RV128
spec_C_SQSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SQSP    mstate           instr =
  let
    -- Instr fields: CSS-type
    (funct3, imm_at_12_7, rs2, op) = ifields_CSS_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_7  5  4)  4))
              .|. (shift  (bitSlice  imm_at_12_7  3  0)  6)

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_SQSP)
                && (rv == RV128))

    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero-extended
    instr32      = mkInstr_S_type  imm12  rs2  rs1  funct3_SQ  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_STORE  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SQSP constructed an illegal SQ"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'F' floating point
spec_C_FSWSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FSWSP    mstate           instr =
  let
    -- Instr fields: CSS-type
    (funct3, imm_at_12_7, rs2, op) = ifields_CSS_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_7  5  2)  2))
              .|. (shift  (bitSlice  imm_at_12_7  1  0)  6)

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_FSWSP)
                && (rv == RV32))

    -- Semantics: same as FSW
    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero-extended
    instr32      = mkInstr_S_type  imm12  rs2  rs1  funct3_FSW  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_STORE  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FSWSP constructed an illegal FSW"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'D' floating point
spec_C_FSDSP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FSDSP    mstate           instr =
  let
    -- Instr fields: CSS-type
    (funct3, imm_at_12_7, rs2, op) = ifields_CSS_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_7  5  3)  3))
              .|. (shift  (bitSlice  imm_at_12_7  2  0)  6)

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_FSDSP))

    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero-extended
    instr32      = mkInstr_S_type  imm12  rs2  rs1  funct3_FSD  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_LOAD  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FSDSP constructed an illegal FSD"
  in
    (is_legal, mstate2)
-}

-- ================================================================
-- 'C' Extension Register-Based Loads

funct3_C_LQ  = 0x1 :: InstrField    -- 3'b_001     RV128
funct3_C_FLD = 0x1 :: InstrField    -- 3'b_001     RV32DC, RV64DC

funct3_C_LW  = 0x2 :: InstrField    -- 3'b_010

funct3_C_LD  = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128
funct3_C_FLW = 0x3 :: InstrField    -- 3'b_011     RV32FC

spec_C_LW :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LW    mstate           instr =
  let
    -- Instr fields: CL-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rd', op) = ifields_CL_type  instr
    offset = ((    shift  imm_at_12_10                  3)
              .|. (shift  (bitSlice  imm_at_6_5  1  1)  2)
              .|. (shift  (bitSlice  imm_at_6_5  0  0)  6))

    -- Decode check
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_LW))

    -- Semantics: same as LW
    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero extended
    instr32      = mkInstr_I_type  imm12  rs1'  funct3_LW  rd'  opcode_LOAD
    is_C         = True
    (b, mstate1) = spec_LOAD  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LW constructed an illegal LW"
  in
    (is_legal, mstate2)

spec_C_LD :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LD    mstate           instr =
  let
    -- Instr fields: CL-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rd', op) = ifields_CL_type  instr
    offset = ((    shift  imm_at_12_10  3)
              .|. (shift  imm_at_6_5    6))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_LD)
                && (rv == RV64))

    -- Semantics: same as LD
    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero extended
    instr32      = mkInstr_I_type  offset  rs1'  funct3_LD  rd'  opcode_LOAD
    is_C         = True
    (b, mstate1) = spec_LOAD  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LD constructed an illegal LD"
  in
    (is_legal, mstate2)

{- TODO: Uncomment when we do RV128
spec_C_LQ :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LQ    mstate           instr =
  let
    -- Instr fields: CL-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rd', op) = ifields_CL_type  instr
    offset = ((    shift  imm_at_12_10  3)
              .|. (shift  (bitSlice  imm_at_6_5  2  2)  5)
              .|. (shift  (bitSlice  imm_at_6_5  1  1)  4)
              .|. (shift  (bitSlice  imm_at_6_5  0  0)  8))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_LQ)
                && (rv == RV64))

    -- Semantics: same as LQ
    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero extended
    instr32      = mkInstr_I_type  imm12  rs1'  funct3_LQ  rd'  opcode_LOAD
    is_C         = True
    (b, mstate1) = spec_LOAD  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LQ constructed an illegal LQ"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'F' floating point
spec_C_FLW :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FLW    mstate           instr =
  let
    -- Instr fields: CL-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rd', op) = ifields_CL_type  instr
    offset = ((    shift  imm_at_12_10                  3)
              .|. (shift  (bitSlice  imm_at_6_5  1  1)  2)
              .|. (shift  (bitSlice  imm_at_6_5  0  0)  6))

    -- Decode check
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_FLW))

    -- Semantics: same as FLW
    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero extended
    instr32      = mkInstr_I_type  imm12  rs1'  funct3_FLW  rd'  opcode_LOAD_FP
    is_C         = True
    (b, mstate1) = spec_LOAD_FP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FLW constructed an illegal FLW"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'D' floating point
spec_C_FLD :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FLD    mstate           instr =
  let
    -- Instr fields: CL-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rd', op) = ifields_CL_type  instr
    offset = ((    shift  imm_at_12_10  3)
              .|. (shift  imm_at_6_5    6))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_FLD)
                && (rv == RV64))

    -- Semantics: same as FLD
    rs1          = 2         -- GPR sp
    imm12        = offset    -- zero extended
    instr32      = mkInstr_I_type  imm12  rs1'  funct3_FLD  rd'  opcode_LOAD_FP
    is_C         = True
    (b, mstate1) = spec_LOAD_FP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FLD constructed an illegal FLD"
  in
    (is_legal, mstate2)
-}

-- ================================================================
-- 'C' Extension Register-Based Stores

funct3_C_FSD = 0x5 :: InstrField    -- 3'b_101     RV32DC, RV64DC
funct3_C_SQ  = 0x5 :: InstrField    -- 3'b_101     RV128

funct3_C_SW  = 0x6 :: InstrField    -- 3'b_110

funct3_C_SD  = 0x7 :: InstrField    -- 3'b_111     RV64 and RV128
funct3_C_FSW = 0x7 :: InstrField    -- 3'b_111     RV32FC

spec_C_SW :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SW    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    offset = ((    shift  imm_at_12_10                  3)
              .|. (shift  (bitSlice  imm_at_6_5  1  1)  2)
              .|. (shift  (bitSlice  imm_at_6_5  0  0)  6))

    -- Decode check
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_SW))

    -- Semantics: same as SW
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_S_type  offset  rs2'  rs1'  funct3_SW  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_STORE  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SW constructed an illegal SW"
  in
    (is_legal, mstate2)

spec_C_SD :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SD    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    offset = ((    shift  imm_at_12_10  3)
              .|. (shift  imm_at_6_5    6))

    -- Decode check
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_SD))

    -- Semantics: same as SD
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_S_type  offset  rs2'  rs1'  funct3_SD  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_STORE  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SD constructed an illegal SD"
  in
    (is_legal, mstate2)

{- TODO: Uncomment when we do RV128
spec_C_SQ :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SQ    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_10  2  2)  5)
              .|. (shift  (bitSlice  imm_at_12_10  1  1)  4)
              .|. (shift  (bitSlice  imm_at_12_10  0  0)  8)
              .|. (shift  imm_at_6_5    6))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_SQ)
                && (rv == RV128))
     

    -- Semantics: same as SQ
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_S_type  offset  rs2'  rs1'  funct3_SQ  opcode_STORE
    is_C         = True
    (b, mstate1) = spec_STORE  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SQ constructed an illegal SQ"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'F' floating point
spec_C_FSW :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FSW    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    offset = ((    shift  imm_at_12_10                  3)
              .|. (shift  (bitSlice  imm_at_6_5  1  1)  2)
              .|. (shift  (bitSlice  imm_at_6_5  0  0)  6))

    -- Decode check
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_FSW))

    -- Semantics: same as FSW
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_S_type  offset  rs2'  rs1'  funct3_SW  opcode_STORE_FP
    is_C         = True
    (b, mstate1) = spec_STORE_FP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FSW constructed an illegal FSW"
  in
    (is_legal, mstate2)
-}

{- TODO: Uncomment when we do 'D' floating point
spec_C_FSD :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_FSD    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    offset = ((    shift  imm_at_12_10  3)
              .|. (shift  imm_at_6_5    6))

    -- Decode check
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_FSD))

    -- Semantics: same as FSD
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_S_type  offset  rs2'  rs1'  funct3_SD  opcode_STORE_FP
    is_C         = True
    (b, mstate1) = spec_STORE_FP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_FSD constructed an illegal FSD"
  in
    (is_legal, mstate2)
-}

-- ================================================================
-- 'C' Extension Control Transfer
-- C.J, C.JAL, C.JR, C.JALR, C.BEQZ, C.BNEZ

funct3_C_JAL  = 0x1 :: InstrField    -- 3'b_001     RV32
funct3_C_J    = 0x5 :: InstrField    -- 3'b_101
funct3_C_BEQZ = 0x6 :: InstrField    -- 3'b_110
funct3_C_BNEZ = 0x7 :: InstrField    -- 3'b_111

funct4_C_JR   = 0x8 :: InstrField    -- 4'b_1000
funct4_C_JALR = 0x9 :: InstrField    -- 4'b_1001


spec_C_J :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_J    mstate           instr =
  let
    -- Instr fields: CJ-type
    (funct3, imm_at_12_2, op) = ifields_CJ_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_2  10  10)  11)
              .|. (shift  (bitSlice  imm_at_12_2   9   9)   4)
              .|. (shift  (bitSlice  imm_at_12_2   8   7)   8)
              .|. (shift  (bitSlice  imm_at_12_2   6   6)  10)
              .|. (shift  (bitSlice  imm_at_12_2   5   5)   6)
              .|. (shift  (bitSlice  imm_at_12_2   4   4)   7)
              .|. (shift  (bitSlice  imm_at_12_2   3   1)   1)
              .|. (shift  (bitSlice  imm_at_12_2   0   0)   5))

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_J))

    -- Semantics: same as JAL
    rd           = 0    -- GPR zero
    imm21        = sign_extend  12  21  offset
    instr32      = mkInstr_J_type  imm21  rd  opcode_JAL
    is_C         = True
    (b, mstate1) = spec_JAL  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_J constructed an illegal JAL"
  in
    (is_legal, mstate2)

spec_C_JAL :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_JAL    mstate           instr =
  let
    -- Instr fields: CJ-type
    (funct3, imm_at_12_2, op) = ifields_CJ_type  instr
    offset = ((    shift  (bitSlice  imm_at_12_2  10  10)  11)
              .|. (shift  (bitSlice  imm_at_12_2   9   9)   4)
              .|. (shift  (bitSlice  imm_at_12_2   8   7)   8)
              .|. (shift  (bitSlice  imm_at_12_2   6   6)  10)
              .|. (shift  (bitSlice  imm_at_12_2   5   5)   6)
              .|. (shift  (bitSlice  imm_at_12_2   4   4)   7)
              .|. (shift  (bitSlice  imm_at_12_2   3   1)   1)
              .|. (shift  (bitSlice  imm_at_12_2   0   0)   5))

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_JAL)
                && (rv == RV32));

    -- Semantics: same as JAL
    rd           = 1    -- GPR ra
    imm21        = sign_extend  12  21  offset
    instr32      = mkInstr_J_type  imm21  rd  opcode_JAL
    is_C         = True
    (b, mstate1) = spec_JAL  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_JAL constructed an illegal JAL"
  in
    (is_legal, mstate2)

spec_C_JR :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_JR    mstate           instr =
  let
    -- Instr fields: CR-type
    (funct4, rs1, rs2, op) = ifields_CR_type  instr

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct4 == funct4_C_JR)
                && (rs1 /= 0)
                && (rs2 == 0))

    -- Semantics: same as JALR
    rd           = 0    -- GPR zero
    imm12        = 0
    instr32      = mkInstr_I_type  imm12  rs1  funct3_JALR  rd   opcode_JALR
    is_C         = True
    (b, mstate1) = spec_JALR  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_JR constructed an illegal JALR"
  in
    (is_legal, mstate2)

spec_C_JALR :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_JALR    mstate           instr =
  let
    -- Instr fields: CR-type
    (funct4, rs1, rs2, op) = ifields_CR_type  instr

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct4 == funct4_C_JALR)
                && (rs1 /= 0)
                && (rs2 == 0))

    -- Semantics: same as JALR
    rd           = 1    -- GPR ra
    imm12        = 0
    instr32      = mkInstr_I_type  imm12  rs1  funct3_JALR  rd   opcode_JALR
    is_C         = True
    (b, mstate1) = spec_JALR  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_JALR constructed an illegal JALR"
  in
    (is_legal, mstate2)

spec_C_BEQZ :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_BEQZ    mstate           instr =
  let
    -- Instr fields: CB-type
    (funct3, imm_at_12_10, rs1', imm_at_6_2, op) = ifields_CB_type  instr
    offset = ((    shiftL  (bitSlice  imm_at_12_10  2  2)  8)
              .|. (shiftL  (bitSlice  imm_at_12_10  1  0)  3)
              .|. (shiftL  (bitSlice  imm_at_6_2    4  3)  6)
              .|. (shiftL  (bitSlice  imm_at_6_2    2  1)  1)
              .|. (shiftL  (bitSlice  imm_at_6_2    0  0)  5))

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_BEQZ))

    -- Semantics: same as BEQ
    rs2          = 0    -- GPR zero
    imm13        = sign_extend  9  13  offset
    instr32      = mkInstr_B_type  imm13  rs2  rs1'  funct3_BEQ  opcode_BRANCH
    is_C         = True
    (b, mstate1) = spec_BRANCH  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_BEQZ constructed an illegal BEQ"
  in
    (is_legal, mstate2)

spec_C_BNEZ :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_BNEZ    mstate           instr =
  let
    -- Instr fields: CB-type
    (funct3, imm_at_12_10, rs1', imm_at_6_2, op) = ifields_CB_type  instr
    offset = ((    shiftL  (bitSlice  imm_at_12_10  2  2)  8)
              .|. (shiftL  (bitSlice  imm_at_12_10  1  0)  3)
              .|. (shiftL  (bitSlice  imm_at_6_2    4  3)  6)
              .|. (shiftL  (bitSlice  imm_at_6_2    2  1)  1)
              .|. (shiftL  (bitSlice  imm_at_6_2    0  0)  5))

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_BNEZ))

    -- Semantics: same as BNE
    rs2          = 0    -- GPR zero
    imm13        = sign_extend  9  13  offset
    instr32      = mkInstr_B_type  imm13  rs2  rs1'  funct3_BNE  opcode_BRANCH
    is_C         = True
    (b, mstate1) = spec_BRANCH  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_BNEZ constructed an illegal BNEZ"
  in
    (is_legal, mstate2)

-- ================================================================
-- 'C' Extension Integer Constant-Generation

funct3_C_LI  = 0x2 :: InstrField    -- 3'b_010
funct3_C_LUI = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128

spec_C_LI :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LI    mstate           instr =
  let
    -- Instr fields: CI-type
    (funct3, imm_at_12, rd, imm_at_6_2, op) = ifields_CI_type  instr
    imm6  = ((shiftL  imm_at_12  5) .|. imm_at_6_2)
    imm12 = sign_extend  6  12  imm6

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_LI)
                && (rd /= 0))

    -- Semantics: same as ADDI
    rs1          = 0    -- GPR zero
    instr32      = mkInstr_I_type  imm12  rs1  funct3_ADDI  rd  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LI constructed an illegal ADDI"
  in
    (is_legal, mstate2)

spec_C_LUI :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_LUI    mstate           instr =
  let
    -- Instr fields: CI-type
    (funct3, imm_at_12, rd, imm_at_6_2, op) = ifields_CI_type  instr
    nzimm18 = ((    shiftL  imm_at_12   5)
               .|.          imm_at_6_2)
    imm20   = sign_extend  6  20  nzimm18

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_LUI)
                && (rd /= 0)
                && (rd /= 2)
                && (nzimm18 /= 0))

    -- Semantics: same as LUI
    instr32      = mkInstr_U_type  imm20  rd  opcode_LUI
    is_C         = True
    (b, mstate1) = spec_LUI  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_LUI constructed an illegal LUI"
  in
    (is_legal, mstate2)

-- ================================================================
-- 'C' Extension Integer Register-Immediate Operations

funct3_C_NOP      = 0x0 :: InstrField    -- 3'b_000
funct3_C_ADDI     = 0x0 :: InstrField    -- 3'b_000
funct3_C_ADDIW    = 0x1 :: InstrField    -- 3'b_001
funct3_C_ADDI16SP = 0x3 :: InstrField    -- 3'b_011
funct3_C_ADDI4SPN = 0x0 :: InstrField    -- 3'b_000
funct3_C_SLLI     = 0x0 :: InstrField    -- 3'b_000

funct3_C_SRLI     = 0x4 :: InstrField    -- 3'b_100
funct2_C_SRLI     = 0x0 :: InstrField    -- 2'b_00

funct3_C_SRAI     = 0x4 :: InstrField    -- 3'b_100
funct2_C_SRAI     = 0x1 :: InstrField    -- 2'b_01

funct3_C_ANDI     = 0x4 :: InstrField    -- 3'b_100
funct2_C_ANDI     = 0x2 :: InstrField    -- 2'b_10

spec_C_ADDI :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_ADDI    mstate           instr =
  let
    -- Instr fields: CI-type
    (funct3, imm_at_12, rd_rs1, imm_at_6_2, op) = ifields_CI_type  instr
    nzimm6 = ((shiftL  imm_at_12   5)  .|. imm_at_6_2)

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_ADDI)
                && (rd_rs1 /= 0)
                && (nzimm6 /= 0))

    -- Semantics: same as ADDI
    imm12  = sign_extend  6  12  nzimm6
    instr32      = mkInstr_I_type  imm12  rd_rs1  funct3_ADDI  rd_rs1  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ADDI constructed an illegal ADDI"
  in
    (is_legal, mstate2)

spec_C_NOP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_NOP    mstate           instr =
  let
    -- Instr fields: CI-type
    (funct3, imm_at_12, rd_rs1, imm_at_6_2, op) = ifields_CI_type  instr
    nzimm6 = ((shiftL  imm_at_12   5)  .|. imm_at_6_2)

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_NOP)
                && (rd_rs1 == 0)
                && (nzimm6 == 0))

    -- Semantics: same as ADDI x0, x0, 0 (no op)
    imm12        = 0
    instr32      = mkInstr_I_type  imm12  rd_rs1  funct3_ADDI  rd_rs1  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ADDI constructed an illegal ADDI"
  in
    (is_legal, mstate2)

spec_C_ADDIW :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_ADDIW    mstate           instr =
  let
    -- Instr fields: CI-type
    (funct3, imm_at_12, rd_rs1, imm_at_6_2, op) = ifields_CI_type  instr
    imm6   = ((shiftL  imm_at_12   5)  .|. imm_at_6_2)
    imm12  = sign_extend  6  12  imm6

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_ADDIW)
                && (rd_rs1 /= 0)
                && (rv == RV64))

    -- Semantics: same as ADDI
    instr32      = mkInstr_I_type  imm12  rd_rs1  funct3_ADDIW  rd_rs1  opcode_OP_IMM_32
    is_C         = True
    (b, mstate1) = spec_OP_IMM_32  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ADDIW constructed an illegal ADDIW"
  in
    (is_legal, mstate2)

spec_C_ADDI16SP :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_ADDI16SP    mstate           instr =
  let
    -- Instr fields: CI-type
    (funct3, imm_at_12, rd_rs1, imm_at_6_2, op) = ifields_CI_type  instr
    nzimm10 = ((    shiftL  imm_at_12                     9)
               .|. (shiftL  (bitSlice  imm_at_6_2  4  4)  4)
               .|. (shiftL  (bitSlice  imm_at_6_2  3  3)  6)
               .|. (shiftL  (bitSlice  imm_at_6_2  2  1)  7)
               .|. (shiftL  (bitSlice  imm_at_6_2  0  0)  5))

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_ADDI16SP)
                && (rd_rs1 == 2)    -- GPR sp
                && (nzimm10 /= 0))

    -- Semantics: same as ADDI
    imm12        = sign_extend  10  12  nzimm10
    instr32      = mkInstr_I_type  imm12  rd_rs1  funct3_ADDI  rd_rs1  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ADDI16SP constructed an illegal ADDI"
  in
    (is_legal, mstate2)

spec_C_ADDI4SPN :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_ADDI4SPN    mstate           instr =
  let
    -- Instr fields: CIW-type
    (funct3, imm_at_12_5, rd', op) = ifields_CIW_type  instr
    nzimm10 = ((    shiftL  (bitSlice  imm_at_12_5  7  6)  4)
               .|. (shiftL  (bitSlice  imm_at_12_5  5  2)  6)
               .|. (shiftL  (bitSlice  imm_at_12_5  1  1)  2)
               .|. (shiftL  (bitSlice  imm_at_12_5  0  0)  3))
    imm12   = nzimm10    -- zero-extended from 10b to 12b

    -- Decode check
    is_legal = ((op == opcode_C0)
                && (funct3 == funct3_C_ADDI4SPN)
                && (nzimm10 /= 0))

    -- Semantics: same as ADDI
    rs1          = 2    -- GPR sp
    instr32      = mkInstr_I_type  imm12  rs1  funct3_ADDI  rd'  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ADDI4SPN constructed an illegal ADDI"
  in
    (is_legal, mstate2)

spec_C_SLLI :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SLLI    mstate           instr =
  let
    -- Instr fields: CI-type
    (funct3, imm_at_12, rd_rs1, imm_at_6_2, op) = ifields_CI_type  instr
    shamt6 = ((shiftL  imm_at_12   5) .|. imm_at_6_2)

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C2)
                && (funct3 == funct3_C_SLLI)
                && (rd_rs1 /= 0)
                && (shamt6 /= 0)
                && (if (rv == RV32) then (imm_at_12 == 0) else True))

    -- Semantics: same as SLLI
    imm12        = if (rv == RV32) then
                     ((shiftL  msbs7_SLLI  5) .|. imm_at_6_2)
                   else
                     ((shiftL  msbs6_SLLI  6) .|. shamt6)
    instr32      = mkInstr_I_type  imm12  rd_rs1  funct3_SLLI  rd_rs1  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SLLI constructed an illegal SLLI"
  in
    (is_legal, mstate2)

spec_C_SRLI :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SRLI    mstate           instr =
  let
    -- Instr fields: CB-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_2, op) = ifields_CB_type  instr
    shamt6_5 = bitSlice  imm_at_12_10  2  2
    funct2   = bitSlice  imm_at_12_10  1  0

    shamt6   = ((shiftL  shamt6_5  5) .|. imm_at_6_2)

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_SRLI)
                && (funct2 == funct2_C_SRLI)
                && (shamt6 /= 0)
                && (rd_rs1' /= 0)
                && (if (rv == RV32) then (shamt6_5 == 0) else True))

    -- Semantics: same as SRLI
    imm12        = if (rv == RV32) then
                     ((shiftL  msbs7_SRLI  5) .|. imm_at_6_2)
                   else
                     ((shiftL  msbs6_SRLI  6) .|. shamt6)
    instr32      = mkInstr_I_type  imm12  rd_rs1'  funct3_SRLI  rd_rs1'  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SRLI constructed an illegal SRLI"
  in
    (is_legal, mstate2)

spec_C_SRAI :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SRAI    mstate           instr =
  let
    -- Instr fields: CB-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_2, op) = ifields_CB_type  instr
    shamt6_5 = bitSlice  imm_at_12_10  2  2
    funct2   = bitSlice  imm_at_12_10  1  0

    shamt6   = ((shiftL  shamt6_5  5) .|. imm_at_6_2)

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_SRAI)
                && (funct2 == funct2_C_SRAI)
                && (shamt6 /= 0)
                && (rd_rs1' /= 0)
                && (if (rv == RV32) then (shamt6_5 == 0) else True))

    -- Semantics: same as SRAI
    imm12        = if (rv == RV32) then
                     ((shiftL  msbs7_SRAI  5) .|. imm_at_6_2)
                   else
                     ((shiftL  msbs6_SRAI  6) .|. shamt6)
    instr32      = mkInstr_I_type  imm12  rd_rs1'  funct3_SRAI  rd_rs1'  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SRAI constructed an illegal SRAI"
  in
    (is_legal, mstate2)

spec_C_ANDI :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_ANDI    mstate           instr =
  let
    -- Instr fields: CB-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_2, op) = ifields_CB_type  instr
    imm6_5 = bitSlice  imm_at_12_10  2  2
    imm6   = ((shiftL  imm6_5  5) .|. imm_at_6_2)
    funct2 = bitSlice  imm_at_12_10  1  0

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct3 == funct3_C_ANDI)
                && (funct2 == funct2_C_ANDI))

    -- Semantics: same as ANDI
    imm12        = sign_extend  6  12  imm6
    instr32      = mkInstr_I_type  imm12  rd_rs1'  funct3_ANDI  rd_rs1'  opcode_OP_IMM
    is_C         = True
    (b, mstate1) = spec_OP_IMM  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ANDI constructed an illegal ANDI"
  in
    (is_legal, mstate2)

-- ================================================================
-- 'C' Extension Integer Register-Register Operations

funct4_C_MV       = 0x8 :: InstrField    -- 4'b_1000
funct4_C_ADD      = 0x9 :: InstrField    -- 4'b_1001

funct6_C_AND      = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_AND      = 0x3 :: InstrField    -- 2'b_11

funct6_C_OR       = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_OR       = 0x2 :: InstrField    -- 2'b_10

funct6_C_XOR      = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_XOR      = 0x1 :: InstrField    -- 2'b_01

funct6_C_SUB      = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_SUB      = 0x0 :: InstrField    -- 2'b_01

funct6_C_ADDW     = 0x27 :: InstrField   -- 6'b_100_1_11
funct2_C_ADDW     = 0x1 :: InstrField    -- 2'b_01

funct6_C_SUBW     = 0x27 :: InstrField   -- 6'b_100_1_11
funct2_C_SUBW     = 0x0 :: InstrField    -- 2'b_00

spec_C_MV :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_MV    mstate           instr =
  let
    -- Instr fields: CR-type
    (funct4, rd_rs1, rs2, op) = ifields_CR_type  instr

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct4 == funct4_C_MV)
                && (rd_rs1 /= 0)
                && (rs2 /= 0))

    -- Semantics: same as ADD
    rs1          = 0    -- GPR zero
    instr32      = mkInstr_R_type  funct7_ADD  rs2  rs1  funct3_ADD  rd_rs1  opcode_OP
    is_C         = True
    (b, mstate1) = spec_OP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_MV constructed an illegal ADD"
  in
    (is_legal, mstate2)

spec_C_ADD :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_ADD    mstate           instr =
  let
    -- Instr fields: CR-type
    (funct4, rd_rs1, rs2, op) = ifields_CR_type  instr

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct4 == funct4_C_ADD)
                && (rd_rs1 /= 0)
                && (rs2 /= 0))

    -- Semantics: same as ADD
    instr32      = mkInstr_R_type  funct7_ADD  rs2  rd_rs1  funct3_ADD  rd_rs1  opcode_OP
    is_C         = True
    (b, mstate1) = spec_OP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ADD constructed an illegal ADD"
  in
    (is_legal, mstate2)

spec_C_AND :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_AND    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    funct6 = ((shiftL  funct3  3) .|.  imm_at_12_10)
    funct2 = imm_at_6_5

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct6 == funct6_C_AND)
                && (funct2 == funct2_C_AND))

    -- Semantics: same as AND
    instr32      = mkInstr_R_type  funct7_AND  rs2'  rd_rs1'  funct3_AND  rd_rs1'  opcode_OP
    is_C         = True
    (b, mstate1) = spec_OP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_AND constructed an illegal AND"
  in
    (is_legal, mstate2)

spec_C_OR :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_OR    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    funct6 = ((shiftL  funct3  3) .|.  imm_at_12_10)
    funct2 = imm_at_6_5

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct6 == funct6_C_OR)
                && (funct2 == funct2_C_OR))

    -- Semantics: same as OR
    instr32      = mkInstr_R_type  funct7_OR  rs2'  rd_rs1'  funct3_OR  rd_rs1'  opcode_OP
    is_C         = True
    (b, mstate1) = spec_OP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_OR constructed an illegal OR"
  in
    (is_legal, mstate2)

spec_C_XOR :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_XOR    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    funct6 = ((shiftL  funct3  3) .|.  imm_at_12_10)
    funct2 = imm_at_6_5

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct6 == funct6_C_XOR)
                && (funct2 == funct2_C_XOR))

    -- Semantics: same as XOR
    instr32      = mkInstr_R_type  funct7_XOR  rs2'  rd_rs1'  funct3_XOR  rd_rs1'  opcode_OP
    is_C         = True
    (b, mstate1) = spec_OP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_XOR constructed an illegal XOR"
  in
    (is_legal, mstate2)

spec_C_SUB :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SUB    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    funct6 = ((shiftL  funct3  3) .|.  imm_at_12_10)
    funct2 = imm_at_6_5

    -- Decode check
    is_legal = ((op == opcode_C1)
                && (funct6 == funct6_C_SUB)
                && (funct2 == funct2_C_SUB))

    -- Semantics: same as SUB
    instr32      = mkInstr_R_type  funct7_SUB  rs2'  rd_rs1'  funct3_SUB  rd_rs1'  opcode_OP
    is_C         = True
    (b, mstate1) = spec_OP  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SUB constructed an illegal SUB"
  in
    (is_legal, mstate2)

spec_C_ADDW :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_ADDW    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    funct6 = ((shiftL  funct3  3) .|.  imm_at_12_10)
    funct2 = imm_at_6_5

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C1)
                && (funct6 == funct6_C_ADDW)
                && (funct2 == funct2_C_ADDW)
                && (rv == RV64))

    -- Semantics: same as ADDW
    instr32      = mkInstr_R_type  funct7_ADDW  rs2'  rd_rs1'  funct3_ADDW  rd_rs1'  opcode_OP_32
    is_C         = True
    (b, mstate1) = spec_OP_32  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_ADDW constructed an illegal ADDW"
  in
    (is_legal, mstate2)

spec_C_SUBW :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_SUBW    mstate           instr =
  let
    -- Instr fields: CS-type
    (funct3, imm_at_12_10, rd_rs1', imm_at_6_5, rs2', op) = ifields_CS_type  instr
    funct6 = ((shiftL  funct3  3) .|.  imm_at_12_10)
    funct2 = imm_at_6_5

    -- Decode check
    rv       = mstate_rv_read    mstate
    is_legal = ((op == opcode_C1)
                && (funct6 == funct6_C_SUBW)
                && (funct2 == funct2_C_SUBW)
                && (rv == RV64))

    -- Semantics: same as SUBW
    instr32      = mkInstr_R_type  funct7_SUBW  rs2'  rd_rs1'  funct3_SUBW  rd_rs1'  opcode_OP_32
    is_C         = True
    (b, mstate1) = spec_OP_32  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_SUBW constructed an illegal SUBW"
  in
    (is_legal, mstate2)

-- ================================================================
-- 'C' Extension EBREAK

funct4_C_EBREAK = 0x9 :: InstrField    -- 4'b_1001

spec_C_EBREAK :: Machine_State -> Instr_C -> (Bool, Machine_State)
spec_C_EBREAK    mstate           instr =
  let
    -- Instr fields: CR-type
    (funct4, rd_rs1, rs2, op) = ifields_CR_type  instr

    -- Decode check
    is_legal = ((op == opcode_C2)
                && (funct4 == funct4_C_EBREAK)
                && (rd_rs1 == 0)
                && (rs2 == 0))

    -- Semantics: same as EBREAK
    imm12        = funct12_EBREAK
    instr32      = mkInstr_I_type  imm12  rd_rs1  funct3_PRIV  rd_rs1   opcode_SYSTEM
    is_C         = True
    (b, mstate1) = spec_SYSTEM_EBREAK  mstate  instr32  is_C
    mstate2      = if b then mstate1
                   else error "INTERNAL ERROR: spec_C_EBREAK constructed an illegal EBREAK"
  in
    (is_legal, mstate2)

-- ================================================================

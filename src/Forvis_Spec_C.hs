-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
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

import Forvis_Spec_I         -- Base RV32/RV64 instr set ('C' is defined in terms of Base)
import Forvis_Spec_I64       -- Base RV64 instr set      ('C' is defined in terms of Base)

#ifdef FLOAT
import Forvis_Spec_F           -- Extension 'F' (single-precision floating point)
import Forvis_Spec_D           -- Extension 'D' (double-precision floating point)
#endif

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- 'C' Extension ("Compressed") major opcodes ('quadrants' 0, 1 and 2)

-- NOTE: the presentation order here (data type, decode, despatch, and
-- per-instruction semantics) follows the instruction listing for RVC
-- in Tables 16.5 (quadrant 0), 16.6 (quadrant 1) and 16.7 (quadrant
-- 2)

-- ================================================================
-- 'C' Extension ("Compressed") major opcodes ('quadrants' 0, 1 and 2)

opcode_C0 = 0x0 :: InstrField    -- 2'b00
opcode_C1 = 0x1 :: InstrField    -- 2'b01
opcode_C2 = 0x2 :: InstrField    -- 2'b10

funct3_C_LWSP     = 0x2 :: InstrField    -- 3'b_010
funct3_C_LDSP     = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128
funct3_C_LQSP     = 0x1 :: InstrField    -- 3'b_001     RV128

#ifdef FLOAT
funct3_C_FLWSP    = 0x3 :: InstrField    -- 3'b_011     RV32FC
funct3_C_FLDSP    = 0x1 :: InstrField    -- 3'b_001     RV32DC, RV64DC
#endif

funct3_C_SWSP     = 0x6 :: InstrField    -- 3'b_110
funct3_C_SQSP     = 0x5 :: InstrField    -- 3'b_101     RV128
funct3_C_FSDSP    = 0x5 :: InstrField    -- 3'b_101     RV32DC, RV64DC

funct3_C_SDSP     = 0x7 :: InstrField    -- 3'b_111     RV64 and RV128
funct3_C_FSWSP    = 0x7 :: InstrField    -- 3'b_111     RV32FC

funct3_C_LQ       = 0x1 :: InstrField    -- 3'b_001     RV128
funct3_C_FLD      = 0x1 :: InstrField    -- 3'b_001     RV32DC, RV64DC
funct3_C_LW       = 0x2 :: InstrField    -- 3'b_010
funct3_C_LD       = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128
funct3_C_FLW      = 0x3 :: InstrField    -- 3'b_011     RV32FC

funct3_C_FSD      = 0x5 :: InstrField    -- 3'b_101     RV32DC, RV64DC
funct3_C_SQ       = 0x5 :: InstrField    -- 3'b_101     RV128
funct3_C_SW       = 0x6 :: InstrField    -- 3'b_110
funct3_C_SD       = 0x7 :: InstrField    -- 3'b_111     RV64 and RV128
funct3_C_FSW      = 0x7 :: InstrField    -- 3'b_111     RV32FC

funct3_C_JAL      = 0x1 :: InstrField    -- 3'b_001     RV32
funct3_C_J        = 0x5 :: InstrField    -- 3'b_101
funct3_C_BEQZ     = 0x6 :: InstrField    -- 3'b_110
funct3_C_BNEZ     = 0x7 :: InstrField    -- 3'b_111

funct4_C_JR       = 0x8 :: InstrField    -- 4'b_1000
funct4_C_JALR     = 0x9 :: InstrField    -- 4'b_1001

funct3_C_LI       = 0x2 :: InstrField    -- 3'b_010
funct3_C_LUI      = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128

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

funct4_C_EBREAK   = 0x9 :: InstrField    -- 4'b_1001

-- ================================================================
-- Data structure for instructions in 'C' (compressed instruction set)

data Instr_C = C_ADDI4SPN  GPR_Addr  InstrField              -- rd       nzuimm10

             -- Quadrant 0 (op = 2'b00)
#ifdef FLOAT
             | C_FLD       GPR_Addr  GPR_Addr  InstrField    -- rd      rs1  uimm8
#endif
          -- | C_LQ        GPR_Addr  GPR_Addr  InstrField    -- rd      rs1  uimm9
             | C_LW        GPR_Addr  GPR_Addr  InstrField    -- rd      rs1  uimm7
#ifdef FLOAT
             | C_FLW       GPR_Addr  GPR_Addr  InstrField    -- rd      rs1  uimm7
#endif
             | C_LD        GPR_Addr  GPR_Addr  InstrField    -- rd      rs1  uimm8

#ifdef FLOAT
             | C_FSD       GPR_Addr  GPR_Addr  InstrField    -- rs1     rs2  uimm8
#endif
          -- | C_SQ        GPR_Addr  GPR_Addr  InstrField    -- rs1     rs2  uimm9
             | C_SW        GPR_Addr  GPR_Addr  InstrField    -- rs1     rs2  uimm7
#ifdef FLOAT
             | C_FSW       GPR_Addr  GPR_Addr  InstrField    -- rs1     rs2  uimm7
#endif
             | C_SD        GPR_Addr  GPR_Addr  InstrField    -- rs1     rs2  uimm8

             -- Quadrant 1 (op = 2'b01)
             | C_NOP
             | C_ADDI      GPR_Addr            InstrField    -- rd_rs1       nzimm6

             | C_JAL                           InstrField    --              imm12
             | C_ADDIW     GPR_Addr            InstrField    -- rd_rs1       imm6

             | C_LI        GPR_Addr            InstrField    -- rd           imm6

             | C_ADDI16SP                      InstrField    --              nzimm10
             | C_LUI       GPR_Addr            InstrField    -- rd           imm6

             | C_SRLI      GPR_Addr            InstrField    -- rd_rs1       shamt6
             | C_SRAI      GPR_Addr            InstrField    -- rd_rs1       nzuimm6
             | C_ANDI      GPR_Addr            InstrField    -- rd_rs1       imm6
             | C_SUB       GPR_Addr  GPR_Addr                -- rd_rs1  rs2
             | C_XOR       GPR_Addr  GPR_Addr                -- rd_rs1  rs2
             | C_OR        GPR_Addr  GPR_Addr                -- rd_rs1  rs2
             | C_AND       GPR_Addr  GPR_Addr                -- rd_rs1  rs2
             | C_SUBW      GPR_Addr  GPR_Addr                -- rd_rs1  rs2
             | C_ADDW      GPR_Addr  GPR_Addr                -- rd_rs1  rs2

             | C_J                             InstrField    --              imm12

             | C_BEQZ      GPR_Addr            InstrField    -- rs1          imm9
             | C_BNEZ      GPR_Addr            InstrField    -- rs1          imm9

             -- Quadrant 2 (op = 2'b10)
             | C_SLLI      GPR_Addr            InstrField    -- rd_rs1       shamt6

#ifdef FLOAT
             | C_FLDSP     GPR_Addr            InstrField    -- rd           uimm9
#endif
          -- | C_LQSP      GPR_Addr            InstrField    -- rd           uimm10
             | C_LWSP      GPR_Addr            InstrField    -- rd           uimm8
#ifdef FLOAT
             | C_FLWSP     GPR_Addr            InstrField    -- rd           uimm8
#endif
             | C_LDSP      GPR_Addr            InstrField    -- rd           uimm9

             | C_JR        GPR_Addr                          -- rs1
             | C_MV        GPR_Addr  GPR_Addr                -- rd      rs1
             | C_EBREAK
             | C_JALR      GPR_Addr                          -- rs1
             | C_ADD       GPR_Addr  GPR_Addr                -- rd_rs1  rs2

#ifdef FLOAT
             | C_FSDSP     GPR_Addr            InstrField    -- rs2          uimm9
#endif
          -- | C_SQSP      GPR_Addr            InstrField    -- rs2          uimm10
             | C_SWSP      GPR_Addr            InstrField    -- rs2          uimm8
#ifdef FLOAT
             | C_FSWSP     GPR_Addr            InstrField    -- rs2          uimm8
#endif
             | C_SDSP      GPR_Addr            InstrField    -- rs2          uimm9

  deriving (Eq, Show)

-- ================================================================
-- Fields of the different Instr_16b formats

instr_16b_fields_CI :: Instr_16b -> (InstrField, InstrField, InstrField, InstrField, InstrField)
instr_16b_fields_CI  instr_16b =
  let
    funct3       = bitSlice  instr_16b  15  13
    imm_12       = bitSlice  instr_16b  12  12
    rd_rs1       = bitSlice  instr_16b  11   7
    imm_6_2      = bitSlice  instr_16b   6   2
    op           = bitSlice  instr_16b   1   0
  in
    (funct3, imm_12, rd_rs1, imm_6_2, op)

instr_16b_fields_CB :: Instr_16b -> (InstrField, InstrField, InstrField, InstrField, InstrField)
instr_16b_fields_CB  instr_16b =
  let
    funct3       = bitSlice  instr_16b  15  13
    offset_12_10 = bitSlice  instr_16b  12  10
    rs1_prime    = bitSlice  instr_16b   9   7
    offset_6_2   = bitSlice  instr_16b   6   2
    op           = bitSlice  instr_16b   1   0
  in
    (funct3, offset_12_10, rs1_prime, offset_6_2, op)

-- ================================================================
-- Decode from 32b representation to Instr_M data structure

decode_C :: RV -> Integer -> Instr_16b -> Maybe Instr_C
decode_C    rv    misa       instr_16b =
  let
    misa_f       = (misa_flag  misa  'F')
    misa_d       = (misa_flag  misa  'D')

    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    op           = bitSlice  instr_16b   1   0
    funct3       = bitSlice  instr_16b  15  13

    -- CR format
    funct4_CR    = bitSlice  instr_16b  15  12
    rd_rs1_CR    = bitSlice  instr_16b  11   7
    rs2_CR       = bitSlice  instr_16b   6   2

    -- CI format
    rd_rs1_CI    = bitSlice  instr_16b  11   7

    imm6_CI      = ((    shiftL  (bitSlice  instr_16b  12  12)  5)
                    .|.          (bitSlice  instr_16b   6   2))

    imm8_CI      = ((    shiftL  (bitSlice  instr_16b  12  12)  5)
                    .|. (shiftL  (bitSlice  instr_16b   6   4)  2)
                    .|. (shiftL  (bitSlice  instr_16b   3   2)  6))

    imm9_CI      = ((    shiftL  (bitSlice  instr_16b  12  12)  5)
                    .|. (shiftL  (bitSlice  instr_16b   6   5)  3)
                    .|. (shiftL  (bitSlice  instr_16b   4   2)  6))

    imm10_CI     = ((    shiftL  (bitSlice  instr_16b  12  12)  5)
                    .|. (shiftL  (bitSlice  instr_16b   6   6)  4)
                    .|. (shiftL  (bitSlice  instr_16b   5   2)  6))

    nzimm10_CI   = ((    shiftL  (bitSlice  instr_16b  12  12)  9)
                    .|. (shiftL  (bitSlice  instr_16b   6   6)  4)
                    .|. (shiftL  (bitSlice  instr_16b   5   5)  6)
                    .|. (shiftL  (bitSlice  instr_16b   4   3)  7)
                    .|. (shiftL  (bitSlice  instr_16b   2   2)  5))

    -- CSS format
    rs2_CSS      = bitSlice  instr_16b   6   2

    uimm8_CSS    = ((    shiftL  (bitSlice  instr_16b  12   9)  2)
                    .|. (shiftL  (bitSlice  instr_16b   8   7)  6))

    uimm9_CSS    = ((    shiftL  (bitSlice  instr_16b  12  10)  3)
                    .|. (shiftL  (bitSlice  instr_16b   9   7)  6))

    uimm10_CSS   = ((    shiftL  (bitSlice  instr_16b  12  11)  4)
                    .|. (shiftL  (bitSlice  instr_16b  10   7)  6))

    -- CIW format
    rd_CIW       = (0x8 .|. (bitSlice  instr_16b   4   2))
    nzuimm10_CIW = ((    shiftL  (bitSlice  instr_16b  12  11)  4)
                    .|. (shiftL  (bitSlice  instr_16b  10   7)  6)
                    .|. (shiftL  (bitSlice  instr_16b   6   6)  2)
                    .|. (shiftL  (bitSlice  instr_16b   5   5)  3))

    -- CL format
    rd_CL        = (0x8 .|. (bitSlice  instr_16b   4   2))
    rs1_CL       = (0x8 .|. (bitSlice  instr_16b   9   7))
    uimm7_CL     = ((    shiftL  (bitSlice  instr_16b  12  10)  3)
                    .|. (shiftL  (bitSlice  instr_16b   6   6)  2)
                    .|. (shiftL  (bitSlice  instr_16b   5   5)  6))
    uimm8_CL     = ((    shiftL  (bitSlice  instr_16b  12  10)  3)
                    .|. (shiftL  (bitSlice  instr_16b   6   5)  6))
    uimm9_CL     = ((    shiftL  (bitSlice  instr_16b  12  11)  3)
                    .|. (shiftL  (bitSlice  instr_16b  10  10)  8)
                    .|. (shiftL  (bitSlice  instr_16b   6   5)  6))

    -- CS format
    rs2_CS       = (0x8 .|. (bitSlice  instr_16b   4   2))
    rs1_CS       = (0x8 .|. (bitSlice  instr_16b   9   7))
    uimm7_CS     = ((    shiftL  (bitSlice  instr_16b  12  10)  3)
                    .|. (shiftL  (bitSlice  instr_16b   6   6)  2)
                    .|. (shiftL  (bitSlice  instr_16b   5   5)  6))
    uimm8_CS     = ((    shiftL  (bitSlice  instr_16b  12  10)  3)
                    .|. (shiftL  (bitSlice  instr_16b   6   5)  6))
    uimm9_CS     = ((    shiftL  (bitSlice  instr_16b  12  11)  4)
                    .|. (shiftL  (bitSlice  instr_16b  10  10)  8)
                    .|. (shiftL  (bitSlice  instr_16b   6   5)  6))

    -- CA format
    funct6_CA    = bitSlice  instr_16b  15  10
    rd_rs1_CA    = (0x8 .|. (bitSlice  instr_16b   9   7))
    funct2_CA    = bitSlice  instr_16b   6   5
    rs2_CA       = (0x8 .|. (bitSlice  instr_16b   4   2))

    -- CB format
    rd_rs1_CB       = (0x8 .|. (bitSlice  instr_16b   9   7))

    imm6_CB         = ((    shiftL  (bitSlice  instr_16b  12  12) 5)
                       .|. (shiftL  (bitSlice  instr_16b   6   2) 0))

    imm9_CB         = ((    shiftL  (bitSlice  instr_16b  12  12) 8)
                       .|. (shiftL  (bitSlice  instr_16b  11  10) 3)
                       .|. (shiftL  (bitSlice  instr_16b   6   5) 6)
                       .|. (shiftL  (bitSlice  instr_16b   4   3) 1)
                       .|. (shiftL  (bitSlice  instr_16b   2   2) 5))

    -- CJ format
    imm12_CJ        = ((    shiftL  (bitSlice  instr_16b  12  12) 11)
                       .|. (shiftL  (bitSlice  instr_16b  11  11)  4)
                       .|. (shiftL  (bitSlice  instr_16b  10   9)  8)
                       .|. (shiftL  (bitSlice  instr_16b   8   8) 10)
                       .|. (shiftL  (bitSlice  instr_16b   7   7)  6)
                       .|. (shiftL  (bitSlice  instr_16b   6   6)  7)
                       .|. (shiftL  (bitSlice  instr_16b   5   3)  1)
                       .|. (shiftL  (bitSlice  instr_16b   2   2)  5))

    instr_C
      -- Quadrant 0
      | op==opcode_C0, funct3==funct3_C_ADDI4SPN, nzuimm10_CIW/=0             = Just (C_ADDI4SPN  rd_CIW  nzuimm10_CIW)

#ifdef FLOAT
      | op==opcode_C0, funct3==funct3_C_FLD, ((rv==RV32)||(rv==RV64)), misa_d = Just (C_FLD       rd_CL   rs1_CL  uimm8_CL)
#endif
   -- | op==opcode_C0, funct3==funct3_C_LQ,       rv==RV128                   = Just (C_LQ        rd_CL   rs1_CL  uimm9_CL)
      | op==opcode_C0, funct3==funct3_C_LW                                    = Just (C_LW        rd_CL   rs1_CL  uimm7_CL)
#ifdef FLOAT
      | op==opcode_C0, funct3==funct3_C_FLW,      rv==RV32,  misa_f           = Just (C_FLW       rd_CL   rs1_CL  uimm7_CL)
#endif
      | op==opcode_C0, funct3==funct3_C_LD,       rv/=RV32                    = Just (C_LD        rd_CL   rs1_CL  uimm8_CL)

#ifdef FLOAT
      | op==opcode_C0, funct3==funct3_C_FSD, ((rv==RV32)||(rv==RV64)), misa_d = Just (C_FSD       rs1_CS  rs2_CS  uimm8_CS)
#endif
   -- | op==opcode_C0, funct3==funct3_C_SQ,       rv==RV128                   = Just (C_SQ        rs1_CS  rs2_CS  uimm9_CS)
      | op==opcode_C0, funct3==funct3_C_SW                                    = Just (C_SW        rs1_CS  rs2_CS  uimm7_CS)
#ifdef FLOAT
      | op==opcode_C0, funct3==funct3_C_FSW,      rv==RV32,  misa_f           = Just (C_FSW       rs1_CS  rs2_CS  uimm7_CS)
#endif
      | op==opcode_C0, funct3==funct3_C_SD,       rv/=RV32                    = Just (C_SD        rs1_CS  rs2_CS  uimm8_CS)

      -- Quadrant 1
      | op==opcode_C1, funct3==funct3_C_NOP,      rd_rs1_CI==0, imm6_CI==0    = Just (C_NOP)
      | op==opcode_C1, funct3==funct3_C_ADDI,     rd_rs1_CI/=0, imm6_CI/=0    = Just (C_ADDI      rd_rs1_CI       imm6_CI)

      | op==opcode_C1, funct3==funct3_C_JAL,                    rv==RV32      = Just (C_JAL                       imm12_CJ)
      | op==opcode_C1, funct3==funct3_C_ADDIW,    rd_rs1_CI/=0, rv/=RV32      = Just (C_ADDIW     rd_rs1_CI       imm6_CI)

      | op==opcode_C1, funct3==funct3_C_LI,       rd_rs1_CI/=0                = Just (C_LI        rd_rs1_CI       imm6_CI)

      | op==opcode_C1, funct3==funct3_C_ADDI16SP, rd_rs1_CI==2, nzimm10_CI/=0 = Just (C_ADDI16SP                  nzimm10_CI)
      | op==opcode_C1, funct3==funct3_C_LUI, rd_rs1_CI/=0, rd_rs1_CI/=2, imm6_CI/=0 = Just (C_LUI   rd_rs1_CI   imm6_CI)

      | is_C_SRLI  rv  instr_16b                                                    = Just (C_SRLI  rd_rs1_CB   imm6_CB)
      | is_C_SRAI  rv  instr_16b                                                    = Just (C_SRAI  rd_rs1_CB   imm6_CB)
      | is_C_ANDI  rv  instr_16b                                                    = Just (C_ANDI  rd_rs1_CB   imm6_CB)

      | op==opcode_C1, funct6_CA==funct6_C_SUB,  funct2_CA==funct2_C_SUB            = Just (C_SUB  rd_rs1_CA  rs2_CA)
      | op==opcode_C1, funct6_CA==funct6_C_XOR,  funct2_CA==funct2_C_XOR            = Just (C_XOR  rd_rs1_CA  rs2_CA)
      | op==opcode_C1, funct6_CA==funct6_C_OR,   funct2_CA==funct2_C_OR             = Just (C_OR   rd_rs1_CA  rs2_CA)
      | op==opcode_C1, funct6_CA==funct6_C_AND,  funct2_CA==funct2_C_AND            = Just (C_AND  rd_rs1_CA  rs2_CA)
      | op==opcode_C1, funct6_CA==funct6_C_SUBW, funct2_CA==funct2_C_SUBW, rv/=RV32 = Just (C_SUBW  rd_rs1_CA  rs2_CA)
      | op==opcode_C1, funct6_CA==funct6_C_ADDW, funct2_CA==funct2_C_ADDW, rv/=RV32 = Just (C_ADDW  rd_rs1_CA  rs2_CA)

      | op==opcode_C1, funct3==funct3_C_J                                           = Just (C_J                   imm12_CJ)

      | op==opcode_C1, funct3==funct3_C_BEQZ                                        = Just (C_BEQZ  rd_rs1_CB  imm9_CB)
      | op==opcode_C1, funct3==funct3_C_BNEZ                                        = Just (C_BNEZ  rd_rs1_CB  imm9_CB)

      -- Quadrant 2

      | is_C_SLLI  rv  instr_16b                                                    = Just (C_SLLI  rd_rs1_CI   imm6_CI)

#ifdef FLOAT
      | op==opcode_C2, funct3==funct3_C_FLDSP, ((rv==RV32)||(rv==RV64))             = Just (C_FLDSP  rd_rs1_CI  imm9_CI)
#endif
   -- | op==opcode_C2, funct3==funct3_C_LQSP, rd_rs1_CI/=0, rv/=RV32, rv/=RV64      = Just (C_LQSP   rd_rs1_CI  imm10_CI)
      | op==opcode_C2, funct3==funct3_C_LWSP, rd_rs1_CI/=0                          = Just (C_LWSP   rd_rs1_CI  imm8_CI)
#ifdef FLOAT
      | op==opcode_C2, funct3==funct3_C_FLWSP, rv==RV32                             = Just (C_FLWSP  rd_rs1_CI  imm8_CI)
#endif
      | op==opcode_C2, funct3==funct3_C_LDSP, rd_rs1_CI/=0, rv/=RV32                = Just (C_LDSP   rd_rs1_CI  imm9_CI)

      | op==opcode_C2, funct4_CR==funct4_C_JR,     rd_rs1_CR/=0, rs2_CR==0          = Just (C_JR     rd_rs1_CR)
      | op==opcode_C2, funct4_CR==funct4_C_MV,     rd_rs1_CR/=0, rs2_CR/=0          = Just (C_MV     rd_rs1_CR  rs2_CR)
      | op==opcode_C2, funct4_CR==funct4_C_EBREAK, rd_rs1_CR==0, rs2_CR==0          = Just (C_EBREAK)
      | op==opcode_C2, funct4_CR==funct4_C_JALR,   rd_rs1_CR/=0, rs2_CR==0          = Just (C_JALR   rd_rs1_CR)
      | op==opcode_C2, funct4_CR==funct4_C_ADD,    rd_rs1_CR/=0, rs2_CR/=0          = Just (C_ADD    rd_rs1_CR  rs2_CR)

#ifdef FLOAT
      | op==opcode_C2, funct3==funct3_C_FSDSP, ((rv==RV32)||(rv==RV64))             = Just (C_FSDSP  rs2_CSS    uimm9_CSS)
#endif
   -- | op==opcode_C2, funct3==funct3_C_SQSP,  rv/=RV32, rv/=RV64                   = Just (C_SQSP   rs2_CSS    uimm10_CSS)
      | op==opcode_C2, funct3==funct3_C_SWSP                                        = Just (C_SWSP   rs2_CSS    uimm8_CSS)
#ifdef FLOAT
      | op==opcode_C2, funct3==funct3_C_FSWSP, rv==RV32                             = Just (C_FSWSP  rs2_CSS    uimm8_CSS)
#endif
      | op==opcode_C2, funct3==funct3_C_SDSP,  rv/=RV32                             = Just (C_SDSP   rs2_CSS    uimm9_CSS)

      | True = Nothing
  in
    instr_C

-- ================================================================
-- Execution of Instr_C

type Spec_Instr_C = Instr_C -> Machine_State -> Machine_State
--                  instr_C    mstate           mstate'

exec_instr_C :: Spec_Instr_C
exec_instr_C  instr_C  mstate =
  case instr_C of
    -- Quadrant 0
    C_ADDI4SPN  rd           nzuimm10 -> exec_C_ADDI4SPN  rd           nzuimm10  mstate
#ifdef FLOAT
    C_FLD       rd      rs1  uimm8    -> exec_C_FLD       rd      rs1  uimm8     mstate
#endif
 -- C_LQ        rd      rs1  uimm9    -> exec_C_LQ        rd      rs1  uimm9     mstate
    C_LW        rd      rs1  uimm7    -> exec_C_LW        rd      rs1  uimm7     mstate
#ifdef FLOAT
    C_FLW       rd      rs1  uimm7    -> exec_C_FLW       rd      rs1  uimm7     mstate
#endif
    C_LD        rd      rs1  uimm8    -> exec_C_LD        rd      rs1  uimm8     mstate

#ifdef FLOAT
    C_FSD       rs1     rs2  uimm8    -> exec_C_FSD       rs1     rs2  uimm8     mstate
#endif
 -- C_SQ        rs1     rs2  uimm9    -> exec_C_SQ        rs1     rs2  uimm9     mstate
    C_SW        rs1     rs2  uimm7    -> exec_C_SW        rs1     rs2  uimm7     mstate
#ifdef FLOAT
    C_FSW       rs1     rs2  uimm7    -> exec_C_FSW       rs1     rs2  uimm7     mstate
#endif
    C_SD        rs1     rs2  uimm8    -> exec_C_SW        rs1     rs2  uimm8     mstate

    -- Quadrant 1
    C_NOP                             -> exec_C_NOP                              mstate
    C_ADDI      rd_rs1       imm6     -> exec_C_ADDI      rd_rs1       imm6      mstate
    C_JAL                    imm12    -> exec_C_JAL                    imm12     mstate
    C_ADDIW     rd_rs1       imm6     -> exec_C_ADDIW     rd_rs1       imm6      mstate
    C_LI        rd_rs1       imm6     -> exec_C_LI        rd_rs1       imm6      mstate
    C_ADDI16SP               nzimm10  -> exec_C_ADDI16SP               nzimm10   mstate
    C_LUI       rd_rs1       imm6     -> exec_C_LUI       rd_rs1       imm6      mstate

    C_SRLI      rd_rs1       imm6     -> exec_C_SRLI      rd_rs1       imm6      mstate
    C_SRAI      rd_rs1       imm6     -> exec_C_SRAI      rd_rs1       imm6      mstate
    C_ANDI      rd_rs1       imm6     -> exec_C_ANDI      rd_rs1       imm6      mstate

    C_SUB       rd_rs1  rs2           -> exec_C_SUB       rd_rs1  rs2            mstate
    C_XOR       rd_rs1  rs2           -> exec_C_XOR       rd_rs1  rs2            mstate

    C_OR        rd_rs1  rs2           -> exec_C_OR        rd_rs1  rs2            mstate
    C_AND       rd_rs1  rs2           -> exec_C_AND       rd_rs1  rs2            mstate
    C_SUBW      rd_rs1  rs2           -> exec_C_SUBW      rd_rs1  rs2            mstate
    C_ADDW      rd_rs1  rs2           -> exec_C_ADDW      rd_rs1  rs2            mstate

    C_J         imm12                 -> exec_C_J                      imm12     mstate

    C_BEQZ      rs1          imm9     -> exec_C_BEQZ      rs1          imm9      mstate
    C_BNEZ      rs1          imm9     -> exec_C_BNEZ      rs1          imm9      mstate

    -- Quadrant 2
    C_SLLI      rd_rs1       imm6     -> exec_C_SLLI      rd_rs1       imm6      mstate

#ifdef FLOAT
    C_FLDSP     rd_rs1       uimm9    -> exec_C_FLDSP     rd_rs1       uimm9     mstate
#endif
 -- C_LQSP      rd_rs1       uimm10   -> exec_C_LQSP      rd_rs1       uimm10    mstate
    C_LWSP      rd_rs1       uimm8    -> exec_C_LWSP      rd_rs1       uimm8     mstate
#ifdef FLOAT
    C_FLWSP     rd_rs1       uimm8    -> exec_C_FLWSP     rd_rs1       uimm8     mstate
#endif
    C_LDSP      rd_rs1       uimm9    -> exec_C_LDSP      rd_rs1       uimm9     mstate

    C_JR        rs1                   -> exec_C_JR        rs1                    mstate
    C_MV        rd      rs2           -> exec_C_MV        rd    rs2              mstate
    C_EBREAK                          -> exec_C_EBREAK                           mstate
    C_JALR      rs1                   -> exec_C_JALR      rs1                    mstate
    C_ADD       rd      rs2           -> exec_C_ADD       rd    rs2              mstate

#ifdef FLOAT
    C_FSDSP             rs2  uimm9    -> exec_C_FSDSP           rs2    uimm9     mstate
#endif
 -- C_SQSP              rs2  uimm10   -> exec_C_SQSP            rs2    uimm10    mstate
    C_SWSP              rs2  uimm8    -> exec_C_SWSP            rs2    uimm8     mstate
#ifdef FLOAT
    C_FSWSP             rs2  uimm8    -> exec_C_FSWSP           rs2    uimm8     mstate
#endif
    C_SDSP              rs2  uimm9    -> exec_C_SDSP            rs2    uimm9     mstate

-- ================================================================
-- C_ADDI4SPN (expands to ADDI)

exec_C_ADDI4SPN :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_ADDI4SPN    rd          nzuimm10      mstate =
  let
    rs1     = 2    -- GPR sp
    imm12   = nzuimm10    -- zero-extended from 10b to 12b
    instr_I = ADDI  rd  rs1  imm12

    is_C    = True
    mstate1 = exec_ADDI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FLD (expands to FLD)

#ifdef FLOAT
exec_C_FLD :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FLD    rd          rs1         imm8          mstate =
  let
    imm12        = imm8      -- zero extended
    instr_D      = FLD  rd  rs1  imm12
    is_C         = True
    mstate1      = exec_FLD  is_C  instr_D  mstate
  in
    mstate1
#endif

-- ================================================================
-- C_LQ (expands to LQ)

{- TODO: Uncomment if/when we do RV128
exec_C_LQ :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LQ    rd          rs1         imm9          mstate =
  let
    imm12        = imm9      -- zero extended
    instr_I128   = LQ  rd  rs1  imm12
    is_C         = True
    mstate1      = exec_LQ  is_C  instr_I128  mstate
  in
    mstate1
-}

-- ================================================================
-- C_LW (expands to LW)

exec_C_LW :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LW    rd          rs1         imm7          mstate =
  let
    imm12        = imm7      -- zero extended
    instr_I      = LW  rd  rs1  imm12
    is_C         = True
    mstate1      = exec_LW  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FLW (expands to FLW)

#ifdef FLOAT
exec_C_FLW :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FLW    rd          rs1         imm7          mstate =
  let
    imm12   = imm7      -- zero extended
    instr_F = FLW  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_FLW  is_C  instr_F  mstate
  in
    mstate1
#endif

-- ================================================================
-- C_LD (expands to LD)

exec_C_LD :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LD    rd          rs1         imm8          mstate =
  let
    imm12   = imm8      -- zero extended
    instr_I = LD  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_LD  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FSD (expands to FSD)

#ifdef FLOAT
exec_C_FSD :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FSD    rs1         rs2         imm8          mstate =
  let
    imm12   = imm8      -- zero extended
    instr_D = FSD  rs1  rs2  imm12
    is_C    = True
    mstate1 = exec_FSD  is_C  instr_D  mstate
  in
    mstate1
#endif

-- ================================================================

{- TODO: Uncomment if/when we do RV128
exec_C_SQ :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SQ    rs1         rs2         imm9          mstate =
  let
    imm12      = imm9      -- zero extended
    instr_I128 = FSD  rs1  rs2  imm12
    is_C       = True
    mstate1    = exec_SQ  is_C  instr_I128  mstate
  in
    mstate1
-}

-- ================================================================
-- C_SW: expands to SW

exec_C_SW :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SW    rs1         rs2         imm7          mstate =
  let
    imm12    = imm7      -- zero extended
    instr_I  = SW  rs1  rs2  imm12
    is_C     = True
    mstate1  = exec_SW  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FSW: expands to SW

#ifdef FLOAT
exec_C_FSW :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FSW    rs1         rs2         imm7          mstate =
  let
    imm12   = imm7      -- zero extended
    instr_F = FSW  rs1  rs2  imm12
    is_C    = True
    mstate1 = exec_FSW  is_C  instr_F  mstate
  in
    mstate1
#endif

-- ================================================================
-- C_SD: expands to SD

exec_C_SD :: GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SD    rs1         rs2         imm8          mstate =
  let
    imm12    = imm8      -- zero extended
    instr_I  = SD  rs1  rs2  imm12
    is_C     = True
    mstate1  = exec_SD  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_NOP: expands to 'nop' (ADDI x0, x0, 0)

exec_C_NOP :: Machine_State -> Machine_State
exec_C_NOP    mstate =
  let
    rd      = 0
    rs1     = 0
    imm12   = 0
    instr_I = ADDI  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_ADDI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_ADDI: expands to ADDI

exec_C_ADDI :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_ADDI    rd_rs1      nzimm6        mstate =
  let
    imm12   = sign_extend  6  12  nzimm6
    instr_I = ADDI  rd_rs1  rd_rs1  imm12
    is_C    = True
    mstate1 = exec_ADDI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_JAL: expands to JAL

exec_C_JAL :: InstrField -> Machine_State -> Machine_State
exec_C_JAL    imm12         mstate =
  let
    rd      = 1    -- GPR ra
    imm21   = sign_extend  12  21  imm12
    instr_I = JAL  rd  imm21
    is_C    = True
    mstate1 = exec_JAL  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_ADDIW: expands to ADDIW

exec_C_ADDIW :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_ADDIW    rd_rs1      imm6          mstate =
  let
    imm12   = sign_extend  6  12  imm6
    instr_I = ADDIW  rd_rs1  rd_rs1  imm12
    is_C    = True
    mstate1 = exec_ADDIW  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_LI: expands to ADDI

exec_C_LI :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LI    rd_rs1      imm6          mstate =
  let
    imm12   = sign_extend  6  12  imm6
    rs1     = 0    -- GPR zero
    instr_I = ADDI  rd_rs1  0  imm12
    is_C    = True
    mstate1 = exec_ADDI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_ADDI16SP: expands to ADDI  x2  x2  nzimm10

exec_C_ADDI16SP :: InstrField -> Machine_State -> Machine_State
exec_C_ADDI16SP    nzimm10       mstate =
  let
    imm12   = sign_extend  10  12  nzimm10
    rd      = 2    -- GPR sp
    rs1     = 2    -- GPR sp
    instr_I = ADDI  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_ADDI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_LUI: expands to LUI  rd  sign-extend imm6

exec_C_LUI :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LUI    rd_rs1      nzimm6        mstate =
  let
    imm20   = sign_extend  6  20  nzimm6
    instr_I = LUI  rd_rs1  imm20
    is_C    = True
    mstate1 = exec_LUI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_SRLI: expands to SRLI

is_C_SRLI :: RV -> Instr_16b -> Bool
is_C_SRLI    rv    instr_16b =
  let
    (funct3, offset_12_10, r_prime, offset_6_2, op) = instr_16b_fields_CB  instr_16b
    shamt6_5 = bitSlice  offset_12_10  2  2
    shamt6   = ((shiftL  shamt6_5  5) .|. offset_6_2)
    funct2   = bitSlice  offset_12_10  1  0
    rd_rs1   = (0x8 .|.  r_prime)

    result = ((   funct3 == funct3_C_SRLI)
              && (funct2 == funct2_C_SRLI)
              && (op == opcode_C1)
              && (if rv == RV32 then shamt6_5 == 0 else True)
              && (if (rv == RV32) || (rv == RV64) then shamt6 /= 0 else True))
  in
    result
    
exec_C_SRLI :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SRLI    rd_rs1      shamt6        mstate =
  let
    instr_I = SRLI  rd_rs1  rd_rs1  shamt6
    is_C    = True
    mstate1 = exec_SRLI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_SRAI: expands to SRAI

is_C_SRAI :: RV -> Instr_16b -> Bool
is_C_SRAI    rv    instr_16b =
  let
    (funct3, offset_12_10, r_prime, offset_6_2, op) = instr_16b_fields_CB  instr_16b
    shamt6_5 = bitSlice  offset_12_10  2  2
    shamt6   = ((shiftL  shamt6_5  5) .|. offset_6_2)
    funct2   = bitSlice  offset_12_10  1  0

    result = ((   funct3 == funct3_C_SRAI)
              && (funct2 == funct2_C_SRAI)
              && (op == opcode_C1)
              && (if rv == RV32 then shamt6_5 == 0 else True)
              && (if (rv == RV32) || (rv == RV64) then shamt6 /= 0 else True))
  in
    result
    
exec_C_SRAI :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SRAI    rd_rs1      shamt6        mstate =
  let
    instr_I = SRAI  rd_rs1  rd_rs1  shamt6
    is_C    = True
    mstate1 = exec_SRAI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_ANDI: expands to ANDI

is_C_ANDI :: RV -> Instr_16b -> Bool
is_C_ANDI    rv    instr_16b =
  let
    (funct3, offset_12_10, r_prime, offset_6_2, op) = instr_16b_fields_CB  instr_16b
    funct2   = bitSlice  offset_12_10  1  0

    result = ((   funct3 == funct3_C_ANDI)
              && (funct2 == funct2_C_ANDI)
              && (op == opcode_C1))
  in
    result
    
exec_C_ANDI :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_ANDI    rd_rs1      imm6          mstate =
  let
    imm12   = sign_extend  6  12  imm6
    instr_I = ANDI  rd_rs1  rd_rs1  imm12
    is_C    = True
    mstate1 = exec_ANDI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_SUB: expands to SUB

exec_C_SUB :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_SUB    rd_rs1      rs2         mstate =
  let
    instr_I = SUB  rd_rs1  rd_rs1  rs2
    is_C    = True
    mstate1 = exec_SUB  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_XOR: expands to XOR

exec_C_XOR :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_XOR    rd_rs1      rs2         mstate =
  let
    instr_I = XOR  rd_rs1  rd_rs1  rs2
    is_C    = True
    mstate1 = exec_XOR  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_OR: expands to OR

exec_C_OR :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_OR    rd_rs1      rs2         mstate =
  let
    instr_I = OR  rd_rs1  rd_rs1  rs2
    is_C    = True
    mstate1 = exec_OR  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_AND: expands to AND

exec_C_AND :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_AND    rd_rs1      rs2         mstate =
  let
    instr_I = AND  rd_rs1  rd_rs1  rs2
    is_C    = True
    mstate1 = exec_AND  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_SUBW: expands to SUBW

exec_C_SUBW :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_SUBW    rd_rs1      rs2         mstate =
  let
    instr_I = SUBW  rd_rs1  rd_rs1  rs2
    is_C    = True
    mstate1 = exec_SUBW  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_ADDW: expands to ADDW

exec_C_ADDW :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_ADDW    rd_rs1      rs2         mstate =
  let
    instr_I = ADDW  rd_rs1  rd_rs1  rs2
    is_C    = True
    mstate1 = exec_ADDW  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_J: expands to JAL

exec_C_J :: InstrField -> Machine_State -> Machine_State
exec_C_J    imm12         mstate =
  let
    rd      = 0    -- GPR zero
    imm21   = sign_extend  12  21  imm12
    instr_I = JAL  rd  imm21
    is_C    = True
    mstate1 = exec_JAL  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_BEQZ: expands to BEQ

exec_C_BEQZ :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_BEQZ    rs1         imm9          mstate =
  let
    rs2     = 0    -- GPR zero
    imm13   = sign_extend  9  13  imm9
    instr_I = BEQ  rs1 rs2  imm13
    is_C    = True
    mstate1 = exec_BEQ  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_BNEZ: expands to BNE

exec_C_BNEZ :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_BNEZ    rs1         imm9          mstate =
  let
    rs2     = 0    -- GPR zero
    imm13   = sign_extend  9  13  imm9
    instr_I = BNE  rs1 rs2  imm13
    is_C    = True
    mstate1 = exec_BNE  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_SLLI: expands to SLLI

is_C_SLLI :: RV -> Instr_16b -> Bool
is_C_SLLI    rv    instr_16b =
  let
    (funct3, imm_12, rd_rs1, imm_6_2, op) = instr_16b_fields_CI  instr_16b
    shamt6_5 = imm_12
    shamt6   = ((shiftL  shamt6_5  5) .|. imm_6_2)

    result = ((   funct3 == funct3_C_SLLI)
              && (op == opcode_C2)
              && (if rv == RV32 then shamt6_5 == 0 else True)
              && (if (rv == RV32) || (rv == RV64) then shamt6 /= 0 else True))
  in
    result
    
exec_C_SLLI :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SLLI    rd_rs1      shamt6        mstate =
  let
    instr_I = SLLI  rd_rs1  rd_rs1  shamt6
    is_C    = True
    mstate1 = exec_SLLI  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FLDSP :: expands for FLD

#ifdef FLOAT
exec_C_FLDSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FLDSP    rd          uimm9         mstate =
  let
    rs1     = 2        -- GPR sp
    imm12   = uimm9    -- zero-extended
    instr_D = FLD  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_FLD  is_C  instr_D  mstate
  in
    mstate1
#endif

-- ================================================================
-- C_LQSP: expands to LQ

{- TODO: Uncomment if/when we implement RV128
exec_C_LQSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LQSP    rd          uimm10        mstate =
  let
    rs1        = 2         -- GPR sp
    imm12      = uimm10    -- zero-extended
    instr_I128 = LQ  rd  rs1  imm12
    is_C       = True
    mstate1    = exec_LQ  is_C  instr_I128  mstate
  in
    mstate1
-}

-- ================================================================
-- C_LWSP: expands to LW

exec_C_LWSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LWSP    rd          uimm8         mstate =
  let
    rs1        = 2        -- GPR sp
    imm12      = uimm8    -- zero-extended
    instr_I    = LW  rd  rs1  imm12
    is_C       = True
    mstate1    = exec_LW  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FLWSP: expands to FLW

#ifdef FLOAT
exec_C_FLWSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FLWSP    rd          uimm8         mstate =
  let
    rs1     = 2        -- GPR sp
    imm12   = uimm8    -- zero-extended
    instr_D = FLW  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_FLW  is_C  instr_D  mstate
  in
    mstate1
#endif

-- ================================================================
-- C_LDSP: expands to LD

exec_C_LDSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_LDSP    rd          uimm9         mstate =
  let
    rs1        = 2        -- GPR sp
    imm12      = uimm9    -- zero-extended
    instr_I    = LD  rd  rs1  imm12
    is_C       = True
    mstate1    = exec_LD  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_JR: expands to JALR

exec_C_JR :: GPR_Addr -> Machine_State -> Machine_State
exec_C_JR    rs1         mstate =
  let
    rd      = 0    -- GPR zero
    imm12   = 0
    instr_I = JALR  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_JALR  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_MV: expands to ADD

exec_C_MV :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_MV    rd          rs2         mstate =
  let
    rs1     = 0    -- GPR zero
    instr_I = ADD  rd  rs1  rs2
    is_C    = True
    mstate1 = exec_ADD  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_EBREAK: expands to EBREAK

exec_C_EBREAK :: Machine_State -> Machine_State
exec_C_EBREAK    mstate =
  let
    rs1     = 0    -- GPR zero
    instr_I = EBREAK
    is_C    = True
    mstate1 = exec_EBREAK  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_JALR: expands to JALR

exec_C_JALR :: GPR_Addr -> Machine_State -> Machine_State
exec_C_JALR    rs1         mstate =
  let
    rd      = 1    -- GPR ra
    imm12   = 0
    instr_I = JALR  rd  rs1  imm12
    is_C    = True
    mstate1 = exec_JALR  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_ADD: expands to ADD

exec_C_ADD :: GPR_Addr -> GPR_Addr -> Machine_State -> Machine_State
exec_C_ADD    rd          rs2         mstate =
  let
    rs1     = rd
    instr_I = ADD  rd  rs1  rs2
    is_C    = True
    mstate1 = exec_ADD  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FSDSP: expands to FSD

#ifdef FLOAT
exec_C_FSDSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FSDSP    rs2         uimm9         mstate =
  let
    rs1     = 2        -- GPR sp
    imm12   = uimm9    -- zero-extended
    instr_D = FSD  rs1  rs2  imm12
    is_C    = True
    mstate1 = exec_FSD  is_C  instr_D  mstate
  in
    mstate1
#endif

-- ================================================================
-- C_SQSP: expands to SQ

{- TODO: Uncomment if/when we implement RV128
exec_C_SQSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SQSP    rs2         uimm10        mstate =
  let
    rs1        = 2         -- GPR sp
    imm12      = uimm10    -- zero-extended
    instr_I128 = SQ  rs1  rs2  imm12
    is_C       = True
    mstate1    = exec_SQ  is_C  instr_I128  mstate
  in
    mstate1
-}

-- ================================================================
-- C_SWSP: expands to SW

exec_C_SWSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SWSP    rs2         uimm8        mstate =
  let
    rs1     = 2        -- GPR sp
    imm12   = uimm8    -- zero-extended
    instr_I = SW  rs1  rs2  imm12
    is_C    = True
    mstate1 = exec_SW  is_C  instr_I  mstate
  in
    mstate1

-- ================================================================
-- C_FSWSP: expands to FSW

#ifdef FLOAT
exec_C_FSWSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_FSWSP    rs2         uimm8         mstate =
  let
    rs1     = 2        -- GPR sp
    imm12   = uimm8    -- zero-extended
    instr_F = FSW  rs1  rs2  imm12
    is_C    = True
    mstate1 = exec_FSW  is_C  instr_F  mstate
  in
    mstate1
#endif

-- ================================================================
-- C_SDSP: expands to SD

exec_C_SDSP :: GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_C_SDSP    rs2         uimm9        mstate =
  let
    rs1       = 2        -- GPR sp
    imm12     = uimm9    -- zero-extended
    instr_I64 = SD  rs1  rs2  imm12
    is_C      = True
    mstate1   = exec_SD  is_C  instr_I64  mstate
  in
    mstate1

-- ================================================================

-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
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
-- Data structure for instructions in 'M'

data Instr_A = LR_W       GPR_Addr  GPR_Addr            InstrField  InstrField    -- rd,  rs1,      aq, rl
             | SC_W       GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl

             | LR_D       GPR_Addr  GPR_Addr            InstrField  InstrField    -- rd,  rs1,      aq, rl
             | SC_D       GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl

             | AMOSWAP_W  GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOADD_W   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOXOR_W   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOAND_W   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOOR_W    GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMIN_W   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMAX_W   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMINU_W  GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMAXU_W  GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl

             | AMOSWAP_D  GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOADD_D   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOXOR_D   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOAND_D   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOOR_D    GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMIN_D   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMAX_D   GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMINU_D  GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
             | AMOMAXU_D  GPR_Addr  GPR_Addr  GPR_Addr  InstrField  InstrField    -- rd,  rs1, rs2, aq, rl
  deriving (Eq, Show)

-- ================================================================
-- Decode from 32b representation to Instr_M data structure

decode_A :: RV -> Instr_32b -> Maybe Instr_A
decode_A    rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode  = bitSlice  instr_32b   6   0
    rd      = bitSlice  instr_32b  11   7
    funct3  = bitSlice  instr_32b  14  12
    rs1     = bitSlice  instr_32b  19  15
    rs2     = bitSlice  instr_32b  24  20
    rl      = bitSlice  instr_32b  25  25
    aq      = bitSlice  instr_32b  26  26
    msbs5   = bitSlice  instr_32b  31  27

    instr_M
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_LR, rs2==0 = Just  (LR_W       rd rs1     aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_SC         = Just  (SC_W       rd rs1 rs2 aq rl)

      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_LR, rs2==0 = Just  (LR_D       rd rs1     aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_SC         = Just  (SC_D       rd rs1 rs2 aq rl)

      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_SWAP       = Just  (AMOSWAP_W  rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_ADD        = Just  (AMOADD_W   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_XOR        = Just  (AMOXOR_W   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_AND        = Just  (AMOAND_W   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_OR         = Just  (AMOOR_W    rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_MIN        = Just  (AMOMIN_W   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_MAX        = Just  (AMOMAX_W   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_MINU       = Just  (AMOMINU_W  rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_W, msbs5==msbs5_AMO_MAXU       = Just  (AMOMAXU_W  rd rs1 rs2 aq rl)

      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_SWAP       = Just  (AMOSWAP_D  rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_ADD        = Just  (AMOADD_D   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_XOR        = Just  (AMOXOR_D   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_AND        = Just  (AMOAND_D   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_OR         = Just  (AMOOR_D    rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_MIN        = Just  (AMOMIN_D   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_MAX        = Just  (AMOMAX_D   rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_MINU       = Just  (AMOMINU_D  rd rs1 rs2 aq rl)
      | opcode==opcode_AMO, funct3==funct3_AMO_D, msbs5==msbs5_AMO_MAXU       = Just  (AMOMAXU_D  rd rs1 rs2 aq rl)

      | True = Nothing
  in
    instr_M

-- ================================================================
-- Execution of Instr_A

type Spec_Instr_A = Bool -> Instr_A -> Machine_State -> Machine_State
--                  is_C    instr_A    mstate           mstate'

exec_instr_A :: Spec_Instr_A
exec_instr_A  is_C  instr_A  mstate =
  case instr_A of
    LR_W       rd  rs1       aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_LR    is_C  rd  rs1    0  aq  rl  mstate
    SC_W       rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_SC    is_C  rd  rs1  rs2  aq  rl  mstate

    LR_D       rd  rs1       aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_LR    is_C  rd  rs1    0  aq  rl  mstate
    SC_D       rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_SC    is_C  rd  rs1  rs2  aq  rl  mstate

    AMOSWAP_W  rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_SWAP  is_C  rd  rs1  rs2  aq  rl  mstate
    AMOADD_W   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_ADD   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOXOR_W   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_XOR   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOAND_W   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_AND   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOOR_W    rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_OR    is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMIN_W   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_MIN   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMAX_W   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_MAX   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMINU_W  rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_MINU  is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMAXU_W  rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_W  msbs5_AMO_MAXU  is_C  rd  rs1  rs2  aq  rl  mstate

    AMOSWAP_D  rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_SWAP  is_C  rd  rs1  rs2  aq  rl  mstate
    AMOADD_D   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_ADD   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOXOR_D   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_XOR   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOAND_D   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_AND   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOOR_D    rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_OR    is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMIN_D   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_MIN   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMAX_D   rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_MAX   is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMINU_D  rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_MINU  is_C  rd  rs1  rs2  aq  rl  mstate
    AMOMAXU_D  rd  rs1  rs2  aq rl -> exe_AMO  funct3_AMO_D  msbs5_AMO_MAXU  is_C  rd  rs1  rs2  aq  rl  mstate


exe_AMO :: InstrField ->
           InstrField ->
           Bool ->
           GPR_Addr ->
           GPR_Addr ->
           GPR_Addr ->
           InstrField ->
           InstrField ->
           Machine_State -> Machine_State
exe_AMO  funct3  msbs5  is_C  rd  rs1  rs2  aq  rl  mstate =
  let
    rv      = mstate_rv_read  mstate
    rs2_val = mstate_gpr_read  mstate  rs2

    -- Compute effective address
    eaddr1  = mstate_gpr_read  mstate  rs1
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    -- Do the AMO op
    (result1, mstate1) = mstate_vm_amo  mstate  funct3  msbs5  aq  rl  eaddr2  rs2_val

    -- Finish with trap, or finish with loading Rd with AMO result
    mstate2 = case result1 of
                Mem_Result_Err exc_code ->
                  finish_trap  mstate1  exc_code  eaddr2

                Mem_Result_Ok  x        ->
                  finish_rd_and_pc_incr  mstate1  rd  x  is_C
  in
    mstate2

-- ================================================================

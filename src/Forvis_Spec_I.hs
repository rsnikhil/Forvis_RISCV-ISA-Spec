-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_I where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'I' (Base) instructions

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import ALU
import Arch_Defs
import Machine_State
import CSR_File
import Virtual_Mem

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- 'I' Base instruction set

-- NOTE: opcode_XXX, funct3_XXX are defined in module Arch_Defs

-- ================================================================
-- Data structure for instructions in 'I' (base instruction set)

data Instr_I = LUI    GPR_Addr  InstrField              -- rd,  imm20
             | AUIPC  GPR_Addr  InstrField              -- rd,  imm20

             | JAL    GPR_Addr  InstrField              -- rd,  imm21
             | JALR   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12

             | BEQ    GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm13
             | BNE    GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm13
             | BLT    GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm13
             | BGE    GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm13
             | BLTU   GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm13
             | BGEU   GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm13

             | LB     GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | LH     GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | LW     GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | LBU    GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | LHU    GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12

             | SB     GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm12
             | SH     GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm12
             | SW     GPR_Addr  GPR_Addr  InstrField    -- rs1, rs2, imm12

             | ADDI   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | SLTI   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | SLTIU  GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | XORI   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | ORI    GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12
             | ANDI   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, imm12

             | SLLI   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, shamt
             | SRLI   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, shamt
             | SRAI   GPR_Addr  GPR_Addr  InstrField    -- rd,  rs1, shamt

             | ADD    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | SUB    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | SLL    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | SLT    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | SLTU   GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | XOR    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | SRL    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | SRA    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | OR     GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2
             | AND    GPR_Addr  GPR_Addr  GPR_Addr      -- rd,  rs1, rs2

             | FENCE  InstrField  InstrField  InstrField    -- fm, pred, succ
             | ECALL
             | EBREAK
  deriving (Eq, Show)

-- ================================================================
-- Decode from 32b representation to Instr_I data structure

decode_I :: RV -> Instr_32b -> Maybe Instr_I
decode_I    rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode  = bitSlice  instr_32b   6   0
    rd      = bitSlice  instr_32b  11   7
    funct3  = bitSlice  instr_32b  14  12
    rs1     = bitSlice  instr_32b  19  15
    rs2     = bitSlice  instr_32b  24  20
    funct7  = bitSlice  instr_32b  31  25

    imm12_I = bitSlice  instr_32b  31  20

    imm12_S = (shiftL  (bitSlice  instr_32b  31  25) 5) .|. (bitSlice  instr_32b  11 7)

    imm13_B = ((    shiftL  (bitSlice  instr_32b  31  31)  12)
               .|. (shiftL  (bitSlice  instr_32b  30  25)   5)
               .|. (shiftL  (bitSlice  instr_32b  11   8)   1)
               .|. (shiftL  (bitSlice  instr_32b   7   7)  11))

    imm21_J = ((    shiftL  (bitSlice  instr_32b  31  31)  20)
               .|. (shiftL  (bitSlice  instr_32b  30  21)   1)
               .|. (shiftL  (bitSlice  instr_32b  20  20)  11)
               .|. (shiftL  (bitSlice  instr_32b  19  12)  12))

    imm20_U = bitSlice  instr_32b  31  12

    -- for SLLI/SRLI/SRAI
    msbs7 = bitSlice instr_32b 31 25
    msbs6 = bitSlice instr_32b 31 26
    shamt_ok = if (rv == RV32) then
                 ((   msbs7 == msbs7_SLLI)
                  || (msbs7 == msbs7_SRLI)
                  || (msbs7 == msbs7_SRAI))
               else
                 ((   msbs6 == msbs6_SLLI)
                  || (msbs6 == msbs6_SRLI)
                  || (msbs6 == msbs6_SRAI))

    shamt = if (rv == RV32) then
              bitSlice instr_32b 24 20
            else
              bitSlice instr_32b 25 20

    -- For FENCE
    fm      = bitSlice  instr_32b  31  28
    pred    = bitSlice  instr_32b  27  24
    succ    = bitSlice  instr_32b  23  20

    instr_I
      | opcode==opcode_LUI   = Just  (LUI    rd  imm20_U)
      | opcode==opcode_AUIPC = Just  (AUIPC  rd  imm20_U)

      | opcode==opcode_JAL  = Just  (JAL   rd  imm21_J)
      | opcode==opcode_JALR = Just  (JALR  rd  rs1  imm12_I)

      | opcode==opcode_BRANCH, funct3==funct3_BEQ  = Just  (BEQ  rs1 rs2 imm13_B)
      | opcode==opcode_BRANCH, funct3==funct3_BNE  = Just  (BNE  rs1 rs2 imm13_B)
      | opcode==opcode_BRANCH, funct3==funct3_BLT  = Just  (BLT  rs1 rs2 imm13_B)
      | opcode==opcode_BRANCH, funct3==funct3_BGE  = Just  (BGE  rs1 rs2 imm13_B)
      | opcode==opcode_BRANCH, funct3==funct3_BLTU = Just  (BLTU rs1 rs2 imm13_B)
      | opcode==opcode_BRANCH, funct3==funct3_BGEU = Just  (BGEU rs1 rs2 imm13_B)

      | opcode==opcode_LOAD, funct3==funct3_LB  = Just  (LB  rd rs1 imm12_I)
      | opcode==opcode_LOAD, funct3==funct3_LH  = Just  (LH  rd rs1 imm12_I)
      | opcode==opcode_LOAD, funct3==funct3_LW  = Just  (LW  rd rs1 imm12_I)
      | opcode==opcode_LOAD, funct3==funct3_LBU = Just  (LBU rd rs1 imm12_I)
      | opcode==opcode_LOAD, funct3==funct3_LHU = Just  (LHU rd rs1 imm12_I)

      | opcode==opcode_STORE, funct3==funct3_SB = Just  (SB rs1 rs2 imm12_S)
      | opcode==opcode_STORE, funct3==funct3_SH = Just  (SH rs1 rs2 imm12_S)
      | opcode==opcode_STORE, funct3==funct3_SW = Just  (SW rs1 rs2 imm12_S)

      | opcode==opcode_OP_IMM, funct3==funct3_ADDI  = Just  (ADDI  rd rs1 imm12_I)
      | opcode==opcode_OP_IMM, funct3==funct3_SLTI  = Just  (SLTI  rd rs1 imm12_I)
      | opcode==opcode_OP_IMM, funct3==funct3_SLTIU = Just  (SLTIU rd rs1 imm12_I)
      | opcode==opcode_OP_IMM, funct3==funct3_XORI  = Just  (XORI  rd rs1 imm12_I)
      | opcode==opcode_OP_IMM, funct3==funct3_ORI   = Just  (ORI   rd rs1 imm12_I)
      | opcode==opcode_OP_IMM, funct3==funct3_ANDI  = Just  (ANDI  rd rs1 imm12_I)

      | opcode==opcode_OP_IMM, funct3==funct3_SLLI, msbs6==msbs6_SLLI, shamt_ok = Just  (SLLI rd rs1 shamt)
      | opcode==opcode_OP_IMM, funct3==funct3_SRLI, msbs6==msbs6_SRLI, shamt_ok = Just  (SRLI rd rs1 shamt)
      | opcode==opcode_OP_IMM, funct3==funct3_SRAI, msbs6==msbs6_SRAI, shamt_ok = Just  (SRAI rd rs1 shamt)

      | opcode==opcode_OP, funct3==funct3_ADD,  funct7==funct7_ADD  = Just  (ADD  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_SUB,  funct7==funct7_SUB  = Just  (SUB  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_SLL,  funct7==funct7_SLL  = Just  (SLL  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_SLT,  funct7==funct7_SLT  = Just  (SLT  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_SLTU, funct7==funct7_SLTU = Just  (SLTU rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_XOR,  funct7==funct7_XOR  = Just  (XOR  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_SRL,  funct7==funct7_SRL  = Just  (SRL  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_SRA,  funct7==funct7_SRA  = Just  (SRA  rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_OR,   funct7==funct7_OR   = Just  (OR   rd rs1 rs2)
      | opcode==opcode_OP, funct3==funct3_AND,  funct7==funct7_AND  = Just  (AND  rd rs1 rs2)

      | opcode==opcode_MISC_MEM, rd==0, funct3==funct3_FENCE,   rs1==0 = Just  (FENCE fm pred succ)

      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, imm12_I==funct12_ECALL  = Just  (ECALL)
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, imm12_I==funct12_EBREAK = Just  (EBREAK)

      | True = Nothing
  in
    instr_I

-- ================================================================
-- Execution of Instr_I

type Spec_Instr_I = Bool -> Instr_I -> Machine_State -> Machine_State
--                  is_C    instr_I    mstate           mstate'

exec_instr_I :: Spec_Instr_I
exec_instr_I  is_C  instr_I  mstate =
  case instr_I of
    LUI    rd   imm20      -> exec_LUI    is_C  instr_I  mstate
    AUIPC  rd   imm20      -> exec_AUIPC  is_C  instr_I  mstate

    JAL    rd   imm21      -> exec_JAL    is_C  instr_I  mstate
    JALR   rd   rs1  imm12 -> exec_JALR   is_C  instr_I  mstate

    BEQ    rs1  rs2  imm13 -> exec_BEQ    is_C  instr_I  mstate
    BNE    rs1  rs2  imm13 -> exec_BNE    is_C  instr_I  mstate
    BLT    rs1  rs2  imm13 -> exec_BLT    is_C  instr_I  mstate
    BGE    rs1  rs2  imm13 -> exec_BGE    is_C  instr_I  mstate
    BLTU   rs1  rs2  imm13 -> exec_BLTU   is_C  instr_I  mstate
    BGEU   rs1  rs2  imm13 -> exec_BGEU   is_C  instr_I  mstate

    LB     rd   rs1  imm12 -> exec_LB     is_C  instr_I  mstate
    LH     rd   rs1  imm12 -> exec_LH     is_C  instr_I  mstate
    LW     rd   rs1  imm12 -> exec_LW     is_C  instr_I  mstate
    LBU    rd   rs1  imm12 -> exec_LBU    is_C  instr_I  mstate
    LHU    rd   rs1  imm12 -> exec_LHU    is_C  instr_I  mstate

    SB     rs1  rs2  imm12 -> exec_SB     is_C  instr_I  mstate
    SH     rs1  rs2  imm12 -> exec_SH     is_C  instr_I  mstate
    SW     rs1  rs2  imm12 -> exec_SW     is_C  instr_I  mstate

    ADDI   rd   rs1  imm12 -> exec_ADDI   is_C  instr_I  mstate
    SLTI   rd   rs1  imm12 -> exec_SLTI   is_C  instr_I  mstate
    SLTIU  rd   rs1  imm12 -> exec_SLTIU  is_C  instr_I  mstate
    XORI   rd   rs1  imm12 -> exec_XORI   is_C  instr_I  mstate
    ORI    rd   rs1  imm12 -> exec_ORI    is_C  instr_I  mstate
    ANDI   rd   rs1  imm12 -> exec_ANDI   is_C  instr_I  mstate

    SLLI   rd   rs1  imm12 -> exec_SLLI   is_C  instr_I  mstate
    SRLI   rd   rs1  imm12 -> exec_SRLI   is_C  instr_I  mstate
    SRAI   rd   rs1  imm12 -> exec_SRAI   is_C  instr_I  mstate

    ADD    rd   rs1  rs2   -> exec_ADD    is_C  instr_I  mstate
    SUB    rd   rs1  rs2   -> exec_SUB    is_C  instr_I  mstate
    SLL    rd   rs1  rs2   -> exec_SLL    is_C  instr_I  mstate
    SLT    rd   rs1  rs2   -> exec_SLT    is_C  instr_I  mstate
    SLTU   rd   rs1  rs2   -> exec_SLTU   is_C  instr_I  mstate
    XOR    rd   rs1  rs2   -> exec_XOR    is_C  instr_I  mstate
    SRL    rd   rs1  rs2   -> exec_SRL    is_C  instr_I  mstate
    SRA    rd   rs1  rs2   -> exec_SRA    is_C  instr_I  mstate
    OR     rd   rs1  rs2   -> exec_OR     is_C  instr_I  mstate
    AND    rd   rs1  rs2   -> exec_AND    is_C  instr_I  mstate

    FENCE  fm   pred  succ -> exec_FENCE  is_C  instr_I  mstate
    ECALL                  -> exec_ECALL  is_C  instr_I  mstate
    EBREAK                 -> exec_EBREAK is_C  instr_I  mstate

-- ================================================================
-- LUI

exec_LUI :: Spec_Instr_I
exec_LUI  is_C  (LUI  rd  imm20)  mstate =
  let
    xlen    = mstate_xlen_read  mstate
    rd_val  = sign_extend  32  xlen  (shiftL  imm20  12)
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1

-- ================================================================
-- AUIPC

exec_AUIPC :: Spec_Instr_I
exec_AUIPC  is_C  (AUIPC  rd  imm20)  mstate =
  let
    xlen     = mstate_xlen_read  mstate
    pc       = mstate_pc_read  mstate
    s_offset = sign_extend  32  xlen  (shiftL  imm20  12)
    rd_val   = alu_add  xlen  pc  s_offset

    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1

-- ================================================================
-- JAL

exec_JAL :: Spec_Instr_I
exec_JAL  is_C  (JAL  rd  imm21)  mstate =
  let
    misa     = mstate_csr_read   mstate  csr_addr_misa
    xlen     = mstate_xlen_read  mstate
    pc       = mstate_pc_read    mstate
    rd_val   = if is_C then pc + 2 else pc + 4

    s_offset = sign_extend  21  xlen  imm21
    new_pc   = alu_add  xlen  pc  s_offset
    aligned  = if (misa_flag  misa  'C') then
                 ((new_pc .&. 0x1) == 0)
               else
                 ((new_pc .&. 0x3) == 0)

    mstate1  = if aligned
               then
                 finish_rd_and_pc  mstate  rd  rd_val  new_pc
               else
                 finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc
  in
    mstate1

-- ================================================================
-- JALR

exec_JALR :: Spec_Instr_I
exec_JALR  is_C  (JALR  rd  rs1  imm12)  mstate =
  let
    misa   = mstate_csr_read   mstate  csr_addr_misa
    xlen   = mstate_xlen_read  mstate
    pc     = mstate_pc_read    mstate
    rd_val = if is_C then pc + 2 else pc + 4

    s_offset = sign_extend  12  xlen  imm12

    rs1_val = mstate_gpr_read  mstate  rs1

    new_pc  = alu_add  xlen  rs1_val  s_offset
    new_pc' = clearBit  new_pc  0
    aligned  = if (misa_flag  misa  'C') then
                 True
               else
                 ((new_pc' .&. 0x3) == 0)

    mstate1 = if aligned then
                finish_rd_and_pc  mstate  rd  rd_val  new_pc'
              else
                finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc'
  in
    mstate1

-- ================================================================
-- BRANCH (BEQ, BNE, BLT, BGE, BLTU, BGEU)

exec_BEQ :: Spec_Instr_I
exec_BEQ   is_C  (BEQ   rs1  rs2  imm13)  mstate =
  exec_BRANCH  alu_eq   is_C  rs1  rs2  imm13  mstate

exec_BNE :: Spec_Instr_I
exec_BNE   is_C  (BNE   rs1  rs2  imm13)  mstate =
  exec_BRANCH  alu_ne   is_C  rs1  rs2  imm13  mstate

exec_BLT :: Spec_Instr_I
exec_BLT   is_C  (BLT   rs1  rs2  imm13)  mstate =
  exec_BRANCH  alu_lt   is_C  rs1  rs2  imm13  mstate

exec_BGE :: Spec_Instr_I
exec_BGE   is_C  (BGE   rs1  rs2  imm13)  mstate =
  exec_BRANCH  alu_ge   is_C  rs1  rs2  imm13  mstate

exec_BLTU :: Spec_Instr_I
exec_BLTU  is_C  (BLTU  rs1  rs2  imm13)  mstate =
  exec_BRANCH  alu_ltu  is_C  rs1  rs2  imm13  mstate

exec_BGEU :: Spec_Instr_I
exec_BGEU  is_C  (BGEU  rs1  rs2  imm13)  mstate =
  exec_BRANCH  alu_geu  is_C  rs1  rs2  imm13  mstate

exec_BRANCH :: (Int -> Integer-> Integer-> Bool) ->
               Bool ->
               GPR_Addr ->
               GPR_Addr ->
               InstrField ->
               Machine_State -> Machine_State
exec_BRANCH  branch_alu_op  is_C  rs1  rs2  imm13  mstate =
  let
    xlen     = mstate_xlen_read  mstate
    rs1_val  = mstate_gpr_read   mstate  rs1
    rs2_val  = mstate_gpr_read   mstate  rs2
    taken    = branch_alu_op   xlen  rs1_val  rs2_val

    pc       = mstate_pc_read  mstate
    s_offset = sign_extend  13  xlen  imm13
    target   = alu_add  xlen  pc  s_offset

    new_pc   = if taken then     target
               else if is_C then pc + 2
                    else         pc + 4

    -- new_pc[0] known to be 0, new_pc[1] must be 0 if 'C' is not supported
    misa     = mstate_csr_read  mstate  csr_addr_misa
    aligned  = (misa_flag  misa  'C' ||  (new_pc .&. 0x2 == 0))
          
    mstate1 = if aligned
              then
                finish_pc  mstate  new_pc
              else
                finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc
  in
    mstate1

-- ================================================================
-- LOAD: LB, LH, LW, LBU, LHU

exec_LB  :: Spec_Instr_I
exec_LB   is_C  (LB   rd  rs1  imm12)  mstate = exec_LOAD  is_C  rd  rs1  imm12  funct3_LB  mstate

exec_LH  :: Spec_Instr_I
exec_LH   is_C  (LH   rd  rs1  imm12)  mstate = exec_LOAD  is_C  rd  rs1  imm12  funct3_LH  mstate

exec_LW  :: Spec_Instr_I
exec_LW   is_C  (LW   rd  rs1  imm12)  mstate = exec_LOAD  is_C  rd  rs1  imm12  funct3_LW  mstate

exec_LBU :: Spec_Instr_I
exec_LBU  is_C  (LBU  rd  rs1  imm12)  mstate = exec_LOAD  is_C  rd  rs1  imm12  funct3_LBU  mstate

exec_LHU :: Spec_Instr_I
exec_LHU  is_C  (LHU  rd  rs1  imm12)  mstate = exec_LOAD  is_C  rd  rs1  imm12  funct3_LHU  mstate

exec_LOAD :: Bool -> GPR_Addr -> GPR_Addr -> InstrField -> InstrField -> Machine_State -> Machine_State
exec_LOAD    is_C    rd          rs1         imm12         funct3        mstate =
  let
    rv      = mstate_rv_read    mstate
    xlen    = mstate_xlen_read  mstate

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

                Mem_Result_Ok  d    ->
                  let rd_val | (funct3 == funct3_LB) = sign_extend  8   xlen  d
                             | (funct3 == funct3_LH) = sign_extend  16  xlen  d
                             | (funct3 == funct3_LW) = sign_extend  32  xlen  d
                             | True  = d
                  in
                    finish_rd_and_pc_incr  mstate2  rd  rd_val  is_C
  in
    mstate3

-- ================================================================
-- STORE: SB, SH, SW

exec_SB :: Spec_Instr_I
exec_SB  is_C  (SB  rs1  rs2  imm12)  mstate = exec_STORE  is_C  rs1  rs2  imm12  funct3_SB  mstate

exec_SH :: Spec_Instr_I
exec_SH  is_C  (SH  rs1  rs2  imm12)  mstate = exec_STORE  is_C  rs1  rs2  imm12  funct3_SH  mstate

exec_SW :: Spec_Instr_I
exec_SW  is_C  (SW  rs1  rs2  imm12)  mstate = exec_STORE  is_C  rs1  rs2  imm12  funct3_SW  mstate

exec_STORE :: Bool -> GPR_Addr -> GPR_Addr -> InstrField -> InstrField -> Machine_State -> Machine_State
exec_STORE    is_C    rs1         rs2         imm12         funct3        mstate =
  let
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate

    rs2_val  = mstate_gpr_read  mstate  rs2    -- store value

    --     Compute effective address
    rs1_val  = mstate_gpr_read  mstate  rs1    -- address base
    s_imm12  = sign_extend  12  xlen  imm12
    eaddr1   = alu_add  xlen  rs1_val  s_imm12
    eaddr2   = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

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
    mstate3

-- ================================================================
-- OP_IMM: ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI

exec_ADDI  :: Spec_Instr_I
exec_ADDI   is_C  (ADDI   rd  rs1  imm12)  mstate = exec_OP_IMM  alu_add   is_C  rd  rs1  imm12  mstate

exec_SLTI  :: Spec_Instr_I
exec_SLTI   is_C  (SLTI   rd  rs1  imm12)  mstate = exec_OP_IMM  alu_slt   is_C  rd  rs1  imm12  mstate

exec_SLTIU :: Spec_Instr_I
exec_SLTIU  is_C  (SLTIU  rd  rs1  imm12)  mstate = exec_OP_IMM  alu_sltu  is_C  rd  rs1  imm12  mstate

exec_XORI  :: Spec_Instr_I
exec_XORI   is_C  (XORI   rd  rs1  imm12)  mstate = exec_OP_IMM  alu_xor   is_C  rd  rs1  imm12  mstate

exec_ORI   :: Spec_Instr_I
exec_ORI    is_C  (ORI    rd  rs1  imm12)  mstate = exec_OP_IMM  alu_or    is_C  rd  rs1  imm12  mstate

exec_ANDI  :: Spec_Instr_I
exec_ANDI   is_C  (ANDI   rd  rs1  imm12)  mstate = exec_OP_IMM  alu_and   is_C  rd  rs1  imm12  mstate

exec_SLLI  :: Spec_Instr_I
exec_SLLI   is_C  (SLLI   rd  rs1  imm12)  mstate = exec_OP_IMM  alu_sll   is_C  rd  rs1  imm12  mstate

exec_SRLI  :: Spec_Instr_I
exec_SRLI   is_C  (SRLI   rd  rs1  imm12)  mstate = exec_OP_IMM  alu_srl   is_C  rd  rs1  imm12  mstate

exec_SRAI  :: Spec_Instr_I
exec_SRAI   is_C  (SRAI   rd  rs1  imm12)  mstate = exec_OP_IMM  alu_sra   is_C  rd  rs1  imm12  mstate

exec_OP_IMM :: (Int -> Integer -> Integer -> Integer) -> Bool -> GPR_Addr -> GPR_Addr -> InstrField -> Machine_State -> Machine_State
exec_OP_IMM    alu_op                                    is_C    rd          rs1         imm12         mstate =
  let
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read  mstate  rs1

    s_imm   = sign_extend  12  xlen  imm12

    rd_val  = alu_op  xlen  rs1_val  s_imm
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1

-- ================================================================
-- OP: ADD, SUB, SLT, SLTU, XOR, OR, AND, SLL, SRL, SRA
                                                                    -- \begin_latex{exec_ADD_1}
exec_ADD  :: Spec_Instr_I
exec_ADD   is_C  (ADD    rd  rs1  rs2)  mstate = exec_OP  alu_add   is_C  rd  rs1  rs2  mstate
                                                                    -- \end_latex{exec_ADD_1}

exec_SUB  :: Spec_Instr_I
exec_SUB   is_C  (SUB    rd  rs1  rs2)  mstate = exec_OP  alu_sub   is_C  rd  rs1  rs2  mstate

exec_SLT  :: Spec_Instr_I
exec_SLT   is_C  (SLT    rd  rs1  rs2)  mstate = exec_OP  alu_slt   is_C  rd  rs1  rs2  mstate

exec_SLTU :: Spec_Instr_I
exec_SLTU  is_C  (SLTU   rd  rs1  rs2)  mstate = exec_OP  alu_sltu  is_C  rd  rs1  rs2  mstate

exec_XOR  :: Spec_Instr_I
exec_XOR   is_C  (XOR    rd  rs1  rs2)  mstate = exec_OP  alu_xor   is_C  rd  rs1  rs2  mstate

exec_OR   :: Spec_Instr_I
exec_OR    is_C  (OR     rd  rs1  rs2)  mstate = exec_OP  alu_or    is_C  rd  rs1  rs2  mstate

exec_AND  :: Spec_Instr_I
exec_AND   is_C  (AND    rd  rs1  rs2)  mstate = exec_OP  alu_and   is_C  rd  rs1  rs2  mstate

exec_SLL  :: Spec_Instr_I
exec_SLL   is_C  (SLL    rd  rs1  rs2)  mstate = exec_OP  alu_sll   is_C  rd  rs1  rs2  mstate

exec_SRL  :: Spec_Instr_I
exec_SRL   is_C  (SRL    rd  rs1  rs2)  mstate = exec_OP  alu_srl   is_C  rd  rs1  rs2  mstate

exec_SRA  :: Spec_Instr_I
exec_SRA   is_C  (SRA    rd  rs1  rs2)  mstate = exec_OP  alu_sra   is_C  rd  rs1  rs2  mstate

                                                                    -- \begin_latex{spec_ADD_2}
exec_OP :: (Int -> Integer -> Integer -> Integer) ->
           Bool ->
           GPR_Addr ->
           GPR_Addr ->
           GPR_Addr ->
           Machine_State -> Machine_State
exec_OP    alu_op  is_C  rd  rs1  rs2  mstate =
  let
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read   mstate  rs1
    rs2_val = mstate_gpr_read   mstate  rs2
    rd_val  = alu_op  xlen  rs1_val  rs2_val
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    mstate1                                                         -- \end_latex{exec_ADD_2}

-- ================================================================
-- MISC_MEM: FENCE
-- This is technically an architectural 'no-op', but it can modify
-- hidden micro-arch state that affects future memory ops

exec_FENCE  :: Spec_Instr_I
exec_FENCE   is_C  (FENCE  fm  pred  succ)  mstate =
  let
    mstate1 = mstate_mem_fence  mstate
    mstate2 = finish_pc_incr  mstate1  is_C
  in
    mstate2

-- ================================================================
-- ECALL

exec_ECALL :: Spec_Instr_I
exec_ECALL    is_C  (ECALL)  mstate =
  let
    priv = mstate_priv_read  mstate
    exc_code | priv == m_Priv_Level = exc_code_ECall_from_M
             | priv == s_Priv_Level = exc_code_ECall_from_S
             | priv == u_Priv_Level = exc_code_ECall_from_U
             | True                 = error ("Illegal priv " ++ show (priv))
    tval = 0

    mstate1 = finish_trap  mstate  exc_code  tval
  in
    mstate1

-- ================================================================
-- EBREAK

exec_EBREAK :: Spec_Instr_I
exec_EBREAK    is_C  (EBREAK)  mstate =
  let
    exc_code = exc_code_breakpoint
    tval     = mstate_pc_read  mstate
    mstate1  = finish_trap  mstate  exc_code  tval
  in
    mstate1

-- ================================================================

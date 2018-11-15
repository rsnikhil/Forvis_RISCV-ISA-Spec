-- Copyright (c) 2018 Rishiyur S. Nikhil
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

import Forvis_Spec_Finish_Instr     -- Canonical ways for finish an instruction

-- ================================================================
-- 'I' Base instruction set

-- NOTE: opcode_XXX, funct3_XXX are defined in module Arch_Defs

-- ================================================================
-- LUI

spec_LUI :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_LUI    mstate           instr    is_C =
  let
    -- Instr fields: U-type
    (imm20, rd, opcode) = ifields_U_type  instr

    -- Decode check
    is_legal = (opcode == opcode_LUI)

    -- Semantics
    xlen   = mstate_xlen_read  mstate
    imm32  = shiftL  imm20  12
    rd_val = sign_extend  32  xlen  imm32

    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    (is_legal, mstate1)

-- ================================================================
-- AUIPC

spec_AUIPC :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_AUIPC    mstate           instr    is_C =
  let
    -- Instr fields: U-type
    (imm20, rd, opcode) = ifields_U_type  instr

    -- Decode check
    is_legal = (opcode == opcode_AUIPC)

    -- Semantics
    xlen     = mstate_xlen_read  mstate
    pc       = mstate_pc_read  mstate
    imm32    = shiftL  imm20  12
    s_offset = sign_extend  32  xlen  imm32
    rd_val   = alu_add  xlen  pc  s_offset

    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    (is_legal, mstate1)

-- ================================================================
-- JAL

spec_JAL :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_JAL    mstate           instr    is_C =
  let
    -- Instr fields: J-type
    (imm21, rd, opcode) = ifields_J_type  instr

    -- Decode check
    is_legal = (opcode == opcode_JAL)

    -- Semantics
    rv     = mstate_rv_read    mstate
    misa   = mstate_csr_read   mstate  csr_addr_misa
    xlen   = mstate_xlen_read  mstate
    pc     = mstate_pc_read    mstate
    rd_val = if is_C then pc + 2 else pc + 4

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
    (is_legal, mstate1)

-- ================================================================
-- JALR

spec_JALR :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_JALR    mstate           instr    is_C =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    is_legal = ((   opcode == opcode_JALR)
                && (funct3 == funct3_JALR))

    -- Semantics
    rv     = mstate_rv_read    mstate
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

    mstate1 = if aligned
              then
                finish_rd_and_pc  mstate  rd  rd_val  new_pc'
              else
                finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc'
  in
    (is_legal, mstate1)

-- ================================================================
-- BRANCH: BEQ, BNE, BLT, BGE, BLTU, BGEU

spec_BRANCH :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_BRANCH    mstate           instr    is_C =
  let
    -- Instr fields: B-type
    (imm13, rs2, rs1, funct3, opcode) = ifields_B_type  instr

    -- Decode check
    is_BEQ   = (funct3 == funct3_BEQ)
    is_BNE   = (funct3 == funct3_BNE)
    is_BLT   = (funct3 == funct3_BLT)
    is_BGE   = (funct3 == funct3_BGE)
    is_BLTU  = (funct3 == funct3_BLTU)
    is_BGEU  = (funct3 == funct3_BGEU)
    is_legal = ((opcode == opcode_BRANCH)
                && (is_BEQ
                    || is_BNE
                    || is_BLT
                    || is_BGE
                    || is_BLTU
                    || is_BGEU))

    -- Semantics
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read   mstate  rs1
    rs2_val = mstate_gpr_read   mstate  rs2

    taken | is_BEQ  = alu_eq   xlen  rs1_val  rs2_val
          | is_BNE  = alu_ne   xlen  rs1_val  rs2_val
          | is_BLT  = alu_lt   xlen  rs1_val  rs2_val
          | is_BGE  = alu_ge   xlen  rs1_val  rs2_val
          | is_BLTU = alu_ltu  xlen  rs1_val  rs2_val
          | is_BGEU = alu_geu  xlen  rs1_val  rs2_val

    pc      = mstate_pc_read  mstate

    s_offset = sign_extend  13  xlen  imm13

    target  = alu_add  xlen  pc  s_offset
    new_pc  = if taken then     target
              else if is_C then pc + 2
                   else         pc + 4
    misa    = mstate_csr_read  mstate  csr_addr_misa
    -- new_pc[0] known to be 0, new_pc[1] must be 0 if 'C' is not supported
    aligned = (misa_flag  misa  'C' ||  (new_pc .&. 0x2 == 0))
          
    mstate1 = if aligned
              then
                finish_pc  mstate  new_pc
              else
                finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc
  in
    (is_legal, mstate1)

-- ================================================================
-- LOAD:
--    RV32: LB, LH, LW, LBU, LHU
--    RV64: LWU, LD

spec_LOAD :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_LOAD    mstate           instr    is_C =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
    is_LB    = (funct3 == funct3_LB)
    is_LH    = (funct3 == funct3_LH)
    is_LW    = (funct3 == funct3_LW)
    is_LD    = (funct3 == funct3_LD)
    is_LBU   = (funct3 == funct3_LBU)
    is_LHU   = (funct3 == funct3_LHU)
    is_LWU   = (funct3 == funct3_LWU)
    is_legal = ((opcode == opcode_LOAD)
                && (is_LB
                    || is_LH
                    || is_LW
                    || (is_LD && (rv == RV64))
                    || is_LBU
                    || is_LHU
                    || (is_LWU && (rv == RV64))))
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

                Mem_Result_Ok  d    ->
                  let rd_val | is_LB = sign_extend  8   xlen  d
                             | is_LH = sign_extend  16  xlen  d
                             | is_LW = sign_extend  32  xlen  d
                             | True  = d
                  in
                    finish_rd_and_pc_incr  mstate2  rd  rd_val  is_C
  in
    (is_legal, mstate3)

-- ================================================================
-- STORE:
--    RV32: SB, SH, SW
--    RV64: SD

spec_STORE :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_STORE    mstate           instr    is_C =
  let
    -- Instr fields: S-type
    (imm12, rs2, rs1, funct3, opcode) = ifields_S_type  instr

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
    is_SB    = (funct3 == funct3_SB)
    is_SH    = (funct3 == funct3_SH)
    is_SW    = (funct3 == funct3_SW)
    is_SD    = ((funct3 == funct3_SD) && (rv == RV64))
    is_legal = ((opcode == opcode_STORE)
                && (is_SB
                    || is_SH
                    || is_SW
                    || is_SD))

    -- Semantics
    rs2_val = mstate_gpr_read  mstate  rs2    -- store value

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
-- OP_IMM: ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI

spec_OP_IMM :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_OP_IMM    mstate           instr    is_C =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr
    (msbs7, shamt5) = ifields_I_type_imm12_32  imm12
    (msbs6, shamt6) = ifields_I_type_imm12_64  imm12

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
    is_ADDI  = (funct3 == funct3_ADDI)
    is_SLTI  = (funct3 == funct3_SLTI)
    is_SLTIU = (funct3 == funct3_SLTIU)
    is_XORI  = (funct3 == funct3_XORI)
    is_ORI   = (funct3 == funct3_ORI)
    is_ANDI  = (funct3 == funct3_ANDI)
    is_SLLI  = ((funct3 == funct3_SLLI) && ((   (rv == RV32) && (msbs7 == msbs7_SLLI))
                                            || ((rv == RV64) && (msbs6 == msbs6_SLLI))))
    is_SRLI  = ((funct3 == funct3_SRLI) && ((   (rv == RV32) && (msbs7 == msbs7_SRLI))
                                            || ((rv == RV64) && (msbs6 == msbs6_SRLI))))
    is_SRAI  = ((funct3 == funct3_SRAI) && ((   (rv == RV32) && (msbs7 == msbs7_SRAI))
                                            || ((rv == RV64) && (msbs6 == msbs6_SRAI))))

    is_legal = ((opcode == opcode_OP_IMM)
                && (is_ADDI
                    || is_SLTI
                    || is_SLTIU
                    || is_XORI
                    || is_ORI
                    || is_ANDI
                    || is_SLLI
                    || is_SRLI
                    || is_SRAI
                   ))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1

    s_imm12 = sign_extend  12  xlen  imm12

    rd_val | is_ADDI  = alu_add   xlen  rs1_val  s_imm12
           | is_SLTI  = alu_slt   xlen  rs1_val  s_imm12
           | is_SLTIU = alu_sltu  xlen  rs1_val  s_imm12
           | is_XORI  = alu_xor   xlen  rs1_val  s_imm12
           | is_ORI   = alu_or    xlen  rs1_val  s_imm12
           | is_ANDI  = alu_and   xlen  rs1_val  s_imm12
           | is_SLLI  = alu_sll   xlen  rs1_val  imm12
           | is_SRLI  = alu_srl   xlen  rs1_val  imm12
           | is_SRAI  = alu_sra   xlen  rs1_val  imm12

    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    (is_legal, mstate1)

-- ================================================================
-- OP: ADD, SUB, SLT, SLTU, XOR, OR, AND, SLL, SRL, SRA
                                                                    -- \begin_latex{spec_ADD_1}
spec_OP :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_OP    mstate           instr    is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    is_ADD   = ((funct3 == funct3_ADD)  && (funct7 == funct7_ADD))
    is_SUB   = ((funct3 == funct3_SUB)  && (funct7 == funct7_SUB))
                                                                    -- \end_latex{spec_ADD_1}
    is_SLT   = ((funct3 == funct3_SLT)  && (funct7 == funct7_SLT))
    is_SLTU  = ((funct3 == funct3_SLTU) && (funct7 == funct7_SLTU))
    is_XOR   = ((funct3 == funct3_XOR)  && (funct7 == funct7_XOR))
    is_OR    = ((funct3 == funct3_OR)   && (funct7 == funct7_OR))
    is_AND   = ((funct3 == funct3_AND)  && (funct7 == funct7_AND))
    is_SLL   = ((funct3 == funct3_SLL)  && (funct7 == funct7_SLL))
    is_SRL   = ((funct3 == funct3_SRL)  && (funct7 == funct7_SRL))
    is_SRA   = ((funct3 == funct3_SRA)  && (funct7 == funct7_SRA))  -- \begin_latex{spec_ADD_2}
    is_legal = ((opcode == opcode_OP)
                && (is_ADD
                    || is_SUB
                    || is_SLT                                       -- \end_latex{spec_ADD_2}
                    || is_SLTU
                    || is_XOR
                    || is_OR
                    || is_AND
                    || is_SLL
                    || is_SRL
                    || is_SRA
                   ))
                                                                    -- \begin_latex{spec_ADD_3}
    -- Semantics
    xlen    = mstate_xlen_read  mstate                              -- \end_latex{spec_ADD_3}
    rs1_val = mstate_gpr_read   mstate  rs1
    rs2_val = mstate_gpr_read   mstate  rs2
                                                                    -- \begin_latex{spec_ADD_4}
    rd_val | is_ADD  = alu_add  xlen  rs1_val  rs2_val
           | is_SUB  = alu_sub  xlen  rs1_val  rs2_val
           | is_SLT  = alu_slt  xlen  rs1_val  rs2_val
           | is_SLTU = alu_sltu xlen  rs1_val  rs2_val
                                                                    -- \end_latex{spec_ADD_4}
           | is_XOR  = alu_xor  xlen  rs1_val  rs2_val
           | is_OR   = alu_or   xlen  rs1_val  rs2_val
           | is_AND  = alu_and  xlen  rs1_val  rs2_val
           | is_SLL  = alu_sll  xlen  rs1_val  rs2_val
           | is_SRL  = alu_srl  xlen  rs1_val  rs2_val
           | is_SRA  = alu_sra  xlen  rs1_val  rs2_val

                                                                    -- \begin_latex{spec_ADD_5}
    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    (is_legal, mstate1)
                                                                    -- \end_latex{spec_ADD_5}
-- ================================================================
-- MISC_MEM: FENCE, FENCE.I
-- These are technically architectural 'no-ops', but they can modify
-- hidden micro-arch state that affects future memory ops

spec_MISC_MEM :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_MISC_MEM    mstate           instr    is_C =
  let
    -- Instr fields: R-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type  instr

    (msbs4, pred, succ) = ifields_I_type_imm12_FENCE  imm12

    -- Decode check
    is_FENCE   = ((funct3 == funct3_FENCE)   && (rd == 0) && (rs1 == 0) && (msbs4 == 0))
    is_FENCE_I = ((funct3 == funct3_FENCE_I) && (rd == 0) && (rs1 == 0) && (imm12 == 0))
    is_legal   = ((opcode == opcode_MISC_MEM)
                  && (is_FENCE
                      || is_FENCE_I))

    -- Semantics
    mstate1 | is_FENCE   = mstate_mem_fence    mstate
            | is_FENCE_I = mstate_mem_fence_i  mstate

    mstate2 = finish_pc_incr  mstate1  is_C
  in
    (is_legal, mstate2)

-- ================================================================
-- SYSTEM instructions in Base Instruction Set:
--    PRIV:  ECALL, EBREAK
--    other: CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI

-- ----------------
-- SYSTEM.PRIV.ECALL

spec_SYSTEM_ECALL :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_SYSTEM_ECALL    mstate           instr    is_C =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct12 == funct12_ECALL)
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    priv = mstate_priv_read  mstate
    exc_code | priv == m_Priv_Level = exc_code_ECall_from_M
             | priv == s_Priv_Level = exc_code_ECall_from_S
             | priv == u_Priv_Level = exc_code_ECall_from_U
             | True                 = error ("Illegal priv " ++ show (priv))
    tval = 0

    mstate1 = finish_trap  mstate  exc_code  tval
  in
    (is_legal, mstate1)

-- ----------------
-- SYSTEM.PRIV.EBREAK

spec_SYSTEM_EBREAK :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_SYSTEM_EBREAK    mstate           instr    is_C =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct12 == funct12_EBREAK)
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    exc_code = exc_code_breakpoint
    tval     = mstate_pc_read  mstate

    mstate1 = finish_trap  mstate  exc_code  tval
  in
    (is_legal, mstate1)

-- ----------------
-- SYSTEM.not PRIV: CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI

spec_SYSTEM_CSRRW :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_SYSTEM_CSRRW    mstate           instr    is_C =
  let
    -- Instr fields: I-Type
    (csr_addr, rs1, funct3, rd, opcode) = ifields_I_type   instr
    zimm = rs1

    -- Decode check
    is_CSRRW  = (funct3 == funct3_CSRRW)
    is_CSRRWI = (funct3 == funct3_CSRRWI)
    is_legal  = ((opcode == opcode_SYSTEM)
                 && (is_CSRRW
                     || is_CSRRWI))

    -- Semantics
    priv       = mstate_priv_read  mstate
    permission = mstate_csr_read_permission  mstate  priv  csr_addr

    legal2 = (permission == CSR_Permission_RW)

    -- Read CSR only if rd is not 0
    old_csr_val = if (rd /= 0) then
                    if (csr_addr == csr_addr_time) then
                      let
                        -- CSR TIME is a read-only shadow of MTIME,
                        -- which is actually a memory-mapped location,
                        -- not a CSR
                        mtime = mstate_mem_read_mtime  mstate
                      in
                        mtime
                    else
                      mstate_csr_read  mstate  csr_addr
                  else
                    0    -- arbitrary; will be discarded (rd==0)

    rs1_val     = mstate_gpr_read  mstate  rs1

    new_csr_val | is_CSRRW  = rs1_val
                | is_CSRRWI = rs1

    rd_val      = old_csr_val

    mstate1 = if legal2 then
                -- FCSR consists of two sub-CSRs, there is a special function
                -- to handle writes to FCSR
                let
                  mstate_a = mstate_csr_write  mstate  csr_addr  new_csr_val
                in
                  finish_rd_and_pc_incr  mstate_a  rd  rd_val  is_C
              else
                let tval = instr
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    (is_legal, mstate1)

spec_SYSTEM_CSRR_S_C :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_SYSTEM_CSRR_S_C    mstate           instr    is_C =
  let
    -- Instr fields: I-Type
    (csr_addr, rs1, funct3, rd, opcode) = ifields_I_type   instr
    zimm = rs1

    -- Decode check
    is_CSRRS  = (funct3 == funct3_CSRRS)
    is_CSRRC  = (funct3 == funct3_CSRRC)
    is_CSRRSI = (funct3 == funct3_CSRRSI)
    is_CSRRCI = (funct3 == funct3_CSRRCI)
    is_legal  = ((opcode == opcode_SYSTEM)
                 && (is_CSRRS
                     || is_CSRRC
                     || is_CSRRSI
                     || is_CSRRCI))

    -- Semantics
    priv       = mstate_priv_read  mstate
    permission = mstate_csr_read_permission  mstate  priv  csr_addr

    legal2 | (permission == CSR_Permission_None) = False
           | (permission == CSR_Permission_RO)   = (rs1 == 0)
           | (permission == CSR_Permission_RW)   = True

    old_csr_val = mstate_csr_read  mstate  csr_addr
    rs1_val     = mstate_gpr_read  mstate  rs1

    new_csr_val | is_CSRRS  = old_csr_val .|. rs1_val
                | is_CSRRC  = old_csr_val .&. (complement rs1_val)
                | is_CSRRSI = old_csr_val .|. rs1
                | is_CSRRCI = old_csr_val .&. (complement rs1)
    rd_val      = old_csr_val

    mstate1 = if legal2 then
                -- Write CSR only if rs1/zimm is not 0
                let mstate_a | (rs1 /= 0) = mstate_csr_write  mstate  csr_addr  new_csr_val
                             | True       = mstate
                in
                  finish_rd_and_pc_incr  mstate_a  rd  rd_val  is_C
              else
                let tval = instr
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-IMM-32: ADDIW, SLLIW, SRLIW, SRAIW

spec_OP_IMM_32 :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_OP_IMM_32    mstate           instr    is_C =
  let
    -- Instr fields: R-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type  instr
    (funct7, shamt_5) = ifields_I_type_imm12_32  imm12

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
    is_ADDIW = ( funct3 == funct3_ADDIW)
    is_SLLIW = ((funct3 == funct3_SLLIW) && (funct7 == funct7_SLLIW))
    is_SRLIW = ((funct3 == funct3_SRLIW) && (funct7 == funct7_SRLIW))
    is_SRAIW = ((funct3 == funct3_SRAIW) && (funct7 == funct7_SRAIW))
    is_legal = ((rv == RV64)
                && (opcode == opcode_OP_IMM_32)
                && (is_ADDIW
                    || is_SLLIW
                    || is_SRLIW
                    || is_SRAIW
                   ))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1

    rd_val | is_ADDIW = alu_addw  rs1_val  (sign_extend  12  xlen  imm12)
           | is_SLLIW = alu_sllw  rs1_val  shamt_5
           | is_SRLIW = alu_srlw  rs1_val  shamt_5
           | is_SRAIW = alu_sraw  rs1_val  shamt_5

    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-32: for RV64: ADDW, SUBW, SLLW, SRLW, SRAW

spec_OP_32 :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_OP_32    mstate           instr    is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    rv      = mstate_rv_read  mstate
    is_ADDW = ((funct3 == funct3_ADDW) && (funct7 == funct7_ADDW))
    is_SUBW = ((funct3 == funct3_SUBW) && (funct7 == funct7_SUBW))
    is_SLLW = ((funct3 == funct3_SLLW) && (funct7 == funct7_SLLW))
    is_SRLW = ((funct3 == funct3_SRLW) && (funct7 == funct7_SRLW))
    is_SRAW = ((funct3 == funct3_SRAW) && (funct7 == funct7_SRAW))
    is_legal = ((rv == RV64)
                && (opcode == opcode_OP_32)
                && (is_ADDW
                    || is_SUBW
                    || is_SLLW
                    || is_SRLW
                    || is_SRAW))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2

    rd_val | is_ADDW = alu_addw  rs1_val  rs2_val
           | is_SUBW = alu_subw  rs1_val  rs2_val
           | is_SLLW = alu_sllw  rs1_val  rs2_val
           | is_SRLW = alu_srlw  rs1_val  rs2_val
           | is_SRAW = alu_sraw  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_incr  mstate  rd  rd_val  is_C
  in
    (is_legal, mstate1)

-- ================================================================

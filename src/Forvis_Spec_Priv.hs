-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Priv where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V Privileged Architecture Instructions

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Project imports

import Bit_Utils
import Arch_Defs
import Machine_State
import CSR_File

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- Data structure for instructions in Privileged Arch

data Instr_Priv = URET
                | SRET
                | MRET
                | WFI
                | SFENCE_VMA  GPR_Addr  GPR_Addr    -- rs1  rs2
  deriving (Eq, Show)

-- ================================================================
-- Sub-opcodes for 'Priv' instructions
-- NOTE: opcode_SYSTEM is defined in module Arch_Defs

-- opcode_SYSTEM sub-opcodes
funct12_URET      = 0x002 :: InstrField    -- 12'b_0000_0000_0010
funct12_SRET      = 0x102 :: InstrField    -- 12'b_0001_0000_0010
funct12_MRET      = 0x302 :: InstrField    -- 12'b_0011_0000_0010
funct12_WFI       = 0x105 :: InstrField    -- 12'b_0001_0000_0101
funct7_SFENCE_VMA = 0x09  :: InstrField    --  7'b_000_1001

-- ================================================================
-- Decode from 32b representation to Instr_I data structure

decode_Priv :: RV -> Instr_32b -> Maybe Instr_Priv
decode_Priv    rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode  = bitSlice  instr_32b   6   0
    rd      = bitSlice  instr_32b  11   7
    funct3  = bitSlice  instr_32b  14  12
    rs1     = bitSlice  instr_32b  19  15
    rs2     = bitSlice  instr_32b  24  20
    funct7  = bitSlice  instr_32b  31  25
    imm12_I = bitSlice  instr_32b  31  20

    instr_Priv
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, imm12_I==funct12_URET = Just URET
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, imm12_I==funct12_SRET = Just SRET
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, imm12_I==funct12_MRET = Just MRET
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, imm12_I==funct12_WFI  = Just WFI
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, funct7==funct7_SFENCE_VMA     = Just (SFENCE_VMA  rs1  rs2)
      | True = Nothing
  in
    instr_Priv

-- ================================================================
-- Execution of Instr_Priv

type Spec_Instr_Priv = Bool -> Instr_Priv -> Machine_State -> Machine_State
--                     is_C    instr_Priv    mstate           mstate'

exec_instr_Priv :: Instr_32b -> Spec_Instr_Priv
exec_instr_Priv  instr_32b  is_C  instr_Priv  mstate =
  case instr_Priv of
    MRET                 -> exec_xRET        instr_32b  is_C  instr_Priv  mstate
    SRET                 -> exec_xRET        instr_32b  is_C  instr_Priv  mstate
    URET                 -> exec_xRET        instr_32b  is_C  instr_Priv  mstate
    WFI                  -> exec_WFI         instr_32b  is_C  instr_Priv  mstate
    SFENCE_VMA  rs1  rs2 -> exec_SFENCE_VMA  instr_32b  is_C  instr_Priv  mstate

-- ================================================================
-- MRET/SRET/URET

exec_xRET :: Instr_32b -> Spec_Instr_Priv
exec_xRET  instr_32b  is_C  instr_Priv  mstate =
  let
    is_MRET  = (instr_Priv == MRET)
    is_SRET  = (instr_Priv == SRET)
    is_URET  = (instr_Priv == URET)
    priv     = mstate_priv_read  mstate
    is_legal = ((    is_MRET && (priv == m_Priv_Level))
                 || (is_SRET && (priv >= s_Priv_Level))
                 || (is_URET && (priv >= u_Priv_Level)))

    mstatus   = mstate_csr_read  mstate  csr_addr_mstatus
    tsr_fault = (is_SRET && (priv == s_Priv_Level) && (testBit  mstatus  mstatus_tsr_bitpos))
    (mpp,spp,mpie,spie,upie,mie,sie,uie) = mstatus_stack_fields  mstatus
    rv        = mstate_rv_read   mstate
    misa      = mstate_csr_read  mstate  csr_addr_misa

    mstate3   = if (tsr_fault)
                then
                  let tval = instr_32b
                  in
                    finish_trap  mstate  exc_code_illegal_instruction  tval

                else
                  let
                    -- New 'previous-priv' is U if supported, else M
                    new_pp = if (misa_flag  misa  'U') then u_Priv_Level else m_Priv_Level

                    -- New priv, and new priv stack
                    (priv',mpp',spp')
                      -- From M
                      | (priv == m_Priv_Level) && is_MRET && (mpp == m_Priv_Level) = (m_Priv_Level, new_pp, spp)
                      | (priv == m_Priv_Level) && is_MRET && (mpp == s_Priv_Level) = (s_Priv_Level, new_pp, spp)
                      | (priv == m_Priv_Level) && is_MRET && (mpp == u_Priv_Level) = (u_Priv_Level, new_pp, spp)

                      | (priv == m_Priv_Level) && is_SRET && (spp == s_Priv_Level) = (s_Priv_Level, new_pp, spp)
                      | (priv == m_Priv_Level) && is_SRET && (spp == u_Priv_Level) = (u_Priv_Level, new_pp, spp)

                      | (priv == m_Priv_Level) && is_URET                          = (u_Priv_Level, new_pp, spp)

                      -- From S
                      | (priv == s_Priv_Level) && is_SRET && (spp == s_Priv_Level) = (s_Priv_Level, mpp, new_pp)
                      | (priv == s_Priv_Level) && is_SRET && (spp == u_Priv_Level) = (u_Priv_Level, mpp, new_pp)

                      | (priv == s_Priv_Level) && is_URET                          = (u_Priv_Level, mpp, new_pp)

                      -- From U
                      | (priv == u_Priv_Level) && is_URET                          = (u_Priv_Level, mpp, spp)

                    -- New interrupt-enable stack in new mstatus
                    (mpie',spie',upie',mie',sie',uie') | is_MRET = (   1, spie, upie, mpie,  sie,   uie)
                                                       | is_SRET = (mpie,    1, upie,  mie, spie,   uie)
                                                       | is_URET = (mpie, spie,    1,  mie,  sie,  upie)
                    mstatus' = mstatus_upd_stack_fields  mstatus  (mpp',spp',mpie',spie',upie',mie',sie',uie')

                    -- New PC
                    pc1 | is_MRET = mstate_csr_read  mstate  csr_addr_mepc
                        | is_SRET = mstate_csr_read  mstate  csr_addr_sepc
                        | is_URET = mstate_csr_read  mstate  csr_addr_uepc
                    pc2 | (rv == RV32) = (pc1 .&. 0xFFFFFFFF)
                        | True         = pc1

                    -- Update arch state
                    mstate1 = mstate_csr_write   mstate   csr_addr_mstatus  mstatus'
                    mstate2 = mstate_priv_write  mstate1  priv'
                  in
                    finish_pc  mstate2  pc2
  in
    mstate3

-- ================================================================
-- WFI

exec_WFI :: Instr_32b -> Spec_Instr_Priv
exec_WFI  instr_32b  is_C  instr_Priv  mstate =
  let
    priv   = mstate_priv_read  mstate

    -- If mstatus.tw is set, illegal instruction trap after bounded timeout
    --     (here, the timeout is 0)
    -- Otherwise it's functionally a no-op
    --     Optionally: pause here in WFI state until interrupt
    mstatus    = mstate_csr_read   mstate  csr_addr_mstatus
    tw_bit_set = testBit  mstatus  mstatus_tw_bitpos
    mstate1    = if (tw_bit_set)
                 then
                   let tval = instr_32b
                   in
                     finish_trap  mstate  exc_code_illegal_instruction  tval
                 else
                   let
                     mstate' = mstate_run_state_write  mstate  Run_State_WFI
                   in
                     finish_pc_incr  mstate'  is_C
  in
    mstate1

-- ================================================================
-- SFENCE.VMA

exec_SFENCE_VMA :: Instr_32b -> Spec_Instr_Priv
exec_SFENCE_VMA  instr_32b  is_C  (SFENCE_VMA  rs1  rs2)  mstate =
  let
    priv   = mstate_priv_read  mstate

    is_legal = (priv >= s_Priv_Level)    -- TODO: allowed in m_Priv_Level?

    -- Functionally a no-op, but can change micro-arch state to affect future mem ops
    rs1_val   = mstate_gpr_read  mstate  rs1
    rs2_val   = mstate_gpr_read  mstate  rs2
    mstatus   = mstate_csr_read  mstate  csr_addr_mstatus
    tvm_fault = testBit  mstatus  mstatus_tvm_bitpos

    mstate2   = if (tvm_fault)
                then
                  let tval = instr_32b
                  in
                    finish_trap  mstate  exc_code_illegal_instruction  tval
                else
                  let
                    mstate1 = mstate_mem_sfence_vma  mstate  rs1_val  rs2_val
                  in
                    finish_pc_incr  mstate1  is_C
  in
    mstate2

-- ================================================================

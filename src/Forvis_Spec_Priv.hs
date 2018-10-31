-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Priv where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V Privileged Architecture Instructions

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import Arch_Defs
import Machine_State
import CSR_File

import Forvis_Spec_Finish_Instr     -- Canonical ways for finish an instruction

-- ================================================================
-- SYSTEM.PRIV.MRET/SRET/URET

funct12_URET     = 0x002 :: InstrField    -- 12'b_0000_0000_0010
funct12_SRET     = 0x102 :: InstrField    -- 12'b_0001_0000_0010
funct12_MRET     = 0x302 :: InstrField    -- 12'b_0011_0000_0010

spec_SYSTEM_xRET :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_SYSTEM_xRET    mstate           instr    is_C =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    priv   = mstate_priv_read  mstate

    -- Decode check
    is_MRET = (funct12 == funct12_MRET)
    is_SRET = (funct12 == funct12_SRET)
    is_URET = (funct12 == funct12_URET)
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && ((is_MRET && (priv == m_Priv_Level))
                     || (is_SRET && (priv >= s_Priv_Level))
                     || (is_URET && (priv >= u_Priv_Level)))
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    mstatus   = mstate_csr_read  mstate  csr_addr_mstatus
    tsr_fault = (is_SRET && (priv == s_Priv_Level) && (testBit  mstatus  mstatus_tsr_bitpos))
    (mpp,spp,mpie,spie,upie,mie,sie,uie) = mstatus_stack_fields  mstatus
    rv        = mstate_rv_read   mstate
    misa      = mstate_csr_read  mstate  csr_addr_misa

    mstate3   = if (tsr_fault)
                then
                  let tval = instr
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
    (is_legal, mstate3)

-- ================================================================
-- SYSTEM.PRIV.WFI

funct12_WFI = 0x105 :: InstrField    -- 12'b_0001_0000_0101

spec_SYSTEM_WFI :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_SYSTEM_WFI    mstate           instr    is_C =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    priv   = mstate_priv_read  mstate

    -- Decode check    TODO: if priv is u_Priv_Level, misa.N must be set (supporting user-mode interrupts)
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct12 == funct12_WFI)
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    -- If mstatus.tw is set, illegal instruction trap after bounded timeout
    --     (here, the timeout is 0)
    -- Otherwise it's functionally a no-op
    --     Optionally: pause here in WFI state until interrupt
    mstatus    = mstate_csr_read   mstate  csr_addr_mstatus
    tw_bit_set = testBit  mstatus  mstatus_tw_bitpos
    mstate1    = if (tw_bit_set)
                 then
                   let tval = instr
                   in
                     finish_trap  mstate  exc_code_illegal_instruction  tval
                 else
                   let
                     mstate' = mstate_run_state_write  mstate  Run_State_WFI
                   in
                     finish_pc_incr  mstate'  is_C
  in
    (is_legal, mstate1)

-- ================================================================
-- SYSTEM.PRIV.SFENCE.VM

funct7_SFENCE_VM = 0x09 :: InstrField    --  7'b_000_1001

spec_SYSTEM_SFENCE_VM :: Machine_State -> Instr -> Bool -> (Bool, Machine_State)
spec_SYSTEM_SFENCE_VM    mstate           instr    is_C =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type   instr

    priv   = mstate_priv_read  mstate

    -- Decode check
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct7 == funct7_SFENCE_VM)
                && (rd == 0)
                && (priv >= s_Priv_Level))    -- TODO: allowed in m_Priv_Level?

    -- Semantics
    -- Functionally a no-op, but can change micro-arch state to affect future mem ops
    rs1_val   = mstate_gpr_read  mstate  rs1
    rs2_val   = mstate_gpr_read  mstate  rs2
    mstatus   = mstate_csr_read  mstate  csr_addr_mstatus
    tvm_fault = testBit  mstatus  mstatus_tvm_bitpos

    mstate2   = if (tvm_fault)
                then
                  let tval = instr
                  in
                    finish_trap  mstate  exc_code_illegal_instruction  tval
                else
                  let
                    mstate1 = mstate_mem_sfence_vm  mstate  rs1_val  rs2_val
                  in
                    finish_pc_incr  mstate1  is_C
  in
    (is_legal, mstate2)

-- ================================================================

-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Zicsr where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'Zicsr' instructions

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import Arch_Defs
import Machine_State
import CSR_File

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- 'Zicsr' instruction set

-- NOTE: opcode_XXX are defined in module Arch_Defs

-- ================================================================
-- Data structure for instructions in 'Zicsr'

data Instr_Zicsr = CSRRW   GPR_Addr  GPR_Addr  CSR_Addr    -- rd, rs1,  csr_addr
                 | CSRRS   GPR_Addr  GPR_Addr  CSR_Addr    -- rd, rs1,  csr_addr
                 | CSRRC   GPR_Addr  GPR_Addr  CSR_Addr    -- rd, rs1,  csr_addr
                 | CSRRWI  GPR_Addr  GPR_Addr  CSR_Addr    -- rd, zimm, csr_addr
                 | CSRRSI  GPR_Addr  GPR_Addr  CSR_Addr    -- rd, zimm, csr_addr
                 | CSRRCI  GPR_Addr  GPR_Addr  CSR_Addr    -- rd, zimm, csr_addr
  deriving (Eq, Show)

-- ================================================================
-- Decode constants for 'Zicsr' instructions

-- opcode_SYSTEM sub-opcodes
funct3_CSRRW     = 0x1   :: InstrField    -- 3'b_001
funct3_CSRRWI    = 0x5   :: InstrField    -- 3'b_101
funct3_CSRRS     = 0x2   :: InstrField    -- 3'b_010
funct3_CSRRC     = 0x3   :: InstrField    -- 3'b_011
funct3_CSRRSI    = 0x6   :: InstrField    -- 3'b_110
funct3_CSRRCI    = 0x7   :: InstrField    -- 3'b_111

-- ================================================================
-- Decode from 32b representation to Instr_Zicsr data structure

decode_Zicsr :: RV -> Instr_32b -> Maybe Instr_Zicsr
decode_Zicsr    rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode  = bitSlice  instr_32b   6   0
    rd      = bitSlice  instr_32b  11   7
    funct3  = bitSlice  instr_32b  14  12
    rs1     = bitSlice  instr_32b  19  15
    imm12_I = bitSlice  instr_32b  31  20

    instr_Zicsr
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRW  = Just  (CSRRW  rd rs1 imm12_I)
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRS  = Just  (CSRRS  rd rs1 imm12_I)
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRC  = Just  (CSRRC  rd rs1 imm12_I)
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRWI = Just  (CSRRWI rd rs1 imm12_I)
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRSI = Just  (CSRRSI rd rs1 imm12_I)
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRCI = Just  (CSRRCI rd rs1 imm12_I)
      | True = Nothing
  in
    instr_Zicsr

-- ================================================================
-- Execution of Instr_Zicsr

type Spec_Instr_Zicsr = Instr_32b -> Bool -> Instr_Zicsr -> Machine_State -> Machine_State
--                      instr_32b    is_C    instr_Zicsr    mstate           mstate'

exec_instr_Zicsr :: Spec_Instr_Zicsr
exec_instr_Zicsr  instr_32b  is_C  instr_Zicsr  mstate =
  case instr_Zicsr of
    CSRRW   rd  rs1  csr_addr -> exec_CSRRW   instr_32b  is_C  instr_Zicsr  mstate
    CSRRWI  rd  rs1  zimm     -> exec_CSRRWI  instr_32b  is_C  instr_Zicsr  mstate
    CSRRS   rd  rs1  csr_addr -> exec_CSRRS   instr_32b  is_C  instr_Zicsr  mstate
    CSRRSI  rd  rs1  zimm     -> exec_CSRRSI  instr_32b  is_C  instr_Zicsr  mstate
    CSRRC   rd  rs1  csr_addr -> exec_CSRRC   instr_32b  is_C  instr_Zicsr  mstate
    CSRRCI  rd  rs1  zimm     -> exec_CSRRCI  instr_32b  is_C  instr_Zicsr  mstate

-- ================================================================
-- CSRRW, CSRRWI

exec_CSRRW :: Spec_Instr_Zicsr
exec_CSRRW  instr_32b  is_C  (CSRRW  rd  rs1  csr_addr)  mstate =
  exec_CSRR_W  instr_32b  funct3_CSRRW  is_C  rd  rs1  csr_addr  mstate

exec_CSRRWI :: Spec_Instr_Zicsr
exec_CSRRWI  instr_32b  is_C  (CSRRWI  rd  zimm  csr_addr)  mstate =
  exec_CSRR_W  instr_32b  funct3_CSRRWI  is_C  rd  zimm  csr_addr  mstate

exec_CSRR_W :: Instr_32b ->
               InstrField ->
               Bool ->
               CSR_Addr ->
               InstrField ->
               CSR_Addr ->
               Machine_State -> Machine_State
exec_CSRR_W  instr_32b  funct3  is_C  rd  rs1  csr_addr  mstate =
  let
    priv       = mstate_priv_read  mstate
    permission = mstate_csr_read_permission  mstate  priv  csr_addr
    legal2     = (permission == CSR_Permission_RW)

    -- Read CSR only if rd is not 0
    -- TODO: mstate_csr_read may have side effects; should return new mstate
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
    new_csr_val | (funct3 == funct3_CSRRW)  = rs1_val
                | (funct3 == funct3_CSRRWI) = rs1
    rd_val      = old_csr_val

    mstate1 = if legal2 then
                let
                  mstate_a = mstate_csr_write  mstate  csr_addr  new_csr_val
                in
                  finish_rd_and_pc_incr  mstate_a  rd  rd_val  is_C
              else
                let tval = instr_32b
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    mstate1

-- ================================================================
-- CSRRS, CSRRSI, CSRRC,CSRRCI

exec_CSRRS :: Spec_Instr_Zicsr
exec_CSRRS  instr_32b  is_C  (CSRRS  rd  rs1  csr_addr)  mstate =
  exec_CSRR_S_C  instr_32b  funct3_CSRRS  is_C  rd  rs1  csr_addr  mstate

exec_CSRRSI :: Spec_Instr_Zicsr
exec_CSRRSI  instr_32b  is_C  (CSRRSI  rd  zimm  csr_addr)  mstate =
  exec_CSRR_S_C  instr_32b  funct3_CSRRSI  is_C  rd  zimm  csr_addr  mstate

exec_CSRRC :: Spec_Instr_Zicsr
exec_CSRRC  instr_32b  is_C  (CSRRC  rd  rs1  csr_addr)  mstate =
  exec_CSRR_S_C  instr_32b  funct3_CSRRC  is_C  rd  rs1  csr_addr  mstate

exec_CSRRCI :: Spec_Instr_Zicsr
exec_CSRRCI  instr_32b  is_C  (CSRRCI  rd  zimm  csr_addr)  mstate =
  exec_CSRR_S_C  instr_32b  funct3_CSRRCI  is_C  rd  zimm  csr_addr  mstate

exec_CSRR_S_C :: Instr_32b ->
                 InstrField ->
                 Bool ->
                 CSR_Addr ->
                 InstrField ->
                 CSR_Addr ->
                 Machine_State -> Machine_State
exec_CSRR_S_C  instr_32b  funct3  is_C  rd  rs1  csr_addr  mstate =
  let
    priv       = mstate_priv_read  mstate
    permission = mstate_csr_read_permission  mstate  priv  csr_addr

    legal2 | (permission == CSR_Permission_None) = False
           | (permission == CSR_Permission_RO)   = (rs1 == 0)
           | (permission == CSR_Permission_RW)   = True

    -- TODO: mstate_csr_read can have side effects: should return new state
    old_csr_val = mstate_csr_read  mstate  csr_addr
    rs1_val     = mstate_gpr_read  mstate  rs1

    new_csr_val | (funct3 == funct3_CSRRS)  = old_csr_val .|. rs1_val
                | (funct3 == funct3_CSRRC)  = old_csr_val .&. (complement rs1_val)
                | (funct3 == funct3_CSRRSI) = old_csr_val .|. rs1
                | (funct3 == funct3_CSRRCI) = old_csr_val .&. (complement rs1)
    rd_val      = old_csr_val

    mstate1 = if legal2 then
                -- Write CSR only if rs1/zimm is not 0
                let mstate_a | (rs1 /= 0) = mstate_csr_write  mstate  csr_addr  new_csr_val
                             | True       = mstate
                in
                  finish_rd_and_pc_incr  mstate_a  rd  rd_val  is_C
              else
                let tval = instr_32b
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    mstate1

-- ================================================================

-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Zifencei where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- This module is the specification of the RISC-V 'Zifencei' instructions

-- ================================================================
-- Haskell lib imports

-- none

-- Local imports

import Bit_Utils
import Arch_Defs
import Machine_State

import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- ================================================================
-- 'Zifencei' instruction set

-- NOTE: opcode_XXX are defined in module Arch_Defs

-- ================================================================
-- Data structure for instructions in 'Zifencei'

data Instr_Zifencei = FENCE_I
  deriving (Eq, Show)

-- ================================================================
-- Decode constants for 'Zifencei' instructions

funct3_FENCE_I   = 0x1   :: InstrField    -- 3'b_001

-- ================================================================
-- Decode from 32b representation to Instr_Zifencei data structure

decode_Zifencei :: RV -> Instr_32b -> Maybe Instr_Zifencei
decode_Zifencei    rv    instr_32b =
  let
    -- Symbolic names for notable bitfields in the 32b instruction 'instr_32b'
    opcode  = bitSlice  instr_32b   6   0
    rd      = bitSlice  instr_32b  11   7
    funct3  = bitSlice  instr_32b  14  12
    rs1     = bitSlice  instr_32b  19  15
    imm12_I = bitSlice  instr_32b  31  20

    instr_Zifencei
      | opcode==opcode_MISC_MEM, imm12_I==0, rs1==0, funct3==funct3_FENCE_I, rd==0 = Just FENCE_I
      | True = Nothing
  in
    instr_Zifencei

-- ================================================================
-- Execution of Instr_Zifencei

type Spec_Instr_Zifencei = Bool -> Instr_Zifencei -> Machine_State -> Machine_State
--                         is_C    instr_Zifencei    mstate           mstate'

exec_instr_Zifencei :: Spec_Instr_Zifencei
exec_instr_Zifencei  is_C  instr_Zifencei  mstate =
  case instr_Zifencei of
    FENCE_I -> exec_FENCE_I  is_C  instr_Zifencei  mstate

-- ================================================================
-- MISC_MEM: FENCE
-- This is technically an architectural 'no-op', but it can modify
-- hidden micro-arch state that affects future memory ops

exec_FENCE_I :: Spec_Instr_Zifencei
exec_FENCE_I  is_C  (FENCE_I)  mstate =
  let
    mstate1 = mstate_mem_fence_i  mstate
    mstate2 = finish_pc_incr  mstate1  is_C
  in
    mstate2

-- ================================================================

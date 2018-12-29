-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Mem_Ops where

-- ================================================================
-- This module defines instruction field values that specify the type
-- and size of memory operations.

-- Note: these are duplicates of defs in Forvis_Spec.hs where they are
-- used in the specs of LOAD, STORE and AMO instructions.  They are
-- repeated here because this information is also needed by memory and
-- I/O servers, and by top-level execution and debug wrappers.

-- Forvis_Spec.hs could have just imported this module, but these defs
-- are repeated there for local, self-contained readability.

-- ================================================================
-- Standard Haskell imports

import Data.Word
import Data.Bits

-- Project imports

import ALU
import Arch_Defs

-- ================================================================
-- NOTE: the following are defined in module Arch_Defs:
--     opcode_LOAD, funct3_LB/LH/LW/LD/LBU/LHU/LWU
--     opcode_STORE, funct3_SB/SH/SW/SD
--     opcode_AMO, funct3_AMO_W/D, msbs5_AMO_LR/SC/ADD/SWAP/XOR/AND/OR/MIN/MAX/MINU/MAXU

-- ================================================================
-- Definitions within opcode_LOAD

is_LOAD_aligned :: InstrField -> Integer -> Bool
is_LOAD_aligned  funct3  addr = ((    (funct3 == funct3_LB) || (funct3 == funct3_LBU))
                                 || (((funct3 == funct3_LH) || (funct3 == funct3_LHU)) && ((addr .&. 0x1) == 0))
                                 || (((funct3 == funct3_LW) || (funct3 == funct3_LWU)) && ((addr .&. 0x3) == 0))
                                 || ( (funct3 == funct3_LD)                            && ((addr .&. 0x7) == 0)))

-- ================================================================
-- Definitions within opcode_STORE

is_STORE_aligned :: InstrField -> Integer -> Bool
is_STORE_aligned  funct3  addr = ((    funct3 == funct3_SB)
                                  || ((funct3 == funct3_SH) && ((addr .&. 0x1) == 0))
                                  || ((funct3 == funct3_SW) && ((addr .&. 0x3) == 0))
                                  || ((funct3 == funct3_SD) && ((addr .&. 0x7) == 0)))

-- ================================================================
-- Definitions within opcode_AMO

is_AMO_aligned :: InstrField -> Integer -> Bool
is_AMO_aligned  funct3  addr = ((   (funct3 == funct3_AMO_W) && ((addr .&. 0x3) == 0))
                                || ((funct3 == funct3_AMO_D) && ((addr .&. 0x7) == 0)))

-- ================================================================
-- ALU for AMO ops
-- Computes new_mem_value from op, store_value and old_mem_value

alu_amo_op  :: InstrField ->    -- funct3: AMO_W or AMO_D
               InstrField ->    -- msbs5:  SC/SWAP/ADD/AND/OR/XOR/MAX/MIN/MAXU/MINU
               Integer    ->    -- store-value
               Integer    ->    -- old mem-value
               Integer          -- new-mem-value

alu_amo_op  funct3  msbs5  store_val  old_mem_val =
  let
    xlen = if (funct3 == funct3_AMO_W) then 32 else 64

    -- New memory value (to be stored back)
    new_mem_val = (if       (msbs5 == msbs5_AMO_SC)   then store_val
                    else if (msbs5 == msbs5_AMO_SWAP) then store_val
                    else if (msbs5 == msbs5_AMO_ADD)  then alu_add  xlen  old_mem_val  store_val
                    else if (msbs5 == msbs5_AMO_AND)  then old_mem_val  .&.  store_val
                    else if (msbs5 == msbs5_AMO_OR)   then old_mem_val  .|.  store_val
                    else if (msbs5 == msbs5_AMO_XOR)  then xor  old_mem_val  store_val
                    else if (msbs5 == msbs5_AMO_MAX)  then (if alu_ge  xlen  old_mem_val  store_val then
                                                              old_mem_val
                                                            else
                                                              store_val)
                    else if (msbs5 == msbs5_AMO_MIN)  then (if alu_lt  xlen  old_mem_val  store_val then
                                                              old_mem_val
                                                            else
                                                              store_val)
                    else if (msbs5 == msbs5_AMO_MAXU) then (if alu_geu  xlen  old_mem_val  store_val then
                                                              old_mem_val
                                                            else
                                                              store_val)
                    else if (msbs5 == msbs5_AMO_MINU) then (if alu_ltu  xlen  old_mem_val  store_val then
                                                              old_mem_val
                                                            else
                                                              store_val)
                    else error ("alu_amo_op: unknown msbs5: " ++ show msbs5))
  in
    new_mem_val

-- ================================================================

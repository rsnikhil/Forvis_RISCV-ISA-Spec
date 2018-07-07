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

-- Project imports

import Arch_Defs

-- ================================================================
-- Definitions within opcode_LOAD

funct3_LB  = 0x0 :: InstrField     -- 3'b_000
funct3_LH  = 0x1 :: InstrField     -- 3'b_001
funct3_LW  = 0x2 :: InstrField     -- 3'b_010
funct3_LD  = 0x3 :: InstrField     -- 3'b_011
funct3_LBU = 0x4 :: InstrField     -- 3'b_100
funct3_LHU = 0x5 :: InstrField     -- 3'b_101
funct3_LWU = 0x6 :: InstrField     -- 3'b_110

-- ================================================================
-- Definitions within opcode_STORE

funct3_SB  = 0x0 :: InstrField     -- 3'b_000
funct3_SH  = 0x1 :: InstrField     -- 3'b_001
funct3_SW  = 0x2 :: InstrField     -- 3'b_010
funct3_SD  = 0x3 :: InstrField     -- 3'b_011

-- ================================================================
-- Definitions within opcode_AMO

funct3_AMO_W   = 0x2 :: InstrField     -- 3'b010
funct3_AMO_D   = 0x3 :: InstrField     -- 3'b011

msbs5_AMO_LR   = 0x02 :: InstrField    -- 5'b00010
msbs5_AMO_SC   = 0x03 :: InstrField    -- 5'b00011
msbs5_AMO_ADD  = 0x00 :: InstrField    -- 5'b00000
msbs5_AMO_SWAP = 0x01 :: InstrField    -- 5'b00001
msbs5_AMO_XOR  = 0x04 :: InstrField    -- 5'b00100
msbs5_AMO_AND  = 0x0C :: InstrField    -- 5'b01100
msbs5_AMO_OR   = 0x08 :: InstrField    -- 5'b01000
msbs5_AMO_MIN  = 0x10 :: InstrField    -- 5'b10000
msbs5_AMO_MAX  = 0x14 :: InstrField    -- 5'b10100
msbs5_AMO_MINU = 0x18 :: InstrField    -- 5'b11000
msbs5_AMO_MAXU = 0x1C :: InstrField    -- 5'b11100

-- ================================================================

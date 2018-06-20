-- See LICENSE for license details

module Mem_Ops where

-- ================================================================
-- This module defines instruction field values that specify the type
-- and size of memory operations.

-- Note: these are duplicates of defs in Forvis_Spec.hs where they are
-- used to decode LOAD, STORE and SMO instructions.  They are repeated
-- here because this information is also needed by memory and I/O
-- servers, and by top-level execution and debug wrappers.

-- Forvis_Spec.hs could have just imported this module, but these defs
-- are repeated there for local, self-contained readability.

-- ================================================================
-- Standard Haskell imports

import Data.Word

-- Project imports

import Arch_Defs

-- ================================================================
-- Definitions within opcode_LOAD

funct3_LB  :: InstrField;    funct3_LB  = 0x0     -- 3'b_000
funct3_LH  :: InstrField;    funct3_LH  = 0x1     -- 3'b_001
funct3_LW  :: InstrField;    funct3_LW  = 0x2     -- 3'b_010
funct3_LD  :: InstrField;    funct3_LD  = 0x3     -- 3'b_011
funct3_LBU :: InstrField;    funct3_LBU = 0x4     -- 3'b_100
funct3_LHU :: InstrField;    funct3_LHU = 0x5     -- 3'b_101
funct3_LWU :: InstrField;    funct3_LWU = 0x6     -- 3'b_110

-- ================================================================
-- Definitions within opcode_STORE

funct3_SB :: InstrField;    funct3_SB = 0x0     -- 3'b_000
funct3_SH :: InstrField;    funct3_SH = 0x1     -- 3'b_001
funct3_SW :: InstrField;    funct3_SW = 0x2     -- 3'b_010
funct3_SD :: InstrField;    funct3_SD = 0x3     -- 3'b_011

-- ================================================================
-- Definitions within opcode_AMO

funct3_AMO_W   :: InstrField;    funct3_AMO_W   = 0x2     -- 3'b010
funct3_AMO_D   :: InstrField;    funct3_AMO_D   = 0x3     -- 3'b011

msbs5_AMO_LR   :: InstrField;    msbs5_AMO_LR   = 0x02    -- 5'b00010;
msbs5_AMO_SC   :: InstrField;    msbs5_AMO_SC   = 0x03    -- 5'b00011;
msbs5_AMO_ADD  :: InstrField;    msbs5_AMO_ADD  = 0x00    -- 5'b00000;
msbs5_AMO_SWAP :: InstrField;    msbs5_AMO_SWAP = 0x01    -- 5'b00001;
msbs5_AMO_XOR  :: InstrField;    msbs5_AMO_XOR  = 0x04    -- 5'b00100;
msbs5_AMO_AND  :: InstrField;    msbs5_AMO_AND  = 0x0C    -- 5'b01100;
msbs5_AMO_OR   :: InstrField;    msbs5_AMO_OR   = 0x08    -- 5'b01000;
msbs5_AMO_MIN  :: InstrField;    msbs5_AMO_MIN  = 0x10    -- 5'b10000;
msbs5_AMO_MAX  :: InstrField;    msbs5_AMO_MAX  = 0x14    -- 5'b10100;
msbs5_AMO_MINU :: InstrField;    msbs5_AMO_MINU = 0x18    -- 5'b11000;
msbs5_AMO_MAXU :: InstrField;    msbs5_AMO_MAXU = 0x1C    -- 5'b11100;

-- ================================================================

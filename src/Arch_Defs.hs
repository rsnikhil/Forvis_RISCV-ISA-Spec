-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Arch_Defs where

-- ================================================================
-- This module has basic, RISC-V definitions (such RV32 vs. RV64,
-- bit-fields of instructions and standard CSRs, etc.) on which the
-- rest of the architectural state and operations depend.

-- ================================================================
-- Standard Haskell imports

import Data.Bits
import Control.Exception (assert)

-- Project imports

import Bit_Utils

-- ================================================================
-- Major architectural parameters                  -- \begin_latex{RV}
-- Future: add RV128

data RV = RV32
        | RV64
        deriving (Eq, Show)
                                                   -- \end_latex{RV}
-- ================================================================
-- Instructions and instruction fields                \begin_latex{Instr}
-- These are just synonyms of 'Integer', for readability

type Instr_32b = Integer
type Instr_16b = Integer

type InstrField = Integer      -- various fields of instructions

-- General-purpose registers

type GPR_Addr = Integer        -- 5-bit addrs, 0..31
type GPR_Val  = Integer        -- 32-bit or 64-bit values

-- Floating-point registers

type FPR_Addr = Integer        -- 5-bit addrs, 0..31

-- CSRs

type CSR_Addr = Integer        -- 12-bit addrs, 0..0xFFF
                                                   -- \end_latex{Instr}
-- ================================================================
-- Predicate to decide whether the arg may be a 'C' (Compressed)    -- \begin_latex{is_instr_C}
-- instruction or not ('C' instrs have 2 lsbs not equal to 2'b11)

is_instr_C :: Integer -> Bool
is_instr_C  u16 = ((u16 .&. 0x3) /= 0x3)

{-# INLINE is_instr_C #-}
                                                                    -- \end_latex{is_instr_C}

-- ================================================================ -- \begin_latex{Major_Opcodes}
-- Major opcodes

-- ----------------
-- 'I' (Base instruction set)

opcode_LUI       = 0x37  :: InstrField    -- 7'b_01_101_11
opcode_AUIPC     = 0x17  :: InstrField    -- 7'b_00_101_11
opcode_JAL       = 0x6F  :: InstrField    -- 7'b_11_011_11
opcode_JALR      = 0x67  :: InstrField    -- 7'b_11_001_11
opcode_BRANCH    = 0x63  :: InstrField    -- 7'b_11_000_11          -- \end_latex{...Major_Opcodes}
opcode_LOAD      = 0x03  :: InstrField    -- 7'b_00_000_11
opcode_STORE     = 0x23  :: InstrField    -- 7'b_01_000_11
opcode_OP_IMM    = 0x13  :: InstrField    -- 7'b_00_100_11
opcode_OP        = 0x33  :: InstrField    -- 7'b_01_100_11
opcode_MISC_MEM  = 0x0F  :: InstrField    -- 7'b_00_011_11
opcode_OP_IMM_32 = 0x1B  :: InstrField    -- 7'b_00_110_11
opcode_OP_32     = 0x3B  :: InstrField    -- 7'b_01_110_11
opcode_SYSTEM    = 0x73  :: InstrField    -- 7'b_11_100_11

-- ----------------
-- Whereas most sub-opcodes are defined locally within
-- Forvis_Spec_*.hs modules, these are defined here because they are
-- also used in the memory and memory-mapped IO modules.

-- opcode_LOAD sub-opcodes
funct3_LB        = 0x0   :: InstrField    -- 3'b_000
funct3_LH        = 0x1   :: InstrField    -- 3'b_001
funct3_LW        = 0x2   :: InstrField    -- 3'b_010
funct3_LD        = 0x3   :: InstrField    -- 3'b_011
funct3_LBU       = 0x4   :: InstrField    -- 3'b_100
funct3_LHU       = 0x5   :: InstrField    -- 3'b_101
funct3_LWU       = 0x6   :: InstrField    -- 3'b_110

-- opcode_STORE sub-opcodes
funct3_SB        = 0x0   :: InstrField    -- 3'b_000
funct3_SH        = 0x1   :: InstrField    -- 3'b_001
funct3_SW        = 0x2   :: InstrField    -- 3'b_010
funct3_SD        = 0x3   :: InstrField    -- 3'b_011

-- opcode_SYSTEM sub-opcodes
funct3_PRIV      = 0x0   :: InstrField    -- 3'b_000

-- ================================================================
-- 'M' extension    (Integer Multiply/Divide)

-- opcode_OP     same as in base instruction set
-- opcode_OP_32  same as in base instruction set

-- ================================================================
-- 'A' extension    (Atomics)

opcode_AMO       = 0x2F  :: InstrField    -- 7'b_01_011_11

-- ----------------
-- Whereas most sub-opcodes are defined locally within
-- Forvis_Spec_*.hs modules, these are defined here because they are
-- also used in the memory and memory-mapped IO modules.

funct3_AMO_W     = 0x2   :: InstrField    -- 3'b010
funct3_AMO_D     = 0x3   :: InstrField    -- 3'b011
msbs5_AMO_LR     = 0x02  :: InstrField    -- 5'b00010
msbs5_AMO_SC     = 0x03  :: InstrField    -- 5'b00011
msbs5_AMO_ADD    = 0x00  :: InstrField    -- 5'b00000
msbs5_AMO_SWAP   = 0x01  :: InstrField    -- 5'b00001
msbs5_AMO_XOR    = 0x04  :: InstrField    -- 5'b00100
msbs5_AMO_AND    = 0x0C  :: InstrField    -- 5'b01100
msbs5_AMO_OR     = 0x08  :: InstrField    -- 5'b01000
msbs5_AMO_MIN    = 0x10  :: InstrField    -- 5'b10000
msbs5_AMO_MAX    = 0x14  :: InstrField    -- 5'b10100
msbs5_AMO_MINU   = 0x18  :: InstrField    -- 5'b11000
msbs5_AMO_MAXU   = 0x1C  :: InstrField    -- 5'b11100

-- ================================================================
-- 'F' and 'D' extensions (single- and double-precision Floating Point)

opcode_LOAD_FP   = 0x07   :: InstrField  -- 7'b_00_001_11
opcode_STORE_FP  = 0x27   :: InstrField  -- 7'b_01_001_11
opcode_FMADD     = 0x43   :: InstrField  -- 7'b_100_0011
opcode_FMSUB     = 0x47   :: InstrField  -- 7'b_100_0111
opcode_FNMSUB    = 0x4B   :: InstrField  -- 7'b_100_1011
opcode_FNMADD    = 0x4F   :: InstrField  -- 7'b_100_1111
opcode_OP_FP     = 0x53   :: InstrField  -- 7'b_10_100_11

-- ================================================================
-- Exception Codes                                     -- \begin_latex{exception_codes_A}

type Exc_Code = Integer

-- Interrupt exception codes
exc_code_u_software_interrupt      =  0 :: Exc_Code
exc_code_s_software_interrupt      =  1 :: Exc_Code
exc_code_m_software_interrupt      =  3 :: Exc_Code    -- \end_latex{...exception_codes_A}
exc_code_u_timer_interrupt         =  4 :: Exc_Code
exc_code_s_timer_interrupt         =  5 :: Exc_Code
exc_code_m_timer_interrupt         =  7 :: Exc_Code
exc_code_u_external_interrupt      =  8 :: Exc_Code
exc_code_s_external_interrupt      =  9 :: Exc_Code
exc_code_m_external_interrupt      = 11 :: Exc_Code
                                                       -- \begin_latex{exception_codes_B}
-- Trap exception codes
exc_code_instr_addr_misaligned     =  0 :: Exc_Code
exc_code_instr_access_fault        =  1 :: Exc_Code
exc_code_illegal_instruction       =  2 :: Exc_Code    -- \end_latex{...exception_codes_B}
exc_code_breakpoint                =  3 :: Exc_Code
exc_code_load_addr_misaligned      =  4 :: Exc_Code
exc_code_load_access_fault         =  5 :: Exc_Code
exc_code_store_AMO_addr_misaligned =  6 :: Exc_Code
exc_code_store_AMO_access_fault    =  7 :: Exc_Code
exc_code_ECall_from_U              =  8 :: Exc_Code
exc_code_ECall_from_S              =  9 :: Exc_Code
exc_code_ECall_from_M              = 11 :: Exc_Code
exc_code_Instruction_Page_Fault    = 12 :: Exc_Code
exc_code_Load_Page_Fault           = 13 :: Exc_Code
exc_code_Store_AMO_Page_Fault      = 15 :: Exc_Code

-- ================
-- For debugging

show_interrupt_exc_code :: Exc_Code -> String
show_interrupt_exc_code  ec  | (ec == exc_code_u_software_interrupt) = "exc_code_u_software_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_s_software_interrupt) = "exc_code_s_software_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_m_software_interrupt) = "exc_code_m_software_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_u_timer_interrupt)    = "exc_code_u_timer_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_s_timer_interrupt)    = "exc_code_s_timer_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_m_timer_interrupt)    = "exc_code_m_timer_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_u_external_interrupt) = "exc_code_u_external_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_s_external_interrupt) = "exc_code_s_external_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_m_external_interrupt) = "exc_code_m_external_interrupt"


show_trap_exc_code :: Exc_Code -> String
show_trap_exc_code  ec  | (ec == exc_code_instr_addr_misaligned)     = "exc_code_instr_addr_misaligned"
show_trap_exc_code  ec  | (ec == exc_code_instr_access_fault)        = "exc_code_instr_access_fault"
show_trap_exc_code  ec  | (ec == exc_code_illegal_instruction)       = "exc_code_illegal_instruction"
show_trap_exc_code  ec  | (ec == exc_code_breakpoint)                = "exc_code_breakpoint"
show_trap_exc_code  ec  | (ec == exc_code_load_addr_misaligned)      = "exc_code_load_addr_misaligned"
show_trap_exc_code  ec  | (ec == exc_code_load_access_fault)         = "exc_code_load_access_fault"
show_trap_exc_code  ec  | (ec == exc_code_store_AMO_addr_misaligned) = "exc_code_store_AMO_addr_misaligned"
show_trap_exc_code  ec  | (ec == exc_code_store_AMO_access_fault)    = "exc_code_store_AMO_access_fault"
show_trap_exc_code  ec  | (ec == exc_code_ECall_from_U)              = "exc_code_ECall_from_U"
show_trap_exc_code  ec  | (ec == exc_code_ECall_from_S)              = "exc_code_ECall_from_S"
show_trap_exc_code  ec  | (ec == exc_code_ECall_from_M)              = "exc_code_ECall_from_M"
show_trap_exc_code  ec  | (ec == exc_code_Instruction_Page_Fault)    = "exc_code_Instruction_Page_Fault"
show_trap_exc_code  ec  | (ec == exc_code_Load_Page_Fault)           = "exc_code_Load_Page_Fault"
show_trap_exc_code  ec  | (ec == exc_code_Store_AMO_Page_Fault)      = "exc_code_Store_AMO_Page_Fault"

-- ================================================================
-- Memory access results                           \begin_latex{Mem_Result}
-- Either Ok with value, or Err with an exception code

data Mem_Result = Mem_Result_Ok   Integer
                | Mem_Result_Err  Exc_Code
                deriving (Show)                 -- \end_latex{Mem_Result}

-- ================================================================
-- Privilege levels                                \begin_latex{Priv_Level}
-- Machine, Supervisor and User

type Priv_Level = InstrField

m_Priv_Level  = 3 :: Priv_Level
s_Priv_Level  = 1 :: Priv_Level
u_Priv_Level  = 0 :: Priv_Level
                                                -- \end_latex{Priv_Level}

-- ================================================================

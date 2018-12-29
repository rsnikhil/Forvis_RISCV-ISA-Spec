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
import Data.Char
import Control.Exception (assert)

-- Project imports

import Bit_Utils

-- ================================================================
-- Major architectural parameters                  -- \begin_latex{RV}

data RV = RV32
        | RV64
        deriving (Eq, Show)
                                                   -- \end_latex{RV}
-- ================================================================
-- Predicate to decide whether the arg may be a 'C' (Compressed)
-- instruction or not ('C' instrs have 2 lsbs not equal to 2'b11)

{-# INLINE is_instr_C #-}
is_instr_C :: Integer -> Bool
is_instr_C  u16 = ((u16 .&. 0x3) /= 0x3)

-- ================================================================
-- Instructions and instruction fields                \begin_latex{Instr}
-- These are just synonyms of 'Integer', for readability

type Instr_32b = Integer
type Instr_16b = Integer

type InstrField = Integer

-- General-purpose registers

type GPR_Addr = Integer
type GPR_Val  = Integer

-- Floating-point registers

type FPR_Addr = Integer

-- CSRs

type CSR_Addr = Integer
                                                   -- \end_latex{Instr}
-- ================================================================
-- Major and minor opcodes

-- ----------------
-- 'I' (Base instruction set)

opcode_LUI       = 0x37  :: InstrField    -- 7'b_01_101_11
opcode_AUIPC     = 0x17  :: InstrField    -- 7'b_00_101_11
opcode_JAL       = 0x6F  :: InstrField    -- 7'b_11_011_11

opcode_JALR      = 0x67  :: InstrField    -- 7'b_11_001_11
funct3_JALR      = 0x0   :: InstrField    -- 3'b_000

opcode_BRANCH    = 0x63  :: InstrField    -- 7'b_11_000_11
funct3_BEQ       = 0x0  :: InstrField     -- 3'b_000
funct3_BNE       = 0x1  :: InstrField     -- 3'b_001
funct3_BLT       = 0x4  :: InstrField     -- 3'b_100
funct3_BGE       = 0x5  :: InstrField     -- 3'b_101
funct3_BLTU      = 0x6  :: InstrField     -- 3'b_110
funct3_BGEU      = 0x7  :: InstrField     -- 3'b_111

opcode_LOAD      = 0x03  :: InstrField    -- 7'b_00_000_11
funct3_LB        = 0x0   :: InstrField    -- 3'b_000
funct3_LH        = 0x1   :: InstrField    -- 3'b_001
funct3_LW        = 0x2   :: InstrField    -- 3'b_010
funct3_LD        = 0x3   :: InstrField    -- 3'b_011
funct3_LBU       = 0x4   :: InstrField    -- 3'b_100
funct3_LHU       = 0x5   :: InstrField    -- 3'b_101
funct3_LWU       = 0x6   :: InstrField    -- 3'b_110

opcode_STORE     = 0x23  :: InstrField    -- 7'b_01_000_11
funct3_SB        = 0x0   :: InstrField    -- 3'b_000
funct3_SH        = 0x1   :: InstrField    -- 3'b_001
funct3_SW        = 0x2   :: InstrField    -- 3'b_010
funct3_SD        = 0x3   :: InstrField    -- 3'b_011

opcode_OP_IMM    = 0x13  :: InstrField    -- 7'b_00_100_11
funct3_ADDI      = 0x0   :: InstrField    -- 3'b_000
funct3_SLTI      = 0x2   :: InstrField    -- 3'b_010
funct3_SLTIU     = 0x3   :: InstrField    -- 3'b_011
funct3_XORI      = 0x4   :: InstrField    -- 3'b_100
funct3_ORI       = 0x6   :: InstrField    -- 3'b_110
funct3_ANDI      = 0x7   :: InstrField    -- 3'b_111
funct3_SLLI      = 0x1   :: InstrField    -- 3'b_001
funct3_SRLI      = 0x5   :: InstrField    -- 3'b_101
funct3_SRAI      = 0x5   :: InstrField    -- 3'b_101

-- OP_IMM.SLLI/SRLI/SRAI for RV32
msbs7_SLLI       = 0x00  :: InstrField    -- 7'b_000_0000
msbs7_SRLI       = 0x00  :: InstrField    -- 7'b_000_0000
msbs7_SRAI       = 0x20  :: InstrField    -- 7'b_010_0000

-- OP_IMM.SLLI/SRLI/SRAI for RV64
msbs6_SLLI       = 0x00  :: InstrField    -- 6'b_00_0000
msbs6_SRLI       = 0x00  :: InstrField    -- 6'b_00_0000
msbs6_SRAI       = 0x10  :: InstrField    -- 6'b_01_0000


opcode_OP        = 0x33  :: InstrField    -- 7'b_01_100_11
funct3_ADD       = 0x0   :: InstrField    -- 3'b_000
funct7_ADD       = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SUB       = 0x0   :: InstrField    -- 3'b_000
funct7_SUB       = 0x20  :: InstrField    -- 7'b_010_0000
funct3_SLT       = 0x2   :: InstrField    -- 3'b_010
funct7_SLT       = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SLTU      = 0x3   :: InstrField    -- 3'b_011
funct7_SLTU      = 0x00  :: InstrField    -- 7'b_000_0000
funct3_XOR       = 0x4   :: InstrField    -- 3'b_100
funct7_XOR       = 0x00  :: InstrField    -- 7'b_000_0000
funct3_OR        = 0x6   :: InstrField    -- 3'b_110
funct7_OR        = 0x00  :: InstrField    -- 7'b_000_0000
funct3_AND       = 0x7   :: InstrField    -- 3'b_111
funct7_AND       = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SLL       = 0x1   :: InstrField    -- 3'b_001
funct7_SLL       = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SRL       = 0x5   :: InstrField    -- 3'b_101
funct7_SRL       = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SRA       = 0x5   :: InstrField    -- 3'b_101
funct7_SRA       = 0x20  :: InstrField    -- 7'b_010_0000

opcode_MISC_MEM  = 0x0F  :: InstrField    -- 7'b_00_011_11
funct3_FENCE     = 0x0   :: InstrField    -- 3'b_000
funct3_FENCE_I   = 0x1   :: InstrField    -- 3'b_001

opcode_OP_IMM_32 = 0x1B  :: InstrField    -- 7'b_00_110_11
funct3_ADDIW     = 0x0   :: InstrField    -- 3'b_000
funct3_SLLIW     = 0x1   :: InstrField    -- 3'b_001
funct7_SLLIW     = 0x00   :: InstrField   -- 7'b_0000000
funct3_SRLIW     = 0x5   :: InstrField    -- 3'b_101
funct7_SRLIW     = 0x00   :: InstrField   -- 7'b_0000000
funct3_SRAIW     = 0x5   :: InstrField    -- 3'b_101
funct7_SRAIW     = 0x20   :: InstrField   -- 7'b_0100000

opcode_OP_32     = 0x3B  :: InstrField    -- 7'b_01_110_11
funct3_ADDW      = 0x0   :: InstrField    -- 3'b_000
funct7_ADDW      = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SUBW      = 0x0   :: InstrField    -- 3'b_000
funct7_SUBW      = 0x20  :: InstrField    -- 7'b_010_0000
funct3_SLLW      = 0x1   :: InstrField    -- 3'b_001
funct7_SLLW      = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SRLW      = 0x5   :: InstrField    -- 3'b_101
funct7_SRLW      = 0x00  :: InstrField    -- 7'b_000_0000
funct3_SRAW      = 0x5   :: InstrField    -- 3'b_101
funct7_SRAW      = 0x20  :: InstrField    -- 7'b_010_0000

-- SYSTEM: user level
opcode_SYSTEM    = 0x73  :: InstrField    -- 7'b_11_100_11
funct3_PRIV      = 0x0   :: InstrField    -- 3'b_000
funct12_ECALL    = 0x000 :: InstrField    -- 12'b_0000_0000_0000
funct12_EBREAK   = 0x001 :: InstrField    -- 12'b_0000_0000_0001
funct3_CSRRW     = 0x1   :: InstrField    -- 3'b_001
funct3_CSRRWI    = 0x5   :: InstrField    -- 3'b_101
funct3_CSRRS     = 0x2   :: InstrField    -- 3'b_010
funct3_CSRRC     = 0x3   :: InstrField    -- 3'b_011
funct3_CSRRSI    = 0x6   :: InstrField    -- 3'b_110
funct3_CSRRCI    = 0x7   :: InstrField    -- 3'b_111

-- SYSTEM: privileged
funct12_URET     = 0x002 :: InstrField    -- 12'b_0000_0000_0010
funct12_SRET     = 0x102 :: InstrField    -- 12'b_0001_0000_0010
funct12_MRET     = 0x302 :: InstrField    -- 12'b_0011_0000_0010
funct12_WFI      = 0x105 :: InstrField    -- 12'b_0001_0000_0101
funct7_SFENCE_VM = 0x09  :: InstrField    --  7'b_000_1001

-- ================================================================
-- 'M' extension

-- Shares opcode_OP with base instr set

funct3_MUL       = 0x0   :: InstrField    -- 3'b_000
funct7_MUL       = 0x01  :: InstrField    -- 7'b_000_0001
funct3_MULH      = 0x1   :: InstrField    -- 3'b_001
funct7_MULH      = 0x01  :: InstrField    -- 7'b_000_0001
funct3_MULHSU    = 0x2   :: InstrField    -- 3'b_010
funct7_MULHSU    = 0x01  :: InstrField    -- 7'b_000_0001
funct3_MULHU     = 0x3   :: InstrField    -- 3'b_011
funct7_MULHU     = 0x01  :: InstrField    -- 7'b_000_0001

funct3_DIV       = 0x4   :: InstrField    -- 3'b_100
funct7_DIV       = 0x01  :: InstrField    -- 7'b_000_0001
funct3_DIVU      = 0x5   :: InstrField    -- 3'b_101
funct7_DIVU      = 0x01  :: InstrField    -- 7'b_000_0001

funct3_REM       = 0x6   :: InstrField    -- 3'b_110
funct7_REM       = 0x01  :: InstrField    -- 7'b_000_0001
funct3_REMU      = 0x7   :: InstrField    -- 3'b_111
funct7_REMU      = 0x01  :: InstrField    -- 7'b_000_0001

-- Shares opcode_OP_32 with base instr set

funct3_MULW     = 0x0    :: InstrField    -- 3'b_000
funct7_MULW     = 0x01   :: InstrField    -- 7'b_000_0001
funct3_DIVW     = 0x4    :: InstrField    -- 3'b_100
funct7_DIVW     = 0x01   :: InstrField    -- 7'b_000_0001
funct3_DIVUW    = 0x5    :: InstrField    -- 3'b_101
funct7_DIVUW    = 0x01   :: InstrField    -- 7'b_000_0001
funct3_REMW     = 0x6    :: InstrField    -- 3'b_110
funct7_REMW     = 0x01   :: InstrField    -- 7'b_000_0001
funct3_REMUW    = 0x7    :: InstrField    -- 3'b_111
funct7_REMUW    = 0x01   :: InstrField    -- 7'b_000_0001

-- ================================================================
-- 'A' extension

opcode_AMO       = 0x2F  :: InstrField    -- 7'b_01_011_11
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
-- 'C' Extension ("Compressed") major opcodes ('quadrants' 0, 1 and 2)

opcode_C0 = 0x0 :: InstrField    -- 2'b00
opcode_C1 = 0x1 :: InstrField    -- 2'b01
opcode_C2 = 0x2 :: InstrField    -- 2'b10

funct3_C_LWSP     = 0x2 :: InstrField    -- 3'b_010
funct3_C_LDSP     = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128
funct3_C_LQSP     = 0x1 :: InstrField    -- 3'b_001     RV128
funct3_C_FLWSP    = 0x3 :: InstrField    -- 3'b_011     RV32FC
funct3_C_FLDSP    = 0x1 :: InstrField    -- 3'b_001     RV32DC, RV64DC

funct3_C_SWSP     = 0x6 :: InstrField    -- 3'b_110

funct3_C_SQSP     = 0x5 :: InstrField    -- 3'b_101     RV128
funct3_C_FSDSP    = 0x5 :: InstrField    -- 3'b_101     RV32DC, RV64DC

funct3_C_SDSP     = 0x7 :: InstrField    -- 3'b_111     RV64 and RV128
funct3_C_FSWSP    = 0x7 :: InstrField    -- 3'b_111     RV32FC

funct3_C_LQ       = 0x1 :: InstrField    -- 3'b_001     RV128
funct3_C_FLD      = 0x1 :: InstrField    -- 3'b_001     RV32DC, RV64DC

funct3_C_LW       = 0x2 :: InstrField    -- 3'b_010

funct3_C_LD       = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128
funct3_C_FLW      = 0x3 :: InstrField    -- 3'b_011     RV32FC

funct3_C_FSD      = 0x5 :: InstrField    -- 3'b_101     RV32DC, RV64DC
funct3_C_SQ       = 0x5 :: InstrField    -- 3'b_101     RV128

funct3_C_SW       = 0x6 :: InstrField    -- 3'b_110

funct3_C_SD       = 0x7 :: InstrField    -- 3'b_111     RV64 and RV128
funct3_C_FSW      = 0x7 :: InstrField    -- 3'b_111     RV32FC

funct3_C_JAL      = 0x1 :: InstrField    -- 3'b_001     RV32
funct3_C_J        = 0x5 :: InstrField    -- 3'b_101
funct3_C_BEQZ     = 0x6 :: InstrField    -- 3'b_110
funct3_C_BNEZ     = 0x7 :: InstrField    -- 3'b_111

funct4_C_JR       = 0x8 :: InstrField    -- 4'b_1000
funct4_C_JALR     = 0x9 :: InstrField    -- 4'b_1001

funct3_C_LI       = 0x2 :: InstrField    -- 3'b_010
funct3_C_LUI      = 0x3 :: InstrField    -- 3'b_011     RV64 and RV128

funct3_C_NOP      = 0x0 :: InstrField    -- 3'b_000
funct3_C_ADDI     = 0x0 :: InstrField    -- 3'b_000
funct3_C_ADDIW    = 0x1 :: InstrField    -- 3'b_001
funct3_C_ADDI16SP = 0x3 :: InstrField    -- 3'b_011
funct3_C_ADDI4SPN = 0x0 :: InstrField    -- 3'b_000
funct3_C_SLLI     = 0x0 :: InstrField    -- 3'b_000

funct3_C_SRLI     = 0x4 :: InstrField    -- 3'b_100
funct2_C_SRLI     = 0x0 :: InstrField    -- 2'b_00

funct3_C_SRAI     = 0x4 :: InstrField    -- 3'b_100
funct2_C_SRAI     = 0x1 :: InstrField    -- 2'b_01

funct3_C_ANDI     = 0x4 :: InstrField    -- 3'b_100
funct2_C_ANDI     = 0x2 :: InstrField    -- 2'b_10

funct4_C_MV       = 0x8 :: InstrField    -- 4'b_1000
funct4_C_ADD      = 0x9 :: InstrField    -- 4'b_1001

funct6_C_AND      = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_AND      = 0x3 :: InstrField    -- 2'b_11

funct6_C_OR       = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_OR       = 0x2 :: InstrField    -- 2'b_10

funct6_C_XOR      = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_XOR      = 0x1 :: InstrField    -- 2'b_01

funct6_C_SUB      = 0x23 :: InstrField   -- 6'b_100_0_11
funct2_C_SUB      = 0x0 :: InstrField    -- 2'b_01

funct6_C_ADDW     = 0x27 :: InstrField   -- 6'b_100_1_11
funct2_C_ADDW     = 0x1 :: InstrField    -- 2'b_01

funct6_C_SUBW     = 0x27 :: InstrField   -- 6'b_100_1_11
funct2_C_SUBW     = 0x0 :: InstrField    -- 2'b_00

funct4_C_EBREAK   = 0x9 :: InstrField    -- 4'b_1001

-- ================================================================
-- Exception Codes                                 \begin_latex{exception_codes_A}

type Exc_Code = Integer

exc_code_u_software_interrupt      =  0 :: Exc_Code
exc_code_s_software_interrupt      =  1 :: Exc_Code
                                                -- \end_latex{exception_codes_A}
exc_code_m_software_interrupt      =  3 :: Exc_Code
exc_code_u_timer_interrupt         =  4 :: Exc_Code
exc_code_s_timer_interrupt         =  5 :: Exc_Code
exc_code_m_timer_interrupt         =  7 :: Exc_Code
exc_code_u_external_interrupt      =  8 :: Exc_Code
exc_code_s_external_interrupt      =  9 :: Exc_Code
exc_code_m_external_interrupt      = 11 :: Exc_Code

                                                -- \begin_latex{exception_codes_B}
exc_code_instr_addr_misaligned     =  0 :: Exc_Code
exc_code_instr_access_fault        =  1 :: Exc_Code
                                                -- \end_latex{exception_codes_B}
exc_code_illegal_instruction       =  2 :: Exc_Code
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
                deriving (Show)
                                                -- \end_latex{Mem_Result}
-- ================================================================
-- Privilege levels                                \begin_latex{Priv_Level}
-- Machine, Supervisor and User

type Priv_Level = InstrField

m_Priv_Level  = 3 :: Priv_Level
s_Priv_Level  = 1 :: Priv_Level
u_Priv_Level  = 0 :: Priv_Level

                                                -- \end_latex{Priv_Level}
-- ================================================================
-- User-Level CSR addresses

csr_addr_ustatus    = 0x000 :: CSR_Addr
csr_addr_uie        = 0x004 :: CSR_Addr
csr_addr_utvec      = 0x005 :: CSR_Addr

csr_addr_uscratch   = 0x040 :: CSR_Addr
csr_addr_uepc       = 0x041 :: CSR_Addr
csr_addr_ucause     = 0x042 :: CSR_Addr
csr_addr_utval      = 0x043 :: CSR_Addr
csr_addr_uip        = 0x044 :: CSR_Addr

csr_addr_fflags     = 0x001 :: CSR_Addr
csr_addr_frm        = 0x002 :: CSR_Addr
csr_addr_fcsr       = 0x003 :: CSR_Addr

csr_addr_cycle      = 0xC00 :: CSR_Addr
csr_addr_time       = 0xC01 :: CSR_Addr
csr_addr_instret    = 0xC02 :: CSR_Addr

csr_addr_cycleh     = 0xC80 :: CSR_Addr
csr_addr_timeh      = 0xC81 :: CSR_Addr
csr_addr_instreth   = 0xC82 :: CSR_Addr

-- TODO: hpmcounterN, hpmcounterNh

-- The following list is in the order printed by print_CSR_File()

u_csr_addrs_and_names :: [(CSR_Addr, String)]
u_csr_addrs_and_names  =
  [ (csr_addr_ustatus,    "ustatus"),
    (csr_addr_uie,        "uie"),
    (csr_addr_uip,        "uip"),
    (csr_addr_time,       "time"),
    (csr_addr_timeh,      "timeh"),

    (csr_addr_utvec,      "utvec"),
    (csr_addr_uepc,       "uepc"),
    (csr_addr_ucause,     "ucause"),
    (csr_addr_utval,      "utval"),
    (csr_addr_uscratch,   "uscratch"),

    (csr_addr_cycle,      "cycle"),
    (csr_addr_cycleh,     "cycleh"),
    (csr_addr_instret,    "instret"),
    (csr_addr_instreth,   "instreth"),

    (csr_addr_fflags,     "fflags"),
    (csr_addr_frm,        "frm"),
    (csr_addr_fcsr,       "fcsr") ]

-- ================================================================
-- Supervisor-Level CSR addresses

csr_addr_sstatus    = 0x100 :: CSR_Addr
csr_addr_sedeleg    = 0x102 :: CSR_Addr
csr_addr_sideleg    = 0x103 :: CSR_Addr
csr_addr_sie        = 0x104 :: CSR_Addr
csr_addr_stvec      = 0x105 :: CSR_Addr
csr_addr_scounteren = 0x106 :: CSR_Addr

csr_addr_sscratch   = 0x140 :: CSR_Addr
csr_addr_sepc       = 0x141 :: CSR_Addr
csr_addr_scause     = 0x142 :: CSR_Addr
csr_addr_stval      = 0x143 :: CSR_Addr
csr_addr_sip        = 0x144 :: CSR_Addr

csr_addr_satp       = 0x180 :: CSR_Addr

-- The following list is in the order printed by print_CSR_File()

s_csr_addrs_and_names :: [(CSR_Addr, String)]
s_csr_addrs_and_names  =
  [ (csr_addr_sstatus,    "sstatus"),
    (csr_addr_sie,        "sie"),
    (csr_addr_sip,        "sip"),
    (csr_addr_sedeleg,    "sedeleg"),
    (csr_addr_sideleg,    "sideleg"),

    (csr_addr_stvec,      "stvec"),
    (csr_addr_sepc,       "sepc"),
    (csr_addr_scause,     "scause"),
    (csr_addr_stval,      "stval"),
    (csr_addr_sscratch,   "sscratch"),

    (csr_addr_satp,       "satp"),
    (csr_addr_scounteren, "scounteren") ]

-- ================================================================
-- Machine-Level CSR addresses

csr_addr_mvendorid  = 0xF11 :: CSR_Addr
csr_addr_marchid    = 0xF12 :: CSR_Addr
csr_addr_mimpid     = 0xF13 :: CSR_Addr
csr_addr_mhartid    = 0xF14 :: CSR_Addr

csr_addr_mstatus    = 0x300 :: CSR_Addr
csr_addr_misa       = 0x301 :: CSR_Addr
csr_addr_medeleg    = 0x302 :: CSR_Addr
csr_addr_mideleg    = 0x303 :: CSR_Addr
csr_addr_mie        = 0x304 :: CSR_Addr
csr_addr_mtvec      = 0x305 :: CSR_Addr
csr_addr_mcounteren = 0x306 :: CSR_Addr

csr_addr_mscratch   = 0x340 :: CSR_Addr
csr_addr_mepc       = 0x341 :: CSR_Addr
csr_addr_mcause     = 0x342 :: CSR_Addr
csr_addr_mtval      = 0x343 :: CSR_Addr
csr_addr_mip        = 0x344 :: CSR_Addr

-- TODO: pmpcfgN, pmpaddrN

csr_addr_mcycle     = 0xB00 :: CSR_Addr
csr_addr_minstret   = 0xB02 :: CSR_Addr

-- TODO: mhpmcounterN

csr_addr_mcycleh    = 0xB80 :: CSR_Addr
csr_addr_minstreth  = 0xB82 :: CSR_Addr

-- TODO: mhpmcounterNh

csr_addr_tselect    = 0x7A0 :: CSR_Addr
csr_addr_data1      = 0x7A1 :: CSR_Addr
csr_addr_data2      = 0x7A2 :: CSR_Addr
csr_addr_data3      = 0x7A3 :: CSR_Addr

csr_addr_dcsr       = 0x7B0 :: CSR_Addr
csr_addr_dpc        = 0x7B1 :: CSR_Addr
csr_addr_dscratch   = 0x7B2 :: CSR_Addr

-- The following list is in the order printed by print_CSR_File()

m_csr_addrs_and_names :: [(CSR_Addr, String)]
m_csr_addrs_and_names  =
  [ (csr_addr_mstatus,    "mstatus"),
    (csr_addr_mie,        "mie"),
    (csr_addr_mip,        "mip"),
    (csr_addr_medeleg,    "medeleg"),
    (csr_addr_mideleg,    "mideleg"),

    (csr_addr_mtvec,      "mtvec"),
    (csr_addr_mepc,       "mepc"),
    (csr_addr_mcause,     "mcause"),
    (csr_addr_mtval,      "mtval"),
    (csr_addr_mscratch,   "mscratch"),

    (csr_addr_minstret,   "minstret"),
    (csr_addr_minstreth,  "minstreth"),
    (csr_addr_mcycle,     "mcycle"),
    (csr_addr_mcycleh,    "mcycleh"),
    (csr_addr_mcounteren, "mcounteren"),

    (csr_addr_mvendorid,  "mvendorid"),
    (csr_addr_marchid,    "marchid"),
    (csr_addr_mimpid,     "mimpid"),
    (csr_addr_mhartid,    "mhartid"),
    (csr_addr_misa,       "misa"),

    (csr_addr_tselect,    "tselect"),
    (csr_addr_data1,      "data1"),
    (csr_addr_data2,      "data2"),
    (csr_addr_data3,      "data3"),
    (csr_addr_dcsr,       "dcsr"),

    (csr_addr_dpc,        "dpc"),
    (csr_addr_dscratch,   "dscratch")
  ]

-- ================================================================
-- MISA bit fields

-- Test whether a particular MISA 'letter' bit (A-Z) is set

{-# INLINE misa_flag #-}
misa_flag :: Integer -> Char -> Bool
misa_flag  misa  letter | isAsciiUpper  letter = (((shiftR  misa  ((ord letter) - (ord 'A'))) .&. 1) == 1)
misa_flag  misa  letter | isAsciiLower  letter = (((shiftR  misa  ((ord letter) - (ord 'a'))) .&. 1) == 1)
                        | otherwise            = False

show_misa :: Integer -> String
show_misa  misa =
  let
    stringify :: Int -> Integer -> String -> String
    stringify    j      misa       s = if ((misa == 0) || (j > 25)) then s
                                       else
                                         let
                                           s1 = if (misa  .&.  0x1) /= 0 then
                                                  (s ++ [chr (j + ord 'A')])
                                                else
                                                  s
                                         in
                                           stringify  (j + 1)  (shiftR  misa  1)  s1
  in
    stringify  0  misa  ""

-- Codes for MXL, SXL, UXL
xl_rv32  = 1 :: Integer
xl_rv64  = 2 :: Integer
xl_rv128 = 3 :: Integer

-- Bit fields
misa_A_bitpos = 0 :: Int
misa_B_bitpos = 1 :: Int
misa_C_bitpos = 2 :: Int
misa_D_bitpos = 3 :: Int

misa_E_bitpos = 4 :: Int
misa_F_bitpos = 5 :: Int
misa_G_bitpos = 6 :: Int
misa_H_bitpos = 7 :: Int

misa_I_bitpos = 8 :: Int
misa_J_bitpos = 9 :: Int
misa_K_bitpos = 10 :: Int
misa_L_bitpos = 11 :: Int

misa_M_bitpos = 12 :: Int
misa_N_bitpos = 13 :: Int
misa_O_bitpos = 14 :: Int
misa_P_bitpos = 15 :: Int

misa_Q_bitpos = 16 :: Int
misa_R_bitpos = 17 :: Int
misa_S_bitpos = 18 :: Int
misa_T_bitpos = 19 :: Int

misa_U_bitpos = 20 :: Int
misa_V_bitpos = 21 :: Int
misa_W_bitpos = 22 :: Int
misa_X_bitpos = 23 :: Int

misa_Y_bitpos = 24 :: Int
misa_Z_bitpos = 25 :: Int

misa_MXL_bitpos_RV32 = 30 :: Int
misa_MXL_bitpos_RV64 = 62 :: Int

-- ================================================================
-- MSTATUS bit fields

mstatus_sd_bitpos_RV64 = 63 :: Int
mstatus_sd_bitpos_RV32 = 31 :: Int

mstatus_sxl_bitpos     = 34 :: Int
mstatus_uxl_bitpos     = 32 :: Int

mstatus_tsr_bitpos     = 22 :: Int
mstatus_tw_bitpos      = 21 :: Int
mstatus_tvm_bitpos     = 20 :: Int

mstatus_mxr_bitpos     = 19 :: Int
mstatus_sum_bitpos     = 18 :: Int
mstatus_mprv_bitpos    = 17 :: Int

mstatus_fs_bitpos      = 15 :: Int
mstatus_xs_bitpos      = 13 :: Int

mstatus_mpp_bitpos     = 11 :: Int
mstatus_spp_bitpos     =  8 :: Int

mstatus_mpie_bitpos    =  7 :: Int
mstatus_spie_bitpos    =  5 :: Int
mstatus_upie_bitpos    =  4 :: Int

mstatus_mie_bitpos     =  3 :: Int
mstatus_sie_bitpos     =  1 :: Int
mstatus_uie_bitpos     =  0 :: Int

-- MSTATUS contains a ``stack'' of ``previous-privilege'' and ``interrupt-enable'' bits

-- Return the stack fields in mstatus

{-# INLINE mstatus_stack_fields #-}
mstatus_stack_fields :: Integer -> (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer)
mstatus_stack_fields  mstatus =
  let
    mpp  = (shiftR  mstatus  mstatus_mpp_bitpos)  .&. 0x3
    spp  = (shiftR  mstatus  mstatus_spp_bitpos)  .&. 0x1
    mpie = (shiftR  mstatus  mstatus_mpie_bitpos) .&. 0x1
    spie = (shiftR  mstatus  mstatus_spie_bitpos) .&. 0x1
    upie = (shiftR  mstatus  mstatus_upie_bitpos) .&. 0x1
    mie  = (shiftR  mstatus  mstatus_mie_bitpos)  .&. 0x1
    sie  = (shiftR  mstatus  mstatus_sie_bitpos)  .&. 0x1
    uie  = (shiftR  mstatus  mstatus_uie_bitpos)  .&. 0x1
  in
    (mpp, spp, mpie, spie, upie, mie, sie, uie)

-- Update the stack fields in mstatus

{-# INLINE mstatus_upd_stack_fields #-}
mstatus_upd_stack_fields :: Integer ->
                            (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer) ->
                            Integer
mstatus_upd_stack_fields  mstatus (mpp, spp, mpie, spie, upie, mie, sie, uie) =
  let
    mstatus_stack_mask :: Integer
    mstatus_stack_mask = 0x1FFF    -- all LSBs up to and including MPP

    mstatus' = ((mstatus .&. (complement  mstatus_stack_mask))
                .|. (shiftL  mpp   mstatus_mpp_bitpos)
                .|. (shiftL  spp   mstatus_spp_bitpos)
                .|. (shiftL  mpie  mstatus_mpie_bitpos)
                .|. (shiftL  spie  mstatus_spie_bitpos)
                .|. (shiftL  upie  mstatus_upie_bitpos)
                .|. (shiftL  mie   mstatus_mie_bitpos)
                .|. (shiftL  sie   mstatus_sie_bitpos)
                .|. (shiftL  uie   mstatus_uie_bitpos))
  in
    mstatus'

-- These masks specify which fields are observed/updated in MSTATUS

mstatus_mask_RV32 :: Integer
mstatus_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                     .|. (shiftL  1  mstatus_tsr_bitpos)
                     .|. (shiftL  1  mstatus_tw_bitpos)
                     .|. (shiftL  1  mstatus_tvm_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)
                     .|. (shiftL  1  mstatus_mprv_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  3  mstatus_mpp_bitpos)
                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_mpie_bitpos)
                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_mie_bitpos)
                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))

mstatus_mask_RV64 :: Integer
mstatus_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                     -- .|. (shiftL  3  mstatus_sxl_bitpos)    -- TODO: this is not-writable implementation choice
                     -- .|. (shiftL  3  mstatus_uxl_bitpos)    -- TODO: this is not-writable implementation choice

                     .|. (shiftL  1  mstatus_tsr_bitpos)
                     .|. (shiftL  1  mstatus_tw_bitpos)
                     .|. (shiftL  1  mstatus_tvm_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)
                     .|. (shiftL  1  mstatus_mprv_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  3  mstatus_mpp_bitpos)
                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_mpie_bitpos)
                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_mie_bitpos)
                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))


-- SSTATUS is a ``view'' of MSTATUS, masking in/out certain fields

sstatus_mask_RV32 :: Integer
sstatus_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))

sstatus_mask_RV64 :: Integer
sstatus_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                     .|. (shiftL  3  mstatus_uxl_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))

-- USTATUS is a ``view'' of MSTATUS, masking in/out certain fields
-- TODO: find out what should be in ustatus (the v1.10 spec doc does not specify)

ustatus_mask_RV32 :: Integer
ustatus_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_uie_bitpos))

ustatus_mask_RV64 :: Integer
ustatus_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                     .|. (shiftL  3  mstatus_uxl_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_uie_bitpos))

-- ================================================================
-- Trap Vectors (mtvec, stvec, utvec) have
--    a 'mode' in bits [1:0]
--    a 'base' in bits [xlen-1:2]
-- MTVEC, STVEC and UTVEC have the same format
--     (for Machine, Supervisor, User privilege levels)

{-# INLINE tvec_mode #-}
tvec_mode :: Integer -> Integer
tvec_mode  tvec = (tvec .&. 3)

tvec_mode_DIRECT   = 0 :: Integer
tvec_mode_VECTORED = 1 :: Integer

{-# INLINE tvec_base #-}
tvec_base :: Integer -> Integer
tvec_base  tvec = shiftL (shiftR  tvec  2) 2

-- ================================================================
-- MIP bit fields (Machine privilege level Interrupt Pending)
-- MIE bit fields (Machine privilege level Interrupt Enable)

mip_usip_bitpos =  0 :: Int
mip_ssip_bitpos =  1 :: Int
mip_msip_bitpos =  3 :: Int

mip_utip_bitpos =  4 :: Int
mip_stip_bitpos =  5 :: Int
mip_mtip_bitpos =  7 :: Int

mip_ueip_bitpos =  8 :: Int
mip_seip_bitpos =  9 :: Int
mip_meip_bitpos = 11 :: Int

-- SIP is a ``view'' of MIP for Supervisor privilege level
-- SIE is a ``view'' of MIE for Supervisor privilege level, with the same mask

sip_mask :: Integer
sip_mask = ((    shiftL 1 mip_seip_bitpos)
            .|. (shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_stip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_ssip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- UIP is a ``view'' of MIP for User privilege level
-- UIE is a ``view'' of MIE for User privilege level, with the same mask

uip_mask :: Integer
uip_mask = ((    shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- ================================================================
-- Function from mstatus, mip, mie and current privilege values to
--     whether or not an interrupt is pending,
-- and if so, the corresponding exception code

{-# INLINE fn_interrupt_pending #-}
fn_interrupt_pending :: Integer ->                    -- MISA
                        Integer ->                    -- MSTATUS
                        Integer ->                    -- MIP
                        Integer ->                    -- MIE
                        Integer ->                    -- MIDELEG
                        Integer ->                    -- SIDELEG
                        Priv_Level -> Maybe Exc_Code
fn_interrupt_pending  misa  mstatus  mip  mie  mideleg  sideleg  priv =
  let
    fn_interrupt_i_pending :: Exc_Code -> Bool
    fn_interrupt_i_pending  ec =
      let
        i = (fromIntegral ec :: Int)
        intr_pending = ((testBit  mip  i) && (testBit  mie  i))
        handler_priv = if (testBit  mideleg  i) then
                         if (misa_flag  misa  'U') then
                           if (misa_flag  misa  'S') then
                             -- System with M, S, U
                             if (testBit  sideleg  i) then
                               if (misa_flag  misa 'N') then
                                 -- M->S->U delegation
                                 u_Priv_Level
                               else
                                 -- TODO: Error: SIDELEG [i] should not be 1 if MISA.N is 0
                                 m_Priv_Level
                             else
                               -- M->S delegation
                               s_Priv_Level
                           else
                             -- System with M, U
                             if (misa_flag  misa  'N') then
                               -- M->U delegation
                               u_Priv_Level
                             else
                               -- TODO: Error: MIDELEG [i] should not be 1 if MISA.N is 0
                               m_Priv_Level
                         else
                           -- Error: System with M only; MIDELEG [i] should not be 1
                           m_Priv_Level
                       else
                         -- no delegation
                         m_Priv_Level

        xie :: Bool
        xie | (priv == u_Priv_Level) = (testBit  mstatus  mstatus_uie_bitpos)
            | (priv == s_Priv_Level) = (testBit  mstatus  mstatus_sie_bitpos)
            | (priv == m_Priv_Level) = (testBit  mstatus  mstatus_mie_bitpos)

        glob_enabled :: Bool
        glob_enabled = ((priv < handler_priv) || ((priv == handler_priv) && xie))
      in
        (intr_pending && glob_enabled)

    -- Check all interrupts in the following decreasing priority order
    m_ec | fn_interrupt_i_pending (exc_code_m_external_interrupt) = Just exc_code_m_external_interrupt
         | fn_interrupt_i_pending (exc_code_m_software_interrupt) = Just exc_code_m_software_interrupt
         | fn_interrupt_i_pending (exc_code_m_timer_interrupt)    = Just exc_code_m_timer_interrupt
         | fn_interrupt_i_pending (exc_code_s_external_interrupt) = Just exc_code_s_external_interrupt
         | fn_interrupt_i_pending (exc_code_s_software_interrupt) = Just exc_code_s_software_interrupt
         | fn_interrupt_i_pending (exc_code_s_timer_interrupt)    = Just exc_code_s_timer_interrupt
         | fn_interrupt_i_pending (exc_code_u_external_interrupt) = Just exc_code_u_external_interrupt
         | fn_interrupt_i_pending (exc_code_u_software_interrupt) = Just exc_code_u_software_interrupt
         | fn_interrupt_i_pending (exc_code_u_timer_interrupt)    = Just exc_code_u_timer_interrupt
         | True                                                   = Nothing
  in
    m_ec

-- ================================================================
-- MCAUSE bit fields

mcause_interrupt_bitpos_RV32 = 31 :: Int
mcause_interrupt_bitpos_RV64 = 63 :: Int

-- Constructor: make an MCAUSE value depending interrupt or trap, and exception code

{-# INLINE mkCause #-}
mkCause :: RV -> Bool -> Exc_Code -> Integer
mkCause  rv  interrupt_not_trap  exc_code =
  let
    msb | interrupt_not_trap && (rv == RV32) = shiftL  1  mcause_interrupt_bitpos_RV32
        | interrupt_not_trap && (rv == RV64) = shiftL  1  mcause_interrupt_bitpos_RV64
        | otherwise                          = 0
  in
    (msb .|. exc_code)

-- ================================================================
-- ISA-F and ISA-D related definitions
-- FCSR bit fields
-- Bit fields
fcsr_nx_bitpos          = 0 :: Int
fcsr_uf_bitpos          = 1 :: Int
fcsr_of_bitpos          = 2 :: Int
fcsr_dz_bitpos          = 3 :: Int
fcsr_nv_bitpos          = 4 :: Int

-- The flags with only one of the bits set
nxFlag = (shiftL  1  fcsr_nx_bitpos) :: Integer
ufFlag = (shiftL  1  fcsr_uf_bitpos) :: Integer
ofFlag = (shiftL  1  fcsr_of_bitpos) :: Integer
dzFlag = (shiftL  1  fcsr_dz_bitpos) :: Integer
nvFlag = (shiftL  1  fcsr_nv_bitpos) :: Integer

-- 7:5
frm_bitpos      = 5 :: Int
fcsr_reserved   = 8 :: Int
fflags_mask     = ((shiftL 1 frm_bitpos) - 1) :: Integer
frm_mask        = (((shiftL 1 fcsr_reserved) - 1)
                .&. (complement fflags_mask)) :: Integer

-- Extract the fflags field from fcsr
fcsr_fflags    :: Integer -> Integer 
fcsr_fflags    fcsr = bitSlice fcsr 4 0

-- Extract the frm field from fcsr
fcsr_frm       :: Integer -> Integer 
fcsr_frm       fcsr = bitSlice fcsr 7 5

-- Check if a frm value in the FCSR.FRM is valid
fcsr_frm_valid :: Integer -> Bool
fcsr_frm_valid   frm  = (   (frm /= 0x5)
                         && (frm /= 0x6)
                         && (frm /= 0x7))

-- Check if a frm value in the instr is valid
instr_frm_valid:: InstrField -> Bool
instr_frm_valid   frm = (   (frm /= 0x5)
                         && (frm /= 0x6))

-- Returns the right rounding mode from the FCSR or the instruction and checks
-- legality
rounding_mode_check :: InstrField -> Integer -> (Integer, Bool)
rounding_mode_check    rm            frm =
  let
    frmVal     = if (rm == 0x7) then frm else rm
    rmIsLegal  = if (rm == 0x7) then
                   fcsr_frm_valid  frmVal
                 else
                   instr_frm_valid  frmVal
  in
    (frmVal, rmIsLegal)


-- Bit positions for the mask describing the class of FP value (table 8.5 v2.2)
fclass_negInf_bitpos       = 0 :: Int
fclass_negNorm_bitpos      = 1 :: Int
fclass_negSubNorm_bitpos   = 2 :: Int
fclass_negZero_bitpos      = 3 :: Int
fclass_posZero_bitpos      = 4 :: Int
fclass_posSubNorm_bitpos   = 5 :: Int
fclass_posNorm_bitpos      = 6 :: Int
fclass_posInf_bitpos       = 7 :: Int
fclass_SNaN_bitpos         = 8 :: Int
fclass_QNaN_bitpos         = 9 :: Int

-- Create a FFLAGS word to accrue into the CSR
form_fflags_word  :: Bool -> Bool -> Bool -> Bool -> Bool -> Integer
form_fflags_word  nxf  uff  off  dzf  nvf  =
  let
    fflags = (    (if (nxf) then (shiftL  1  fcsr_nx_bitpos) else 0)
              .|. (if (uff) then (shiftL  1  fcsr_uf_bitpos) else 0)
              .|. (if (off) then (shiftL  1  fcsr_of_bitpos) else 0)
              .|. (if (dzf) then (shiftL  1  fcsr_dz_bitpos) else 0)
              .|. (if (nvf) then (shiftL  1  fcsr_nv_bitpos) else 0))
  in
    fflags

-- ================================================================

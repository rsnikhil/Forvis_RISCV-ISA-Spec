module ArchDefs64 where

-- ================================================================
-- This module has some basic system-wide RISC-V definitions (such
-- RV32 vs. RV64 and memory map) on which the rest of the
-- architectural state and operations depend.

-- ================================================================
-- Standard Haskell imports

import Data.Word    -- for Word8/16/32/64 (unsigned)
import Data.Int     -- for Int8/16/32/64 (signed)
import Data.Bits

-- Project imports

-- None

-- ================================================================
-- Major architectural parameters

data XLEN = RV32
          | RV64
          deriving (Eq, Show)

xlen :: Int
xlen = 64

type WordXLEN = Word64    -- unsigned
type IntXLEN  = Int64     -- signed

data Register = Rg_x0  | Rg_x1  | Rg_x2  | Rg_x3  | Rg_x4  | Rg_x5  | Rg_x6  | Rg_x7
              | Rg_x8  | Rg_x9  | Rg_x10 | Rg_x11 | Rg_x12 | Rg_x13 | Rg_x14 | Rg_x15
              | Rg_x16 | Rg_x17 | Rg_x18 | Rg_x19 | Rg_x20 | Rg_x21 | Rg_x22 | Rg_x23
              | Rg_x24 | Rg_x25 | Rg_x26 | Rg_x27 | Rg_x28 | Rg_x29 | Rg_x30 | Rg_x31
              deriving (Eq, Ord, Enum, Show)

-- ================================================================
-- Using Word for CSR_Addrs, which are actually only 12 bits

type CSR_Addr = Word

-- ================================================================

data LoadResult t = LoadResult_Ok t | LoadResult_Err Exc_Code
                  deriving (Show)

-- ================================================================
-- Privilege levels

type Priv_Level = Word

u_Priv_Level :: Priv_Level
u_Priv_Level  = 0

s_Priv_Level :: Priv_Level
s_Priv_Level  = 2

m_Priv_Level :: Priv_Level
m_Priv_Level  = 3

-- ================================================================
-- Exception Causes

type Exc_Code = Word

exc_code_u_software_interrupt      :: Exc_Code;    exc_code_u_software_interrupt      =  0;
exc_code_s_software_interrupt      :: Exc_Code;    exc_code_s_software_interrupt      =  1;
exc_code_m_software_interrupt      :: Exc_Code;    exc_code_m_software_interrupt      =  3;

exc_code_u_timer_interrupt         :: Exc_Code;    exc_code_u_timer_interrupt         =  4;
exc_code_s_timer_interrupt         :: Exc_Code;    exc_code_s_timer_interrupt         =  5;
exc_code_m_timer_interrupt         :: Exc_Code;    exc_code_m_timer_interrupt         =  7;

exc_code_u_external_interrupt      :: Exc_Code;    exc_code_u_external_interrupt      =  8;
exc_code_s_external_interrupt      :: Exc_Code;    exc_code_s_external_interrupt      =  9;
exc_code_m_external_interrupt      :: Exc_Code;    exc_code_m_external_interrupt      = 11;

exc_code_instr_addr_misaligned     :: Exc_Code;    exc_code_instr_addr_misaligned     =  0;
exc_code_instr_access_fault        :: Exc_Code;    exc_code_instr_access_fault        =  1;
exc_code_illegal_instruction       :: Exc_Code;    exc_code_illegal_instruction       =  2;
exc_code_breakpoint                :: Exc_Code;    exc_code_breakpoint                =  3;

exc_code_load_addr_misaligned      :: Exc_Code;    exc_code_load_addr_misaligned      =  4;
exc_code_load_access_fault         :: Exc_Code;    exc_code_load_access_fault         =  5;
exc_code_store_AMO_addr_misaligned :: Exc_Code;    exc_code_store_AMO_addr_misaligned =  6;
exc_code_store_AMO_access_fault    :: Exc_Code;    exc_code_store_AMO_access_fault    =  7;

exc_code_ECall_from_U              :: Exc_Code;    exc_code_ECall_from_U              =  8;
exc_code_ECall_from_S              :: Exc_Code;    exc_code_ECall_from_S              =  9;
exc_code_ECall_from_M              :: Exc_Code;    exc_code_ECall_from_M              = 11;

exc_code_Instruction_Page_Fault    :: Exc_Code;    exc_code_Instruction_Page_Fault    = 12;
exc_code_Load_Page_Fault           :: Exc_Code;    exc_code_Load_Page_Fault           = 13;
exc_code_Store_AMO_Page_Fault      :: Exc_Code;    exc_code_Store_AMO_Page_Fault      = 15;

mkCause :: Bool -> Exc_Code -> WordXLEN
mkCause  interrupt_not_trap  exc_code = (msb .|. (fromIntegral  exc_code))
  where msb :: WordXLEN
        msb = if interrupt_not_trap then shiftL  1  (xlen - 1) else 0

-- ================================================================

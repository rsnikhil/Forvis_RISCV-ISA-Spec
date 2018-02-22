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

xlen :: Int
xlen = 64

type MachineWord   = Word64    -- unsigned
type MachineWord_S = Int64     -- signed

data Register = Rg_x0  | Rg_x1  | Rg_x2  | Rg_x3  | Rg_x4  | Rg_x5  | Rg_x6  | Rg_x7
              | Rg_x8  | Rg_x9  | Rg_x10 | Rg_x11 | Rg_x12 | Rg_x13 | Rg_x14 | Rg_x15
              | Rg_x16 | Rg_x17 | Rg_x18 | Rg_x19 | Rg_x20 | Rg_x21 | Rg_x22 | Rg_x23
              | Rg_x24 | Rg_x25 | Rg_x26 | Rg_x27 | Rg_x28 | Rg_x29 | Rg_x30 | Rg_x31
              deriving (Eq, Ord, Enum, Show)

-- ================================================================

data LoadResult t = LoadResult_Ok t | LoadResult_Err TrapCause
                  deriving (Show)

-- ================================================================
-- Exception Causes

data IntrCause = IntrCause_U_SW
               | IntrCause_S_SW
               | IntrCause_Reserved_2
               | IntrCause_M_SW

               | IntrCause_U_Timer
               | IntrCause_S_Timer
               | IntrCause_Reserved_6
               | IntrCause_M_Timer

               | IntrCause_U_External
               | IntrCause_S_External
               | IntrCause_Reserved_10
               | IntrCause_M_External
               deriving (Eq, Ord, Enum, Show)

mk_mcause_from_IntrCause :: IntrCause -> MachineWord
mk_mcause_from_IntrCause  intrCause = fromIntegral ((shiftL  1  (xlen - 1))  .|.  (fromEnum intrCause))

data TrapCause = TrapCause_Instr_Addr_Misaligned
               | TrapCause_Instr_Access_Fault
               | TrapCause_Illegal_Instr
               | TrapCause_Breakpoint
               | TrapCause_Load_Addr_Misaligned
               | TrapCause_Load_Access_Fault
               | TrapCause_Store_AMO_Addr_Misaligned
               | TrapCause_Store_AMO_Access_Fault
               | TrapCause_ECall_from_U
               | TrapCause_ECall_from_S
               | TrapCause_Reserved_10
               | TrapCause_ECall_from_M
               | TrapCause_Instr_Page_Fault
               | TrapCause_Load_Page_Fault
               | TrapCause_Reserved_14
               | TrapCause_Store_AMO_Page_Fault
               deriving (Eq, Ord, Enum, Show)

mk_mcause_from_TrapCause :: TrapCause -> MachineWord
mk_mcause_from_TrapCause  trapCause = fromIntegral (fromEnum trapCause)

-- ================================================================
-- Memory-mapped IO
-- Trivial for now, just recognizes one location, the 'UART console' output.

addr_console_out :: MachineWord
addr_console_out =  0xfff4

is_IO_addr :: MachineWord -> Bool
is_IO_addr  addr = (addr == addr_console_out)

-- ================================================================

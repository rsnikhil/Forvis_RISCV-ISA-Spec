module ArchDefs64 where

-- ================================================================
-- This module has some basic system-wide RISC-V definitions (such
-- RV32 vs. RV64 and memory map) on which the rest of the
-- architectural state and operations depend.

-- ================================================================
-- Standard Haskell imports

import Data.Word    -- for Word8/16/32/64 (unsigned)
import Data.Int     -- for Int8/16/32/64 (signed)

-- Project imports

-- None

-- ================================================================
-- Major architectural parameters

xlen :: Int
xlen = 64

type MachineWord   = Word64    -- unsigned
type MachineWord_S = Int64     -- signed

-- TODO: use an enum type for Registers, instead of a number,
-- and a function to map to/from Bit#(5)
type Register = Word32

-- ================================================================
-- Memory-mapped IO
-- Trivial for now, just recognizes one location, the 'UART console' output.

addr_console_out :: MachineWord
addr_console_out =  0xfff4

is_IO_addr :: MachineWord -> Bool
is_IO_addr  addr = (addr == addr_console_out)

-- ================================================================

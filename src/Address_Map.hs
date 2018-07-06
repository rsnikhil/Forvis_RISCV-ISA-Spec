module Address_Map where

-- ================================================================
-- This is not part of the ISA spec; it is a platform specification.

-- This module specifies a particular choice of "address map" of
-- memory and memory-mapped, I/O devices used by the CPU.

-- ================================================================
-- Standard Haskell imports

import Data.Word

-- Project imports

-- none

-- ================================================================
-- Initial value of PC on "power up"
-- Typical:
--     Boot ROMs at 0x1000, and that code jumps to 0x8000_0000
--     Berkeley ISA tests, Linux image start at 0x8000_0000

pc_reset_value = 0x1000 :: Word64

-- pc_reset_value = 0x80000000 :: Word64

-- ================================================================
-- Supported address ranges (memory and MMMIO)

addr_ranges :: [ (Word64, Word64) ]
addr_ranges  = memory_addr_ranges ++ mmio_addr_ranges

-- ================================================================
-- Memory ranges

-- Boot ROM addr range
addr_base_boot = 0x1000 :: Word64
addr_size_boot = 0x1000 :: Word64

-- Berkeley ISA tests are compiled in this range
addr_base_mem = 0x80000000 :: Word64
addr_size_mem = 0x10000000 :: Word64   -- 256 MiB

-- HTIF are memory-like locations used in the Berkeley tests
-- Note: one address (addr_htif_console_out = 0xfff4) in this range is treated as I/O
addr_base_htif = 0xff80 :: Word64
addr_size_htif = 0x80   :: Word64

-- Supported memory address ranges
memory_addr_ranges :: [ (Word64, Word64) ]
memory_addr_ranges = [(addr_base_boot,  addr_base_boot + addr_size_boot),
                      (addr_base_mem,   addr_base_mem  + addr_size_mem),
                      (addr_base_htif,  addr_base_htif + addr_size_htif)
                     ]

-- ================================================================
-- Memory-mapped IO defs
-- Trivial for now

-- Berkeley ISA tests use htif_console
addr_htif_console_out :: Word64;    addr_htif_console_out = 0xfff4

-- Other programs and tests use a UART for console I/O
addr_base_UART = 0xC0000000            :: Word64
addr_size_UART = 0x80                  :: Word64
addr_UART_out  = (addr_base_UART + 0)  :: Word64

-- Real-time counter
addr_mtime     = 0x0200BFF8 :: Word64

-- Real-time timer-compare, for timer interrupts
addr_mtimecmp  = 0x02004000 :: Word64

-- Generation of software interrupts
addr_msip      = 0x02000000 :: Word64

-- Supported MMIO address ranges
mmio_addr_ranges :: [ (Word64, Word64) ]
mmio_addr_ranges = [ (addr_htif_console_out,  (addr_htif_console_out + 8)),
                     (addr_base_UART,         (addr_base_UART        + addr_size_UART)),
                     (addr_mtime,             (addr_mtime            + 8)),
                     (addr_mtimecmp,          (addr_mtimecmp         + 8)),
                     (addr_msip,              (addr_msip             + 8))
                   ]

-- Check if the given addr is an I/O addr
is_IO_addr :: Word64 -> Bool
is_IO_addr  addr =
  let
    check :: [(Word64, Word64)] -> Bool
    check  []                  = False
    check  ((base,lim):ranges) = if ((base <= addr) && (addr < lim)) then
                                   True
                                 else
                                   check  ranges
  in
    check  mmio_addr_ranges

-- ================================================================

-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Address_Map where

-- ================================================================
-- This is not part of the ISA spec; it is a platform specification.

-- This module specifies a particular choice of "address map" of
-- memory and memory-mapped, I/O devices used by the CPU.

-- ================================================================
-- Standard Haskell imports

-- none

-- Project imports

-- none

-- ================================================================
-- Initial value of PC on "power up"
-- Typical:
--     Boot ROMs at 0x1000, and that code jumps to 0x8000_0000
--     Berkeley ISA tests, Linux image start at 0x8000_0000

pc_reset_value = 0x1000 :: Integer

-- pc_reset_value = 0x80000000 :: Integer

-- ================================================================
-- Supported address ranges (memory and MMMIO)

addr_ranges :: [ (Integer, Integer) ]
addr_ranges  = memory_addr_ranges ++ mmio_addr_ranges

-- ================================================================
-- Memory ranges

-- Boot ROM addr range
addr_base_boot = 0x1000 :: Integer
addr_size_boot = 0x1000 :: Integer

-- Berkeley ISA tests are compiled in this range
addr_base_mem = 0x80000000 :: Integer
addr_size_mem = 0x10000000 :: Integer   -- 256 MiB

-- HTIF are memory-like locations used in the Berkeley tests
-- Note: one address (addr_htif_console_out = 0xfff4) in this range is treated as I/O
addr_base_htif = 0xff80 :: Integer
addr_size_htif = 0x80   :: Integer

-- Supported memory address ranges
memory_addr_ranges :: [ (Integer, Integer) ]
memory_addr_ranges = [(addr_base_boot,  addr_base_boot + addr_size_boot),
                      (addr_base_mem,   addr_base_mem  + addr_size_mem),
                      (addr_base_htif,  addr_base_htif + addr_size_htif)
                     ]

-- ================================================================
-- Memory-mapped IO defs
-- Trivial for now

-- Berkeley ISA tests use htif_console
addr_htif_console_out :: Integer;    addr_htif_console_out = 0xfff4

-- Other programs and tests use a UART for console I/O
addr_base_UART = 0xC0000000               :: Integer
addr_size_UART = 0x80                     :: Integer

-- Real-time counter
addr_mtime     = 0x0200BFF8 :: Integer

-- Real-time timer-compare, for timer interrupts
addr_mtimecmp  = 0x02004000 :: Integer

-- Generation of software interrupts
addr_msip      = 0x02000000 :: Integer

-- Supported MMIO address ranges
mmio_addr_ranges :: [ (Integer, Integer) ]
mmio_addr_ranges = [ (addr_htif_console_out,  (addr_htif_console_out + 8)),
                     (addr_base_UART,         (addr_base_UART        + addr_size_UART)),
                     (addr_mtime,             (addr_mtime            + 8)),
                     (addr_mtimecmp,          (addr_mtimecmp         + 8)),
                     (addr_msip,              (addr_msip             + 8))
                   ]

-- Check if the given addr is an I/O addr
{-# INLINE is_IO_addr #-}
is_IO_addr :: Integer -> Bool
is_IO_addr  addr =
  let
    check :: [(Integer, Integer)] -> Bool
    check  []                  = False
    check  ((base,lim):ranges) = if ((base <= addr) && (addr < lim)) then
                                   True
                                 else
                                   check  ranges
  in
    check  mmio_addr_ranges

-- ================================================================

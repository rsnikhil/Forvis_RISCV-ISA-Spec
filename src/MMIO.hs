-- See LICENSE for license details

module MMIO where

-- ================================================================
-- This module handles memory-mapped I/O reads and writes.
-- At the moment, it only handles console output.
-- 'MMIO' is an abstraction of the Memory-Mapped IO system
-- and is accessed via the exported get/set API.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Word
import Data.Bits
import Data.Char
import qualified Data.Map as Data_Map
import Numeric (showHex, readHex)

-- Project imports

import Arch_Defs
import Mem_Ops

-- ================================================================
-- Memory-mapped IO defs
-- Trivial for now, just recognizes one location, the 'UART console' output.

addr_console_out :: Word64
addr_console_out =  0xfff4

is_IO_addr :: Word64 -> Bool
is_IO_addr  addr = (addr == addr_console_out)

-- ================================================================
-- IO subsystem representation
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

-- 'f_console_out_a' is console output that has already been consumed and printed
-- 'f_console_out_b' is console output that nas not yet been consumed and printed

data MMIO = MMIO { f_console_out_a :: String,
                   f_console_out_b :: String
                 }

mkMMIO :: MMIO
mkMMIO = MMIO { f_console_out_a = [],  f_console_out_b = [] }

-- ================================================================
-- Read data from MMIO
-- Currently only returns a bogus value

mmio_read :: MMIO -> InstrField -> Word64 -> (Mem_Result, MMIO)
mmio_read  mmio  funct3  addr = (Mem_Result_Ok  0xAAAAAAAAAAAAAAAA,
                                 mmio)

-- ================================================================
-- Write data into MMIO
-- Currently only handles console output

mmio_write :: MMIO -> InstrField -> Word64 -> Word64 -> (Mem_Result, MMIO)
mmio_write  mmio  funct3  addr  val =
  if (addr == addr_console_out) then
    let
      -- Console output
      console_out_b = f_console_out_b  mmio
      char          = chr (fromIntegral val)
      mmio'         = mmio { f_console_out_b = console_out_b ++ [char] }
    in
      (Mem_Result_Ok 0, mmio')

  else
    (Mem_Result_Err  exc_code_store_AMO_access_fault, mmio)

-- ================================================================
-- AMO op
-- Currently AMO is not supported on I/O addrs; return access fault.
-- TODO: find out if does LR also return STORE_AMO_ACCESS_FAULT or LOAD_ACCESS_FAULT?

mmio_amo :: MMIO -> Word64 -> InstrField -> InstrField -> InstrField -> InstrField -> Word64 ->
           (Mem_Result, MMIO)

mmio_amo  mmio  addr  funct3  msbs5  aq  rl  val =
  (Mem_Result_Err exc_code_store_AMO_access_fault,  mmio)

-- ================================================================
-- Consume the recorded console-output

mmio_consume_console_output :: MMIO -> (String, MMIO)
mmio_consume_console_output  mmio =
  let
    console_output_a = f_console_out_a  mmio
    console_output_b = f_console_out_b  mmio
    mmio' = MMIO { f_console_out_a = console_output_a ++ console_output_b,
                   f_console_out_b = "" }
  in
    (console_output_b, mmio')

-- ================================================================
-- Read all console output

mmio_read_all_console_output :: MMIO -> String
mmio_read_all_console_output  mmio =
  let
    console_output_a = f_console_out_a  mmio
    console_output_b = f_console_out_b  mmio
  in
    console_output_a ++ console_output_b

-- ================================================================

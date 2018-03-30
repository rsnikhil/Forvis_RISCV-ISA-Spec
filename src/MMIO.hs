module MMIO (is_IO_addr,
             MMIO, mkMMIO,
             mmio_read8, mmio_read16, mmio_read32, mmio_read64,
             mmio_write8, mmio_write16, mmio_write32, mmio_write64
            ) where

-- ================================================================
-- This module handles memory-mapped I/O reads and writes.
-- At the moment, it only handles console output.
-- 'MMIO' is an abstraction of the Memory-Mapped IO system
-- and is accessed via the exported get/set API.

-- ================================================================
-- Standard Haskell imports

import Control.Monad
import Data.Maybe
import Data.Word
import Data.Bits
import Data.Char
import qualified Data.Map as Data_Map
import Numeric (showHex, readHex)

-- Project imports

import ArchDefs

-- ================================================================
-- Memory-mapped IO defs
-- Trivial for now, just recognizes one location, the 'UART console' output.

addr_console_out :: UInt
addr_console_out =  0xfff4

is_IO_addr :: UInt -> Bool
is_IO_addr  addr = (addr == addr_console_out)

-- ================================================================
-- IO subsystem representation: dummy placeholder for now
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

newtype MMIO = MMIO_Con ()

mkMMIO :: MMIO
mkMMIO = MMIO_Con ()

-- ----------------
-- get (read) data from MMIO
-- TODO: lots of details to be filled in
-- including returning potential errors

mmio_read8 :: MMIO -> UInt -> Word8
mmio_read8  mmio  addr = 0xAA    -- bogus placeholder

mmio_read16 :: MMIO -> UInt -> Word16
mmio_read16  mmio  addr = 0xAAAA    -- bogus placeholder

mmio_read32 :: MMIO -> UInt -> Word32
mmio_read32  mmio  addr = 0xAAAAAAAA    -- bogus placeholder

mmio_read64 :: MMIO -> UInt -> Word64
mmio_read64  mmio  addr = 0xAAAAAAAAAAAAAAAA    -- bogus placeholder

-- ----------------
-- set (write) data into MMIO
-- TODO: lots of details to be filled in
-- including returning potential errors

mmio_write8 :: MMIO -> UInt -> Word8 -> IO MMIO
mmio_write8  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " byte 0x" ++ (showHex val ""))
    return mmio

mmio_write16 :: MMIO -> UInt -> Word16 -> IO MMIO
mmio_write16  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " halfword 0x" ++ (showHex val ""))
    return mmio

mmio_write32 :: MMIO -> UInt -> Word32 -> IO MMIO
mmio_write32  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " word 0x" ++ (showHex val ""))
    return mmio

mmio_write64 :: MMIO -> UInt -> Word64 -> IO MMIO
mmio_write64  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " doubleword 0x" ++ (showHex val ""))
    return mmio

-- ================================================================

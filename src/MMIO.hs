module MMIO (MMIO, mkMMIO,
             getMMIO8, getMMIO16, getMMIO32, getMMIO64,
             setMMIO8, setMMIO16, setMMIO32, setMMIO64
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

import ArchDefs64

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

getMMIO8 :: MMIO -> WordXLEN -> Word8
getMMIO8  mmio  addr = 0xAA    -- bogus placeholder

getMMIO16 :: MMIO -> WordXLEN -> Word16
getMMIO16  mmio  addr = 0xAAAA    -- bogus placeholder

getMMIO32 :: MMIO -> WordXLEN -> Word32
getMMIO32  mmio  addr = 0xAAAAAAAA    -- bogus placeholder

getMMIO64 :: MMIO -> WordXLEN -> Word64
getMMIO64  mmio  addr = 0xAAAAAAAAAAAAAAAA    -- bogus placeholder

-- ----------------
-- set (write) data into MMIO
-- TODO: lots of details to be filled in
-- including returning potential errors

setMMIO8 :: MMIO -> WordXLEN -> Word8 -> IO MMIO
setMMIO8  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " byte 0x" ++ (showHex val ""))
    return mmio

setMMIO16 :: MMIO -> WordXLEN -> Word16 -> IO MMIO
setMMIO16  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " halfword 0x" ++ (showHex val ""))
    return mmio

setMMIO32 :: MMIO -> WordXLEN -> Word32 -> IO MMIO
setMMIO32  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " word 0x" ++ (showHex val ""))
    return mmio

setMMIO64 :: MMIO -> WordXLEN -> Word64 -> IO MMIO
setMMIO64  mmio  addr  val =
  if (addr == addr_console_out) then do
    -- Console output
    putChar (chr (fromIntegral val))
    return mmio
  else do
    putStrLn ("IO write: addr 0x" ++ (showHex addr "") ++ " doubleword 0x" ++ (showHex val ""))
    return mmio

-- ================================================================

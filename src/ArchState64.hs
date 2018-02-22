module ArchState64 (ArchState64, mkArchState64, print_ArchState64,
                    Stop_Reason (..),
                    ifetch,
                    get_ArchState64_gpr,            set_ArchState64_gpr,
                    get_ArchState64_csr,            set_ArchState64_csr,   
                    get_ArchState64_csr_from_addr,  set_ArchState64_csr_from_addr,
                    get_ArchState64_PC,             set_ArchState64_PC,
                    get_ArchState64_mem8,           set_ArchState64_mem8,
                    get_ArchState64_mem16,          set_ArchState64_mem16,
                    get_ArchState64_mem32,          set_ArchState64_mem32,
                    get_ArchState64_mem64,          set_ArchState64_mem64,
                    get_ArchState64_verbosity,      set_ArchState64_verbosity,
                    get_ArchState64_stop,           set_ArchState64_stop
                    ) where

-- ================================================================
-- This module defines the data structure holding the RISC-V CPU archtectural state
-- (plus some additional state for debugging/tracing convience)
-- and a set/get API to read and write components of the state.
-- Also defines 'ifetch' (instruction fetch).

-- ================================================================
-- Standard Haskell imports

import Control.Monad
import Data.Maybe
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Map as Data_Map
import Numeric (showHex, readHex)

-- Project imports

import ArchDefs64
import GPRFile
import CSRFile
import Memory
import MMIO

-- ================================================================
-- Architectural State data structure.
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data ArchState64 = ArchState64 { f_pc   :: MachineWord,
                                 f_gprs :: GPRFile,
                                 f_csrs :: CSRFile,
                                 f_mem  :: Mem,
                                 f_mmio :: MMIO,

                                 -- The following are for convenience for debugging only
                                 -- and have no semantic relevance.
                                 f_verbosity :: Int,
                                 f_stop      :: Stop_Reason
                               }

data Stop_Reason = Stop_Running | Stop_Other | Stop_Break | Stop_WFI | Stop_Limit | Stop_Forced
  deriving (Eq, Show)

print_ArchState64 :: String -> ArchState64 -> IO ()
print_ArchState64  indent  (ArchState64 pc gprs csrs mem mmio verbosity stop) = do
  putStrLn (indent ++ "pc: " ++ showHex pc "")
  print_GPRFile  indent  gprs
  print_CSRFile  indent  csrs
  -- We do not print memory or MMIO
  putStrLn (indent ++ (show stop))

-- ================================================================
-- Instruction Fetch; uses the API below
-- TODO: for 'C', fetch first 16 bytes, check length, only then fetch next 16 bytes if not a 'C' instr

ifetch :: ArchState64 -> (LoadResult Word32, ArchState64)
ifetch  astate = get_ArchState64_mem32  astate  pc
  where pc = get_ArchState64_PC  astate

-- ================================================================
-- API to create, set/get components of the Architectural State

-- Make an ArchState64, given initial PC and memory contents
mkArchState64 :: Int -> ([(Int, Word8)]) -> ArchState64
mkArchState64  pc  addr_byte_list =   ArchState64 { f_pc   = fromIntegral pc,
                                                    f_gprs = mkGPRFile,
                                                    f_csrs = mkCSRFile 64,
                                                    f_mem  = mkMem  addr_byte_list,
                                                    f_mmio = mkMMIO,

                                                    f_verbosity = 0,
                                                    f_stop      = Stop_Running}

-- ----------------
-- get/set GPRs

get_ArchState64_gpr :: ArchState64 -> Register -> MachineWord
get_ArchState64_gpr  astate  reg = get_gpr (f_gprs astate)  reg

set_ArchState64_gpr :: ArchState64 -> Register -> MachineWord -> IO ArchState64
set_ArchState64_gpr  astate  reg  val = return (astate { f_gprs = set_gpr (f_gprs astate) reg val })

-- ----------------
-- get/set CSRs

-- TODO: output type of 'get' should reflect that getting a CSR can have errors and side effects
get_ArchState64_csr :: ArchState64 -> CSR -> MachineWord
get_ArchState64_csr  astate  csr = get_csr  (f_csrs astate)  csr

get_ArchState64_csr_from_addr :: ArchState64 -> Word16 -> MachineWord
get_ArchState64_csr_from_addr  astate  csr_addr = get_csr_from_addr  (f_csrs astate)  csr_addr

-- TODO: output type of 'set' should reflect that getting a CSR can have errors and side effects
set_ArchState64_csr :: ArchState64 -> CSR -> MachineWord -> IO ArchState64
set_ArchState64_csr  astate  csr  value =
  return (astate { f_csrs = set_csr  (f_csrs astate)  csr  value })

set_ArchState64_csr_from_addr :: ArchState64 -> Word16 -> MachineWord -> IO ArchState64
set_ArchState64_csr_from_addr  astate  csr_addr  value =
  return (astate { f_csrs = set_csr_from_addr  (f_csrs astate)  csr_addr  value })

-- ----------------
-- get/set PC

get_ArchState64_PC :: ArchState64 -> MachineWord
get_ArchState64_PC  astate = f_pc astate

set_ArchState64_PC :: ArchState64 -> MachineWord -> IO ArchState64
set_ArchState64_PC  astate  val = return (astate { f_pc = val })

-- ----------------
-- get/set memory at various widths
-- TODO: fix up 'get' to triage memory vs. I/O like the 'set' calls and 'get' to IO can change astate

get_ArchState64_mem8 :: ArchState64 -> MachineWord -> (LoadResult Word8, ArchState64)
get_ArchState64_mem8  astate  addr = (getMem8  (f_mem astate)  addr, astate)

get_ArchState64_mem16 :: ArchState64 -> MachineWord -> (LoadResult Word16, ArchState64)
get_ArchState64_mem16  astate  addr = (getMem16  (f_mem astate)  addr, astate)

get_ArchState64_mem32 :: ArchState64 -> MachineWord -> (LoadResult Word32, ArchState64)
get_ArchState64_mem32  astate  addr = (getMem32  (f_mem astate)  addr, astate)

get_ArchState64_mem64 :: ArchState64 -> MachineWord -> (LoadResult Word64, ArchState64)
get_ArchState64_mem64  astate  addr = (getMem64  (f_mem astate)  addr, astate)


set_ArchState64_mem8 :: ArchState64 -> MachineWord -> Word8 -> IO ArchState64
set_ArchState64_mem8  astate addr  val = do
  when (get_ArchState64_verbosity astate > 1) (
    putStrLn ("set_ArchState64_mem8: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem8  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO8  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

set_ArchState64_mem16 :: ArchState64 -> MachineWord -> Word16 -> IO ArchState64
set_ArchState64_mem16  astate addr  val = do
  when (get_ArchState64_verbosity astate > 1) (
    putStrLn ("set_ArchState64_mem16: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem16  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO16  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

set_ArchState64_mem32 :: ArchState64 -> MachineWord -> Word32 -> IO ArchState64
set_ArchState64_mem32  astate addr  val = do
  when (get_ArchState64_verbosity astate > 1) (
    putStrLn ("set_ArchState64_mem32: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem32  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO32  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

set_ArchState64_mem64 :: ArchState64 -> MachineWord -> Word64 -> IO ArchState64
set_ArchState64_mem64  astate addr  val = do
  when (get_ArchState64_verbosity astate > 1) (
    putStrLn ("set_ArchState64_mem64: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem64  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO64  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

-- ----------------
-- get/set misc debug convenience

get_ArchState64_verbosity :: ArchState64 -> Int
get_ArchState64_verbosity  astate = f_verbosity astate

set_ArchState64_verbosity :: ArchState64 -> Int -> IO ArchState64
set_ArchState64_verbosity  astate  verbosity = return  astate { f_verbosity = verbosity }

get_ArchState64_stop :: ArchState64 -> Stop_Reason
get_ArchState64_stop  astate = f_stop  astate

set_ArchState64_stop :: ArchState64 -> Stop_Reason -> IO ArchState64
set_ArchState64_stop  astate  stop = return  astate { f_stop = stop }

-- ================================================================

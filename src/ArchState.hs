module ArchState (ArchState, mkArchState, print_ArchState,
                  archstate_ifetch,
                  archstate_rv_read,             archstate_rv_write,
                  archstate_xlen_read,

                  archstate_gpr_read,            archstate_gpr_write,
                  archstate_csr_read_permission,
                  archstate_csr_read,            archstate_csr_write,
                  archstate_pc_read,             archstate_pc_write,

                  archstate_mem_read8,           archstate_mem_write8,
                  archstate_mem_read16,          archstate_mem_write16,
                  archstate_mem_read32,          archstate_mem_write32,
                  archstate_mem_read64,          archstate_mem_write64,

                  archstate_priv_read,           archstate_priv_write,

                  upd_ArchState_on_trap,
                  upd_ArchState_on_ret,

                  archstate_verbosity_read,      archstate_verbosity_write,

                  Stop_Reason (..),
                  archstate_stop_read,           archstate_stop_write)
where

-- ================================================================
-- This module defines the data structure holding the RISC-V CPU archtectural state
-- (plus some additional state for debugging/tracing convience)
-- and a read/write API to read and write components of the state.
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

import BitManipulation
import ArchDefs
import GPRFile
import CSRFile
import Memory
import MMIO

-- ================================================================
-- Architectural State data structure.
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data ArchState = ArchState { f_rv   :: RV,
                             f_pc   :: UInt,
                             f_gprs :: GPRFile,
                             f_csrs :: CSRFile,
                             f_mem  :: Mem,
                             f_mmio :: MMIO,
                             f_priv :: Priv_Level,

                             -- The following are for convenience for debugging only
                             -- and have no semantic relevance.
                             f_verbosity :: Int,
                             f_stop      :: Stop_Reason
                           }

data Stop_Reason = Stop_Running | Stop_Other | Stop_Break | Stop_WFI | Stop_Limit | Stop_Forced
  deriving (Eq, Show)

print_ArchState :: String -> ArchState -> IO ()
print_ArchState  indent  (ArchState  rv  pc  gprs  csrs  mem  mmio  priv  verbosity  stop) = do
  putStrLn (indent ++ show rv ++ " pc:" ++ showHex pc " priv:" ++ show priv)
  print_GPRFile  indent  gprs
  print_CSRFile  indent  csrs
  -- We do not print memory or MMIO
  putStrLn (indent ++ (show stop))

-- ================================================================
-- Instruction Fetch; uses the API below
-- TODO: for 'C', fetch first 16 bytes, check length, only then fetch next 16 bytes if not a 'C' instr

archstate_ifetch :: ArchState -> (LoadResult Word32, ArchState)
archstate_ifetch  astate = archstate_mem_read32  astate  pc
  where pc = archstate_pc_read  astate

-- ================================================================
-- API to create, read/write components of the Architectural State

-- Make an ArchState, given initial PC and memory contents
mkArchState :: RV -> UInt -> ([(Int, Word8)]) -> ArchState
mkArchState  rv  initial_PC addr_byte_list =   ArchState { f_rv   = rv,
                                                           f_pc   = initial_PC,
                                                           f_gprs = mkGPRFile,
                                                           f_csrs = mkCSRFile  rv,
                                                           f_mem  = mkMem  addr_byte_list,
                                                           f_mmio = mkMMIO,

                                                           f_priv = m_Priv_Level,
                                                           f_verbosity = 0,
                                                           f_stop      = Stop_Running}

-- ----------------
-- read/write RV

archstate_rv_read :: ArchState -> RV
archstate_rv_read  astate = f_rv  astate

archstate_rv_write :: ArchState -> RV -> IO ArchState
archstate_rv_write  astate  rv = return (astate { f_rv = rv })

archstate_xlen_read :: ArchState -> Int
archstate_xlen_read  astate | f_rv  astate == RV32 = 32
                            | f_rv  astate == RV64 = 64

-- ----------------
-- read/write GPRs

archstate_gpr_read :: ArchState -> Register -> UInt
archstate_gpr_read  astate  reg = gpr_read (f_gprs astate)  reg

archstate_gpr_write :: ArchState -> Register -> UInt -> IO ArchState
archstate_gpr_write  astate  reg  val = do
  let rv   = f_rv  astate
      val1 | rv == RV32 = signExtend  val  32
           | rv == RV64 = val
  return (astate { f_gprs = gpr_write  (f_gprs astate)  reg  val1 })

-- ----------------
-- read/write CSRs
-- Assumes CSR exists and access is legal

archstate_csr_read_permission :: ArchState -> Priv_Level -> CSR_Addr -> CSR_Permission
archstate_csr_read_permission  astate  priv  csr_addr = csr_permission  (f_csrs  astate)  priv  csr_addr

archstate_csr_read :: ArchState -> CSR_Addr -> UInt
archstate_csr_read  astate  csr_addr = csr_read  (f_csrs  astate)  csr_addr

archstate_csr_write :: ArchState -> CSR_Addr -> UInt -> IO ArchState
archstate_csr_write  astate  csr_addr  value = do
  let csr_file' = csr_write  (f_csrs  astate)  csr_addr  value
  return (astate { f_csrs = csr_file' })

-- ----------------
-- read/write PC

archstate_pc_read :: ArchState -> UInt
archstate_pc_read  astate = f_pc astate

archstate_pc_write :: ArchState -> UInt -> IO ArchState
archstate_pc_write  astate  val = return (astate { f_pc = val })

-- ----------------
-- read/write memory at various widths
-- TODO: fix up 'read' to triage memory vs. I/O like the 'write' calls and 'read' to IO can change astate

archstate_mem_read8 :: ArchState -> UInt -> (LoadResult Word8, ArchState)
archstate_mem_read8  astate  addr = (mem_read8  (f_mem astate)  addr, astate)

archstate_mem_read16 :: ArchState -> UInt -> (LoadResult Word16, ArchState)
archstate_mem_read16  astate  addr = (mem_read16  (f_mem astate)  addr, astate)

archstate_mem_read32 :: ArchState -> UInt -> (LoadResult Word32, ArchState)
archstate_mem_read32  astate  addr = (mem_read32  (f_mem astate)  addr, astate)

archstate_mem_read64 :: ArchState -> UInt -> (LoadResult Word64, ArchState)
archstate_mem_read64  astate  addr = (mem_read64  (f_mem astate)  addr, astate)

archstate_mem_write8 :: ArchState -> UInt -> Word8 -> IO ArchState
archstate_mem_write8  astate addr  val = do
  when (archstate_verbosity_read astate > 1) (
    putStrLn ("archstate_mem_write8: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = mem_write8  (f_mem astate)  addr  val})
  else do
    mmio' <- mmio_write8  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

archstate_mem_write16 :: ArchState -> UInt -> Word16 -> IO ArchState
archstate_mem_write16  astate addr  val = do
  when (archstate_verbosity_read astate > 1) (
    putStrLn ("archstate_writemem16: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = mem_write16  (f_mem astate)  addr  val})
  else do
    mmio' <- mmio_write16  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

archstate_mem_write32 :: ArchState -> UInt -> Word32 -> IO ArchState
archstate_mem_write32  astate addr  val = do
  when (archstate_verbosity_read astate > 1) (
    putStrLn ("archstate_mem_write32: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = mem_write32  (f_mem astate)  addr  val})
  else do
    mmio' <- mmio_write32  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

archstate_mem_write64 :: ArchState -> UInt -> Word64 -> IO ArchState
archstate_mem_write64  astate addr  val = do
  when (archstate_verbosity_read astate > 1) (
    putStrLn ("archstate_mem_write64: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = mem_write64  (f_mem astate)  addr  val})
  else do
    mmio' <- mmio_write64  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

-- ================================================================
-- read/write current privilege level

archstate_priv_read :: ArchState -> Priv_Level
archstate_priv_read  astate = f_priv  astate

archstate_priv_write :: ArchState -> Priv_Level -> IO ArchState
archstate_priv_write  astate  priv = return  astate { f_priv = priv }

-- ================================================================
-- Trap actions

upd_ArchState_on_trap :: ArchState -> Bool -> Exc_Code -> UInt -> IO ArchState
upd_ArchState_on_trap  astate  is_interrupt  exc_code  tval = do
  let rv      = f_rv    astate
      priv    = f_priv  astate
      pc      = f_pc    astate
      csrfile = f_csrs  astate
      (new_pc, new_priv, csrfile') = upd_csrfile_on_trap  rv  csrfile  priv  pc  is_interrupt  exc_code  tval
  return astate { f_pc = new_pc, f_priv = new_priv, f_csrs = csrfile' }

-- ================================================================
-- xRET actions

upd_ArchState_on_ret :: ArchState -> Priv_Level -> IO ArchState
upd_ArchState_on_ret  astate  priv = do
  let rv      = f_rv    astate
      csrfile = f_csrs  astate
      (new_pc, new_priv, csrfile') = upd_csrfile_on_ret  rv  csrfile  priv
  return astate { f_pc = new_pc, f_priv = new_priv, f_csrs = csrfile' }

-- ================================================================
-- read/write misc debug convenience

archstate_verbosity_read :: ArchState -> Int
archstate_verbosity_read  astate = f_verbosity astate

archstate_verbosity_write :: ArchState -> Int -> IO ArchState
archstate_verbosity_write  astate  verbosity = return  astate { f_verbosity = verbosity }

archstate_stop_read :: ArchState -> Stop_Reason
archstate_stop_read  astate = f_stop  astate

archstate_stop_write :: ArchState -> Stop_Reason -> IO ArchState
archstate_stop_write  astate  stop = return  astate { f_stop = stop }

-- ================================================================

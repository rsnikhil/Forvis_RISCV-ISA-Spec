module ArchState (ArchState, mkArchState, print_ArchState,
                  ifetch,
                  get_ArchState_rv,             set_ArchState_rv,
                  get_ArchState_xlen,

                  get_ArchState_gpr,            set_ArchState_gpr,
                  get_ArchState_csr_permission,
                  get_ArchState_csr,            set_ArchState_csr,
                  get_ArchState_PC,             set_ArchState_PC,

                  get_ArchState_mem8,           set_ArchState_mem8,
                  get_ArchState_mem16,          set_ArchState_mem16,
                  get_ArchState_mem32,          set_ArchState_mem32,
                  get_ArchState_mem64,          set_ArchState_mem64,

                  get_ArchState_priv,           set_ArchState_priv,

                  upd_ArchState_on_trap,
                  upd_ArchState_on_ret,

                  get_ArchState_verbosity,      set_ArchState_verbosity,

                  Stop_Reason (..),
                  get_ArchState_stop,           set_ArchState_stop)
where

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

ifetch :: ArchState -> (LoadResult Word32, ArchState)
ifetch  astate = get_ArchState_mem32  astate  pc
  where pc = get_ArchState_PC  astate

-- ================================================================
-- API to create, set/get components of the Architectural State

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
-- get/set RV

get_ArchState_rv :: ArchState -> RV
get_ArchState_rv  astate = f_rv  astate

set_ArchState_rv :: ArchState -> RV -> IO ArchState
set_ArchState_rv  astate  rv = return (astate { f_rv = rv })

get_ArchState_xlen :: ArchState -> Int
get_ArchState_xlen  astate | f_rv  astate == RV32 = 32
                           | f_rv  astate == RV64 = 64

-- ----------------
-- get/set GPRs

get_ArchState_gpr :: ArchState -> Register -> UInt
get_ArchState_gpr  astate  reg = get_gpr (f_gprs astate)  reg

set_ArchState_gpr :: ArchState -> Register -> UInt -> IO ArchState
set_ArchState_gpr  astate  reg  val = do
  let rv   = f_rv  astate
      val1 | rv == RV32 = signExtend  val  32
           | rv == RV64 = val
  return (astate { f_gprs = set_gpr  (f_gprs astate)  reg  val1 })

-- ----------------
-- get/set CSRs
-- Assumes CSR exists and access is legal

get_ArchState_csr_permission :: ArchState -> Priv_Level -> CSR_Addr -> CSR_Permission
get_ArchState_csr_permission  astate  priv  csr_addr = csr_permission  (f_csrs  astate)  priv  csr_addr

get_ArchState_csr :: ArchState -> CSR_Addr -> UInt
get_ArchState_csr  astate  csr_addr = get_csr  (f_csrs  astate)  csr_addr

set_ArchState_csr :: ArchState -> CSR_Addr -> UInt -> IO ArchState
set_ArchState_csr  astate  csr_addr  value = do
  let csr_file' = set_csr  (f_csrs  astate)  csr_addr  value
  return (astate { f_csrs = csr_file' })

-- ----------------
-- get/set PC

get_ArchState_PC :: ArchState -> UInt
get_ArchState_PC  astate = f_pc astate

set_ArchState_PC :: ArchState -> UInt -> IO ArchState
set_ArchState_PC  astate  val = return (astate { f_pc = val })

-- ----------------
-- get/set memory at various widths
-- TODO: fix up 'get' to triage memory vs. I/O like the 'set' calls and 'get' to IO can change astate

get_ArchState_mem8 :: ArchState -> UInt -> (LoadResult Word8, ArchState)
get_ArchState_mem8  astate  addr = (getMem8  (f_mem astate)  addr, astate)

get_ArchState_mem16 :: ArchState -> UInt -> (LoadResult Word16, ArchState)
get_ArchState_mem16  astate  addr = (getMem16  (f_mem astate)  addr, astate)

get_ArchState_mem32 :: ArchState -> UInt -> (LoadResult Word32, ArchState)
get_ArchState_mem32  astate  addr = (getMem32  (f_mem astate)  addr, astate)

get_ArchState_mem64 :: ArchState -> UInt -> (LoadResult Word64, ArchState)
get_ArchState_mem64  astate  addr = (getMem64  (f_mem astate)  addr, astate)

set_ArchState_mem8 :: ArchState -> UInt -> Word8 -> IO ArchState
set_ArchState_mem8  astate addr  val = do
  when (get_ArchState_verbosity astate > 1) (
    putStrLn ("set_ArchState_mem8: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem8  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO8  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

set_ArchState_mem16 :: ArchState -> UInt -> Word16 -> IO ArchState
set_ArchState_mem16  astate addr  val = do
  when (get_ArchState_verbosity astate > 1) (
    putStrLn ("set_ArchState_mem16: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem16  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO16  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

set_ArchState_mem32 :: ArchState -> UInt -> Word32 -> IO ArchState
set_ArchState_mem32  astate addr  val = do
  when (get_ArchState_verbosity astate > 1) (
    putStrLn ("set_ArchState_mem32: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem32  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO32  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

set_ArchState_mem64 :: ArchState -> UInt -> Word64 -> IO ArchState
set_ArchState_mem64  astate addr  val = do
  when (get_ArchState_verbosity astate > 1) (
    putStrLn ("set_ArchState_mem64: addr " ++ (showHex addr " val ") ++ (showHex val "")))
  if not (is_IO_addr  addr) then
    return (astate { f_mem = setMem64  (f_mem astate)  addr  val})
  else do
    mmio' <- setMMIO64  (f_mmio astate)  addr  val
    return (astate { f_mmio = mmio'})

-- ================================================================
-- get/set current privilege level

get_ArchState_priv :: ArchState -> Priv_Level
get_ArchState_priv  astate = f_priv  astate

set_ArchState_priv :: ArchState -> Priv_Level -> IO ArchState
set_ArchState_priv  astate  priv = return  astate { f_priv = priv }

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
-- get/set misc debug convenience

get_ArchState_verbosity :: ArchState -> Int
get_ArchState_verbosity  astate = f_verbosity astate

set_ArchState_verbosity :: ArchState -> Int -> IO ArchState
set_ArchState_verbosity  astate  verbosity = return  astate { f_verbosity = verbosity }

get_ArchState_stop :: ArchState -> Stop_Reason
get_ArchState_stop  astate = f_stop  astate

set_ArchState_stop :: ArchState -> Stop_Reason -> IO ArchState
set_ArchState_stop  astate  stop = return  astate { f_stop = stop }

-- ================================================================

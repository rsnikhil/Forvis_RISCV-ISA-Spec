module CSRFile (CSR (..),
                CSRFile,  print_CSRFile,  mkCSRFile,
                get_csr,           set_csr,
                get_csr_from_addr, set_csr_from_addr) where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V CSR (Control and Status Register) register file.

-- ================================================================
-- Standard Haskell imports

import Data.Map
import Data.Maybe
import Data.Word
import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

import ArchDefs64

-- ================================================================
-- Symbolic names for CSRs

data CSR = CSR_mstatus |
           CSR_misa |
           CSR_medeleg |
           CSR_mideleg |
           CSR_mie |
           CSR_mtvec |
           CSR_mcounteren |
           CSR_mscratch |
           CSR_mepc |
           CSR_mcause |
           CSR_mtval |
           CSR_mip |
           CSR_mcycle |
           CSR_minstret |
           CSR_INVALID
  deriving (Eq, Ord, Show)

-- ================================================================
-- The CSR file is represented as Data.Map.Map from CSR names to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

newtype CSRFile = CSRFile (Data.Map.Map  CSR  MachineWord)
  deriving (Show)

-- TODO: fill out this function
print_CSRFile :: String -> CSRFile -> IO ()
print_CSRFile  indent  csrfile = do
  putStrLn (indent ++ "minstret:" ++ show (get_csr  csrfile  CSR_minstret))

mkCSRFile :: Integer -> CSRFile
mkCSRFile 32 = CSRFile (Data.Map.fromList [(CSR_misa, misa)])
  where misa :: MachineWord
        misa = shiftL  1  (xlen - 2)
mkCSRFile 64 = CSRFile (Data.Map.fromList [(CSR_misa, misa)])
  where misa :: MachineWord
        misa = shiftL  2  (xlen - 2)

-- The following get/put are based on CSR names

get_csr :: CSRFile -> CSR -> MachineWord
get_csr  (CSRFile csrfile)  csr = fromMaybe 0 (Data.Map.lookup csr csrfile)

set_csr :: CSRFile -> CSR -> MachineWord -> CSRFile
set_csr  (CSRFile csrfile)  csr  value = CSRFile (Data.Map.insert  csr  value  csrfile)

-- The following get/put are based on CSR addresses

get_csr_from_addr :: CSRFile -> Word16 -> MachineWord
get_csr_from_addr  csrfile  csr_addr
      | csr_addr == 0x300 = get_csr  csrfile  CSR_mstatus
      | csr_addr == 0x301 = get_csr  csrfile  CSR_misa
      | csr_addr == 0x302 = get_csr  csrfile  CSR_medeleg
      | csr_addr == 0x303 = get_csr  csrfile  CSR_mideleg
      | csr_addr == 0x304 = get_csr  csrfile  CSR_mie
      | csr_addr == 0x305 = get_csr  csrfile  CSR_mtvec
      | csr_addr == 0x306 = get_csr  csrfile  CSR_mcounteren
      | csr_addr == 0x340 = get_csr  csrfile  CSR_mscratch
      | csr_addr == 0x341 = get_csr  csrfile  CSR_mepc
      | csr_addr == 0x342 = get_csr  csrfile  CSR_mcause
      | csr_addr == 0x343 = get_csr  csrfile  CSR_mtval
      | csr_addr == 0x344 = get_csr  csrfile  CSR_mip
      | csr_addr == 0xB00 = get_csr  csrfile  CSR_mcycle
      | csr_addr == 0xB02 = get_csr  csrfile  CSR_minstret
      | True              = get_csr  csrfile  CSR_INVALID

set_csr_from_addr :: CSRFile -> Word16 -> MachineWord -> CSRFile
set_csr_from_addr  csrfile  csr_addr  value
      | csr_addr == 0x300 = set_csr  csrfile  CSR_mstatus     value
      | csr_addr == 0x301 = set_csr  csrfile  CSR_misa        value
      | csr_addr == 0x302 = set_csr  csrfile  CSR_medeleg     value
      | csr_addr == 0x303 = set_csr  csrfile  CSR_mideleg     value
      | csr_addr == 0x304 = set_csr  csrfile  CSR_mie         value
      | csr_addr == 0x305 = set_csr  csrfile  CSR_mtvec       value
      | csr_addr == 0x306 = set_csr  csrfile  CSR_mcounteren  value
      | csr_addr == 0x340 = set_csr  csrfile  CSR_mscratch    value
      | csr_addr == 0x341 = set_csr  csrfile  CSR_mepc        value
      | csr_addr == 0x342 = set_csr  csrfile  CSR_mcause      value
      | csr_addr == 0x343 = set_csr  csrfile  CSR_mtval       value
      | csr_addr == 0x344 = set_csr  csrfile  CSR_mip         value
      | csr_addr == 0xB00 = set_csr  csrfile  CSR_mcycle      value
      | csr_addr == 0xB02 = set_csr  csrfile  CSR_minstret    value
      | True              = csrfile

-- ================================================================

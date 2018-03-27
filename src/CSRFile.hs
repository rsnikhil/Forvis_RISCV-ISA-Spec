module CSRFile (CSR_Addr,
                CSRFile,
                mkCSRFile,
                print_CSRFile,
                get_csr,
                set_csr,

                csr_addr_mstatus,
                csr_addr_misa,
                csr_addr_medeleg,
                csr_addr_mideleg,
                csr_addr_mie,
                csr_addr_mtvec,
                csr_addr_mcounteren,
                csr_addr_mscratch,
                csr_addr_mepc,
                csr_addr_mcause,
                csr_addr_mtval,
                csr_addr_mip,
                csr_addr_mcycle,
                csr_addr_minstret)
where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V CSR (Control and Status Register) register file.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Word
import Data.Bits
import Numeric (showHex, readHex)
import qualified Data.Map.Strict as Data_Map

-- Project imports

import BitManipulation
import ArchDefs64

-- ================================================================
-- Using Word16 for CSR_Addrs, which are actually only 12 bits

type CSR_Addr = Word16

-- ================================================================
-- Symbolic names for CSRs

csr_addr_mstatus    :: CSR_Addr;    csr_addr_mstatus    = 0x300
csr_addr_misa       :: CSR_Addr;    csr_addr_misa       = 0x301
csr_addr_medeleg    :: CSR_Addr;    csr_addr_medeleg    = 0x302
csr_addr_mideleg    :: CSR_Addr;    csr_addr_mideleg    = 0x303
csr_addr_mie        :: CSR_Addr;    csr_addr_mie        = 0x304
csr_addr_mtvec      :: CSR_Addr;    csr_addr_mtvec      = 0x305
csr_addr_mcounteren :: CSR_Addr;    csr_addr_mcounteren = 0x306
csr_addr_mscratch   :: CSR_Addr;    csr_addr_mscratch   = 0x340
csr_addr_mepc       :: CSR_Addr;    csr_addr_mepc       = 0x341
csr_addr_mcause     :: CSR_Addr;    csr_addr_mcause     = 0x342
csr_addr_mtval      :: CSR_Addr;    csr_addr_mtval      = 0x343
csr_addr_mip        :: CSR_Addr;    csr_addr_mip        = 0x344
csr_addr_mcycle     :: CSR_Addr;    csr_addr_mcycle     = 0xB00
csr_addr_minstret   :: CSR_Addr;    csr_addr_minstret   = 0xB02

m_csr_addrs_and_names :: [(CSR_Addr, String)]
m_csr_addrs_and_names  =
  [ (csr_addr_mstatus,    "mstatus"),
    (csr_addr_misa,       "misa"),
    (csr_addr_medeleg,    "medeleg"),
    (csr_addr_mideleg,    "mideleg"),
    (csr_addr_mie,        "mie"),
    (csr_addr_mtvec,      "mtvec"),
    (csr_addr_mcounteren, "mcounteren"),
    (csr_addr_mscratch,   "mscratch"),
    (csr_addr_mepc,       "mepc"),
    (csr_addr_mcause,     "mcause"),
    (csr_addr_mtval,      "mtval"),
    (csr_addr_mip,        "mip"),
    (csr_addr_mcycle,     "mcycle"),
    (csr_addr_minstret,   "minstret") ]

m_csr_reset_values :: XLEN -> [(CSR_Addr, Word64)]
m_csr_reset_values  xlen =
  [ (csr_addr_mstatus,    0),
    (csr_addr_misa,       fromIntegral (read_vhex (if xlen == RV32 then "0x4000_0000" else "0x8000_0000_0000_0000"))),
    (csr_addr_medeleg,    0),
    (csr_addr_mideleg,    0),
    (csr_addr_mie,        0),
    (csr_addr_mtvec,      0),
    (csr_addr_mcounteren, 0),
    (csr_addr_mscratch,   0),
    (csr_addr_mepc,       0),
    (csr_addr_mcause,     0),
    (csr_addr_mtval,      0),
    (csr_addr_mip,        0),
    (csr_addr_mcycle,     0),
    (csr_addr_minstret,   0) ]

-- ================================================================
-- The CSR file is represented as Data_Map.Map from CSR names to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

newtype CSRFile = CSRFile (Data_Map.Map  CSR_Addr  WordXLEN)
  deriving (Show)

-- ================================================================

mkCSRFile :: XLEN -> CSRFile
mkCSRFile xlen  = CSRFile (Data_Map.fromList  (m_csr_reset_values  xlen))

print_CSRFile :: String -> CSRFile -> IO ()
print_CSRFile  indent  csrfile = do
  mapM_  (\(csr_addr, csr_name) -> case (get_csr  csrfile  csr_addr) of
                                     Nothing       -> putStrLn (indent ++ csr_name ++ "<csr undefined>")
                                     Just wordXLEN -> putStrLn (indent ++ csr_name ++ showHex wordXLEN ""))
         m_csr_addrs_and_names

-- ================================================================
-- Getters and Setters
-- In an implementation, CSR reads and writes can have wide-ranging side effects
--    and therefore the full machine state may be necessary as arg and result.
-- However, in this spec, reads do not have side effects, and writes affect only CSRs;
--    hence we limit the arg to a CSRFile, and the get-result to a WordXLEN
-- We return 'Maybe' types since the csr address may be illegal.


get_csr :: CSRFile -> CSR_Addr -> Maybe WordXLEN
get_csr  (CSRFile map)  csr_addr = Data_Map.lookup  csr_addr  map

-- In set_csr we do a Data_Map.lookup just to check if it is an existing csr address.
-- Thus, set_csr never enters new csr addresses into the map.

set_csr :: CSRFile -> CSR_Addr -> WordXLEN -> Maybe CSRFile
set_csr  (CSRFile map)  csr_addr  value = case (Data_Map.lookup  csr_addr  map) of
                                            Nothing -> Nothing
                                            Just _  -> Just (CSRFile (Data_Map.insert  csr_addr  value  map))

-- ================================================================

module CSRFile (CSR_Addr,
                csr_addr_ustatus,
                csr_addr_uie,
                csr_addr_utvec,

                csr_addr_uscratch,
                csr_addr_uepc,
                csr_addr_ucause,
                csr_addr_utval,
                csr_addr_uip,

                csr_addr_fflags,
                csr_addr_frm,
                csr_addr_fcsr,

                csr_addr_cycle,
                csr_addr_time,
                csr_addr_instret,

                csr_addr_cycleh,
                csr_addr_timeh,
                csr_addr_instreth,

                csr_addr_sstatus,
                csr_addr_sedeleg,
                csr_addr_sideleg,
                csr_addr_sie,
                csr_addr_stvec,
                csr_addr_scounteren,

                csr_addr_sscratch,
                csr_addr_sepc,
                csr_addr_scause,
                csr_addr_stval,
                csr_addr_sip,

                csr_addr_satp,

                csr_addr_mvendorid,
                csr_addr_marchid,
                csr_addr_mimpid,
                csr_addr_mhartid,

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
                csr_addr_minstret,

                csr_addr_mcycleh,
                csr_addr_minstreth,

                CSRFile,  mkCSRFile,  print_CSRFile,
                CSR_Permission (..),  csr_permission,
                get_csr, set_csr,
                misa_flag,
                upd_csrfile_on_trap,
                upd_csrfile_on_ret
               )
where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V CSR (Control and Status Register) register file.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Word
import Data.Bits
import Data.Char
import Numeric (showHex, readHex)
import qualified Data.Map.Strict as Data_Map

-- Project imports

import BitManipulation
import ArchDefs64

-- ================================================================
-- User-Level CSRs

csr_addr_ustatus    :: CSR_Addr;    csr_addr_ustatus    = 0x000
csr_addr_uie        :: CSR_Addr;    csr_addr_uie        = 0x004
csr_addr_utvec      :: CSR_Addr;    csr_addr_utvec      = 0x005

csr_addr_uscratch   :: CSR_Addr;    csr_addr_uscratch   = 0x040
csr_addr_uepc       :: CSR_Addr;    csr_addr_uepc       = 0x041
csr_addr_ucause     :: CSR_Addr;    csr_addr_ucause     = 0x042
csr_addr_utval      :: CSR_Addr;    csr_addr_utval      = 0x043
csr_addr_uip        :: CSR_Addr;    csr_addr_uip        = 0x044

csr_addr_fflags     :: CSR_Addr;    csr_addr_fflags     = 0x001
csr_addr_frm        :: CSR_Addr;    csr_addr_frm        = 0x002
csr_addr_fcsr       :: CSR_Addr;    csr_addr_fcsr       = 0x003

csr_addr_cycle      :: CSR_Addr;    csr_addr_cycle      = 0xC00
csr_addr_time       :: CSR_Addr;    csr_addr_time       = 0xC01
csr_addr_instret    :: CSR_Addr;    csr_addr_instret    = 0xC02

csr_addr_cycleh     :: CSR_Addr;    csr_addr_cycleh     = 0xC80
csr_addr_timeh      :: CSR_Addr;    csr_addr_timeh      = 0xC81
csr_addr_instreth   :: CSR_Addr;    csr_addr_instreth   = 0xC82

-- TODO: hpmcounterN, hpmcounterNh

u_csr_addrs_and_names :: [(CSR_Addr, String)]
u_csr_addrs_and_names  =
  [ (csr_addr_ustatus,    "ustatus"),
    (csr_addr_uie,        "uie"),
    (csr_addr_utvec,      "utvec"),

    (csr_addr_uscratch,   "uscratch"),
    (csr_addr_uepc,       "uepc"),
    (csr_addr_ucause,     "ucause"),
    (csr_addr_utval,      "utval"),
    (csr_addr_uip,        "uip"),

    (csr_addr_fflags,     "fflags"),
    (csr_addr_frm,        "frm"),
    (csr_addr_fcsr,       "fcsr"),

    (csr_addr_cycle,      "cycle"),
    (csr_addr_time,       "time"),
    (csr_addr_instret,    "instret"),

    (csr_addr_cycleh,     "cycleh"),
    (csr_addr_timeh,      "timeh"),
    (csr_addr_instreth,   "instreth") ]

u_csr_reset_values :: XLEN -> [(CSR_Addr, WordXLEN)]
u_csr_reset_values  xlen =
  [ (csr_addr_ustatus,    0),
    (csr_addr_uie,        0),
    (csr_addr_utvec,      0),

    (csr_addr_uscratch,   0),
    (csr_addr_uepc,       0),
    (csr_addr_ucause,     0),
    (csr_addr_utval,      0),
    (csr_addr_uip,        0),

    (csr_addr_fflags,     0),
    (csr_addr_frm,        0),
    (csr_addr_fcsr,       0),

    (csr_addr_cycle,      0),
    (csr_addr_time,       0),
    (csr_addr_instret,    0),

    (csr_addr_cycleh,     0),
    (csr_addr_timeh,      0),
    (csr_addr_instreth,   0) ]

-- ================================================================
-- Supervisor-Level CSRs

csr_addr_sstatus    :: CSR_Addr;    csr_addr_sstatus    = 0x100
csr_addr_sedeleg    :: CSR_Addr;    csr_addr_sedeleg    = 0x102
csr_addr_sideleg    :: CSR_Addr;    csr_addr_sideleg    = 0x103
csr_addr_sie        :: CSR_Addr;    csr_addr_sie        = 0x104
csr_addr_stvec      :: CSR_Addr;    csr_addr_stvec      = 0x105
csr_addr_scounteren :: CSR_Addr;    csr_addr_scounteren = 0x106

csr_addr_sscratch   :: CSR_Addr;    csr_addr_sscratch   = 0x140
csr_addr_sepc       :: CSR_Addr;    csr_addr_sepc       = 0x141
csr_addr_scause     :: CSR_Addr;    csr_addr_scause     = 0x142
csr_addr_stval      :: CSR_Addr;    csr_addr_stval      = 0x143
csr_addr_sip        :: CSR_Addr;    csr_addr_sip        = 0x144

csr_addr_satp       :: CSR_Addr;    csr_addr_satp       = 0x180

s_csr_addrs_and_names :: [(CSR_Addr, String)]
s_csr_addrs_and_names  =
  [ (csr_addr_sstatus,    "sstatus"),
    (csr_addr_sedeleg,    "sedeleg"),
    (csr_addr_sideleg,    "sideleg"),
    (csr_addr_sie,        "sie"),
    (csr_addr_stvec,      "stvec"),
    (csr_addr_scounteren, "scounteren"),

    (csr_addr_sscratch,   "sscratch"),
    (csr_addr_sepc,       "sepc"),
    (csr_addr_scause,     "scause"),
    (csr_addr_stval,      "stval"),
    (csr_addr_sip,        "sip"),

    (csr_addr_satp,       "satp") ]

s_csr_reset_values :: XLEN -> [(CSR_Addr, WordXLEN)]
s_csr_reset_values  xlen =
  [ (csr_addr_sstatus,    0),
    (csr_addr_sedeleg,    0),
    (csr_addr_sideleg,    0),
    (csr_addr_sie,        0),
    (csr_addr_stvec,      0),
    (csr_addr_scounteren, 0),

    (csr_addr_sscratch,   0),
    (csr_addr_sepc,       0),
    (csr_addr_scause,     0),
    (csr_addr_stval,      0),
    (csr_addr_sip,        0),

    (csr_addr_satp,       0) ]

-- ================================================================
-- Machine-Level CSRs

csr_addr_mvendorid  :: CSR_Addr;    csr_addr_mvendorid  = 0xF11
csr_addr_marchid    :: CSR_Addr;    csr_addr_marchid    = 0xF12
csr_addr_mimpid     :: CSR_Addr;    csr_addr_mimpid     = 0xF13
csr_addr_mhartid    :: CSR_Addr;    csr_addr_mhartid    = 0xF14

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

-- TODO: pmpcfgN, pmpaddrN

csr_addr_mcycle     :: CSR_Addr;    csr_addr_mcycle     = 0xB00
csr_addr_minstret   :: CSR_Addr;    csr_addr_minstret   = 0xB02

-- TODO: mhpmcounterN

csr_addr_mcycleh    :: CSR_Addr;    csr_addr_mcycleh    = 0xB80
csr_addr_minstreth  :: CSR_Addr;    csr_addr_minstreth  = 0xB82

-- TODO: mhpmcounterNh

-- TODO: tselect, tdata1, tdata2, tdata3, dcsr, dpc, dscratch

m_csr_addrs_and_names :: [(CSR_Addr, String)]
m_csr_addrs_and_names  =
  [ (csr_addr_mvendorid,  "mvendorid"),
    (csr_addr_marchid,    "marchid"),
    (csr_addr_mimpid,     "mimpid"),
    (csr_addr_mhartid,    "mhartid"),

    (csr_addr_mstatus,    "mstatus"),
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
    (csr_addr_minstret,   "minstret"),

    (csr_addr_mcycleh,    "mcycleh"),
    (csr_addr_minstreth,  "minstreth") ]

m_csr_reset_values :: XLEN -> [(CSR_Addr, WordXLEN)]
m_csr_reset_values  xlen =
  [ (csr_addr_mvendorid,  0),
    (csr_addr_marchid,    0),
    (csr_addr_mimpid,     0),
    (csr_addr_mhartid,    0),

    (csr_addr_mstatus,    0),
    -- (csr_addr_misa,       fromIntegral (read_vhex "0x4000_0000")),    -- TODO: RV32 only
    (csr_addr_misa,       fromIntegral (read_vhex "0x8000_0000_0000_0000")),    -- TODO: RV64 only
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
    (csr_addr_minstret,   0),

    (csr_addr_mcycleh,    0),
    (csr_addr_minstreth,  0) ]

-- ================================================================
-- The CSR file is represented as Data_Map.Map from CSR names to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

newtype CSRFile = CSRFile (Data_Map.Map  CSR_Addr  WordXLEN)
  deriving (Show)

-- ================================================================

mkCSRFile :: XLEN -> CSRFile
mkCSRFile xlen  = CSRFile (Data_Map.fromList  ((u_csr_reset_values  xlen) ++
                                               (s_csr_reset_values  xlen) ++
                                               (m_csr_reset_values  xlen)   ))

print_CSRFile :: String -> CSRFile -> IO ()
print_CSRFile  indent  csrfile = do
  mapM_
    (\(csr_addr, csr_name) -> do
        let wordXLEN = get_csr  csrfile  csr_addr
        putStrLn (indent ++ csr_name ++ ":" ++ showHex wordXLEN ""))
    (u_csr_addrs_and_names ++
     s_csr_addrs_and_names ++
     m_csr_addrs_and_names)

-- ================================================================
-- Access permissions for a CSR, at a given Privilege Level
-- [Note: csr_addr [11:10] indicates 'read-only' if == 2'b11
--        csr_addr [ 9: 8] indicates minimum privilege for access

data  CSR_Permission = CSR_Permission_None | CSR_Permission_RO | CSR_Permission_RW
  deriving (Eq, Show)

csr_permission :: CSRFile -> Priv_Level -> CSR_Addr -> CSR_Permission
csr_permission  (CSRFile dm)  priv  csr_addr =
  let
    exists     = Data_Map.member  csr_addr  dm

    addr_9_8   = bitSlice csr_addr   8  10
    priv_ok    = priv >= fromIntegral addr_9_8

    addr_11_10 = bitSlice csr_addr  10  12
  in
    if (not exists) || (not priv_ok)
    then CSR_Permission_None
    else if (addr_11_10 == 3)
         then CSR_Permission_RO
         else CSR_Permission_RW

-- ================================================================
-- Getter and Setter
-- In an implementation, CSR reads and writes can have wide-ranging side effects
--    and therefore the full machine state may be necessary as arg and result.
-- However, in this spec, reads do not have side effects, and writes affect only CSRs;
--    hence we limit the arg to a CSRFile, and the get-result to a WordXLEN
-- These functions assume legality checks have already been done.


get_csr :: CSRFile -> CSR_Addr -> WordXLEN
get_csr  (CSRFile dm)  csr_addr = fromMaybe  0  (Data_Map.lookup  csr_addr  dm)

-- In set_csr checks 'member' to avoid inserting new csr_addr into the map

set_csr :: CSRFile -> CSR_Addr -> WordXLEN -> CSRFile
set_csr  csrfile  csr_addr  value = CSRFile dm'
  where CSRFile dm = csrfile
        dm' = if Data_Map.member csr_addr dm
              then Data_Map.insert  csr_addr  value  dm
              else dm

-- ================================================================
-- MISA

misa_flag :: WordXLEN -> Char -> Bool
misa_flag  misa  letter | isAsciiUpper  letter = (((shiftR  misa  ((ord letter) - (ord 'A'))) .&. 1) == 1)
misa_flat  misa  letter | isAsciiLower  letter = (((shiftR  misa  ((ord letter) - (ord 'a'))) .&. 1) == 1)
                        | otherwise            = False

-- ================================================================
-- upd_csrfile_on_trap performs all the CSR manipulations for an exception (trap/interrupt)

upd_csrfile_on_trap :: CSRFile
                    -> Priv_Level      -- from privilege
                    -> WordXLEN        -- PC
                    -> Bool            -- is interrupt, not trap
                    -> Exc_Code        -- interrupt or trap code
                    -> WordXLEN        -- trap value
                    -> (WordXLEN,      -- new PC
                        Priv_Level,    -- new privilege
                        CSRFile)       -- updated CSR file
upd_csrfile_on_trap  csrfile  priv  pc  interrupt_not_trap  exc_code  xtval = result
  where misa        = get_csr  csrfile  csr_addr_misa
        mstatus     = get_csr  csrfile  csr_addr_mstatus
        medeleg     = get_csr  csrfile  csr_addr_medeleg
        mideleg     = get_csr  csrfile  csr_addr_mideleg
        sstatus     = get_csr  csrfile  csr_addr_sstatus
        sedeleg     = get_csr  csrfile  csr_addr_sedeleg
        sideleg     = get_csr  csrfile  csr_addr_sideleg

        new_priv    = new_priv_on_exception  misa  priv  interrupt_not_trap  exc_code  medeleg  mideleg  sedeleg  sideleg

        new_mstatus = upd_status_on_trap  mstatus priv  new_priv

        (csr_addr_xepc,
         csr_addr_xcause,
         csr_addr_xtval,
         csr_addr_xtvec,  xtvec) = if (new_priv == m_Priv_Level)
                                   then (csr_addr_mepc,
                                         csr_addr_mcause,
                                         csr_addr_mtval,
                                         csr_addr_mtvec,  get_csr  csrfile  csr_addr_mtvec)
                                   else (csr_addr_sepc,
                                         csr_addr_scause,
                                         csr_addr_stval,
                                         csr_addr_stvec,  get_csr  csrfile  csr_addr_stvec)

        -- Record new status, epc, cause, tval
        csrfile1 = set_csr  csrfile   csr_addr_mstatus  new_mstatus
        csrfile2 = set_csr  csrfile1  csr_addr_xepc  pc
        csrfile3 = set_csr  csrfile2  csr_addr_xcause  (mkCause  interrupt_not_trap  exc_code)
        csrfile4 = if (not interrupt_not_trap)
                   then set_csr  csrfile3  csr_addr_xtval  xtval
                   else csrfile3

        -- Compute the new PC
        vector_offset = fromIntegral (exc_code * 4)
        new_pc = if interrupt_not_trap && (tvec_mode (xtvec) == tvec_mode_VECTORED)
                 then ((tvec_base xtvec) * 4) + vector_offset
                 else ((tvec_base xtvec) * 4)

        result = (new_pc, new_priv, csrfile4)

-- Compute new privilege level on an exception, taking into accountdelegation

new_priv_on_exception :: WordXLEN   ->    -- misa
                         Priv_Level ->    -- priv at which the exception occurred
                         Bool       ->    -- is interrupt, not trap
                         Exc_Code   ->    -- trap or interrupt code
                         WordXLEN   ->    -- medeleg
                         WordXLEN   ->    -- mideleg
                         WordXLEN   ->    -- sedeleg
                         WordXLEN   ->    -- sideleg
                         Priv_Level       -- new priv
new_priv_on_exception  misa  priv  interrupt_not_trap  exc_code  medeleg  mideleg  sedeleg  sideleg =
  let misa_s       = misa_flag  misa  'S'
      misa_n       = misa_flag  misa  'N'
      j            = (fromIntegral exc_code) :: Int
      m_delegating = testBit  (if interrupt_not_trap then mideleg else medeleg)  j
      s_delegating = testBit  (if interrupt_not_trap then sideleg else sedeleg)  j

      deleg_m_to_s = (priv < m_Priv_Level)  && misa_s && m_delegating
      deleg_s_to_u = (priv == u_Priv_Level) && misa_s && misa_n && s_delegating
      deleg_m_to_u = (priv == u_Priv_Level) && (not misa_s) && misa_n && m_delegating
  in
    if deleg_m_to_s then
      if deleg_s_to_u then
        u_Priv_Level
      else
        s_Priv_Level
    else if deleg_m_to_u then
      u_Priv_Level
    else
      m_Priv_Level

-- Update the mstatus register based on a trap

upd_status_on_trap :: WordXLEN -> Priv_Level -> Priv_Level -> WordXLEN
upd_status_on_trap  status  from_priv_y  to_priv_x =
  let
    -- Push the interrupt-enable stack (ie to pie)
    ie_j  = fromIntegral to_priv_x
    pie_j = fromIntegral to_priv_x + 4
    status1 = if testBit  status  ie_j
              then setBit    status  pie_j
              else clearBit  status  pie_j
    status2 = clearBit  status1  ie_j

    -- Set mPP or sPP to y
    y = (fromIntegral from_priv_y) :: WordXLEN

    status3 = if to_priv_x == m_Priv_Level
              then (status .&. complement 0x1800) .|. (shiftL  y  11)
              else (status .&. complement 0x0400) .|. (shiftL  (y .&. 1)   8)
  in
    status3

-- ================================================================
-- Trap Vectors (mtvec, stvec, utvec) have
--    a 'mode' in bits [1:0]
--    a 'base' in bits [xlen-1:2]

tvec_mode :: WordXLEN -> Word
tvec_mode  tvec = fromIntegral (tvec .&. 3)

tvec_mode_DIRECT   :: Word;  tvec_mode_DIRECT   = 0
tvec_mode_VECTORED :: Word;  tvec_mode_VECTORED = 1

tvec_base :: WordXLEN -> WordXLEN
tvec_base  tvec = shiftR  tvec  2

-- ================================================================
-- upd_csrfile_on_ret performs all the CSR manipulations for an xRET

upd_csrfile_on_ret :: CSRFile
                   -> Priv_Level
                   -> (WordXLEN,      -- new PC
                       Priv_Level,    -- new privilege
                       CSRFile)       -- updated CSR file
upd_csrfile_on_ret  csrfile  priv = result
  where misa                    = get_csr  csrfile  csr_addr_misa
        mstatus                 = get_csr  csrfile  csr_addr_mstatus
        (new_priv, new_mstatus) = upd_status_on_ret  misa  mstatus  priv
        new_pc                  = get_csr  csrfile  (if priv == m_Priv_Level
                                                     then csr_addr_mepc
                                                     else if priv == s_Priv_Level
                                                          then csr_addr_sepc
                                                          else csr_addr_uepc)
        new_csrfile             = set_csr  csrfile  csr_addr_mstatus  new_mstatus
        result                  = (new_pc, new_priv, new_csrfile)

-- Update the mstatus register based on an xRET

upd_status_on_ret :: WordXLEN -> WordXLEN -> Priv_Level -> (Priv_Level, WordXLEN)
upd_status_on_ret  misa  status  from_priv_x =
  let
    -- Pop the interrupt-enable stack (pie to ie)
    ie_j  = fromIntegral from_priv_x
    pie_j = fromIntegral from_priv_x + 4
    status1 = if testBit  status  pie_j
              then setBit    status  ie_j
              else clearBit  status  ie_j
    -- Enable interrupt at from_priv_x
    status2 = setBit  status  pie_j

    -- Default previous-priv at bottom of xPP stack
    default_pp = fromIntegral (if (misa_flat  misa  'U') then u_Priv_Level else m_Priv_Level)

    -- Pop the previous priv level
    to_priv_y = fromIntegral (if (from_priv_x == m_Priv_Level)
                              then bitSlice  status2  11 13
                              else bitSlice  status    8  9)
    status3   = if (from_priv_x == m_Priv_Level)
                then
                  ((status2 .&. complement 0x1800) .|. shiftL  default_pp  11)
                else
                  ((status2 .&. complement 0x400) .|. shiftL  (default_pp .&. 1)  8)
  in
    (to_priv_y, status3)

-- ================================================================

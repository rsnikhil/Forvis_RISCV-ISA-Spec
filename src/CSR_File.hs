-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module CSR_File where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V CSR (Control and Status Register) register file.
-- Support CSRs for M (Machine), S (Supervisor) and U (User)
-- privilege levels.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Bits
import Data.Char
import Numeric (showHex, readHex)
import qualified Data.Map.Strict as Data_Map

-- Project imports

import Bit_Utils
import Arch_Defs

-- ****************************************************************
-- ****************************************************************
-- ****************************************************************
-- The CSR Register File

-- ================================================================
-- The CSR file is represented as Data_Map.Map from CSR names to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

newtype CSR_File = CSR_File (Data_Map.Map  CSR_Addr  Integer)

-- ================================================================
-- Constructor: make and return a new CSR file

mkCSR_File :: RV -> Integer -> CSR_File
mkCSR_File    rv    misa =
  let
    dm = (Data_Map.fromList  (u_csr_reset_values ++
                              s_csr_reset_values ++
                              (m_csr_reset_values  rv  misa)))
  in
    CSR_File  dm

-- ================================================================
-- For debugging only

print_CSR_File :: String -> RV -> CSR_File -> IO ()
print_CSR_File  indent  rv  csr_file = do
  let unflatten :: Int -> [t] -> [[t]]
      unflatten  n  xs | length xs >= n = (take n  xs):unflatten  n  (drop  n  xs)
                       | True           = [xs]

  let print_csr  (csr_addr, csr_name) = (do
                                            let csr_val = csr_read  rv  csr_file  csr_addr
                                            putStr ("  " ++ csr_name ++ ":" ++ showHex csr_val ""))
  -- Print n CSRs per line
  let print_n_csrs  [] = return ()
      print_n_csrs  xs = do
        putStr  indent
        mapM_  print_csr  xs
        putStrLn ""

      n = 5

  mapM_  print_n_csrs  (unflatten  n  m_csr_addrs_and_names)
  mapM_  print_n_csrs  (unflatten  n  s_csr_addrs_and_names)
  mapM_  print_n_csrs  (unflatten  n  u_csr_addrs_and_names)

-- ================================================================
-- Access permissions for a CSR, at a given Privilege Level
-- Note: csr_addr [11:10] indicates 'read-only' if == 2'b11, 'read-write' otherwise
--       csr_addr [ 9: 8] indicates minimum privilege for access

data  CSR_Permission = CSR_Permission_None | CSR_Permission_RO | CSR_Permission_RW
  deriving (Eq, Show)

csr_permission :: CSR_File ->    Priv_Level -> CSR_Addr -> CSR_Permission
csr_permission    (CSR_File dm)  priv          csr_addr =
  let
    exists     = ((   csr_addr == csr_addr_sstatus)
                  || (csr_addr == csr_addr_ustatus)
                  || (csr_addr == csr_addr_sie)
                  || (csr_addr == csr_addr_uie)
                  || (csr_addr == csr_addr_sip)
                  || (csr_addr == csr_addr_uip)
                  || (Data_Map.member  csr_addr  dm))

    addr_9_8   = bitSlice csr_addr   9  8
    priv_ok    = priv >= addr_9_8

    -- TVM fault: cannot access SATP if MSTATUS.TVM is set
    rv        = RV64    -- using RV64 because it's don't care: only matters for mstatus.sd
    mstatus   = csr_mstatus_read  rv  (CSR_File dm)
    tvm_fault = ((csr_addr == csr_addr_satp) && (testBit  mstatus  mstatus_tvm_bitpos))

    -- TODO: MxDELEG fault: MIDELEG and MEDELEG do not exist in
    -- systems with only m_Priv and systems with m_Priv and u_Priv but
    -- without support for U-mode traps

    addr_11_10 = bitSlice csr_addr  11  10
  in
    if (not exists) || (not priv_ok) || tvm_fault
    then CSR_Permission_None
    else if (addr_11_10 == 3)
         then CSR_Permission_RO
         else CSR_Permission_RW

{-# INLINE csr_permission #-}

-- ================================================================
-- CSR reads
-- These are just raw CSR reads, and assume legal csr addrs
-- and csr read permissions

-- TODO: zeroExtend?
-- For FCSR upper bits beyond fflags and frm are reserved. Writes
-- are to be ignored, reads return zero.

csr_read :: RV -> CSR_File -> CSR_Addr -> Integer
csr_read  rv  (CSR_File dm)  csr_addr =
  let
    mstatus      = csr_mstatus_read  rv  (CSR_File dm)
    ustatus_mask = if (rv == RV32) then ustatus_read_mask_RV32 else ustatus_read_mask_RV64
    sstatus_mask = if (rv == RV32) then sstatus_read_mask_RV32 else sstatus_read_mask_RV64
    mstatus_mask = if (rv == RV32) then mstatus_read_mask_RV32 else mstatus_read_mask_RV64

    mip      = fromMaybe  0  (Data_Map.lookup  csr_addr_mip       dm)
    mie      = fromMaybe  0  (Data_Map.lookup  csr_addr_mie       dm)
    minstret = fromMaybe  0  (Data_Map.lookup  csr_addr_minstret  dm)
    mcycle   = fromMaybe  0  (Data_Map.lookup  csr_addr_mcycle    dm)

    fcsr     = fromMaybe  0  (Data_Map.lookup  csr_addr_fcsr      dm)


    val | (csr_addr == csr_addr_ustatus)  = (mstatus .&. ustatus_mask)
        | (csr_addr == csr_addr_uip)      = (mip .&. uip_mask)
        | (csr_addr == csr_addr_uie)      = (mie .&. uip_mask)
        | (csr_addr == csr_addr_cycle)    = mcycle
        | (csr_addr == csr_addr_cycleh)   = (shiftR  mcycle  32)
        | (csr_addr == csr_addr_instret)  = minstret
        | (csr_addr == csr_addr_instreth) = (shiftR  minstret  32)
        | (csr_addr == csr_addr_frm)      = (shiftR  (fcsr .&. frm_mask)  frm_bitpos)
        | (csr_addr == csr_addr_fflags)   = (fcsr .&. fflags_mask)
        | (csr_addr == csr_addr_fcsr)     = (fcsr .&. (fflags_mask .|. frm_mask))

        | (csr_addr == csr_addr_sstatus)  = (mstatus .&. sstatus_mask)
        | (csr_addr == csr_addr_sip)      = (mip .&. sip_mask)
        | (csr_addr == csr_addr_sie)      = (mie .&. sip_mask)

        | (csr_addr == csr_addr_mstatus)  = (mstatus .&. mstatus_mask)
        | True                            = fromMaybe  0  (Data_Map.lookup  csr_addr  dm)
  in
    val

{-# INLINE csr_read #-}

-- ================================================================
-- CSR writes
-- These are just raw CSR writes, and assume legal csr addrs
-- and csr write permissions

-- csr_write checks 'member' to avoid inserting new csr_addr into the map
-- Note: silently ignores csr_addrs that are not in the map

-- Some CSR writes which dynamically change architectural features can
-- have wide-ranging side-effects, e.g., changing MISA.C, MSTATUS.MXL
-- Those details are handled in other functions, which use these raw
-- reads/writes for individual updates.

csr_write :: RV -> CSR_File    -> CSR_Addr -> Integer -> CSR_File
csr_write    rv    (CSR_File dm)  csr_addr    value =
  let
    mstatus = csr_mstatus_read  rv  (CSR_File dm)
    mip     = fromMaybe  0  (Data_Map.lookup  csr_addr_mip      dm)
    mie     = fromMaybe  0  (Data_Map.lookup  csr_addr_mie      dm)
    fcsr    = fromMaybe  0  (Data_Map.lookup  csr_addr_fcsr     dm)

    mstatus_mask = if (rv == RV32) then mstatus_write_mask_RV32 else mstatus_write_mask_RV64
    sstatus_mask = if (rv == RV32) then sstatus_write_mask_RV32 else sstatus_write_mask_RV64
    ustatus_mask = if (rv == RV32) then ustatus_write_mask_RV32 else ustatus_write_mask_RV64

    (csr_addr', value')
      | (csr_addr == csr_addr_mstatus) = (csr_addr_mstatus, ((mstatus .&. (complement  mstatus_mask))
                                                             .|. (value .&. mstatus_mask)))
      | (csr_addr == csr_addr_sstatus) = (csr_addr_mstatus, ((mstatus .&. (complement  sstatus_mask))
                                                             .|. (value .&. sstatus_mask)))
      | (csr_addr == csr_addr_ustatus) = (csr_addr_mstatus, ((mstatus .&. (complement  ustatus_mask))
                                                             .|. (value .&. ustatus_mask)))

      | (csr_addr == csr_addr_sip)     = (csr_addr_mip,     ((mip .&. (complement  sip_mask))
                                                             .|. (value .&. sip_mask)))
      | (csr_addr == csr_addr_uip)     = (csr_addr_mip,     ((mip .&. (complement  uip_mask))
                                                             .|. (value .&. uip_mask)))

      | (csr_addr == csr_addr_sie)     = (csr_addr_mie,     ((mie .&. (complement  sip_mask))
                                                             .|. (value .&. sip_mask)))
      | (csr_addr == csr_addr_uie)     = (csr_addr_mie,     ((mie .&. (complement  uip_mask))
                                                             .|. (value .&. uip_mask)))
      | (csr_addr == csr_addr_frm)     = (csr_addr_fcsr,    ((fcsr .&. (complement  frm_mask))
                                                             .|. ((shiftL value frm_bitpos) .&. frm_mask)))
      | (csr_addr == csr_addr_fflags)  = (csr_addr_fcsr,    ((fcsr .&. (complement  fflags_mask))
                                                             .|. (value .&. fflags_mask)))

      -- TODO: remove these if implementing all counters
      | (csr_addr == csr_addr_mcounteren) = (csr_addr,  (value .&. 0x7))
      | (csr_addr == csr_addr_scounteren) = (csr_addr,  (value .&. 0x7))

      | True                              = (csr_addr,  value)

    dm' = if (Data_Map.member csr_addr' dm)
          then Data_Map.insert  csr_addr'  value'  dm
          else dm
  in
    CSR_File dm'

{-# INLINE csr_write #-}

-- ****************************************************************
-- ****************************************************************
-- ****************************************************************
-- CSR addresses

-- ================================================================
-- User-Level CSR addresses                     -- \begin_latex{CSR_Addresses}

csr_addr_ustatus    = 0x000 :: CSR_Addr
csr_addr_uie        = 0x004 :: CSR_Addr
csr_addr_utvec      = 0x005 :: CSR_Addr

csr_addr_uscratch   = 0x040 :: CSR_Addr
csr_addr_uepc       = 0x041 :: CSR_Addr         -- \end_latex{...CSR_Addresses}
csr_addr_ucause     = 0x042 :: CSR_Addr
csr_addr_utval      = 0x043 :: CSR_Addr
csr_addr_uip        = 0x044 :: CSR_Addr

csr_addr_fflags     = 0x001 :: CSR_Addr
csr_addr_frm        = 0x002 :: CSR_Addr
csr_addr_fcsr       = 0x003 :: CSR_Addr

csr_addr_cycle      = 0xC00 :: CSR_Addr
csr_addr_time       = 0xC01 :: CSR_Addr
csr_addr_instret    = 0xC02 :: CSR_Addr

csr_addr_cycleh     = 0xC80 :: CSR_Addr
csr_addr_timeh      = 0xC81 :: CSR_Addr
csr_addr_instreth   = 0xC82 :: CSR_Addr

-- TODO: hpmcounterN, hpmcounterNh

-- ----------------
-- User-Level CSR reset values

u_csr_reset_values :: [(CSR_Addr, Integer)]
u_csr_reset_values =
  [ (csr_addr_utvec,      0),

    (csr_addr_uscratch,   0),
    (csr_addr_uepc,       0),
    (csr_addr_ucause,     0),
    (csr_addr_utval,      0),

    (csr_addr_fflags,     0),
    (csr_addr_frm,        0),
    (csr_addr_fcsr,       0),

    (csr_addr_cycle,      0),
    (csr_addr_time,       0),
    (csr_addr_instret,    0),

    (csr_addr_cycleh,     0),
    (csr_addr_timeh,      0),
    (csr_addr_instreth,   0) ]

-- ----------------
-- These names are only used for printing instruction traces during
-- simulation, and have no semantic significance.

-- The following list is in the order printed by print_CSR_File()

u_csr_addrs_and_names :: [(CSR_Addr, String)]
u_csr_addrs_and_names  =
  [ (csr_addr_ustatus,    "ustatus"),
    (csr_addr_uie,        "uie"),
    (csr_addr_uip,        "uip"),
    (csr_addr_time,       "time"),
    (csr_addr_timeh,      "timeh"),

    (csr_addr_utvec,      "utvec"),
    (csr_addr_uepc,       "uepc"),
    (csr_addr_ucause,     "ucause"),
    (csr_addr_utval,      "utval"),
    (csr_addr_uscratch,   "uscratch"),

    (csr_addr_cycle,      "cycle"),
    (csr_addr_cycleh,     "cycleh"),
    (csr_addr_instret,    "instret"),
    (csr_addr_instreth,   "instreth"),

    (csr_addr_fflags,     "fflags"),
    (csr_addr_frm,        "frm"),
    (csr_addr_fcsr,       "fcsr") ]

-- ================================================================
-- Supervisor-Level CSR addresses

csr_addr_sstatus    = 0x100 :: CSR_Addr
csr_addr_sedeleg    = 0x102 :: CSR_Addr
csr_addr_sideleg    = 0x103 :: CSR_Addr
csr_addr_sie        = 0x104 :: CSR_Addr
csr_addr_stvec      = 0x105 :: CSR_Addr
csr_addr_scounteren = 0x106 :: CSR_Addr

csr_addr_sscratch   = 0x140 :: CSR_Addr
csr_addr_sepc       = 0x141 :: CSR_Addr
csr_addr_scause     = 0x142 :: CSR_Addr
csr_addr_stval      = 0x143 :: CSR_Addr
csr_addr_sip        = 0x144 :: CSR_Addr

csr_addr_satp       = 0x180 :: CSR_Addr

-- ----------------
-- Supervisor-Level CSRs reset values

s_csr_reset_values :: [(CSR_Addr, Integer)]
s_csr_reset_values =
  [ (csr_addr_sedeleg,    0),
    (csr_addr_sideleg,    0),
    (csr_addr_stvec,      0),
    (csr_addr_scounteren, 0),

    (csr_addr_sscratch,   0),
    (csr_addr_sepc,       0),
    (csr_addr_scause,     0),
    (csr_addr_stval,      0),

    (csr_addr_satp,       0) ]

-- ----------------
-- These names are only used for printing instruction traces during
-- simulation, and have no semantic significance.

-- The following list is in the order printed by print_CSR_File()

s_csr_addrs_and_names :: [(CSR_Addr, String)]
s_csr_addrs_and_names  =
  [ (csr_addr_sstatus,    "sstatus"),
    (csr_addr_sie,        "sie"),
    (csr_addr_sip,        "sip"),
    (csr_addr_sedeleg,    "sedeleg"),
    (csr_addr_sideleg,    "sideleg"),

    (csr_addr_stvec,      "stvec"),
    (csr_addr_sepc,       "sepc"),
    (csr_addr_scause,     "scause"),
    (csr_addr_stval,      "stval"),
    (csr_addr_sscratch,   "sscratch"),

    (csr_addr_satp,       "satp"),
    (csr_addr_scounteren, "scounteren") ]

-- ================================================================
-- Machine-Level CSR addresses

csr_addr_mvendorid  = 0xF11 :: CSR_Addr
csr_addr_marchid    = 0xF12 :: CSR_Addr
csr_addr_mimpid     = 0xF13 :: CSR_Addr
csr_addr_mhartid    = 0xF14 :: CSR_Addr

csr_addr_mstatus    = 0x300 :: CSR_Addr
csr_addr_misa       = 0x301 :: CSR_Addr
csr_addr_medeleg    = 0x302 :: CSR_Addr
csr_addr_mideleg    = 0x303 :: CSR_Addr
csr_addr_mie        = 0x304 :: CSR_Addr
csr_addr_mtvec      = 0x305 :: CSR_Addr
csr_addr_mcounteren = 0x306 :: CSR_Addr

csr_addr_mscratch   = 0x340 :: CSR_Addr
csr_addr_mepc       = 0x341 :: CSR_Addr
csr_addr_mcause     = 0x342 :: CSR_Addr
csr_addr_mtval      = 0x343 :: CSR_Addr
csr_addr_mip        = 0x344 :: CSR_Addr

-- TODO: pmpcfgN, pmpaddrN

csr_addr_mcycle     = 0xB00 :: CSR_Addr
csr_addr_minstret   = 0xB02 :: CSR_Addr

-- TODO: mhpmcounterN

csr_addr_mcycleh    = 0xB80 :: CSR_Addr
csr_addr_minstreth  = 0xB82 :: CSR_Addr

-- TODO: mhpmcounterNh

csr_addr_tselect    = 0x7A0 :: CSR_Addr
csr_addr_data1      = 0x7A1 :: CSR_Addr
csr_addr_data2      = 0x7A2 :: CSR_Addr
csr_addr_data3      = 0x7A3 :: CSR_Addr

csr_addr_dcsr       = 0x7B0 :: CSR_Addr
csr_addr_dpc        = 0x7B1 :: CSR_Addr
csr_addr_dscratch   = 0x7B2 :: CSR_Addr

-- ----------------
-- Machine-Level CSR reset values

m_csr_reset_values :: RV -> Integer -> [(CSR_Addr, Integer)]
m_csr_reset_values    rv    misa =
  [ (csr_addr_mvendorid,  0),
    (csr_addr_marchid,    0),
    (csr_addr_mimpid,     0),
    (csr_addr_mhartid,    0),

    (csr_addr_mstatus,    if (rv == RV32)
                          then 0
                          else ((    shiftL  xl_rv64  mstatus_sxl_bitpos)
                                .|. (shiftL  xl_rv64  mstatus_uxl_bitpos)
#ifdef FLOAT
                                .|. (shiftL  1        mstatus_fs_bitpos)
#endif
                               )),
    (csr_addr_misa,       misa),
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
    (csr_addr_minstreth,  0),

    (csr_addr_tselect,    0),
    (csr_addr_data1,      0),
    (csr_addr_data2,      0),
    (csr_addr_data3,      0),
    (csr_addr_dcsr,       0),
    (csr_addr_dpc,        0),
    (csr_addr_dscratch,   0)
  ]

-- ----------------
-- These names are only used for printing instruction traces during
-- simulation, and have no semantic significance.

-- The following list is in the order printed by print_CSR_File()

m_csr_addrs_and_names :: [(CSR_Addr, String)]
m_csr_addrs_and_names  =
  [ (csr_addr_mstatus,    "mstatus"),
    (csr_addr_mie,        "mie"),
    (csr_addr_mip,        "mip"),
    (csr_addr_medeleg,    "medeleg"),
    (csr_addr_mideleg,    "mideleg"),

    (csr_addr_mtvec,      "mtvec"),
    (csr_addr_mepc,       "mepc"),
    (csr_addr_mcause,     "mcause"),
    (csr_addr_mtval,      "mtval"),
    (csr_addr_mscratch,   "mscratch"),

    (csr_addr_minstret,   "minstret"),
    (csr_addr_minstreth,  "minstreth"),
    (csr_addr_mcycle,     "mcycle"),
    (csr_addr_mcycleh,    "mcycleh"),
    (csr_addr_mcounteren, "mcounteren"),

    (csr_addr_mvendorid,  "mvendorid"),
    (csr_addr_marchid,    "marchid"),
    (csr_addr_mimpid,     "mimpid"),
    (csr_addr_mhartid,    "mhartid"),
    (csr_addr_misa,       "misa"),

    (csr_addr_tselect,    "tselect"),
    (csr_addr_data1,      "data1"),
    (csr_addr_data2,      "data2"),
    (csr_addr_data3,      "data3"),
    (csr_addr_dcsr,       "dcsr"),

    (csr_addr_dpc,        "dpc"),
    (csr_addr_dscratch,   "dscratch")
  ]

-- ****************************************************************
-- ****************************************************************
-- ****************************************************************
-- MISA CSR                                                           -- \begin_latex{MISA_fields_A}

-- Codes for MXL, SXL, UXL
xl_rv32  = 1 :: Integer
xl_rv64  = 2 :: Integer
xl_rv128 = 3 :: Integer
                                                                      -- \end_latex{MISA_fields_A}
                                                                      -- \begin_latex{MISA_fields_B}
-- Test whether a particular MISA 'letter' bit (A-Z) is set

misa_flag :: Integer -> Char -> Bool
misa_flag    misa       letter =
  if (     isAsciiUpper  letter) then
    (((shiftR  misa  ((ord letter) - (ord 'A'))) .&. 1) == 1)
  else if (isAsciiLower  letter) then
    (((shiftR  misa  ((ord letter) - (ord 'a'))) .&. 1) == 1)
  else
    error "Illegal argument to misa_flag"
                                                                      -- \end_latex{MISA_fields_B}
{-# INLINE misa_flag #-}

-- Bit fields                                                         -- \begin_latex{MISA_fields_C}
misa_A_bitpos = 0 :: Int
misa_B_bitpos = 1 :: Int
misa_C_bitpos = 2 :: Int
misa_D_bitpos = 3 :: Int                                              -- \end_latex{...MISA_fields_C}

misa_E_bitpos = 4 :: Int
misa_F_bitpos = 5 :: Int
misa_G_bitpos = 6 :: Int
misa_H_bitpos = 7 :: Int

misa_I_bitpos = 8 :: Int
misa_J_bitpos = 9 :: Int
misa_K_bitpos = 10 :: Int
misa_L_bitpos = 11 :: Int

misa_M_bitpos = 12 :: Int
misa_N_bitpos = 13 :: Int
misa_O_bitpos = 14 :: Int
misa_P_bitpos = 15 :: Int

misa_Q_bitpos = 16 :: Int
misa_R_bitpos = 17 :: Int
misa_S_bitpos = 18 :: Int
misa_T_bitpos = 19 :: Int

misa_U_bitpos = 20 :: Int
misa_V_bitpos = 21 :: Int
misa_W_bitpos = 22 :: Int
misa_X_bitpos = 23 :: Int

misa_Y_bitpos = 24 :: Int
misa_Z_bitpos = 25 :: Int

misa_MXL_bitpos_RV32 = 30 :: Int
misa_MXL_bitpos_RV64 = 62 :: Int

-- For printouts
show_misa :: Integer -> String
show_misa  misa =
  let
    stringify :: Int -> Integer -> String -> String
    stringify    j      misa       s = if ((misa == 0) || (j > 25)) then s
                                       else
                                         let
                                           s1 = if (misa  .&.  0x1) /= 0 then
                                                  (s ++ [chr (j + ord 'A')])
                                                else
                                                  s
                                         in
                                           stringify  (j + 1)  (shiftR  misa  1)  s1
  in
    stringify  0  misa  ""

-- ****************************************************************
-- ****************************************************************
-- ****************************************************************
-- MSTATUS CSR

-- ================================================================
-- MSTATUS bit fields

mstatus_sd_bitpos_RV64 = 63 :: Int
mstatus_sd_bitpos_RV32 = 31 :: Int

mstatus_sxl_bitpos     = 34 :: Int
mstatus_uxl_bitpos     = 32 :: Int

mstatus_tsr_bitpos     = 22 :: Int
mstatus_tw_bitpos      = 21 :: Int
mstatus_tvm_bitpos     = 20 :: Int

mstatus_mxr_bitpos     = 19 :: Int
mstatus_sum_bitpos     = 18 :: Int
mstatus_mprv_bitpos    = 17 :: Int

mstatus_xs_bitpos      = 15 :: Int
mstatus_fs_bitpos      = 13 :: Int

mstatus_mpp_bitpos     = 11 :: Int
mstatus_spp_bitpos     =  8 :: Int

mstatus_mpie_bitpos    =  7 :: Int
mstatus_spie_bitpos    =  5 :: Int
mstatus_upie_bitpos    =  4 :: Int

mstatus_mie_bitpos     =  3 :: Int
mstatus_sie_bitpos     =  1 :: Int
mstatus_uie_bitpos     =  0 :: Int

-- ================================================================
-- These masks specify which fields are observed/updated in MSTATUS

mstatus_read_mask_RV32 :: Integer
mstatus_read_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                          .|. (shiftL  1  mstatus_tsr_bitpos)
                          .|. (shiftL  1  mstatus_tw_bitpos)
                          .|. (shiftL  1  mstatus_tvm_bitpos)

                          .|. (shiftL  1  mstatus_mxr_bitpos)
                          .|. (shiftL  1  mstatus_sum_bitpos)
                          .|. (shiftL  1  mstatus_mprv_bitpos)

                          .|. (shiftL  3  mstatus_xs_bitpos)
                          .|. (shiftL  3  mstatus_fs_bitpos)

                          .|. (shiftL  3  mstatus_mpp_bitpos)
                          .|. (shiftL  1  mstatus_spp_bitpos)

                          .|. (shiftL  1  mstatus_mpie_bitpos)
                          .|. (shiftL  1  mstatus_spie_bitpos)
                          .|. (shiftL  1  mstatus_upie_bitpos)

                          .|. (shiftL  1  mstatus_mie_bitpos)
                          .|. (shiftL  1  mstatus_sie_bitpos)
                          .|. (shiftL  1  mstatus_uie_bitpos))

mstatus_write_mask_RV32 :: Integer
mstatus_write_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                           .|. (shiftL  1  mstatus_tsr_bitpos)
                           .|. (shiftL  1  mstatus_tw_bitpos)
                           .|. (shiftL  1  mstatus_tvm_bitpos)

                           .|. (shiftL  1  mstatus_mxr_bitpos)
                           .|. (shiftL  1  mstatus_sum_bitpos)
                           .|. (shiftL  1  mstatus_mprv_bitpos)

                           .|. (shiftL  3  mstatus_xs_bitpos)
                           .|. (shiftL  3  mstatus_fs_bitpos)

                           .|. (shiftL  3  mstatus_mpp_bitpos)
                           .|. (shiftL  1  mstatus_spp_bitpos)

                           .|. (shiftL  1  mstatus_mpie_bitpos)
                           .|. (shiftL  1  mstatus_spie_bitpos)
                           .|. (shiftL  1  mstatus_upie_bitpos)

                           .|. (shiftL  1  mstatus_mie_bitpos)
                           .|. (shiftL  1  mstatus_sie_bitpos)
                           .|. (shiftL  1  mstatus_uie_bitpos))

mstatus_read_mask_RV64 :: Integer
mstatus_read_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                          .|. (shiftL  3  mstatus_sxl_bitpos)
                          .|. (shiftL  3  mstatus_uxl_bitpos)

                          .|. (shiftL  1  mstatus_tsr_bitpos)
                          .|. (shiftL  1  mstatus_tw_bitpos)
                          .|. (shiftL  1  mstatus_tvm_bitpos)

                          .|. (shiftL  1  mstatus_mxr_bitpos)
                          .|. (shiftL  1  mstatus_sum_bitpos)
                          .|. (shiftL  1  mstatus_mprv_bitpos)

                          .|. (shiftL  3  mstatus_xs_bitpos)
                          .|. (shiftL  3  mstatus_fs_bitpos)

                          .|. (shiftL  3  mstatus_mpp_bitpos)
                          .|. (shiftL  1  mstatus_spp_bitpos)

                          .|. (shiftL  1  mstatus_mpie_bitpos)
                          .|. (shiftL  1  mstatus_spie_bitpos)
                          .|. (shiftL  1  mstatus_upie_bitpos)

                          .|. (shiftL  1  mstatus_mie_bitpos)
                          .|. (shiftL  1  mstatus_sie_bitpos)
                          .|. (shiftL  1  mstatus_uie_bitpos))

mstatus_write_mask_RV64 :: Integer
mstatus_write_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                          -- .|. (shiftL  3  mstatus_sxl_bitpos)    -- TODO: this is not-writable implementation choice
                          -- .|. (shiftL  3  mstatus_uxl_bitpos)    -- TODO: this is not-writable implementation choice

                           .|. (shiftL  1  mstatus_tsr_bitpos)
                           .|. (shiftL  1  mstatus_tw_bitpos)
                           .|. (shiftL  1  mstatus_tvm_bitpos)

                           .|. (shiftL  1  mstatus_mxr_bitpos)
                           .|. (shiftL  1  mstatus_sum_bitpos)
                           .|. (shiftL  1  mstatus_mprv_bitpos)

                          -- .|. (shiftL  3  mstatus_xs_bitpos)    -- TODO: this is not-writable implementation choice
                           .|. (shiftL  3  mstatus_fs_bitpos)

                           .|. (shiftL  3  mstatus_mpp_bitpos)
                           .|. (shiftL  1  mstatus_spp_bitpos)

                           .|. (shiftL  1  mstatus_mpie_bitpos)
                           .|. (shiftL  1  mstatus_spie_bitpos)
                           .|. (shiftL  1  mstatus_upie_bitpos)

                           .|. (shiftL  1  mstatus_mie_bitpos)
                           .|. (shiftL  1  mstatus_sie_bitpos)
                           .|. (shiftL  1  mstatus_uie_bitpos))


-- SSTATUS is a ``view'' of MSTATUS, masking in/out certain fields

sstatus_read_mask_RV32 :: Integer
sstatus_read_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                          .|. (shiftL  1  mstatus_mxr_bitpos)
                          .|. (shiftL  1  mstatus_sum_bitpos)

                          .|. (shiftL  3  mstatus_xs_bitpos)
                          .|. (shiftL  3  mstatus_fs_bitpos)

                          .|. (shiftL  1  mstatus_spp_bitpos)

                          .|. (shiftL  1  mstatus_spie_bitpos)
                          .|. (shiftL  1  mstatus_upie_bitpos)

                          .|. (shiftL  1  mstatus_sie_bitpos)
                          .|. (shiftL  1  mstatus_uie_bitpos))

sstatus_write_mask_RV32 :: Integer
sstatus_write_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                           .|. (shiftL  1  mstatus_mxr_bitpos)
                           .|. (shiftL  1  mstatus_sum_bitpos)

                           .|. (shiftL  3  mstatus_xs_bitpos)
                           .|. (shiftL  3  mstatus_fs_bitpos)

                           .|. (shiftL  1  mstatus_spp_bitpos)

                           .|. (shiftL  1  mstatus_spie_bitpos)
                           .|. (shiftL  1  mstatus_upie_bitpos)

                           .|. (shiftL  1  mstatus_sie_bitpos)
                           .|. (shiftL  1  mstatus_uie_bitpos))

sstatus_read_mask_RV64 :: Integer
sstatus_read_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                          .|. (shiftL  3  mstatus_uxl_bitpos)

                          .|. (shiftL  1  mstatus_mxr_bitpos)
                          .|. (shiftL  1  mstatus_sum_bitpos)

                          .|. (shiftL  3  mstatus_xs_bitpos)
                          .|. (shiftL  3  mstatus_fs_bitpos)

                          .|. (shiftL  1  mstatus_spp_bitpos)

                          .|. (shiftL  1  mstatus_spie_bitpos)
                          .|. (shiftL  1  mstatus_upie_bitpos)

                          .|. (shiftL  1  mstatus_sie_bitpos)
                          .|. (shiftL  1  mstatus_uie_bitpos))

sstatus_write_mask_RV64 :: Integer
sstatus_write_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                           -- .|. (shiftL  3  mstatus_uxl_bitpos)    -- TODO: this is not-writable implementation choice

                           .|. (shiftL  1  mstatus_mxr_bitpos)
                           .|. (shiftL  1  mstatus_sum_bitpos)

                           -- .|. (shiftL  3  mstatus_xs_bitpos)    -- TODO: this is not-writable implementation choice
                           .|. (shiftL  3  mstatus_fs_bitpos)

                           .|. (shiftL  1  mstatus_spp_bitpos)

                           .|. (shiftL  1  mstatus_spie_bitpos)
                           .|. (shiftL  1  mstatus_upie_bitpos)

                           .|. (shiftL  1  mstatus_sie_bitpos)
                           .|. (shiftL  1  mstatus_uie_bitpos))

-- USTATUS is a ``view'' of MSTATUS, masking in/out certain fields
-- TODO: find out what should be in ustatus (the v1.10 spec doc does not specify)

ustatus_read_mask_RV32 :: Integer
ustatus_read_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                          .|. (shiftL  1  mstatus_mxr_bitpos)
                          .|. (shiftL  1  mstatus_sum_bitpos)

                          .|. (shiftL  3  mstatus_xs_bitpos)
                          .|. (shiftL  3  mstatus_fs_bitpos)

                          .|. (shiftL  1  mstatus_upie_bitpos)

                          .|. (shiftL  1  mstatus_uie_bitpos))

ustatus_write_mask_RV32 :: Integer
ustatus_write_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                           .|. (shiftL  1  mstatus_mxr_bitpos)
                           .|. (shiftL  1  mstatus_sum_bitpos)

                           -- .|. (shiftL  3  mstatus_xs_bitpos)    -- TODO: this is not-writable implementation choice
                           .|. (shiftL  3  mstatus_fs_bitpos)

                           .|. (shiftL  1  mstatus_upie_bitpos)

                           .|. (shiftL  1  mstatus_uie_bitpos))

ustatus_read_mask_RV64 :: Integer
ustatus_read_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                          .|. (shiftL  3  mstatus_uxl_bitpos)

                          .|. (shiftL  1  mstatus_mxr_bitpos)
                          .|. (shiftL  1  mstatus_sum_bitpos)

                          .|. (shiftL  3  mstatus_xs_bitpos)
                          .|. (shiftL  3  mstatus_fs_bitpos)

                          .|. (shiftL  1  mstatus_upie_bitpos)

                          .|. (shiftL  1  mstatus_uie_bitpos))

ustatus_write_mask_RV64 :: Integer
ustatus_write_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                           .|. (shiftL  3  mstatus_uxl_bitpos)

                           .|. (shiftL  1  mstatus_mxr_bitpos)
                           .|. (shiftL  1  mstatus_sum_bitpos)

                           -- .|. (shiftL  3  mstatus_xs_bitpos)    -- TODO: this is not-writable implementation choice
                           .|. (shiftL  3  mstatus_fs_bitpos)

                           .|. (shiftL  1  mstatus_upie_bitpos)

                           .|. (shiftL  1  mstatus_uie_bitpos))

-- ----------------
-- Read MSTATUS, filling in the virtual field MSTATUS.SD

csr_mstatus_read :: RV -> CSR_File -> Integer
csr_mstatus_read    rv    (CSR_File dm) =
  let
    mstatus      = fromMaybe  0  (Data_Map.lookup  csr_addr_mstatus   dm)
    fs_is_dirty  = (((shiftR  mstatus  mstatus_fs_bitpos) .&. 3) == 3)
    xs_is_dirty  = (((shiftR  mstatus  mstatus_xs_bitpos) .&. 3) == 3)
    sd_shift_amt = if (rv == RV32) then mstatus_sd_bitpos_RV32 else mstatus_sd_bitpos_RV64
    sd           = if (fs_is_dirty || xs_is_dirty) then (shiftL 1 sd_shift_amt)
                   else 0
  in
    (mstatus .|. sd)

-- ================================================================
-- MSTATUS contains a ``stack'' of ``previous-privilege'' and ``interrupt-enable'' bits

-- Return the stack fields in mstatus

mstatus_stack_fields :: Integer -> (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer)
mstatus_stack_fields  mstatus =
  let
    mpp  = (shiftR  mstatus  mstatus_mpp_bitpos)  .&. 0x3
    spp  = (shiftR  mstatus  mstatus_spp_bitpos)  .&. 0x1
    mpie = (shiftR  mstatus  mstatus_mpie_bitpos) .&. 0x1
    spie = (shiftR  mstatus  mstatus_spie_bitpos) .&. 0x1
    upie = (shiftR  mstatus  mstatus_upie_bitpos) .&. 0x1
    mie  = (shiftR  mstatus  mstatus_mie_bitpos)  .&. 0x1
    sie  = (shiftR  mstatus  mstatus_sie_bitpos)  .&. 0x1
    uie  = (shiftR  mstatus  mstatus_uie_bitpos)  .&. 0x1
  in
    (mpp, spp, mpie, spie, upie, mie, sie, uie)

-- Update the stack fields in mstatus

mstatus_upd_stack_fields :: Integer ->
                            (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer) ->
                            Integer
mstatus_upd_stack_fields  mstatus (mpp, spp, mpie, spie, upie, mie, sie, uie) =
  let
    mstatus_stack_mask :: Integer
    mstatus_stack_mask = 0x1FFF    -- all LSBs up to and including MPP

    mstatus' = ((mstatus .&. (complement  mstatus_stack_mask))
                .|. (shiftL  mpp   mstatus_mpp_bitpos)
                .|. (shiftL  spp   mstatus_spp_bitpos)
                .|. (shiftL  mpie  mstatus_mpie_bitpos)
                .|. (shiftL  spie  mstatus_spie_bitpos)
                .|. (shiftL  upie  mstatus_upie_bitpos)
                .|. (shiftL  mie   mstatus_mie_bitpos)
                .|. (shiftL  sie   mstatus_sie_bitpos)
                .|. (shiftL  uie   mstatus_uie_bitpos))
  in
    mstatus'

{-# INLINE mstatus_stack_fields #-}
{-# INLINE mstatus_upd_stack_fields #-}

-- ================================================================
-- Trap Vectors (mtvec, stvec, utvec) have
--    a 'mode' in bits [1:0]
--    a 'base' in bits [xlen-1:2]
-- MTVEC, STVEC and UTVEC have the same format
--     (for Machine, Supervisor, User privilege levels)

tvec_mode_DIRECT   = 0 :: Integer
tvec_mode_VECTORED = 1 :: Integer

tvec_mode :: Integer -> Integer
tvec_mode  tvec = (tvec .&. 3)

tvec_base :: Integer -> Integer
tvec_base  tvec = shiftL (shiftR  tvec  2) 2

{-# INLINE tvec_mode #-}
{-# INLINE tvec_base #-}

-- ================================================================
-- MIP bit fields (Machine privilege level Interrupt Pending)
-- MIE bit fields (Machine privilege level Interrupt Enable)

mip_usip_bitpos =  0 :: Int
mip_ssip_bitpos =  1 :: Int
mip_msip_bitpos =  3 :: Int

mip_utip_bitpos =  4 :: Int
mip_stip_bitpos =  5 :: Int
mip_mtip_bitpos =  7 :: Int

mip_ueip_bitpos =  8 :: Int
mip_seip_bitpos =  9 :: Int
mip_meip_bitpos = 11 :: Int

-- SIP is a ``view'' of MIP for Supervisor privilege level
-- SIE is a ``view'' of MIE for Supervisor privilege level, with the same mask

sip_mask :: Integer
sip_mask = ((    shiftL 1 mip_seip_bitpos)
            .|. (shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_stip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_ssip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- UIP is a ``view'' of MIP for User privilege level
-- UIE is a ``view'' of MIE for User privilege level, with the same mask

uip_mask :: Integer
uip_mask = ((    shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- ================================================================
-- MCAUSE bit fields

mcause_interrupt_bitpos_RV32 = 31 :: Int
mcause_interrupt_bitpos_RV64 = 63 :: Int

-- Constructor: make an MCAUSE value depending interrupt or trap, and exception code

mkCause :: RV -> Bool -> Exc_Code -> Integer
mkCause  rv  interrupt_not_trap  exc_code =
  let
    msb | interrupt_not_trap && (rv == RV32) = shiftL  1  mcause_interrupt_bitpos_RV32
        | interrupt_not_trap && (rv == RV64) = shiftL  1  mcause_interrupt_bitpos_RV64
        | otherwise                          = 0
  in
    (msb .|. exc_code)

{-# INLINE mkCause #-}

-- ================================================================
-- ISA-F and ISA-D related definitions
-- FCSR bit fields
-- Bit fields
fcsr_nx_bitpos          = 0 :: Int
fcsr_uf_bitpos          = 1 :: Int
fcsr_of_bitpos          = 2 :: Int
fcsr_dz_bitpos          = 3 :: Int
fcsr_nv_bitpos          = 4 :: Int

-- The flags with only one of the bits set
nxFlag = (shiftL  1  fcsr_nx_bitpos) :: Integer
ufFlag = (shiftL  1  fcsr_uf_bitpos) :: Integer
ofFlag = (shiftL  1  fcsr_of_bitpos) :: Integer
dzFlag = (shiftL  1  fcsr_dz_bitpos) :: Integer
nvFlag = (shiftL  1  fcsr_nv_bitpos) :: Integer

-- 7:5
frm_bitpos      = 5 :: Int
fcsr_reserved   = 8 :: Int
fflags_mask     = ((shiftL 1 frm_bitpos) - 1) :: Integer
frm_mask        = (((shiftL 1 fcsr_reserved) - 1)
                .&. (complement fflags_mask)) :: Integer

-- Extract the fflags field from fcsr
fcsr_fflags    :: Integer -> Integer 
fcsr_fflags    fcsr = bitSlice fcsr 4 0

-- Extract the frm field from fcsr
fcsr_frm       :: Integer -> Integer 
fcsr_frm       fcsr = bitSlice fcsr 7 5

-- Check if a frm value in the FCSR.FRM is valid
fcsr_frm_valid :: Integer -> Bool
fcsr_frm_valid   frm  = (   (frm /= 0x5)
                         && (frm /= 0x6)
                         && (frm /= 0x7))

-- Check if a frm value in the instr is valid
instr_frm_valid:: InstrField -> Bool
instr_frm_valid   frm = (   (frm /= 0x5)
                         && (frm /= 0x6))

-- Returns the right rounding mode from the FCSR or the instruction and checks
-- legality
rounding_mode_check :: InstrField -> Integer -> (Integer, Bool)
rounding_mode_check    rm            frm =
  let
    frmVal     = if (rm == 0x7) then frm else rm
    rmIsLegal  = if (rm == 0x7) then
                   fcsr_frm_valid  frmVal
                 else
                   instr_frm_valid  frmVal
  in
    (frmVal, rmIsLegal)


-- Bit positions for the mask describing the class of FP value (table 8.5 v2.2)
fclass_negInf_bitpos       = 0 :: Int
fclass_negNorm_bitpos      = 1 :: Int
fclass_negSubNorm_bitpos   = 2 :: Int
fclass_negZero_bitpos      = 3 :: Int
fclass_posZero_bitpos      = 4 :: Int
fclass_posSubNorm_bitpos   = 5 :: Int
fclass_posNorm_bitpos      = 6 :: Int
fclass_posInf_bitpos       = 7 :: Int
fclass_SNaN_bitpos         = 8 :: Int
fclass_QNaN_bitpos         = 9 :: Int

-- Create a FFLAGS word to accrue into the CSR
form_fflags_word  :: Bool -> Bool -> Bool -> Bool -> Bool -> Integer
form_fflags_word  nxf  uff  off  dzf  nvf  =
  let
    fflags = (    (if (nxf) then (shiftL  1  fcsr_nx_bitpos) else 0)
              .|. (if (uff) then (shiftL  1  fcsr_uf_bitpos) else 0)
              .|. (if (off) then (shiftL  1  fcsr_of_bitpos) else 0)
              .|. (if (dzf) then (shiftL  1  fcsr_dz_bitpos) else 0)
              .|. (if (nvf) then (shiftL  1  fcsr_nv_bitpos) else 0))
  in
    fflags

-- ****************************************************************
-- ****************************************************************
-- ****************************************************************
-- Trap and Interrupt manipulations of CSRs

-- ================================================================
-- Function from mstatus, mip, mie and current privilege values to
--     whether or not an interrupt is pending,
-- and if so, the corresponding exception code

csr_interrupt_pending :: Integer ->                    -- MISA
                         Integer ->                    -- MSTATUS
                         Integer ->                    -- MIP
                         Integer ->                    -- MIE
                         Integer ->                    -- MIDELEG
                         Integer ->                    -- SIDELEG
                         Priv_Level ->                 -- current privilege level
                         Maybe  Exc_Code
csr_interrupt_pending  misa  mstatus  mip  mie  mideleg  sideleg  priv =
  let
    fn_interrupt_i_pending :: Exc_Code -> Bool
    fn_interrupt_i_pending  ec =
      let
        i = (fromIntegral ec :: Int)
        intr_pending = ((testBit  mip  i) && (testBit  mie  i))
        handler_priv = if (testBit  mideleg  i) then
                         if (misa_flag  misa  'U') then
                           if (misa_flag  misa  'S') then
                             -- System with M, S, U
                             if (testBit  sideleg  i) then
                               if (misa_flag  misa 'N') then
                                 -- M->S->U delegation
                                 u_Priv_Level
                               else
                                 -- TODO: Error: SIDELEG [i] should not be 1 if MISA.N is 0
                                 m_Priv_Level
                             else
                               -- M->S delegation
                               s_Priv_Level
                           else
                             -- System with M, U
                             if (misa_flag  misa  'N') then
                               -- M->U delegation
                               u_Priv_Level
                             else
                               -- TODO: Error: MIDELEG [i] should not be 1 if MISA.N is 0
                               m_Priv_Level
                         else
                           -- Error: System with M only; MIDELEG [i] should not be 1
                           m_Priv_Level
                       else
                         -- no delegation
                         m_Priv_Level

        xie :: Bool
        xie | (priv == u_Priv_Level) = (testBit  mstatus  mstatus_uie_bitpos)
            | (priv == s_Priv_Level) = (testBit  mstatus  mstatus_sie_bitpos)
            | (priv == m_Priv_Level) = (testBit  mstatus  mstatus_mie_bitpos)

        glob_enabled :: Bool
        glob_enabled = ((priv < handler_priv) || ((priv == handler_priv) && xie))
      in
        (intr_pending && glob_enabled)

    -- Check all interrupts in the following decreasing priority order
    m_ec | fn_interrupt_i_pending (exc_code_m_external_interrupt) = Just exc_code_m_external_interrupt
         | fn_interrupt_i_pending (exc_code_m_software_interrupt) = Just exc_code_m_software_interrupt
         | fn_interrupt_i_pending (exc_code_m_timer_interrupt)    = Just exc_code_m_timer_interrupt
         | fn_interrupt_i_pending (exc_code_s_external_interrupt) = Just exc_code_s_external_interrupt
         | fn_interrupt_i_pending (exc_code_s_software_interrupt) = Just exc_code_s_software_interrupt
         | fn_interrupt_i_pending (exc_code_s_timer_interrupt)    = Just exc_code_s_timer_interrupt
         | fn_interrupt_i_pending (exc_code_u_external_interrupt) = Just exc_code_u_external_interrupt
         | fn_interrupt_i_pending (exc_code_u_software_interrupt) = Just exc_code_u_software_interrupt
         | fn_interrupt_i_pending (exc_code_u_timer_interrupt)    = Just exc_code_u_timer_interrupt
         | True                                                   = Nothing
  in
    m_ec

{-# INLINE csr_interrupt_pending #-}

-- ================================================================
-- Check if an interrupt is pending to resume from WFI state.

-- Note: this is a weaker condition than csr_interrupt_pending since
-- it is unaffected by MSTATUS.MIE/SIE/UIE and MIDELEG and MEDELEG.

csr_wfi_resume :: Integer -> Integer -> Bool
csr_wfi_resume    mip        mie      = ((mip .&. mie) /= 0)

-- ================================================================

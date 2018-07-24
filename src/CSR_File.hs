-- Copyright (c) 2018 Rishiyur S. Nikhil
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
import Data.Word
import Data.Bits
import Numeric (showHex, readHex)
import qualified Data.Map.Strict as Data_Map

-- Project imports

import Bit_Manipulation
import Arch_Defs

-- ================================================================
-- User-Level CSR reset values

u_csr_reset_values :: RV -> [(CSR_Addr, Word64)]
u_csr_reset_values  rv =
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

-- ================================================================
-- Supervisor-Level CSRs reset values

s_csr_reset_values :: RV -> [(CSR_Addr, Word64)]
s_csr_reset_values  rv =
  [ (csr_addr_sedeleg,    0),
    (csr_addr_sideleg,    0),
    (csr_addr_stvec,      0),
    (csr_addr_scounteren, 0),

    (csr_addr_sscratch,   0),
    (csr_addr_sepc,       0),
    (csr_addr_scause,     0),
    (csr_addr_stval,      0),

    (csr_addr_satp,       0) ]

-- ================================================================
-- Machine-Level CSR reset values

m_csr_reset_values :: RV -> [(CSR_Addr, Word64)]
m_csr_reset_values  rv =
  [ (csr_addr_mvendorid,  0),
    (csr_addr_marchid,    0),
    (csr_addr_mimpid,     0),
    (csr_addr_mhartid,    0),

    (csr_addr_mstatus,    if (rv == RV32)
                          then 0
                          else ((    shiftL  xl_rv64  mstatus_sxl_bitpos)
                                .|. (shiftL  xl_rv64  mstatus_uxl_bitpos))),
    (csr_addr_misa,       (let
                              lsbs = ((    shiftL  1  misa_A_bitpos)
                                      -- .|. (shiftL  1  misa_C_bitpos)    TODO: uncomment after adding 'C' extension
                                      .|. (shiftL  1  misa_I_bitpos)
                                      .|. (shiftL  1  misa_M_bitpos)
                                      .|. (shiftL  1  misa_N_bitpos)
                                      .|. (shiftL  1  misa_S_bitpos)
                                      .|. (shiftL  1  misa_U_bitpos))
                              msbs | (rv == RV32) = (shiftL  xl_rv32  misa_MXL_bitpos_RV32)
                                   | (rv == RV64) = (shiftL  xl_rv64  misa_MXL_bitpos_RV64)
                           in
                             (msbs .|. lsbs))),
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

-- ================================================================
-- The CSR file is represented as Data_Map.Map from CSR names to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data CSR_File = CSR_File (Data_Map.Map  CSR_Addr  Word64)

-- ================================================================
-- Constructor: make and return a new CSR file

mkCSR_File :: RV -> CSR_File
mkCSR_File rv  =
  let
    dm = (Data_Map.fromList  ((u_csr_reset_values  rv) ++
                              (s_csr_reset_values  rv) ++
                              (m_csr_reset_values  rv)   ))
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
                                            putStr (indent ++ csr_name ++ ":" ++ showHex csr_val ""))
  -- Print n CSRs per line
  let print_n_csrs  [] = return ()
      print_n_csrs  xs = do
        mapM_  print_csr  xs
        putStrLn ""

      n = 5

  mapM_  print_n_csrs  (unflatten  n  m_csr_addrs_and_names)
  mapM_  print_n_csrs  (unflatten  n  s_csr_addrs_and_names)
  mapM_  print_n_csrs  (unflatten  n  u_csr_addrs_and_names)

-- ================================================================
-- Access permissions for a CSR, at a given Privilege Level
-- [Note: csr_addr [11:10] indicates 'read-only' if == 2'b11
--        csr_addr [ 9: 8] indicates minimum privilege for access

data  CSR_Permission = CSR_Permission_None | CSR_Permission_RO | CSR_Permission_RW
  deriving (Eq, Show)

csr_permission :: CSR_File -> Priv_Level -> CSR_Addr -> CSR_Permission
csr_permission  (CSR_File dm)  priv  csr_addr =
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
    mstatus    = fromMaybe  0  (Data_Map.lookup  csr_addr_mstatus  dm)
    tvm_fault  = ((csr_addr == csr_addr_satp) && (testBit  mstatus  mstatus_tvm_bitpos))

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

-- ================================================================
-- CSR reads
-- These are just raw CSR reads, and assume legal csr addrs
-- and csr read permissions

-- TODO: zeroExtend?

csr_read :: RV -> CSR_File -> CSR_Addr -> Word64
csr_read  rv  (CSR_File dm)  csr_addr =
  let
    mstatus = fromMaybe  0  (Data_Map.lookup  csr_addr_mstatus  dm)
    mip     = fromMaybe  0  (Data_Map.lookup  csr_addr_mip      dm)
    mie     = fromMaybe  0  (Data_Map.lookup  csr_addr_mie      dm)

    ustatus_mask = if (rv == RV32) then ustatus_mask_RV32 else ustatus_mask_RV64
    sstatus_mask = if (rv == RV32) then sstatus_mask_RV32 else sstatus_mask_RV64

    val | (csr_addr == csr_addr_ustatus)  = (mstatus .&. ustatus_mask)
        | (csr_addr == csr_addr_uip)      = (mip .&. uip_mask)
        | (csr_addr == csr_addr_uie)      = (mie .&. uip_mask)
        | (csr_addr == csr_addr_cycle)    = fromMaybe  0  (Data_Map.lookup  csr_addr_mcycle    dm)
        | (csr_addr == csr_addr_cycleh)   = (shiftR  (fromMaybe  0  (Data_Map.lookup  csr_addr_mcycle    dm))  32)
        | (csr_addr == csr_addr_instret)  = fromMaybe  0  (Data_Map.lookup  csr_addr_minstret  dm)
        | (csr_addr == csr_addr_instreth) = (shiftR  (fromMaybe  0  (Data_Map.lookup  csr_addr_minstret  dm))  32)

        | (csr_addr == csr_addr_sstatus) = (mstatus .&. sstatus_mask)
        | (csr_addr == csr_addr_sip)     = (mip .&. sip_mask)
        | (csr_addr == csr_addr_sie)     = (mie .&. sip_mask)

        | True                           = fromMaybe  0  (Data_Map.lookup  csr_addr  dm)
  in
    val

-- ================================================================
-- CSR writes
-- These are just raw CSR writes, and assume legal csr addrs
-- and csr write permissions

-- csr_write checks 'member' to avoid inserting new csr_addr into the map
-- Note: silently ignores csr_addrs that are not in the map

-- Some CSR writes which dynamically change architectural features can
-- have wide-ranging side-effects, e.g., changing MISA.C, MSTATUS.MXL
-- Those details are handled in module Machine_State, which uses these
-- raw reads/writes for individual updates.

csr_write :: RV -> CSR_File -> CSR_Addr -> Word64 -> CSR_File
csr_write  rv  (CSR_File dm)  csr_addr  value =
  let
    mstatus = fromMaybe  0  (Data_Map.lookup  csr_addr_mstatus  dm)
    mip     = fromMaybe  0  (Data_Map.lookup  csr_addr_mip      dm)
    mie     = fromMaybe  0  (Data_Map.lookup  csr_addr_mie      dm)

    mstatus_mask = if (rv == RV32) then mstatus_mask_RV32 else mstatus_mask_RV64
    sstatus_mask = if (rv == RV32) then sstatus_mask_RV32 else sstatus_mask_RV64
    ustatus_mask = if (rv == RV32) then ustatus_mask_RV32 else ustatus_mask_RV64

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

      -- TODO: remove these if implementing all counters
      | (csr_addr == csr_addr_mcounteren)  = (csr_addr,         (value .&. 0x7))
      | (csr_addr == csr_addr_scounteren)  = (csr_addr,         (value .&. 0x7))

      | True                           = (csr_addr,         value)

    dm' = if (Data_Map.member csr_addr' dm)
          then Data_Map.insert  csr_addr'  value'  dm
          else dm
  in
    CSR_File dm'

-- ================================================================

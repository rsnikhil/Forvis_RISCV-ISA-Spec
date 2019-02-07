-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Common where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- These are common definitions used by the spec functions of all instructions
--
--   - Functions describing canonical ways to 'finish' an instruction:
--     - possibly writing to a GPR and/or FPR and/or CSR,
--     - possibly incrementing the PC or writing a new value to the PC
--     - possibly updating many CSRs due to a trap
--     In the per-instruction semantics, each function ends by calling
--     one or more of these functions.

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import Arch_Defs
import Machine_State
import CSR_File

#ifdef FLOAT
import FPU
#endif

-- ================================================================
-- Common ways to finish an instruction.
-- Each opcode of course does something unique, but they all finish with
-- a few common actions:
--     - updating register Rd
--     - updating the PC with PC+4 or a new PC
--     - updating a CSR
--     - upating the MINSTRET register (number of instructions retired)
-- These functions capture those standard finishes.

-- ----------------
-- Update GPR RD, increment PC by 2 or 4, increment INSTRET           \begin_latex{finish_rd_and_pc_incr}

finish_rd_and_pc_incr :: GPR_Addr -> Integer -> Bool -> Machine_State -> Machine_State
finish_rd_and_pc_incr    rd          rd_val     is_C    mstate =
  let mstate1 = mstate_gpr_write  rd  rd_val  mstate
      pc      = mstate_pc_read    mstate1
      delta   = if is_C then 2 else 4
      mstate2 = mstate_pc_write  (pc + delta)   mstate1
      mstate3 = incr_minstret     mstate2
  in
    mstate3
                                                                   -- \end_latex{finish_rd_and_pc_incr}
{-# INLINE finish_rd_and_pc_incr #-}

-- ----------------
-- Update GPR RD, CSR.FFlags, increment PC by 4, increment INSTRET       \begin_latex{finish_grd_fflags_and_pc_plus_4}

finish_grd_fflags_and_pc_plus_4 :: GPR_Addr -> Integer -> Integer -> Machine_State -> Machine_State
finish_grd_fflags_and_pc_plus_4    rd          rd_val     fflags     mstate =
  let
    mstate1 = mstate_csr_update  csr_addr_fflags  fflags  mstate
    is_C    = False
    mstate2 = finish_rd_and_pc_incr  rd  rd_val  is_C  mstate1
  in
    mstate2
                                                                      -- \end_latex{finish_grd_fflags_and_pc_plus_4}
{-# INLINE finish_grd_fflags_and_pc_plus_4 #-}

-- ----------------
-- Update GPR RD, update PC to new value, increment INSTRET

finish_rd_and_pc :: GPR_Addr -> Integer -> Integer -> Machine_State -> Machine_State
finish_rd_and_pc    rd          rd_val     new_pc     mstate =
  let
    mstate1 = mstate_gpr_write  rd  rd_val  mstate
    mstate2 = mstate_pc_write  new_pc  mstate1
    mstate3 = incr_minstret  mstate2
  in
    mstate3

{-# INLINE finish_rd_and_pc #-}

-- ----------------
-- Increment PC by 2 or 4, increment INSTRET

finish_pc_incr :: Bool -> Machine_State -> Machine_State
finish_pc_incr    is_C    mstate =
  let
    pc      = mstate_pc_read  mstate
    delta   = if is_C then 2 else 4
    mstate1 = mstate_pc_write  (pc + delta)  mstate
    mstate2 = incr_minstret  mstate1
  in
    mstate2

{-# INLINE finish_pc_incr #-}

-- ----------------
-- Update PC to new value

finish_pc :: Integer -> Machine_State -> Machine_State
finish_pc    new_pc     mstate =
  let
    mstate1 = mstate_pc_write  new_pc  mstate
    mstate2 = incr_minstret  mstate1
  in
    mstate2

{-# INLINE finish_pc #-}

-- ----------------
-- Trap with given exception code and trap value

finish_trap :: Exc_Code -> Integer -> Machine_State -> Machine_State
finish_trap    exc_code    tval       mstate =
  let
    mstate1 = mstate_upd_on_trap  False  exc_code  tval  mstate
    mstate2 = incr_minstret  mstate1
    mstate3 = mstate_last_instr_trapped_write  True  mstate2
  in
    mstate3

{-# INLINE finish_trap #-}

-- ----------------
-- Every completed instruction increments minstret

incr_minstret :: Machine_State -> Machine_State
incr_minstret    mstate =
  let
    minstret = mstate_csr_read   csr_addr_minstret  mstate
    mstate1  = mstate_csr_write  csr_addr_minstret  (minstret + 1)  mstate
  in
    mstate1

{-# INLINE incr_minstret #-}

#ifdef FLOAT

-- ----------------
-- Update FPU.RD, CSR.FFlags, increment PC by 4, increment INSTRET       \begin_latex{finish_frd_fflags_and_pc_plus_4}

finish_frd_fflags_and_pc_plus_4 :: FPR_Addr      ->
                                   Integer       ->
                                   Integer       ->
                                   Bool          ->
                                   Machine_State ->
                                   Machine_State
finish_frd_fflags_and_pc_plus_4  rd  rd_val  fflags  is_n_lt_FLEN  mstate =
  let
    mstate1 = mstate_csr_update  csr_addr_fflags  fflags  mstate
    mstate2 = finish_frd_and_pc_plus_4  rd  rd_val  is_n_lt_FLEN  mstate1
  in
    mstate2
                                                                      -- \end_latex{finish_frd_fflags_and_pc_plus_4}
{-# INLINE finish_frd_fflags_and_pc_plus_4 #-}

-- ----------------
-- Update FPU.FRD, increment PC by 4, increment INSTRET           \begin_latex{finish_frd_and_pc_plus_4}

finish_frd_and_pc_plus_4 :: FPR_Addr -> Integer -> Bool ->       Machine_State -> Machine_State
finish_frd_and_pc_plus_4    rd          rd_val     is_n_lt_FLEN  mstate =
  let mstate1 = if (is_n_lt_FLEN) then 
                   mstate_fpr_write  rd  (nanBox rd_val)  mstate
                else
                   mstate_fpr_write  rd  rd_val  mstate
      pc      = mstate_pc_read    mstate1
      mstate2 = mstate_pc_write  (pc + 4)   mstate1
      mstate3 = incr_minstret     mstate2
  in
    mstate3
#endif
                                                               -- \end_latex{finish_frd_and_pc_plus_4}

-- ================================================================
-- Trap actions:                                               -- \begin_latex{mstate_upd_on_trap}
--   - Compute new privilege level
--   - Update privilege
--   - Update interrupt-enable stacks in CSR MSTATUS
--   - Update CSRs xEPC, xCAUSE, xTVAL
--   - Compute new PC from xTVEC and branch to it

mstate_upd_on_trap :: Bool ->       Exc_Code -> Integer -> Machine_State -> Machine_State
mstate_upd_on_trap    is_interrupt  exc_code    tval       mstate =
  let                                                          -- \end_latex{mstate_upd_on_trap}
    rv      = mstate_rv_read    mstate
    priv    = mstate_priv_read  mstate
    pc      = mstate_pc_read    mstate

    -- Compute new privilege level
    new_priv    = (let
                      misa         = mstate_csr_read  csr_addr_misa  mstate
                      misa_s       = misa_flag  misa  'S'
                      misa_n       = misa_flag  misa  'N'

                      medeleg      = mstate_csr_read  csr_addr_medeleg  mstate
                      mideleg      = mstate_csr_read  csr_addr_mideleg  mstate
                      sedeleg      = mstate_csr_read  csr_addr_sedeleg  mstate
                      sideleg      = mstate_csr_read  csr_addr_sideleg  mstate
                      j            = (fromIntegral exc_code) :: Int
                      m_delegating = testBit  (if is_interrupt then mideleg else medeleg)  j
                      s_delegating = testBit  (if is_interrupt then sideleg else sedeleg)  j

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
                        m_Priv_Level)

    -- Update interrupt-enable stacks in CSR MSTATUS
    mstatus     = mstate_csr_read  csr_addr_mstatus  mstate
    new_mstatus = (let
                      (mpp,spp,mpie,spie,upie,mie,sie,uie) = mstatus_stack_fields  mstatus

                      -- New privilege stack fields
                      (mpp',spp') | (new_priv == m_Priv_Level) = (priv,  spp)
                                  | (new_priv == s_Priv_Level) = (mpp,  priv)

                      -- New interrupt-enable stack fields
                      (mpie',spie',upie',mie',sie',uie')
                        | (new_priv == m_Priv_Level) = ( mie, spie, upie,   0, sie, uie)
                        | (new_priv == s_Priv_Level) = (mpie,  sie, upie, mie,   0, uie)
                        | (new_priv == u_Priv_Level) = (mpie, spie,  uie, mie, sie,   0)
                   in
                      mstatus_upd_stack_fields  mstatus  (mpp',spp',mpie',spie',upie',mie',sie',uie'))

    -- Update CSRs xEPC, xCAUSE, xTVAL
    (csr_addr_xepc,
     csr_addr_xcause,
     csr_addr_xtval,
     csr_addr_xtvec,  xtvec) = if (new_priv == m_Priv_Level)
                               then (csr_addr_mepc,
                                     csr_addr_mcause,
                                     csr_addr_mtval,
                                     csr_addr_mtvec,  mstate_csr_read  csr_addr_mtvec  mstate)
                               else (csr_addr_sepc,
                                     csr_addr_scause,
                                     csr_addr_stval,
                                     csr_addr_stvec,  mstate_csr_read  csr_addr_stvec  mstate)

    -- Compute new PC from xTVEC
    vector_offset = exc_code * 4
    pc1           = if is_interrupt && (tvec_mode (xtvec) == tvec_mode_VECTORED)
                    then tvec_base xtvec + vector_offset
                    else tvec_base xtvec
    pc2           = if rv == RV64
                    then pc1
                    else pc1 .&. 0xFFFFFFFF

    -- Record new priv, pc (next pc), and CSRs status, epc, cause, tval
    cause = mkCause  rv  is_interrupt  exc_code
    mstate1 = mstate_priv_write  new_priv  mstate
    mstate2 = mstate_pc_write    pc2                             mstate1
    mstate3 = mstate_csr_write   csr_addr_mstatus  new_mstatus   mstate2
    mstate4 = mstate_csr_write   csr_addr_xepc     pc            mstate3
    mstate5 = mstate_csr_write   csr_addr_xcause   cause         mstate4
    mstate6 = mstate_csr_write   csr_addr_xtval    tval          mstate5
  in
    mstate6

{-# INLINE mstate_upd_on_trap #-}

-- ================================================================

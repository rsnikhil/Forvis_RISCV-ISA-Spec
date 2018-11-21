-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Common where

-- ================================================================
-- Part of: specification of all RISC-V instructions.

-- These are common definitions used by the spec functions of all instructions
--
--   - The common type of each spec function
--
--   - Functions describing canonical ways to 'finish' an instruction:
--     - possibly writing to a GPR and/or FPR and/or CSR,
--     - possibly incrementing the PC or writing a new value to the PC
--     - possibly updating many CSRs due to a trap
--     In the per-instruction semantics, each function ends by calling
--     one or more of the following functions.

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.

-- Local imports

import Bit_Utils
import FPU
import Arch_Defs
import Machine_State
import CSR_File

-- ================================================================
-- The spec is organized as a collection of functions.
-- Some functions specify just one kind of instruction (e.g., LUI).
-- Some functions specify a small family of related instructions (e.g., BRANCH)
-- Each function has the following type:

type Instr_Spec   = Machine_State -> Instr   -> Bool -> (Bool, Machine_State)
type Instr_C_Spec = Machine_State -> Instr_C -> (Bool, Machine_State)

-- The first argument is a machine state (which contains the architectural state).
-- The second argument is an instruction, a 32-bit word.
-- It returns a 2-tuple (x,y) where:
--   x is True if this instruction is handled by this function
-- and
--   y is the transformed architecture state due executing this instruction, if x is True,
--     and irrelevant otherwise.

-- ================================================================
-- Common ways to finish an instruction.
-- Each opcode of course does something unique, but they all finish with
-- a few common actions:
--     - updating register Rd
--     - updating the PC with PC+4 or a new PC
--     - updating a CSR
--     - upating the MINSTRET register (number of instructions retired)
-- These functions capture those standard finishes.

-- Update GPR RD, increment PC by 2 or 4, increment INSTRET           \begin_latex{finish_rd_and_pc_incr}
{-# INLINE finish_rd_and_pc_incr #-}
finish_rd_and_pc_incr :: Machine_State -> GPR_Addr -> Integer -> Bool -> Machine_State
finish_rd_and_pc_incr    mstate           rd          rd_val     is_C =
  let mstate1 = mstate_gpr_write  mstate  rd  rd_val
      pc      = mstate_pc_read    mstate1
      delta   = if is_C then 2 else 4
      mstate2 = mstate_pc_write   mstate1  (pc + delta)
      mstate3 = incr_minstret     mstate2
  in
    mstate3
                                                               -- \end_latex{finish_rd_and_pc_incr}

-- Update GPR RD, CSR.FFlags, increment PC by 4, increment INSTRET       \begin_latex{finish_grd_fflags_and_pc_plus_4}
{-# INLINE finish_grd_fflags_and_pc_plus_4 #-}
finish_grd_fflags_and_pc_plus_4 :: Machine_State -> GPR_Addr -> Integer -> Integer -> Machine_State
finish_grd_fflags_and_pc_plus_4    mstate           rd          rd_val     fflags  =
  let
    mstate1 = mstate_csr_update  mstate  csr_addr_fflags  fflags
    is_C    = False
    mstate2 = finish_rd_and_pc_incr  mstate1  rd  rd_val  is_C
  in
    mstate2
                                                               -- \end_latex{finish_grd_fflags_and_pc_plus_4}

-- Update GPR RD, update PC to new value, increment INSTRET
{-# INLINE finish_rd_and_pc #-}
finish_rd_and_pc :: Machine_State -> GPR_Addr -> Integer -> Integer -> Machine_State
finish_rd_and_pc    mstate           rd          rd_val     new_pc =
  let
    mstate1 = mstate_gpr_write  mstate  rd  rd_val
    mstate2 = mstate_pc_write  mstate1  new_pc
    mstate3 = incr_minstret  mstate2
  in
    mstate3

-- Increment PC by 2 or 4, increment INSTRET
{-# INLINE finish_pc_incr #-}
finish_pc_incr :: Machine_State -> Bool -> Machine_State
finish_pc_incr    mstate           is_C =
  let
    pc      = mstate_pc_read  mstate
    delta   = if is_C then 2 else 4
    mstate1 = mstate_pc_write  mstate  (pc + delta)
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Update PC to new value
{-# INLINE finish_pc #-}
finish_pc :: Machine_State -> Integer -> Machine_State
finish_pc    mstate           new_pc =
  let
    mstate1 = mstate_pc_write  mstate  new_pc
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Update FPU.RD, CSR.FFlags, increment PC by 4, increment INSTRET       \begin_latex{finish_frd_fflags_and_pc_plus_4}
{-# INLINE finish_frd_fflags_and_pc_plus_4 #-}
finish_frd_fflags_and_pc_plus_4 :: Machine_State -> FPR_Addr -> Integer -> Integer -> Bool -> Machine_State
finish_frd_fflags_and_pc_plus_4    mstate           rd          rd_val     fflags     is_n_lt_FLEN =
  let
    mstate1 = mstate_csr_update  mstate  csr_addr_fflags  fflags
    mstate2 = finish_frd_and_pc_plus_4  mstate1  rd  rd_val  is_n_lt_FLEN
  in
    mstate2
                                                               -- \end_latex{finish_frd_and_pc_plus_4}

-- Update FPU.FRD, increment PC by 4, increment INSTRET               \begin_latex{finish_frd_and_pc_plus_4}
{-# INLINE finish_frd_and_pc_plus_4 #-}
finish_frd_and_pc_plus_4 :: Machine_State -> FPR_Addr -> Integer -> Bool -> Machine_State
finish_frd_and_pc_plus_4    mstate           rd          rd_val     is_n_lt_FLEN =
  let mstate1 = if (is_n_lt_FLEN) then 
                   mstate_fpr_write  mstate  rd  (nanBox rd_val)
                else
                   mstate_fpr_write  mstate  rd  rd_val
      pc      = mstate_pc_read    mstate1
      mstate2 = mstate_pc_write   mstate1  (pc + 4)
      mstate3 = incr_minstret     mstate2
  in
    mstate3
                                                               -- \end_latex{finish_frd_and_pc_plus_4}
-- Trap with given exception code and trap value
{-# INLINE finish_trap #-}
finish_trap :: Machine_State -> Exc_Code -> Integer -> Machine_State
finish_trap    mstate           exc_code    tval =
  let
    mstate1 = mstate_upd_on_trap  mstate  False  exc_code  tval
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Every completed instruction increments minstret
{-# INLINE incr_minstret #-}
incr_minstret :: Machine_State -> Machine_State
incr_minstret  mstate =
  let
    minstret = mstate_csr_read  mstate  csr_addr_minstret
    mstate1  = mstate_csr_write  mstate  csr_addr_minstret  (minstret + 1)
  in
    mstate1

-- ================================================================
-- Trap actions:
--   - Compute new privilege level
--   - Update privilege and interrupt-enable stacks in CSR MSTATUS
--   - Update CSRs xEPC, xCAUSE, xTVAL
--   - Compute new PC from xTVEC and branch to it

{-# INLINE mstate_upd_on_trap #-}
mstate_upd_on_trap :: Machine_State -> Bool ->       Exc_Code -> Integer -> Machine_State
mstate_upd_on_trap    mstate           is_interrupt  exc_code    tval =
  let
    rv      = mstate_rv_read    mstate
    priv    = mstate_priv_read  mstate
    pc      = mstate_pc_read    mstate

    -- START
    new_priv    = (let
                      misa         = mstate_csr_read  mstate  csr_addr_misa
                      misa_s       = misa_flag  misa  'S'
                      misa_n       = misa_flag  misa  'N'

                      medeleg      = mstate_csr_read  mstate  csr_addr_medeleg
                      mideleg      = mstate_csr_read  mstate  csr_addr_mideleg
                      sedeleg      = mstate_csr_read  mstate  csr_addr_sedeleg
                      sideleg      = mstate_csr_read  mstate  csr_addr_sideleg
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

    mstatus     = mstate_csr_read  mstate  csr_addr_mstatus
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

    (csr_addr_xepc,
     csr_addr_xcause,
     csr_addr_xtval,
     csr_addr_xtvec,  xtvec) = if (new_priv == m_Priv_Level)
                               then (csr_addr_mepc,
                                     csr_addr_mcause,
                                     csr_addr_mtval,
                                     csr_addr_mtvec,  mstate_csr_read  mstate  csr_addr_mtvec)
                               else (csr_addr_sepc,
                                     csr_addr_scause,
                                     csr_addr_stval,
                                     csr_addr_stvec,  mstate_csr_read  mstate  csr_addr_stvec)

    -- Compute the new PC
    vector_offset = exc_code * 4
    pc1           = if is_interrupt && (tvec_mode (xtvec) == tvec_mode_VECTORED)
                    then tvec_base xtvec + vector_offset
                    else tvec_base xtvec
    pc2           = if rv == RV64
                    then pc1
                    else pc1 .&. 0xFFFFFFFF

    -- Record new priv, pc, and CSRs status, epc, cause, tval
    mstate1 = mstate_priv_write  mstate   new_priv
    mstate2 = mstate_pc_write    mstate1  pc2

    mstate3 = mstate_csr_write   mstate2  csr_addr_mstatus  new_mstatus
    mstate4 = mstate_csr_write   mstate3  csr_addr_xepc     pc
    mstate5 = mstate_csr_write   mstate4  csr_addr_xcause   (mkCause  rv  is_interrupt  exc_code)
    mstate6 = mstate_csr_write   mstate5  csr_addr_xtval  tval
  in
    mstate6

-- ================================================================

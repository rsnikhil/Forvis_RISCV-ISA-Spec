-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Interrupts where

-- ================================================================
-- This module defines functions related to taking interrupts.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe

-- Project imports

import Arch_Defs
import Machine_State
import CSR_File
import Forvis_Spec_Common

-- ================================================================
-- Take interrupt if an interrupt is pending and enabled           -- \begin_latex{take_interrupt}

mstate_take_interrupt_if_any :: Machine_State -> (Maybe Exc_Code, Machine_State)
mstate_take_interrupt_if_any    mstate =
  let                                                              -- \end_latex{...take_interrupt}
    misa    = mstate_csr_read  mstate  csr_addr_misa
    mstatus = mstate_csr_read  mstate  csr_addr_mstatus
    mip     = mstate_csr_read  mstate  csr_addr_mip
    mie     = mstate_csr_read  mstate  csr_addr_mie
    mideleg = mstate_csr_read  mstate  csr_addr_mideleg
    sideleg = mstate_csr_read  mstate  csr_addr_sideleg
    priv    = mstate_priv_read  mstate

    intr_pending :: Maybe  Exc_Code
    intr_pending = csr_interrupt_pending  misa  mstatus  mip  mie  mideleg  sideleg  priv
  in
    case intr_pending of
      Nothing        -> (Nothing, mstate)
      Just  exc_code ->
        let
          tval    = 0
          mstate1 = mstate_upd_on_trap  mstate  True  exc_code  tval
          mstate2 = mstate_run_state_write  mstate1  Run_State_Running
        in
          (intr_pending, mstate2)

{-# INLINE mstate_take_interrupt_if_any #-}

-- ================================================================
-- Check if an interrupt is pending to resume from WFI state.  Note:
-- resuming from WFI does not actually take the interrupt, just
-- resumes at the instruction following WFI (which may, in turn, take
-- the interrupt).

-- Thus, the resumption condition is weaker than the condition for
-- actually taking an interrupt (see csr_wfi_resume), since it is
-- unaffected by MSTATUS.MIE/SIE/UIE and MIDELEG and MEDELEG.
                                                           -- \begin_latex{wfi_resume}
mstate_wfi_resume :: Machine_State -> Bool
mstate_wfi_resume    mstate =
  let                                                      -- \end_latex{...wfi_resume}
    mip = mstate_csr_read  mstate  csr_addr_mip
    mie = mstate_csr_read  mstate  csr_addr_mie
  in
    csr_wfi_resume  mip  mie

-- ================================================================

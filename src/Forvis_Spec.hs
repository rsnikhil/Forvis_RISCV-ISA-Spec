-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec where

-- ================================================================
-- Instruction fetch; execute one instruction; take interrupt

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.
import Data.Int     -- For Intxx type (signed fixed-width ints)
import Numeric (showHex)

-- Local imports

import Bit_Utils
import Arch_Defs
import Machine_State
import Virtual_Mem
import Forvis_Spec_Common    -- Canonical ways for finish an instruction

-- User-level instructions
import Forvis_Spec_I           -- 'I'     (Base) instruction set (RV32 and RV64)
import Forvis_Spec_I64         -- 'I64'   (Base) instruction set (RV64 only)
import Forvis_Spec_Zicsr       -- 'Zicsr' instruction set
import Forvis_Spec_Zifencei    -- 'Zicsr' instruction set
import Forvis_Spec_M           -- Extension 'M' (Integer Multiply/Divide)

import Forvis_Spec_A           -- Extension 'A' (Atomic Memory Ops (AMO))
import Forvis_Spec_C           -- Extension 'C' (Compressed 16-bit instrs)

import Forvis_Spec_F           -- Extension 'F' (single-precision floating point)
import Forvis_Spec_D           -- Extension 'D' (double-precision floating point)

-- Privileged Architecture instructions
import Forvis_Spec_Priv

-- ================================================================ \begin_latex{instr_fetch}
-- Instruction fetch
-- This function attempts an insruction fetch based on the current PC.

-- We do not blindly fetch 4 bytes, since the fetch of the latter 2
-- bytes may trap, which may not be relevant if the first 2 bytes are
-- a 'C' (compressed) instruction (which may trap or jump elsewhere).

-- So:
--   - We first attempt to read 2 bytes only
--   - This fetch-attempt may trap
--   - Else we check if it's a 'C' instruction; if so, we're done
--   - Else we attempt to read the next 2 bytes (remaining 2 bytes of a 32b instr)
--   -      This fetch-attempt may also trap

data Fetch_Result = Fetch_Trap  Exc_Code
                  | Fetch_C     Integer
                  | Fetch       Integer
                  deriving (Show)

{-# INLINE instr_fetch #-}
instr_fetch :: Machine_State -> (Fetch_Result, Machine_State)
instr_fetch  mstate =
  let                                                              -- \end_latex{instr_fetch}
    rv                = mstate_rv_read   mstate
    pc | (rv == RV32) = (mstate_pc_read  mstate .&. 0xFFFFFFFF)
       | (rv == RV64) = mstate_pc_read   mstate
    misa              = mstate_csr_read  mstate  csr_addr_misa
  in
    if (not  (misa_flag  misa  'C')) then
      -- 32b instructions only.
      -- This is a simulation-speed optimization where we don't do the
      -- default 16-bit fetches if MISA.C is false.
      let
        -- Read 4 instr bytes
        -- with virtual-to-physical translation if necessary.
        (result1, mstate1) = read_n_instr_bytes  mstate  4  pc
      in
        case result1 of
          Mem_Result_Err  exc_code -> (let
                                          tval    = pc
                                          mstate2 = finish_trap  mstate1  exc_code  tval
                                       in
                                          (Fetch_Trap  exc_code,  mstate2))
          Mem_Result_Ok  u32       -> (Fetch  u32,  mstate1)

    else
      let
        -- 16b and 32b instructions; read 2 instr bytes first
        -- with virtual-to-physical translation if necessary.
        (result1, mstate1) = read_n_instr_bytes  mstate  2  pc
      in
        case result1 of
          Mem_Result_Err  exc_code -> (let
                                          tval    = pc
                                          mstate2 = finish_trap  mstate1  exc_code  tval
                                       in
                                         (Fetch_Trap  exc_code, mstate2))

          Mem_Result_Ok   u16_lo ->
            if is_instr_C  u16_lo then
              -- Is a 'C' instruction; done
              (Fetch_C  u16_lo,  mstate1)
            else
              (let
                  -- Not a 'C' instruction; read remaining 2 instr bytes
                  -- with virtual-to-physical translation if necessary.
                  -- Note: pc and pc+2 may translate to non-contiguous pages.
                  (result2, mstate2) = read_n_instr_bytes  mstate  2  (pc + 2)
                in
                  case result2 of
                    Mem_Result_Err  exc_code -> (let
                                                    tval = pc + 2
                                                    mstate3 = finish_trap  mstate2  exc_code  tval
                                                 in
                                                    (Fetch_Trap  exc_code, mstate3))
                    Mem_Result_Ok  u16_hi    -> (let
                                                    u32 = bitconcat_u16_u16_to_u32  u16_hi  u16_lo
                                                 in
                                                    (Fetch  u32,  mstate2)))

{-# INLINE read_n_instr_bytes #-}
read_n_instr_bytes :: Machine_State -> Int   -> Integer -> (Mem_Result, Machine_State)
read_n_instr_bytes    mstate           n_bytes  va =
  let
    -- Read mem, possibly with virtual mem translation
    is_instr = True
    funct3   = if (n_bytes == 4) then  funct3_LW  else  funct3_LH

    (result1, mstate1) = mstate_vm_read  mstate  is_instr  exc_code_instr_access_fault  funct3  va
  in
    (result1, mstate1)

-- ================================================================
-- Execute one 32b instruction
-- Tries I, Zifencei, Zicsr, I64, ... in that order

exec_instr_32b :: Instr_32b -> Machine_State -> (Machine_State, String)
exec_instr_32b    instr_32b    mstate =
  let
    rv   = mstate_rv_read  mstate
    misa = mstate_csr_read  mstate  csr_addr_misa
    frm  = mstate_csr_read  mstate  csr_addr_frm
    is_C = False

    dec_I         = decode_I         rv        instr_32b
    dec_Zifencei  = decode_Zifencei  rv        instr_32b
    dec_Zicsr     = decode_Zicsr     rv        instr_32b
    dec_I64       = decode_I64       rv        instr_32b
    dec_M         = decode_M         rv        instr_32b
    dec_A         = decode_A         rv        instr_32b
    dec_F         = decode_F   frm   rv        instr_32b
    dec_D         = decode_D   frm   rv        instr_32b
    dec_Priv      = decode_Priv      rv        instr_32b
  in
    case dec_I of
      Just  instr_I -> (exec_instr_I  is_C  instr_I  mstate,
                        show  instr_I)
      Nothing ->
        case dec_Zifencei of
          Just instr_Zifencei -> (exec_instr_Zifencei  is_C  instr_Zifencei  mstate,
                                  show  instr_Zifencei)
          Nothing ->
            case dec_Zicsr of
              Just instr_Zicsr -> (exec_instr_Zicsr  instr_32b  is_C  instr_Zicsr  mstate,
                                   show  instr_Zicsr)
              Nothing ->
                case dec_I64 of
                  Just instr_I64 -> (exec_instr_I64  is_C  instr_I64  mstate,
                                     show  instr_I64)
                  Nothing ->
                    case dec_M of
                      Just instr_M -> (exec_instr_M  is_C  instr_M  mstate,
                                       show  instr_M)
                      Nothing ->
                        case dec_A of
                          Just instr_A -> (exec_instr_A  is_C  instr_A  mstate,
                                           show  instr_A)
                          Nothing ->
                            case dec_F of
                              Just instr_F -> (exec_instr_F  is_C  instr_F  mstate,
                                               show  instr_F)
                              Nothing ->
                                case dec_D of
                                  Just instr_D -> (exec_instr_D  is_C  instr_D  mstate,
                                                   show  instr_D)
                                  Nothing ->
                                    case dec_Priv of
                                      Just instr_Priv -> (exec_instr_Priv  instr_32b  is_C  instr_Priv  mstate,
                                                          show  instr_Priv)
                                      Nothing ->
                                        -- Illegal instruction trap, since does not decode to any 32b instr
                                        let
                                          tval    = instr_32b
                                          mstate1 = finish_trap  mstate  exc_code_illegal_instruction  tval
                                        in
                                          (mstate1, "Illegal instr 0x" ++ showHex instr_32b "")

-- ================================================================
-- Execute one 16b instruction

-- See 'instr_specs_C' in Forvis_Spec_C.hs for all the 'Compressed' instructions

-- 'exec_instr_C' takes a machine state and a 16-bit compressed instruction and
-- returns a new machine state after executing that instruction.  It
-- attempts all the specs in 'instr_C_specs' and, if none of them apply,
-- performs an illegal-instruction trap.

exec_instr_16b :: Instr_16b -> Machine_State -> (Machine_State, String)
exec_instr_16b    instr_16b    mstate =
  let
    rv   = mstate_rv_read  mstate
    misa = mstate_csr_read  mstate  csr_addr_misa

    dec_C = decode_C  rv  misa  instr_16b
  in
    case dec_C of
      Just  instr_C -> (exec_instr_C  instr_C  mstate,
                        show  instr_C)
      Nothing ->
        -- Illegal instruction trap, since does not decode to any 16b instr
        let
          tval    = instr_16b
          mstate1 = finish_trap  mstate  exc_code_illegal_instruction  tval
        in
          (mstate1, "Illegal instr " ++ showHex instr_16b "")

-- ================================================================
-- Take interrupt if interrupts pending and enabled                   \begin_latex{take_interrupt}

{-# INLINE take_interrupt_if_any #-}
take_interrupt_if_any :: Machine_State -> (Maybe Exc_Code, Machine_State)
take_interrupt_if_any    mstate =
  let                                                              -- \end_latex{take_interrupt}
    misa    = mstate_csr_read  mstate  csr_addr_misa
    mstatus = mstate_csr_read  mstate  csr_addr_mstatus
    mip     = mstate_csr_read  mstate  csr_addr_mip
    mie     = mstate_csr_read  mstate  csr_addr_mie
    mideleg = mstate_csr_read  mstate  csr_addr_mideleg
    sideleg = mstate_csr_read  mstate  csr_addr_sideleg

    priv    = mstate_priv_read  mstate

    tval    = 0
    intr_pending = fn_interrupt_pending  misa  mstatus  mip  mie  mideleg  sideleg  priv
  in
    case intr_pending of
      Nothing        -> (intr_pending, mstate)
      Just  exc_code ->
        let
          mstate1 = mstate_upd_on_trap  mstate  True  exc_code  tval
          mstate2 = mstate_run_state_write  mstate1  Run_State_Running
        in
          (intr_pending, mstate2)

-- ================================================================

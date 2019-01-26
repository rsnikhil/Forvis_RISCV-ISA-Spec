-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Execute where

-- ================================================================
-- This module describes the top-level 'Execute' functions for 32-bit
-- and 16-bit (compressed) instructions. These merely dispatch to the
-- appropriate extension-specific 'Execute' function (such as I, I64,
-- A, C, F, D, M, Priv, Zicsr, Zifencei)

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.
import Data.Int     -- For Intxx type (signed fixed-width ints)
import Numeric (showHex)

-- Project imports

import Bit_Utils
import Arch_Defs
import Machine_State
import CSR_File
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

#ifdef FLOAT
import Forvis_Spec_F           -- Extension 'F' (single-precision floating point)
import Forvis_Spec_D           -- Extension 'D' (double-precision floating point)
#endif

-- Privileged Architecture instructions
import Forvis_Spec_Priv

-- ================================================================
-- exec_instr_32b: Execute one 32b instruction

-- This function takes a 32-bit instruction and a machine state and
-- returns a new machine state after decoding and executing that
-- instruction.  The second component of the result (String) is just
-- for printing instruction traces during simulation, and has no
-- semantic significance.

-- It attempts to decode the 32-bit instruction into each of the
-- supported extensions.  It then dispatches, based on a successful
-- decode, to that extension's 'exec' function.  If no decode is
-- successful, it performs an illegal-instruction trap.

exec_instr_32b :: Instr_32b -> Machine_State -> (Machine_State, String)
exec_instr_32b    instr_32b    mstate =
  let
    rv   = mstate_rv_read  mstate
    misa = mstate_csr_read  csr_addr_misa  mstate
    frm  = mstate_csr_read  csr_addr_frm   mstate
    is_C = False

    dec_I         = decode_I         rv        instr_32b
    dec_Zifencei  = decode_Zifencei  rv        instr_32b
    dec_Zicsr     = decode_Zicsr     rv        instr_32b
    dec_I64       = decode_I64       rv        instr_32b
    dec_M         = decode_M         rv        instr_32b
    dec_A         = decode_A         rv        instr_32b
    dec_Priv      = decode_Priv      rv        instr_32b
#ifdef FLOAT
    dec_F         = decode_F   frm   rv        instr_32b
    dec_D         = decode_D   frm   rv        instr_32b
#endif
  in
                                                                       -- \end_latex{...exec_instr_32b}
                                                                       -- \begin_latex{exec_instr_32b_2}
    case dec_I of
      Just  instr_I -> (exec_instr_I  is_C  instr_I  mstate,
                        show  instr_I)
      Nothing ->
        case dec_I64 of
          Just instr_I64 -> (exec_instr_I64  is_C  instr_I64  mstate,
                             show  instr_I64)
          Nothing ->
            case dec_M of
              Just instr_M -> (exec_instr_M  is_C  instr_M  mstate,
                               show  instr_M)
                                                                       -- \end_latex{...exec_instr_32b_2}
              Nothing ->
                case dec_A of
                  Just instr_A -> (exec_instr_A  is_C  instr_A  mstate,
                                   show  instr_A)
                  Nothing ->
                    case dec_Priv of
                      Just instr_Priv -> (exec_instr_Priv  instr_32b  is_C  instr_Priv  mstate,
                                          show  instr_Priv)
                      Nothing ->
                        case dec_Zifencei of
                          Just instr_Zifencei -> (exec_instr_Zifencei  is_C  instr_Zifencei  mstate,
                                                  show  instr_Zifencei)
                          Nothing ->
                            case dec_Zicsr of
                              Just instr_Zicsr -> (exec_instr_Zicsr  instr_32b  is_C  instr_Zicsr  mstate,
                                                   show  instr_Zicsr)
                              Nothing ->
#ifdef FLOAT
                                case dec_F of
                                  Just instr_F -> (exec_instr_F  is_C  instr_F  mstate,
                                                   show  instr_F)
                                  Nothing ->
                                    case dec_D of
                                      Just instr_D -> (exec_instr_D  is_C  instr_D  mstate,
                                                       show  instr_D)
                                      Nothing ->
#endif
                                                                       -- \begin_latex{exec_instr_32b_3}
                                        -- Illegal instruction trap, since does
                                        -- not decode to any 32b instr
                                        let
                                          tval    = instr_32b
                                          mstate1 = finish_trap  exc_code_illegal_instruction
                                                                 tval
                                                                 mstate
                                        in
                                          (mstate1, "Illegal instr 0x" ++
                                                    showHex instr_32b "")
                                                                       -- \end_latex{exec_instr_32b_3}

{-# INLINE exec_instr_32b #-}

-- ================================================================
-- exec_instr_16b: Execute one 16b instruction

-- This function takes a 16-bit compressed instruction and a machine
-- state and returns a new machine state after decoding and executing
-- that instruction.  If it does not decode to a legal C instruction,
-- it performs an illegal-instruction trap, other wise it invokes
-- 'exec_instr_C' in Forvis_Spec_C.hs to perform the action.

exec_instr_16b :: Instr_16b -> Machine_State -> (Machine_State, String)
exec_instr_16b    instr_16b    mstate =
  let
    rv   = mstate_rv_read  mstate
    misa = mstate_csr_read  csr_addr_misa  mstate

    dec_C = decode_C  rv  misa  instr_16b
  in
    case dec_C of
      Just  instr_C -> (exec_instr_C  instr_C  mstate,
                        show  instr_C)
      Nothing ->
        -- Illegal instruction trap, since does not decode to any 16b instr
        let
          tval    = instr_16b
          mstate1 = finish_trap  exc_code_illegal_instruction  tval  mstate
        in
          (mstate1, "Illegal instr " ++ showHex instr_16b "")

{-# INLINE exec_instr_16b #-}

-- ================================================================

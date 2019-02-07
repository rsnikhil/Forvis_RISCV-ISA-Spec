-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec_Instr_Fetch where

-- ================================================================
-- This module describes the semantics of fetching an instruction

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

-- ================================================================
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
                                                                   -- \begin_latex{Fetch_Result}
data Fetch_Result = Fetch       Integer     -- Normal 32-bit instr
                  | Fetch_C     Integer     -- C (compressed) 16-bit instr
                  | Fetch_Trap  Exc_Code    -- e.g., misaligned access, page fault, etc.
                  deriving (Show)                                  -- \end_latex{Fetch_Result}

                                                                   -- \begin_latex{instr_fetch}
instr_fetch :: Machine_State -> (Fetch_Result, Machine_State)
instr_fetch    mstate =                                            -- \end_latex{instr_fetch}
  let                  
    rv                = mstate_rv_read   mstate
    pc | (rv == RV32) = (mstate_pc_read  mstate .&. 0xFFFFFFFF)
       | (rv == RV64) = mstate_pc_read   mstate
    misa              = mstate_csr_read  csr_addr_misa  mstate
  in
    if (not  (misa_flag  misa  'C')) then
      -- 32b instructions only.
      -- This is a simulation-speed optimization where we don't do the
      -- default 16-bit fetches if MISA.C is false.                     -- \begin_latex{instr_fetch_1}
      let
        -- Read 4 instr bytes
        -- with virtual-to-physical translation if necessary.
        (result1, mstate1) = read_n_instr_bytes  mstate  4  pc
      in
        case result1 of
          Mem_Result_Err  exc_code -> (let
                                          tval    = pc
                                          mstate2 = finish_trap  exc_code  tval  mstate1
                                       in
                                          (Fetch_Trap  exc_code,  mstate2))
          Mem_Result_Ok  u32       -> (Fetch  u32,  mstate1)
                                                                        -- \end_latex{instr_fetch_1}
    else                                                                -- \begin_latex{instr_fetch_2}
      let
        -- 16b and 32b instructions; read 2 instr bytes first
        -- with virtual-to-physical translation if necessary.
        (result1, mstate1) = read_n_instr_bytes  mstate  2  pc
      in
        case result1 of
          Mem_Result_Err  exc_code -> (let
                                          tval    = pc
                                          mstate2 = finish_trap  exc_code  tval  mstate1
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
                in                                                      -- \end_latex{...instr_fetch_2}
                  case result2 of
                    Mem_Result_Err  exc_code -> (let
                                                    tval = pc + 2
                                                    mstate3 = finish_trap  exc_code  tval  mstate2
                                                 in
                                                    (Fetch_Trap  exc_code, mstate3))
                    Mem_Result_Ok  u16_hi    -> (let
                                                    u32 = bitconcat_u16_u16_to_u32  u16_hi  u16_lo
                                                 in
                                                    (Fetch  u32,  mstate2)))

-- ----------------
-- Help-function for instr_fetch

read_n_instr_bytes :: Machine_State -> Int   -> Integer -> (Mem_Result, Machine_State)
read_n_instr_bytes    mstate           n_bytes  va =
  let
    -- Read mem, possibly with virtual mem translation
    is_instr = True
    funct3   = if (n_bytes == 4) then  funct3_LW  else  funct3_LH

    (result1, mstate1) = mstate_vm_read  mstate  is_instr  exc_code_instr_access_fault  funct3  va
  in
    (result1, mstate1)

{-# INLINE instr_fetch #-}
{-# INLINE read_n_instr_bytes #-}

-- ================================================================

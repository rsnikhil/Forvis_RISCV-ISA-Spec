-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module MMIO where

-- ================================================================
-- This module handles memory-mapped I/O reads and writes.
-- Fairly trivial I/O for the moment.

-- ================================================================
-- Standard Haskell imports

import Data.Word
import Data.Bits
-- import Data.Char
import Numeric (showHex, readHex)

-- Project imports

import Bit_Manipulation
import Arch_Defs
import Mem_Ops
import Address_Map
import UART

-- ================================================================
-- IO subsystem representation
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data MMIO = MMIO {
  -- Real time clock,
  -- real time comparison point, to generate timer interrupts
  f_mtime    :: Word64,
  f_mtimecmp :: Word64,
  f_mtip     :: Bool,

  -- Location for generating software interrupts
  f_msip :: Word64,

  -- UART for console I/O
  f_uart :: UART_NS16550A
  }

mkMMIO :: MMIO
mkMMIO = MMIO { f_mtime         = 1,    -- greater than mtimecmp, to avoid initial interrupt
                f_mtimecmp      = 0,
                f_mtip          = False,

                f_msip          = 0,

                f_uart          = mkUART
              }

-- ================================================================
-- Tick mtime.
-- Set mtip field if mtime reaches mtimecmp.

mmio_tick_mtime :: MMIO -> MMIO
mmio_tick_mtime  mmio =
  let
    mtime            = f_mtime     mmio
    mtimecmp         = f_mtimecmp  mmio
    mtip             = f_mtip      mmio

    mtime'           = mtime + 1
    mtip'            = (mtip || (mtime' >= mtimecmp))

    mmio'            = mmio { f_mtime    = mtime',
                              f_mtip     = mtip'}
  in
    mmio'

-- ================================================================
-- Check for interrupts (external, timer, software)

mmio_has_interrupts :: MMIO -> (Bool, Bool, Bool)
mmio_has_interrupts  mmio =
  let
    eip  = uart_has_interrupt  (f_uart  mmio)
    tip  = f_mtip  mmio
    sip  = ((f_msip  mmio) /= 0)
  in
    (eip, tip, sip)

-- ================================================================
-- Read data from MMIO
-- TODO: currently returns 'ok' with bogus value on most addrs; should return Mem_Result_Err

mmio_read :: MMIO -> InstrField -> Word64 -> (Mem_Result, MMIO)
mmio_read  mmio  funct3  addr =
  if (addr == addr_mtime) then
    let
      mtime = f_mtime  mmio
    in
      (Mem_Result_Ok  mtime,  mmio)

  else if (addr == addr_mtimecmp) then
    let
      mtimecmp = f_mtimecmp  mmio
    in
      (Mem_Result_Ok  mtimecmp,  mmio)

  else if (addr == addr_msip) then
    let
      msip = f_msip  mmio
    in
      (Mem_Result_Ok  msip,  mmio)

  else if ((addr_base_UART <= addr) && (addr < (addr_base_UART + addr_size_UART))) then
    let
      uart       = f_uart  mmio
      (v, uart') = uart_read  uart  (addr - addr_base_UART)
      v_u64      = zeroExtend_u8_to_u64  v
      mmio'      = mmio { f_uart = uart' }
    in
      (Mem_Result_Ok  v_u64,  mmio')

  else
    (Mem_Result_Err  exc_code_load_access_fault,  mmio)

-- ================================================================
-- Write data into MMIO

mmio_write :: MMIO -> InstrField -> Word64 -> Word64 -> (Mem_Result, MMIO)
mmio_write  mmio  funct3  addr  val =
  if (addr == addr_mtimecmp) then
    let
      mmio' = mmio { f_mtimecmp = val,
                     f_mtip     = False }
    in
      (Mem_Result_Ok  0,  mmio')

  else if (addr == addr_msip) then
    let
      mmio' = mmio { f_msip = val }
    in
      (Mem_Result_Ok  0,  mmio')

  else if (addr == addr_htif_console_out) then
    mmio_write  mmio  funct3  (addr_base_UART + addr_UART_thr)  val

  else if ((addr_base_UART <= addr) && (addr < (addr_base_UART + addr_size_UART))) then
    let
      uart  = f_uart  mmio
      uart' = uart_write  uart  (addr - addr_base_UART)  val
      mmio' = mmio {f_uart = uart'}
    in
      (Mem_Result_Ok  0, mmio')

  else
    (Mem_Result_Err  exc_code_store_AMO_access_fault, mmio)

-- ================================================================
-- AMO op
-- TODO: find out if does LR also return STORE_AMO_ACCESS_FAULT or LOAD_ACCESS_FAULT?

mmio_amo :: MMIO -> Word64 -> InstrField -> InstrField -> InstrField -> InstrField -> Word64 ->
           (Mem_Result, MMIO)

mmio_amo  mmio  addr  funct3  msbs5  aq  rl  stv_d =
  if (not (is_AMO_aligned  funct3  addr)) then
    (Mem_Result_Err exc_code_store_AMO_addr_misaligned,  mmio)

  else if ((addr /= addr_msip) &&
           (addr /= addr_mtimecmp)) then
         (Mem_Result_Err exc_code_store_AMO_access_fault,  mmio)

  else
    let
      stv_w0 = trunc_u64_to_u32  stv_d
      stv_w1 = trunc_u64_to_u32  (shiftR  stv_d  32)

      -- Get old values (omvs)    -- TODO: UART locations
      omv_d  = if (addr == addr_msip) then f_msip mmio
               else if (addr == addr_mtimecmp) then f_mtimecmp  mmio
                    else if (addr == addr_mtime) then f_mtime  mmio
                         else 0
      omv_w0 = trunc_u64_to_u32  omv_d
      omv_w1 = trunc_u64_to_u32  (shiftR  omv_d  32)

      -- Load-value (to be returned to CPU)
      ldv | (msbs5  == msbs5_AMO_SC) = 1    -- always fail (SC success = 0, SC failure = non-zero)
          | (funct3 == funct3_AMO_W) = bitconcat_u32_u32_to_u64  0  omv_w0
          | (funct3 == funct3_AMO_D) = omv_d

      -- New memory value (to be stored back)
      (nmv_w1, nmv_w0) | (msbs5 == msbs5_AMO_SC)   = (stv_w1, stv_w0)
                       | (msbs5 == msbs5_AMO_SWAP) = (stv_w1, stv_w0)
                       | (msbs5 == msbs5_AMO_ADD)  = (if (funct3 == funct3_AMO_W) then
                                                        let
                                                          z_w = cvt_s32_to_u32 ((cvt_u32_to_s32  omv_w0) + (cvt_u32_to_s32  stv_w0))
                                                        in
                                                          (0, z_w)
                                                      else
                                                        let
                                                          z_d = cvt_s64_to_u64 ((cvt_u64_to_s64  omv_d) + (cvt_u64_to_s64  stv_d))
                                                        in
                                                          (trunc_u64_to_u32 (shiftR  z_d  32),  trunc_u64_to_u32  z_d))

                       | (msbs5 == msbs5_AMO_AND)  = ((omv_w1 .&. stv_w1),
                                                      (omv_w0 .&. stv_w0))

                       | (msbs5 == msbs5_AMO_OR)   = ((omv_w1 .|. stv_w1),
                                                      (omv_w0 .|. stv_w0))

                       | (msbs5 == msbs5_AMO_XOR)  = ((xor  omv_w1  stv_w1),
                                                      (xor  omv_w0  stv_w0))

                       | (msbs5 == msbs5_AMO_MAX)  = (if (funct3 == funct3_AMO_W) then
                                                        let
                                                          z_w = if ((cvt_u32_to_s32  omv_w0) > (cvt_u32_to_s32  stv_w0)) then
                                                                  omv_w0
                                                                else
                                                                  stv_w0
                                                        in
                                                          (0, z_w)
                                                      else
                                                        if ((cvt_u64_to_s64  omv_d) > (cvt_u64_to_s64  stv_d)) then
                                                          (omv_w1, omv_w0)
                                                        else
                                                          (stv_w1, stv_w0))

                       | (msbs5 == msbs5_AMO_MIN)  = (if (funct3 == funct3_AMO_W) then
                                                        let
                                                          z_w = if ((cvt_u32_to_s32  omv_w0) < (cvt_u32_to_s32  stv_w0)) then
                                                                  omv_w0
                                                                else
                                                                  stv_w0
                                                        in
                                                          (0, z_w)
                                                      else
                                                        if ((cvt_u64_to_s64  omv_d) < (cvt_u64_to_s64  stv_d)) then
                                                          (omv_w1, omv_w0)
                                                        else
                                                          (stv_w1, stv_w0))

                       | (msbs5 == msbs5_AMO_MAXU) = (if (funct3 == funct3_AMO_W) then
                                                        let
                                                          z_w = if (omv_w0 > stv_w0) then
                                                                  omv_w0
                                                                else
                                                                  stv_w0
                                                        in
                                                          (0, z_w)
                                                      else
                                                        if (omv_d > stv_d) then
                                                          (omv_w1, omv_w0)
                                                        else
                                                          (stv_w1, stv_w0))
                       | (msbs5 == msbs5_AMO_MINU) = (if (funct3 == funct3_AMO_W) then
                                                        let
                                                          z_w = if (omv_w0 < stv_w0) then
                                                                  omv_w0
                                                                else
                                                                  stv_w0
                                                        in
                                                          (0, z_w)
                                                      else
                                                        if (omv_d < stv_d) then
                                                          (omv_w1, omv_w0)
                                                        else
                                                          (stv_w1, stv_w0))
      nmv_d = bitconcat_u32_u32_to_u64  nmv_w1  nmv_w0

      -- Update locations
      mmio' | (msbs5 == msbs5_AMO_LR)  = mmio
            | (msbs5 == msbs5_AMO_SC)  = mmio
            | True                     = if (addr == addr_mtimecmp) then mmio {f_mtimecmp = nmv_d,
                                                                               f_mtip     = False}
                                         else if (addr == addr_msip)
                                              then mmio {f_msip = nmv_d}
                                              else mmio    -- TODO: UART addrs
    in
      (Mem_Result_Ok  ldv, mmio')

-- ================================================================
-- Convenience function to read MTIME

mmio_read_mtime :: MMIO -> Word64
mmio_read_mtime  mmio = f_mtime  mmio

-- ================================================================
-- Enqueue (tty -> UART -> MMIO -> CPU) console input

mmio_enq_console_input :: MMIO -> String -> MMIO
mmio_enq_console_input  mmio  s =
  let
    uart  = f_uart  mmio
    uart' = uart_enq_input  uart  s
    mmio' = mmio {f_uart = uart'}
  in
    mmio'

-- ================================================================
-- Dequeue (CPU -> MMIO -> UART -> tty) console output

mmio_deq_console_output :: MMIO -> (String, MMIO)
mmio_deq_console_output  mmio =
  let
    uart       = f_uart  mmio
    (s, uart') = uart_deq_output  uart
    mmio'      = if (s == "") then mmio
                 else mmio {f_uart = uart'}
  in
    (s, mmio')

-- ================================================================
-- Read all console input

mmio_all_console_input :: MMIO -> (String, String)
mmio_all_console_input  mmio =
  let
    uart = f_uart  mmio
  in
    uart_all_input  uart

-- ================================================================
-- Read all console output

mmio_all_console_output :: MMIO -> (String, String)
mmio_all_console_output  mmio =
  let
    uart = f_uart  mmio
  in
    uart_all_output  uart

-- ================================================================

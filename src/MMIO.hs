-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module MMIO where

-- ================================================================
-- This module handles memory-mapped I/O reads and writes.
-- Fairly trivial I/O for the moment.

-- ================================================================
-- Standard Haskell imports

import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

import Bit_Utils
import Arch_Defs
import Mem_Ops
import Address_Map
                                                                -- \begin_latex{import_UART}
import UART
                                                                -- \end_latex{import_UART}

-- ================================================================
-- IO subsystem representation
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data MMIO = MMIO {
  -- Real time clock,
  -- real time comparison point, to generate timer interrupts
  f_mtime    :: Integer,
  f_mtimecmp :: Integer,
  f_mtip     :: Bool,

  -- Location for generating software interrupts
  f_msip :: Integer,
                                                                -- \begin_latex{MMIO_f_uart}
  -- UART for console I/O
  f_uart :: UART_NS16550A
                                                                -- \end_latex{MMIO_f_uart}
  }
  deriving Eq

mkMMIO :: MMIO
mkMMIO = MMIO { f_mtime         = 1,    -- greater than mtimecmp, to avoid initial interrupt
                f_mtimecmp      = 0,
                f_mtip          = False,

                f_msip          = 0,
                                                                -- \begin_latex{mkMMIO_f_uart}
                f_uart          = mkUART
                                                                -- \end_latex{mkMMIO_f_uart}
              }

-- ================================================================
-- Tick devices, i.e., ``run'' their concurrent processes
-- Set mtip field if mtime reaches mtimecmp.
                                                                -- \begin_latex{mmio_tick}
mmio_tick :: MMIO -> MMIO
mmio_tick  mmio =
                                                                -- \end_latex{mmio_tick}
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
                                                                -- \begin_latex{mmio_has_interrupts}
mmio_has_interrupts :: MMIO -> (Bool, Bool, Bool)
mmio_has_interrupts  mmio =
  let
    eip  = uart_has_interrupt  (f_uart  mmio)
                                                                -- \end_latex{...mmio_has_interrupts}
    tip  = f_mtip  mmio
    sip  = ((f_msip  mmio) /= 0)
  in
    (eip, tip, sip)

-- ================================================================
-- Read data from MMIO
-- TODO: currently returns 'ok' with bogus value on most addrs; should return Mem_Result_Err

mmio_read :: MMIO -> InstrField -> Integer -> (Mem_Result, MMIO)
mmio_read  mmio  funct3  addr =
  -- MTIME
  if ((addr == addr_mtime) && (funct3 == funct3_LD)) then
    let
      mtime = f_mtime  mmio
    in
      (Mem_Result_Ok  mtime,  mmio)

  else if ((addr == addr_mtime) && (funct3 == funct3_LW)) then
    let
      mtime = (f_mtime  mmio) .&. 0xFFFFffff
    in
      (Mem_Result_Ok  mtime,  mmio)

  else if ((addr == (addr_mtime + 4)) && (funct3 == funct3_LW)) then
    let
      mtime = (shiftR  (f_mtime  mmio)  32)
    in
      (Mem_Result_Ok  mtime,  mmio)

  -- MTIMECMP
  else if ((addr == addr_mtimecmp) && (funct3 == funct3_LD)) then
    let
      mtimecmp = f_mtimecmp  mmio
    in
      (Mem_Result_Ok  mtimecmp,  mmio)

  else if ((addr == addr_mtimecmp) && (funct3 == funct3_LW)) then
    let
      mtimecmp = (f_mtimecmp  mmio) .&. 0xFFFFffff
    in
      (Mem_Result_Ok  mtimecmp,  mmio)

  else if ((addr == (addr_mtimecmp + 4)) && (funct3 == funct3_LW)) then
    let
      mtimecmp = (shiftR  (f_mtimecmp  mmio)  32)
    in
      (Mem_Result_Ok  mtimecmp,  mmio)

  -- MSIP
  else if (addr == addr_msip) then
    let
      msip = f_msip  mmio
    in
      (Mem_Result_Ok  msip,  mmio)

                                                                -- \begin_latex{mmio_uart_read}
  -- UART
  else if ((addr_base_UART <= addr) && (addr < (addr_base_UART + addr_size_UART))) then
    let
      uart       = f_uart  mmio
      (v, uart') = uart_read  uart  (addr - addr_base_UART)
      mmio'      = mmio { f_uart = uart' }
    in
      (Mem_Result_Ok  v,  mmio')
                                                                -- \end_latex{mmio_uart_read}
  -- UNKNOWN IO ADDR
  else
    (Mem_Result_Err  exc_code_load_access_fault,  mmio)

-- ================================================================
-- Write data into MMIO

mmio_write :: MMIO -> InstrField -> Integer -> Integer -> (Mem_Result, MMIO)
mmio_write  mmio  funct3  addr  val =
  -- MTIMECMP
  if ((addr == addr_mtimecmp) && (funct3 == funct3_SD)) then
    let
      mmio' = mmio { f_mtimecmp = val,
                     f_mtip     = False }
    in
      (Mem_Result_Ok  0,  mmio')

  else if ((addr == addr_mtimecmp) && (funct3 == funct3_SW)) then
    let
      mtimecmp  = f_mtimecmp  mmio
      lower32   = (val      .&. 0xFFFFffff)
      upper32   = (mtimecmp .&. 0xFFFFffff00000000)
      mmio' = mmio { f_mtimecmp = (upper32 .|. lower32),
                     f_mtip     = False }
    in
      (Mem_Result_Ok  0,  mmio')

  else if ((addr == addr_mtimecmp + 4) && (funct3 == funct3_SW)) then
    let
      mtimecmp  = f_mtimecmp  mmio
      lower32   = (mtimecmp .&. 0xFFFFffff)
      upper32   = (shiftL  (val .&. 0xFFFFffff)  32)
      mmio' = mmio { f_mtimecmp = (upper32 .|. lower32),
                     f_mtip     = False }
    in
      (Mem_Result_Ok  0,  mmio')

  -- MSIP
  else if (addr == addr_msip) then
    let
      mmio' = mmio { f_msip = val }
    in
      (Mem_Result_Ok  0,  mmio')

  -- HTIF CONSOLE OUT
  else if (addr == addr_htif_console_out) then
    mmio_write  mmio  funct3  (addr_base_UART + addr_UART_thr)  val
                                                                -- \begin_latex{mmio_uart_write}
  -- UART
  else if ((addr_base_UART <= addr) && (addr < (addr_base_UART + addr_size_UART))) then
    let
      uart  = f_uart  mmio
      uart' = uart_write  uart  (addr - addr_base_UART)  val
      mmio' = mmio {f_uart = uart'}
    in
      (Mem_Result_Ok  0, mmio')
                                                                -- \end_latex{mmio_uart_write}
  -- UNKNOWN IO ADDR
  else
    (Mem_Result_Err  exc_code_store_AMO_access_fault, mmio)

-- ================================================================
-- AMO op
-- TODO: find out if does LR also return STORE_AMO_ACCESS_FAULT or LOAD_ACCESS_FAULT?

mmio_amo :: MMIO       ->         -- memory-mapped io state
            Integer    ->         -- address
            InstrField ->         -- funct3: AMO_W or AMO_D
            InstrField ->         -- msbs5:  LR/SC/SWAP/ADD/AND/OR/XOR/MAX/MIN/MAXU/MINU
            InstrField ->         -- acquire
            InstrField ->         -- release
            Integer    ->         -- store-value
            (Mem_Result, MMIO)    -- memory-result (load-value or err), new memory-mapped io state

mmio_amo  mmio  addr  funct3  msbs5  aq  rl  store_val =
  if (not (is_AMO_aligned  funct3  addr)) then
    (Mem_Result_Err exc_code_store_AMO_addr_misaligned,  mmio)

  else if ((addr /= addr_msip) &&
           (addr /= addr_mtimecmp)) then
         (Mem_Result_Err exc_code_store_AMO_access_fault,  mmio)

  else
    let
      -- Get old values (omvs)    -- TODO: UART locations
      old_mem_val  = if (addr == addr_msip) then f_msip mmio
                     else if (addr == addr_mtimecmp) then f_mtimecmp  mmio
                          else if (addr == addr_mtime) then f_mtime  mmio
                               else 0

      -- Load-value (to be returned to CPU)
      load_val | (msbs5  == msbs5_AMO_SC) = 1    -- always fail (SC success = 0, SC failure = non-zero)
               | (funct3 == funct3_AMO_W) = sign_extend  32 64  old_mem_val
               | (funct3 == funct3_AMO_D) = old_mem_val

      -- New memory value (to be stored back)
      new_mem_val      = alu_amo_op  funct3  msbs5  store_val  old_mem_val

      -- Write new memory value back to memory
      mmio' | (msbs5 == msbs5_AMO_LR)  = mmio
            | (msbs5 == msbs5_AMO_SC)  = mmio
            | True                     = if (addr == addr_mtimecmp) then mmio {f_mtimecmp = new_mem_val,
                                                                               f_mtip     = False}
                                         else if (addr == addr_msip)
                                              then mmio {f_msip = new_mem_val}
                                              else mmio    -- TODO: UART addrs
    in
      (Mem_Result_Ok  load_val, mmio')

-- ================================================================
-- Convenience function to read MTIME

mmio_read_mtime :: MMIO -> Integer
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

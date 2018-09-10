-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Main_Test_Virtual_Mem where

-- ================================================================
-- This module is an alternative 'main' function
-- for testing translation of virtual addresses to physical addresses
-- It builds a hand-crafted page table in the memory of a machine state
-- and then runs a series of 'vm_translate' tests against it.

-- ================================================================
-- Standard Haskell imports

import Control.Monad
import Data.Maybe
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Map as Data_Map
import Numeric (showHex, readHex)

-- Project imports

import Bit_Manipulation
import Arch_Defs
import Mem_Ops
import Machine_State
import Virtual_Mem

-- ================================================================

main_test_virtual_mem :: IO ()
main_test_virtual_mem = do
  -- main_test_sv32
  main_test_sv39

-- ================================================================

main_test_sv32 :: IO ()
main_test_sv32 = do
  let initial_PC     = 0
      mem_base       = 0
      mem_size       = 0xFFFFFFFFFFFFFFFF
      addr_ranges    = [(mem_base, mem_base + mem_size)]
      addr_byte_list = []
      ms1 = mkMachine_State  RV32  initial_PC  addr_ranges  addr_byte_list
      ms2 = mstate_csr_write  ms1  csr_addr_satp  (mk_satp_rv32  sv32  0  0x80002000)
      sample_pt = (mk_sample_page_table  sv32)
      ms3 = load_sample_page_table  ms2  funct3_SW  sample_pt

  do_tests  ms3  sv32  tests_sv32

-- ================

mk_satp_rv32 :: Word64 -> Word64 -> Word64 -> Word64
mk_satp_rv32  mode  asid  pt_base_addr = ((shiftL  (mode .&. 0x1)    31) .|.
                                          (shiftL  (asid .&. 0x1FF)  22) .|.
                                          ((shiftR  pt_base_addr  12) .&. 0x3FFFFF))

-- ================

mk_sv32_va  :: Word64 -> Word64 -> Word64 -> Word64
mk_sv32_va  vpn1  vpn0  offset = ((shiftL  vpn1  22) .|.  (shiftL  vpn0  12) .|.  offset)

-- ================

tests_sv32 :: [ (Word64, Priv_Level, Bool, Bool, Word64, String) ]
tests_sv32 = [ (0, m_Priv_Level,  True,  True, (mk_sv32_va  0  0  0), "Invalid lev 1 PTE"),

               (0, m_Priv_Level,  True,  True, (mk_sv32_va  1  0  0), "Invalid lev 0 PTE"),

               (0, m_Priv_Level,  True,  True, (mk_sv32_va  1  1  0), "Fetch: X is 0"),
               (0, m_Priv_Level, False, False, (mk_sv32_va  1  1  0), "Store: W is 0"),
               (0, m_Priv_Level, False,  True, (mk_sv32_va  1  1  0), "Load: R is 1"),

               (0, m_Priv_Level,  True,  True, (mk_sv32_va  1  2  0), "Fetch: X is 0"),
               (0, m_Priv_Level, False, False, (mk_sv32_va  1  2  0), "Store: W is 1"),
               (0, m_Priv_Level, False,  True, (mk_sv32_va  1  2  0), "Load: R is 1"),

               (0, m_Priv_Level,  True,  True, (mk_sv32_va  1  3  0), "Fetch: X is 1"),
               (0, m_Priv_Level, False, False, (mk_sv32_va  1  3  0), "Store: W is 0"),
               (0, m_Priv_Level, False,  True, (mk_sv32_va  1  3  0), "Load: R is 0"),

               (0, m_Priv_Level,  True,  True, (mk_sv32_va  7  0  0), "Megapage: Fetch: X is 1"),
               (0, m_Priv_Level,  True,  True, (mk_sv32_va  7  0  4), "Megapage: Fetch: X is 1"),

               (0, m_Priv_Level,  True,  True, (mk_sv32_va  8  0  0), "Megapage: misaligned")
             ]

-- ================================================================
-- SV39 tests

main_test_sv39 :: IO ()
main_test_sv39 = do
  let initial_PC     = 0
      mem_base       = 0
      mem_size       = 0xFFFFFFFFFFFFFFFF
      addr_ranges    = [(mem_base, mem_base + mem_size)]
      addr_byte_list = []
      ms1 = mkMachine_State  RV64  initial_PC  addr_ranges  addr_byte_list
      ms2 = mstate_csr_write  ms1  csr_addr_satp  (mk_satp_rv64  sv39  0  0x80001000)
      sample_pt = (mk_sample_page_table  sv39)
      ms3 = load_sample_page_table  ms2  funct3_SD  sample_pt

  do_tests  ms3  sv39  tests_sv39

-- ================

mk_satp_rv64 :: Word64 -> Word64 -> Word64 -> Word64
mk_satp_rv64  mode  asid  pt_base_addr = ((shiftL  (mode .&. 0xF)  60) .|.
                                          (shiftL  (asid .&. 0xFFFF)  44) .|.
                                          ((shiftR  pt_base_addr  12) .&. 0xFFFFFFFFFFF))

-- ================

mk_sv39_va  :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
mk_sv39_va  vpn2  vpn1  vpn0  offset = ((shiftL  vpn2  30) .|.
                                        (shiftL  vpn1  21) .|.
                                        (shiftL  vpn0  12) .|.  offset)

-- ================

tests_sv39 :: [ (Word64, Priv_Level, Bool, Bool, Word64, String) ]
tests_sv39 = [ (0, m_Priv_Level,  True,  True, (mk_sv39_va  0  0  0  0), "Invalid lev 0 PTE"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  0  0  0), "Invalid lev 1 PTE"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  1  0  0), "Invalid lev 0 PTE"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  1  1  0), "Fetch: X is 0"),
               (0, m_Priv_Level, False, False, (mk_sv39_va  1  1  1  0), "Store: W is 0"),
               (0, m_Priv_Level, False,  True, (mk_sv39_va  1  1  1  0), "Load: R is 1"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  1  2  0), "Fetch: X is 0"),
               (0, m_Priv_Level, False, False, (mk_sv39_va  1  1  2  0), "Store: W is 1"),
               (0, m_Priv_Level, False,  True, (mk_sv39_va  1  1  2  0), "Load: R is 1"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  1  3  0), "Fetch: X is 1"),
               (0, m_Priv_Level, False, False, (mk_sv39_va  1  1  3  0), "Store: W is 0"),
               (0, m_Priv_Level, False,  True, (mk_sv39_va  1  1  3  0), "Load: R is 0"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  7  0  0), "Megapage: Fetch: X is 1"),
               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  7  0  4), "Megapage: Fetch: X is 1"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  1  8  0  0), "Megapage: misaligned"),

               (0, m_Priv_Level,  True,  True, (mk_sv39_va  5  0  0  0), "Gigapage: Fetch: X is 1"),
               (0, m_Priv_Level,  True,  True, (mk_sv39_va  5  0  0  4), "Gigapage: Fetch: X is 1"),
               (0, m_Priv_Level,  True,  True, (mk_sv39_va  6  0  0  0), "Gigapage: misaligned")
             ]

-- ================================================================

do_tests :: Machine_State -> Word64 -> [ (Word64, Priv_Level, Bool, Bool, Word64, String) ] -> IO ()
do_tests  mstate  sv  [] = return ()
do_tests  mstate  sv  (test:tests) = do
  let (mstatus, priv, is_instr, is_read, va, comment) = test
  putStrLn ("----------------------------------------------------------------")
  putStr ("Test: '" ++ comment ++ "'")
  putStr ("  mstatus " ++ showHex  mstatus  "")
  putStr (" priv " ++ show  priv)
  when (is_instr       && is_read)       (putStr " fetch")
  when ((not is_instr) && is_read)       (putStr " load")
  when ((not is_instr) && (not is_read)) (putStr " store")
  putStr (" va " ++ showHex  va  " = (")
  when (sv /= sv32) (putStr (showHex  (va_vpn_J   sv  va  2) "."))
  putStr (showHex  (va_vpn_J   sv  va  1) ".")
  putStr (showHex  (va_vpn_J   sv  va  0) ".")
  putStr (showHex  (va_offset  va)  ")")
  putStrLn ("")

  let ms_b               = mstate_priv_write  mstate  priv
      ms_c               = mstate_csr_write  ms_b  csr_addr_mstatus  mstatus
      (mem_result, ms_d) = vm_translate  ms_c  is_instr  is_read  va
  case mem_result of
    Mem_Result_Err  ec -> putStrLn ("Trap " ++ show_trap_exc_code  ec)
    Mem_Result_Ok   v  -> putStrLn ("Ok: 0x" ++ showHex  v  "")

  do_tests  ms_d  sv  tests

-- ================================================================
-- Load the machine state with a sample page table

load_sample_page_table :: Machine_State -> InstrField -> [(Word64, Word64)] -> Machine_State
load_sample_page_table  mstate  funct3  []          = mstate
load_sample_page_table  mstate  funct3  ((a,v):avs) =
  let
    (mem_result, mstate1) = mstate_mem_write  mstate  funct3  a  v
  in
    load_sample_page_table  mstate1  funct3  avs

-- ================
-- A hand-crafted sample page table

mk_sample_page_table :: Word64 -> [ (Word64, Word64) ]
mk_sample_page_table  sv =
  let
    pte_size_bytes | (sv == sv32) = 4
                   | (sv == sv39) = 8
                   | (sv == sv48) = 8

    mk_pte  addr uxwr = ((shiftL  (shiftR  addr 12)  10) .|.  (shiftL  uxwr  1) .|. 0xC1)

    mem_contents =
      [  -- leaf pages
        (0x11000, 0x11000),
        (0x12000, 0x12000),
        (0x13000, 0x13000),
        (0x14000, 0x14000),
        (0x15000, 0x15000),
        (0x16000, 0x16000),
        (0x17000, 0x17000),
        (0x18000, 0x18000),
        (0x19000, 0x19000),
        (0x1A000, 0x1A000),

        -- leaf megapage
        (0x400000, 0x400000),

        -- leaf gigapage
        (0x40000000, 0x40000000),

        -- level 0 PTE (for Sv32, Sv39, Sv48)
        (0x80003000 + (0  * pte_size_bytes), 0),                           -- invalid
        (0x80003000 + (1  * pte_size_bytes), (mk_pte  0x11000  0x1)),
        (0x80003000 + (2  * pte_size_bytes), (mk_pte  0x12000  0x3)),
        (0x80003000 + (3  * pte_size_bytes), (mk_pte  0x13000  0x4)),
        (0x80003000 + (4  * pte_size_bytes), (mk_pte  0x14000  0x5)),
        (0x80003000 + (5  * pte_size_bytes), (mk_pte  0x15000  0x7)),
        (0x80003000 + (6  * pte_size_bytes), (mk_pte  0x16000  0x9)),
        (0x80003000 + (7  * pte_size_bytes), (mk_pte  0x17000  0xB)),
        (0x80003000 + (8  * pte_size_bytes), (mk_pte  0x18000  0xC)),
        (0x80003000 + (9  * pte_size_bytes), (mk_pte  0x19000  0xD)),
        (0x80003000 + (10 * pte_size_bytes), (mk_pte  0x1A000  0xF)),
        (0x80003000 + (11 * pte_size_bytes), 0),                          -- invalid

        -- level 1 PTE (for Sv32, Sv39, Sv48)
        (0x80002000 + (0 * pte_size_bytes), 0),                            -- invalid
        (0x80002000 + (1 * pte_size_bytes), (mk_pte  0x80003000  0x0)),   -- non-leaf to PTN
        (0x80002000 + (2 * pte_size_bytes), 0),                            -- invalid
        (0x80002000 + (3 * pte_size_bytes), 0),                            -- invalid
        (0x80002000 + (4 * pte_size_bytes), 0),                            -- invalid
        (0x80002000 + (5 * pte_size_bytes), 0),                            -- invalid
        (0x80002000 + (6 * pte_size_bytes), 0),                            -- invalid
        (0x80002000 + (7 * pte_size_bytes), (mk_pte  0x400000    0x7)),    -- leaf megapage
        (0x80002000 + (8 * pte_size_bytes), (mk_pte  0x500000    0x7)),    -- leaf megapage misaligned
        (0x80002000 + (9 * pte_size_bytes), 0),                            -- invalid

        -- level 2 PTE (for Sv39, Sv48)
        (0x80001000 + (0 * pte_size_bytes), 0),                            -- invalid
        (0x80001000 + (1 * pte_size_bytes), (mk_pte  0x80002000  0x0)),    -- non-leaf to PTN
        (0x80001000 + (2 * pte_size_bytes), 0),                            -- invalid
        (0x80001000 + (3 * pte_size_bytes), 0),                            -- invalid
        (0x80001000 + (4 * pte_size_bytes), 0),                            -- invalid
        (0x80001000 + (5 * pte_size_bytes), (mk_pte  0x40000000  0xF)),    -- leaf gigapage
        (0x80001000 + (6 * pte_size_bytes), (mk_pte  0x50000000  0xF)),    -- leaf gigapage misaligned
        (0x80001000 + (7 * pte_size_bytes), 0)                             -- invalid

        -- TODO: level 3 PTE (for Sv48)
      ]
  in
    mem_contents

-- ================================================================

-- See LICENSE for license details

module Virtual_Mem where

-- ================================================================
-- This module defines the Sv32, Sv39 and Sv48 Virtual Memory schemes
-- for translating virtual addresses to physical addresses by
-- performing "page table walks" on the page table.

-- Abbreviations:
--    PT    Page Table
--    PTW   Page Table Walk
--    PTN   Page Table Node
--    PTE   Page Table Entry
--    VA    Virtual Address
--    PA    Physical Address
--    VPN   Virtual Page Number
--    PPN   Physical Page Number
--    ASID  Addres Space Id

-- Note: Most actual implementations have MMUs (Memory-Managment
-- Units) with TLBs (Translation Look-aside Buffers) but those are
-- implementation optimizations and implementation-specific; here we
-- simply do a full memory translation (page table walk) for each access.

-- ================================================================
-- Standard Haskell imports

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

-- ================================================================
-- Check if Virtual Memory is active or not               -- \begin_latex{fn_vm_is_active}

fn_vm_is_active :: Machine_State -> Bool -> Bool
fn_vm_is_active  mstate  is_instr =
  let                                                     -- \end_latex{fn_vm_is_active}
    rv      = mstate_rv_read  mstate

    satp              = mstate_csr_read   mstate  csr_addr_satp
    (satp_mode, _, _) = satp_fields  rv  satp

    -- Compute effective privilege modulo MSTATUS.MPRV
    priv    = mstate_priv_read  mstate
    mstatus = mstate_csr_read   mstate  csr_addr_mstatus
    mprv    = testBit  mstatus  mstatus_mprv_bitpos
    mpp     = trunc_u64_to_u32  ((shiftR  mstatus  mstatus_mpp_bitpos)  .&. 0x3)
    priv'   = if (mprv && (not  is_instr)) then mpp else priv

    vm_active | (rv == RV32) = ((priv' <= s_Priv_Level) && (satp_mode == sv32))
              | (rv == RV64) = ((priv' <= s_Priv_Level) && ((satp_mode == sv39) || (satp_mode == sv48)))
  in
    vm_active

-- ================================================================
-- This is the main function of this module.
-- It translates a virtual address into a physical address.
-- Notes:
--   - 'is_instr' is True if this is for an instruction-fetch as opposed to LOAD/STORE
--   - 'is_read'  is True for LOAD, False for STORE/AMO
--   - 1st component of tuple result is 'Mem_Result_Err exc_code' if there was a trap
--   -     and 'Mem_Result_Ok pa' if it successfully translated to a phys addr
--   - 2nd component of tuple result is new mem state,  potentially modified
--         (page table A D bits, cache tracking, TLB tracking, ...)               -- \begin_latex{vm_translate}

vm_translate :: Machine_State -> Bool  ->  Bool  -> Word64 -> (Mem_Result, Machine_State)
vm_translate    mstate           is_instr  is_read  va =
  let                                                                             -- \end_latex{vm_translate}
    -- Get relevant architecture state components
    rv      = mstate_rv_read    mstate
    priv    = mstate_priv_read  mstate
    mstatus = mstate_csr_read   mstate  csr_addr_mstatus

    -- Compute effective privilege modulo MSTATUS.MPRV
    mprv    = testBit  mstatus  mstatus_mprv_bitpos
    mpp     = trunc_u64_to_u32  ((shiftR  mstatus  mstatus_mpp_bitpos)  .&. 0x3)
    priv'   = if (mprv && (not  is_instr)) then mpp else priv

    -- If there is an access fault, the kind of access fault
    exc_code_access | is_instr = exc_code_instr_access_fault
                    | is_read  = exc_code_load_access_fault
                    | True     = exc_code_store_AMO_access_fault

    -- If there is a page fault, the kind of page fault
    exc_code_page_fault | is_instr = exc_code_Instruction_Page_Fault
                        | is_read  = exc_code_Load_Page_Fault
                        | True     = exc_code_Store_AMO_Page_Fault

    -- Memory access size code to load a PTE (4 byte words in SV32, 8 byte doublewords in SV39 and SV48)
    (funct3, pte_size_bytes) | (sv == sv32) = (funct3_LW, 4)
                             | (sv == sv39) = (funct3_LD, 8)
                             | (sv == sv48) = (funct3_LD, 8)

    -- Get SATP and its fields from arch state
    satp                    = mstate_csr_read   mstate  csr_addr_satp
    (sv, asid, pt_base_ppn) = satp_fields  rv  satp
    pt_base_addr            = (shiftL  pt_base_ppn  12)

    -- This function 'ptw' is the recursive Page Table Walk
    -- 'ptn_pa' is the address of a a Page Table Node at given 'level'
    ptw :: Machine_State -> Word64 -> Int ->  (Mem_Result, Machine_State)
    ptw    mstate           ptn_pa    level =
      let
        -- A PTE is indexed by VPN[J] in the PTN, i.e., PTN [VPN [J]]
        -- Compute byte addr of PTE (PTEs are 4 bytes in SV32, 8 bytes in SV32 and SV48)
        vpn_J  = va_vpn_J  sv  va  level
        pte_pa = ptn_pa + (vpn_J * pte_size_bytes)

        -- Load PTE from mem
        (mem_result, mstate1) = mstate_mem_read  mstate  exc_code_access  funct3  pte_pa
      in
        case mem_result of
          Mem_Result_Err  exc_code -> (mem_result, mstate1)
          Mem_Result_Ok   pte ->
            let
              -- Compute various PTE conditions
              is_leaf                = ((pte_R  pte) || (pte_X  pte))
              is_valid_pte           = pte_V  pte
              is_valid_pte_R_W       = ((pte_R  pte) || (not  (pte_W  pte)))

              -- Check PTE permission bits
              is_permitted           = fn_is_permitted  priv'  is_instr  is_read  mstatus  pte
              -- Check alignment of PTE.PPN for mega-, giga- and terapages
              is_misaligned_pte_ppn  = fn_is_misaligned_pte_ppn  sv  pte  level

              -- Check PTE.A and PTE.D bits.
              -- Currently: page-fault on PTE.A=0 or if is_write and PTE.D==0
              -- TODO: the other implementation option to update pte.A and pte.D
              is_bad_pte_A_D         = ((not (pte_A  pte)) || ((not  is_read) && (not (pte_D  pte))))
            in
              if ((not  is_valid_pte) || (not  is_valid_pte_R_W)) then
                (Mem_Result_Err  exc_code_page_fault, mstate1)

              else if (is_leaf && ((not is_permitted) || is_misaligned_pte_ppn || is_bad_pte_A_D)) then
                     (Mem_Result_Err  exc_code_page_fault, mstate1)

                   else if is_leaf then
                          let
                            -- Create final translated physical byte address
                            pa = mk_pa_in_page  sv  pte  va  level
                          in
                            (Mem_Result_Ok  pa,  mstate1)

                        else if (level == 0) then
                               -- Non-leaf at level 0
                               (Mem_Result_Err  exc_code_page_fault,  mstate1)

                             else
                               -- Non-leaf; do recursive ptw call to next level
                               ptw  mstate1  (mk_ptn_pa_from_pte  sv  pte)  (level - 1)

    -- Tree level at which the page-table walk starts
    start_level | (sv == sv32) = 1
                | (sv == sv39) = 2
                | (sv == sv48) = 3

    -- Invoke the recursive page table walk
    (mem_result, mstate1) = ptw  mstate  pt_base_addr  start_level
  in
    (mem_result, mstate1)

-- ================================================================
-- Supervisor Mode Virtual Memory modes

sv32 :: Word64;    sv32 = 1
sv39 :: Word64;    sv39 = 8
sv48 :: Word64;    sv48 = 9
-- sv57 :: Word64;    sv57 = 10    -- Future
-- sv64 :: Word64;    sv64 = 11    -- Future

-- ================================================================
-- Extract VPN [J] and OFFSET from a virtual address

va_vpn_J :: Word64 -> Word64 -> Int -> Word64
va_vpn_J    sv        va        level
  | (sv == sv32) && (level == 0) = bitSlice  va  21  12
  | (sv == sv32) && (level == 1) = bitSlice  va  31  22

  | (sv == sv39) && (level == 0) = bitSlice  va  20  12
  | (sv == sv39) && (level == 1) = bitSlice  va  29  21
  | (sv == sv39) && (level == 2) = bitSlice  va  38  30


  | (sv == sv48) && (level == 0) = bitSlice  va  20  12
  | (sv == sv48) && (level == 1) = bitSlice  va  29  21
  | (sv == sv48) && (level == 2) = bitSlice  va  38  30
  | (sv == sv48) && (level == 3) = bitSlice  va  47  39


va_offset :: Word64 -> Word64
va_offset  va = (va .&. 0xFFF)

-- ================================================================
-- Extract fields of SATP values (values in CSR SATP)

satp_fields :: RV -> Word64 -> (Word64, Word64, Word64)
satp_fields rv satp | (rv == RV32) = (let
                                         mode = bitSlice  satp  31  31
                                         asid = bitSlice  satp  30  22
                                         ppn  = bitSlice  satp  21   0
                                      in
                                        (mode, asid, ppn))
                    | (rv == RV64) = (let
                                         mode = bitSlice  satp  63  60
                                         asid = bitSlice  satp  59  44
                                         ppn  = bitSlice  satp  43   0
                                      in
                                        (mode, asid, ppn))

-- ================================================================
-- Extract fields of Page Table Entries

pte_D :: Word64 -> Bool;    pte_D  pte = testBit   pte  7
pte_A :: Word64 -> Bool;    pte_A  pte = testBit   pte  6
pte_G :: Word64 -> Bool;    pte_G  pte = testBit   pte  5
pte_U :: Word64 -> Bool;    pte_U  pte = testBit   pte  4
pte_X :: Word64 -> Bool;    pte_X  pte = testBit   pte  3
pte_W :: Word64 -> Bool;    pte_W  pte = testBit   pte  2
pte_R :: Word64 -> Bool;    pte_R  pte = testBit   pte  1
pte_V :: Word64 -> Bool;    pte_V  pte = testBit   pte  0

{- DELETE
pte_ppn :: Word64 -> Word64 -> Word64
pte_ppn  sv  pte | (sv == sv32) = bitSlice  pte  31  10
                 | (sv == sv39) = bitSlice  pte  53  10
                 | (sv == sv48) = bitSlice  pte  53  10
-}

pte_ppn_J :: Word64 -> Word64 -> Int -> Word64
pte_ppn_J  sv  pte  0 | (sv == sv32) = bitSlice  pte  19  10
                      | (sv == sv39) = bitSlice  pte  18  10
                      | (sv == sv48) = bitSlice  pte  18  10

pte_ppn_J  sv  pte  1 | (sv == sv32) = bitSlice  pte  31  20
                      | (sv == sv39) = bitSlice  pte  27  19
                      | (sv == sv48) = bitSlice  pte  27  19

pte_ppn_J  sv  pte  2 | (sv == sv32) = 0
                      | (sv == sv39) = bitSlice  pte  53  28
                      | (sv == sv48) = bitSlice  pte  36  28

pte_ppn_J  sv  pte  3 | (sv == sv32) = 0
                      | (sv == sv39) = 0
                      | (sv == sv48) = bitSlice  pte  53  37

-- ================================================================
-- Checks if PTE's U,X,W,R permission bits allow the access
-- based on type of access, current privilege, and MSTATUS.SUM and MSTATUS.MXR

fn_is_permitted :: Priv_Level -> Bool -> Bool -> Word64 -> Word64 -> Bool
fn_is_permitted  priv  is_instr  is_read  mstatus  pte =
  let
    mstatus_mxr = testBit  mstatus  mstatus_mxr_bitpos
    mstatus_sum = testBit  mstatus  mstatus_sum_bitpos

    -- User privilege can access page only if PTE.U is 1
    -- Supervisor privilege can access pages with PTE.U == 1 only if MSTATUS.SUM is set
    priv_ok | (priv == u_Priv_Level)                   = (pte_U  pte)
            | ((priv == s_Priv_Level) && (pte_U  pte)) = mstatus_sum
            | True                                     = True

    -- Make Executable Readable: when MSTATUS.MXR=1, can do data-reads from pages with PTE.R or PTE.X set
    r_mxr = ((pte_R  pte) || ((pte_X  pte) && mstatus_mxr))

    access_ok = ((is_instr && is_read && (pte_X  pte))                    -- instruction fetch
                 || ((not is_instr) && is_read && r_mxr)                  -- data load
                 || ((not is_instr) && (not is_read) && (pte_W  pte)))    -- data store
  in
    (priv_ok && access_ok)

-- ================================================================
-- For a leaf PTE not at level 0, i.e., at:
--    level 1 (megapage): PTE.PPN[0]                         must be zero
--    level 2 (gigapage): PTE.PPN[0], PTE.PPN[1]             must be zero
--    level 3 (terapage): PTE.PPN[0], PTE.PPN[1], PTE.PPN[2] must be zero
-- else misaligned

fn_is_misaligned_pte_ppn :: Word64 -> Word64 -> Int -> Bool
fn_is_misaligned_pte_ppn  sv  pte  leaf_level =
  if      ((leaf_level >= 1) && ((pte_ppn_J  sv  pte  0) /= 0)) then True
  else if ((leaf_level >= 2) && ((pte_ppn_J  sv  pte  1) /= 0)) then True
  else if ((leaf_level >= 3) && ((pte_ppn_J  sv  pte  2) /= 0)) then True
  else False

-- ================================================================

mk_pa_in_page :: Word64 -> Word64 -> Word64 -> Int -> Word64
mk_pa_in_page    sv        pte       va        level =
  let
    pte_ppn_3 = pte_ppn_J  sv  pte  3    -- irrelevant for sv32, sv39
    pte_ppn_2 = pte_ppn_J  sv  pte  2    -- irrelevant for sv32
    pte_ppn_1 = pte_ppn_J  sv  pte  1
    pte_ppn_0 = pte_ppn_J  sv  pte  0

    va_vpn_3  = va_vpn_J  sv  va   3    -- irrelevant for sv32, sv39
    va_vpn_2  = va_vpn_J  sv  va   2    -- irrelevant for sv32
    va_vpn_1  = va_vpn_J  sv  va   1
    va_vpn_0  = va_vpn_J  sv  va   0

    offset    = va_offset  va

    pa | (sv == sv32) && (level == 1) = ((shiftL  pte_ppn_1  22) .|.
                                         (shiftL  va_vpn_0   12) .|. offset)

       | (sv == sv32) && (level == 0) = ((shiftL  pte_ppn_1  22) .|.
                                         (shiftL  pte_ppn_0  12) .|. offset)

       | (sv == sv39) && (level == 2) = ((shiftL  pte_ppn_2  30) .|.
                                         (shiftL  va_vpn_1   21) .|.
                                         (shiftL  va_vpn_0   12) .|. offset)

       | (sv == sv39) && (level == 1) = ((shiftL  pte_ppn_2  30) .|.
                                         (shiftL  pte_ppn_1  21) .|.
                                         (shiftL  va_vpn_0   12) .|. offset)

       | (sv == sv39) && (level == 0) = ((shiftL  pte_ppn_2  30) .|.
                                         (shiftL  pte_ppn_1  21) .|.
                                         (shiftL  pte_ppn_0  12) .|. offset)

       | (sv == sv48) && (level == 3) = ((shiftL  pte_ppn_3  39) .|.
                                         (shiftL  va_vpn_2   30) .|.
                                         (shiftL  va_vpn_1   21) .|.
                                         (shiftL  va_vpn_0   12) .|. offset)

       | (sv == sv48) && (level == 2) = ((shiftL  pte_ppn_3  39) .|.
                                         (shiftL  pte_ppn_2  30) .|.
                                         (shiftL  va_vpn_1   21) .|.
                                         (shiftL  va_vpn_0   12) .|. offset)

       | (sv == sv48) && (level == 1) = ((shiftL  pte_ppn_3  39) .|.
                                         (shiftL  pte_ppn_2  30) .|.
                                         (shiftL  pte_ppn_1  21) .|.
                                         (shiftL  va_vpn_0   12) .|. offset)

       | (sv == sv48) && (level == 0) = ((shiftL  pte_ppn_3  39) .|.
                                         (shiftL  pte_ppn_2  30) .|.
                                         (shiftL  pte_ppn_1  21) .|.
                                         (shiftL  pte_ppn_0  12) .|. offset)
  in
    pa

-- ================================================================
-- Make phys addr of PTN pointed at by a leaf PTE

mk_ptn_pa_from_pte :: Word64 -> Word64 -> Word64
mk_ptn_pa_from_pte    sv        pte =
  let
    pte_ppn_3 = pte_ppn_J  sv  pte  3    -- irrelevant for sv32, sv39
    pte_ppn_2 = pte_ppn_J  sv  pte  2    -- irrelevant for sv32
    pte_ppn_1 = pte_ppn_J  sv  pte  1
    pte_ppn_0 = pte_ppn_J  sv  pte  0

    offset    = 0

    pa | (sv == sv32) = ((shiftL  pte_ppn_1  22) .|.
                         (shiftL  pte_ppn_0  12) .|. offset)

       | (sv == sv39) = ((shiftL  pte_ppn_2  30) .|.
                         (shiftL  pte_ppn_1  21) .|.
                         (shiftL  pte_ppn_0  12) .|. offset)

       | (sv == sv48) = ((shiftL  pte_ppn_3  39) .|.
                         (shiftL  pte_ppn_2  30) .|.
                         (shiftL  pte_ppn_1  21) .|.
                         (shiftL  pte_ppn_0  12) .|. offset)
  in
    pa

-- ================================================================

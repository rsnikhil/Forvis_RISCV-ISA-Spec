-- See LICENSE for license details

module Forvis_Spec where

-- ================================================================
-- Specification of all RISC-V instructions.

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.
import Data.Word    -- For Wordxx type (unsigned fixed-width ints)
import Data.Int     -- For Intxx type (signed fixed-width ints)

-- Local imports

import Bit_Manipulation
import Arch_Defs
import Machine_State
import CSR_File
import Virtual_Mem

-- ================================================================ \begin_latex{instr_fetch}
-- Instruction fetch
-- This function attempts an insruction fetch based on the current PC.

-- It first attempts to read 2 bytes only, in case the next
-- instruction is a 'C' (compressed) instruction. This may trap; if
-- not, we can decide if it's a C instruction, and read the next 2
-- bytes if it is not a C instruction; this, too, may trap.

data Fetch_Result = Fetch_Trap  Exc_Code
                  | Fetch_C     Word16
                  | Fetch       Word32
                  deriving (Show)

instr_fetch :: Machine_State -> (Fetch_Result, Machine_State)
instr_fetch  mstate =
  let                                                              -- \end_latex{instr_fetch}
    rv                = mstate_rv_read  mstate
    pc | (rv == RV32) = (mstate_pc_read  mstate .&. 0xFFFFFFFF)
       | (rv == RV64) = mstate_pc_read  mstate

    -- Read 2 instr bytes
    -- with virtual-to-physical translation if necessary.
    (result1, mstate1) = read_2_instr_bytes  mstate  pc
  in
    case result1 of
      Mem_Result_Err  exc_code -> (let
                                      tval    = pc
                                      mstate2 = finish_trap  mstate1  exc_code  tval
                                   in
                                      (Fetch_Trap  exc_code, mstate2))

      Mem_Result_Ok   u64_lo ->
        (let
            u16_lo = trunc_u64_to_u16  u64_lo
         in
            if is_instr_C  u16_lo then
              -- Is a 'C' instruction; done
              (Fetch_C  u16_lo, mstate1)
            else
              (let
                  -- Not a 'C' instruction; read remaining 2 instr bytes
                  -- with virtual-to-physical translation if necessary.
                  -- Note: pc and pc+2 may translate to non-contiguous pages.
                  (result2, mstate2) = read_2_instr_bytes  mstate  (pc + 2)
               in
                  case result2 of
                    Mem_Result_Err  exc_code -> (let
                                                    tval = pc + 2
                                                    mstate3 = finish_trap  mstate2  exc_code  tval
                                                 in
                                                   (Fetch_Trap  exc_code, mstate3))
                    Mem_Result_Ok  u64_hi    -> (let
                                                    u16_hi = trunc_u64_to_u16  u64_hi
                                                    u32    = concat_u16_u16_to_u32  u16_lo  u16_hi
                                                 in
                                                   (Fetch  u32, mstate2))))

read_2_instr_bytes :: Machine_State -> Word64 -> (Mem_Result, Machine_State)
read_2_instr_bytes  mstate  va =
  let
    is_instr = True
    is_read  = True

    --     If Virtual Mem is active, translate pc to a physical addr
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  va
                         else
                           (Mem_Result_Ok  va, mstate)

    --     If no trap due to Virtual Mem translation, read 2 bytes from memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   pa ->
                             mstate_mem_read   mstate1  exc_code_instr_access_fault  funct3_LH  pa
  in
    (result2, mstate2)

-- ================================================================
-- LUI

opcode_LUI :: InstrField
opcode_LUI = 0x37    -- 7'b_01_101_11

spec_LUI :: Machine_State -> Instr -> (Bool, Machine_State)
spec_LUI    mstate       instr =
  let
    -- Instr fields: U-type
    (imm20, rd, opcode) = ifields_U_type  instr

    -- Decode check
    is_legal = (opcode == opcode_LUI)

    -- Semantics
    x_u32  = shiftL  imm20  12
    rd_val = signExtend_u32_to_u64  x_u32

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- AUIPC

opcode_AUIPC :: InstrField
opcode_AUIPC = 0x17    -- 7'b_00_101_11

spec_AUIPC :: Machine_State -> Instr -> (Bool, Machine_State)
spec_AUIPC    mstate       instr =
  let
    -- Instr fields: U-type
    (imm20, rd, opcode) = ifields_U_type  instr

    -- Decode check
    is_legal = (opcode == opcode_AUIPC)

    -- Semantics
    pc      = mstate_pc_read  mstate
    x_u32   = shiftL  imm20  12
    x_u64   = signExtend_u32_to_u64  x_u32
    rd_val  = cvt_s64_to_u64 ((cvt_u64_to_s64  x_u64) + (cvt_u64_to_s64  pc))

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- JAL

opcode_JAL :: InstrField
opcode_JAL = 0x6F    -- 7'b_11_011_11

spec_JAL :: Machine_State -> Instr -> (Bool, Machine_State)
spec_JAL    mstate       instr =
  let
    -- Instr fields: J-type
    (imm20, rd, opcode) = ifields_J_type  instr

    -- Decode check
    is_legal = (opcode == opcode_JAL)

    -- Semantics
    pc     = mstate_pc_read  mstate
    rd_val = pc + 4

    x_u64   = zeroExtend_u32_to_u64  imm20
    y_u64   = shiftL  x_u64  1                -- offset imm20 is in multiples of 2 bytes
    z_u64   = signExtend  y_u64  21           -- sign-extend 21 lsbs
    new_pc  = cvt_s64_to_u64 ((cvt_u64_to_s64  z_u64) + (cvt_u64_to_s64  pc))
    aligned = ((new_pc .&. 0x3) == 0)

    mstate1 = if aligned
              then
                finish_rd_and_pc  mstate  rd  rd_val  new_pc
              else
                finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc
  in
    (is_legal, mstate1)

-- ================================================================
-- JALR

opcode_JALR :: InstrField
opcode_JALR = 0x67    -- 7'b_11_001_11

spec_JALR :: Machine_State -> Instr -> (Bool, Machine_State)
spec_JALR    mstate       instr =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    is_legal = (opcode == opcode_JALR)

    -- Semantics
    pc      = mstate_pc_read  mstate
    rd_val  = pc + 4

    x_u64   = zeroExtend_u32_to_u64  imm12
    y_u64   = signExtend  x_u64  12           -- sign-extend 12 lsbs

    rs1_val = mstate_gpr_read  mstate  rs1

    new_pc  = cvt_s64_to_u64 ((cvt_u64_to_s64  y_u64) + (cvt_u64_to_s64  rs1_val))
    new_pc' = clear_bit  new_pc  0
    aligned = ((new_pc' .&. 0x3) == 0)

    mstate1 = if aligned
              then
                finish_rd_and_pc  mstate  rd  rd_val  new_pc'
              else
                finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc'
  in
    (is_legal, mstate1)

-- ================================================================
-- BRANCH: BEQ, BNE, BLT, BGE, BLTU, BGEU

opcode_BRANCH :: InstrField
opcode_BRANCH = 0x63    -- 7'b_11_000_11

funct3_BEQ  :: InstrField;    funct3_BEQ  = 0x0     -- 3'b_000
funct3_BNE  :: InstrField;    funct3_BNE  = 0x1     -- 3'b_001
funct3_BLT  :: InstrField;    funct3_BLT  = 0x4     -- 3'b_100
funct3_BGE  :: InstrField;    funct3_BGE  = 0x5     -- 3'b_101
funct3_BLTU :: InstrField;    funct3_BLTU = 0x6     -- 3'b_110
funct3_BGEU :: InstrField;    funct3_BGEU = 0x7     -- 3'b_111

spec_BRANCH :: Machine_State -> Instr -> (Bool, Machine_State)
spec_BRANCH    mstate       instr =
  let
    -- Instr fields: B-type
    (imm12, rs2, rs1, funct3, opcode) = ifields_B_type  instr

    -- Decode check
    is_BEQ   = (funct3 == funct3_BEQ)
    is_BNE   = (funct3 == funct3_BNE)
    is_BLT   = (funct3 == funct3_BLT)
    is_BGE   = (funct3 == funct3_BGE)
    is_BLTU  = (funct3 == funct3_BLTU)
    is_BGEU  = (funct3 == funct3_BGEU)
    is_legal = ((opcode == opcode_BRANCH)
                && (is_BEQ
                    || is_BNE
                    || is_BLT
                    || is_BGE
                    || is_BLTU
                    || is_BGEU))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2

    taken | is_BEQ  = (rs1_val == rs2_val)
          | is_BNE  = (rs1_val /= rs2_val)
          | is_BLT  = (cvt_u64_to_s64 (rs1_val) <  cvt_u64_to_s64 (rs2_val))
          | is_BGE  = (cvt_u64_to_s64 (rs1_val) >= cvt_u64_to_s64 (rs2_val))
          | is_BLTU = (rs1_val <  rs2_val)
          | is_BGEU = (rs1_val >= rs2_val)

    pc      = mstate_pc_read  mstate

    x_u64   = zeroExtend_u32_to_u64  imm12
    y_u64   = shiftL  x_u64  1                -- offset imm12 is in multiples of 2 bytes
    z_u64   = signExtend  y_u64  13           -- sign-extend 13 lsbs

    target  = cvt_s64_to_u64 ((cvt_u64_to_s64  pc) + (cvt_u64_to_s64  z_u64))
    new_pc  = if taken then target else pc + 4
    misa    = mstate_csr_read  mstate  csr_addr_misa
    -- new_pc[0] known to be 0, new_pc[1] must be 0 if 'C' is not supported
    aligned = (misa_flag  misa  'C' ||  (new_pc .&. 0x2 == 0))
          
    mstate1 = if aligned
              then
                finish_pc  mstate  new_pc
              else
                finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc
  in
    (is_legal, mstate1)

-- ================================================================
-- LOAD:
--    RV32: LB, LH, LW, LBU, LHU
--    RV64: LWU, LD

opcode_LOAD :: InstrField
opcode_LOAD = 0x03    -- 7'b_00_000_11

-- Note: these are duplicates of defs in Mem_Ops.hs
funct3_LB  :: InstrField;    funct3_LB  = 0x0     -- 3'b_000
funct3_LH  :: InstrField;    funct3_LH  = 0x1     -- 3'b_001
funct3_LW  :: InstrField;    funct3_LW  = 0x2     -- 3'b_010
funct3_LD  :: InstrField;    funct3_LD  = 0x3     -- 3'b_011
funct3_LBU :: InstrField;    funct3_LBU = 0x4     -- 3'b_100
funct3_LHU :: InstrField;    funct3_LHU = 0x5     -- 3'b_101
funct3_LWU :: InstrField;    funct3_LWU = 0x6     -- 3'b_110

spec_LOAD :: Machine_State -> Instr -> (Bool, Machine_State)
spec_LOAD    mstate       instr =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    rv       = mstate_rv_read  mstate
    is_LB    = (funct3 == funct3_LB)
    is_LH    = (funct3 == funct3_LH)
    is_LW    = (funct3 == funct3_LW)
    is_LD    = (funct3 == funct3_LD)
    is_LBU   = (funct3 == funct3_LBU)
    is_LHU   = (funct3 == funct3_LHU)
    is_LWU   = (funct3 == funct3_LWU)
    is_legal = ((opcode == opcode_LOAD)
                && (is_LB
                    || is_LH
                    || is_LW
                    || (is_LD && (rv == RV64))
                    || is_LBU
                    || is_LHU
                    || (is_LWU && (rv == RV64))))
    -- Semantics
    --     Compute effective address
    rs1_val = mstate_gpr_read  mstate  rs1
    x_u64   = zeroExtend_u32_to_u64  imm12
    y_u64   = signExtend  x_u64  12           -- sign-extend 12 lsbs
    eaddr1  = cvt_s64_to_u64 ((cvt_u64_to_s64  y_u64) + (cvt_u64_to_s64  rs1_val))
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    --     If Virtual Mem is active, translate to a physical addr
    is_instr = False
    is_read  = True
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  eaddr2
                         else
                           (Mem_Result_Ok  eaddr2, mstate)

    --     If no trap due to Virtual Mem translation, read from memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   eaddr2_pa ->
                             mstate_mem_read   mstate1  exc_code_load_access_fault  funct3  eaddr2_pa

    --     Finally: finish with trap, or finish with loading Rd with load-value
    mstate3 = case result2 of
                Mem_Result_Err exc_code ->
                  finish_trap  mstate2  exc_code  eaddr2

                Mem_Result_Ok  d_u64    ->
                  let rd_val | is_LB = signExtend  d_u64  8
                             | is_LH = signExtend  d_u64  16
                             | is_LW = signExtend  d_u64  32
                             | True  = d_u64
                  in
                    finish_rd_and_pc_plus_4  mstate2  rd  rd_val
  in
    (is_legal, mstate3)

-- ================================================================
-- STORE:
--    RV32: SB, SH, SW
--    RV64: SD

opcode_STORE :: InstrField
opcode_STORE = 0x23    -- 7'b_01_000_11

-- Note: these are duplicates of defs in Mem_Ops.hs
funct3_SB :: InstrField;    funct3_SB = 0x0     -- 3'b_000
funct3_SH :: InstrField;    funct3_SH = 0x1     -- 3'b_001
funct3_SW :: InstrField;    funct3_SW = 0x2     -- 3'b_010
funct3_SD :: InstrField;    funct3_SD = 0x3     -- 3'b_011

spec_STORE :: Machine_State -> Instr -> (Bool, Machine_State)
spec_STORE    mstate       instr =
  let
    -- Instr fields: S-type
    (imm12, rs2, rs1, funct3, opcode) = ifields_S_type  instr

    -- Decode check
    rv       = mstate_rv_read  mstate
    is_SB    = (funct3 == funct3_SB)
    is_SH    = (funct3 == funct3_SH)
    is_SW    = (funct3 == funct3_SW)
    is_SD    = ((funct3 == funct3_SD) && (rv == RV64))
    is_legal = ((opcode == opcode_STORE)
                && (is_SB
                    || is_SH
                    || is_SW
                    || is_SD))

    -- Semantics
    rs2_val = mstate_gpr_read  mstate  rs2    -- store value

    --     Compute effective address
    rs1_val = mstate_gpr_read  mstate  rs1    -- address base
    x_u64   = zeroExtend_u32_to_u64  imm12
    y_u64   = signExtend  x_u64  12           -- sign-extend 12 lsbs
    eaddr1  = cvt_s64_to_u64 ((cvt_u64_to_s64  rs1_val) + (cvt_u64_to_s64  y_u64))
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    --     If Virtual Mem is active, translate to a physical addr
    is_instr = False
    is_read  = False
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  eaddr2
                         else
                           (Mem_Result_Ok  eaddr2, mstate)

    --     If no trap due to Virtual Mem translation, store to memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   eaddr2_pa ->
                             mstate_mem_write   mstate1  funct3  eaddr2_pa  rs2_val

    --     Finally: finish with trap, or finish with fall-through
    mstate3 = case result2 of
                Mem_Result_Err exc_code -> finish_trap  mstate2  exc_code  eaddr2
                Mem_Result_Ok  _        -> finish_pc_plus_4  mstate2
  in
    (is_legal, mstate3)

-- ================================================================
-- OP_IMM: ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI

opcode_OP_IMM :: InstrField
opcode_OP_IMM = 0x13    -- 7'b_00_100_11

funct3_ADDI  :: InstrField;    funct3_ADDI  = 0x0      -- 3'b_000
funct3_SLTI  :: InstrField;    funct3_SLTI  = 0x2      -- 3'b_010
funct3_SLTIU :: InstrField;    funct3_SLTIU = 0x3      -- 3'b_011
funct3_XORI  :: InstrField;    funct3_XORI  = 0x4      -- 3'b_100
funct3_ORI   :: InstrField;    funct3_ORI   = 0x6      -- 3'b_110
funct3_ANDI  :: InstrField;    funct3_ANDI  = 0x7      -- 3'b_111
funct3_SLLI  :: InstrField;    funct3_SLLI  = 0x1      -- 3'b_001
funct3_SRLI  :: InstrField;    funct3_SRLI  = 0x5      -- 3'b_101
funct3_SRAI  :: InstrField;    funct3_SRAI  = 0x5      -- 3'b_101

-- OP_IMM.SLLI/SRLI/SRAI for RV64
msbs7_SLLI  :: InstrField;     msbs7_SLLI  = 0x00     -- 7'b_0000000
msbs7_SRLI  :: InstrField;     msbs7_SRLI  = 0x00     -- 7'b_0000000
msbs7_SRAI  :: InstrField;     msbs7_SRAI  = 0x20     -- 7'b_0100000

-- OP_IMM.SLLI/SRLI/SRAI for RV64
msbs6_SLLI  :: InstrField;     msbs6_SLLI  = 0x00     -- 6'b_000000
msbs6_SRLI  :: InstrField;     msbs6_SRLI  = 0x00     -- 6'b_000000
msbs6_SRAI  :: InstrField;     msbs6_SRAI  = 0x10     -- 6'b_010000


spec_OP_IMM :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_IMM    mstate       instr =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr
    (msbs7, shamt5) = i_imm12_fields_7_5  imm12
    (msbs6, shamt6) = i_imm12_fields_6_6  imm12

    -- Decode check
    rv       = mstate_rv_read  mstate
    is_ADDI  = (funct3 == funct3_ADDI)
    is_SLTI  = (funct3 == funct3_SLTI)
    is_SLTIU = (funct3 == funct3_SLTIU)
    is_XORI  = (funct3 == funct3_XORI)
    is_ORI   = (funct3 == funct3_ORI)
    is_ANDI  = (funct3 == funct3_ANDI)
    is_SLLI  = ((funct3 == funct3_SLLI) && ((   (rv == RV32) && (msbs7 == msbs7_SLLI))
                                            || ((rv == RV64) && (msbs6 == msbs6_SLLI))))
    is_SRLI  = ((funct3 == funct3_SRLI) && ((   (rv == RV32) && (msbs7 == msbs7_SRLI))
                                            || ((rv == RV64) && (msbs6 == msbs6_SRLI))))
    is_SRAI  = ((funct3 == funct3_SRAI) && ((   (rv == RV32) && (msbs7 == msbs7_SRAI))
                                            || ((rv == RV64) && (msbs6 == msbs6_SRAI))))

    is_legal = ((opcode == opcode_OP_IMM)
                && (is_ADDI
                    || is_SLTI
                    || is_SLTIU
                    || is_XORI
                    || is_ORI
                    || is_ANDI
                    || is_SLLI
                    || is_SRLI
                    || is_SRAI
                   ))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1

    v2_u64  = signExtend  (zeroExtend_u32_to_u64  imm12)  12

    shamt   = cvt_u32_to_Int (if (rv == RV32) then shamt5 else shamt6)

    rd_val | is_ADDI  = cvt_s64_to_u64  ((cvt_u64_to_s64  rs1_val) + (cvt_u64_to_s64  v2_u64))
           | is_SLTI  = if (cvt_u64_to_s64  rs1_val) < (cvt_u64_to_s64  v2_u64) then 1 else 0
           | is_SLTIU = if rs1_val < v2_u64 then 1 else 0
           | is_XORI  = xor  rs1_val  v2_u64
           | is_ORI   = rs1_val  .|.  v2_u64
           | is_ANDI  = rs1_val  .&.  v2_u64
           | is_SLLI  = shiftL  rs1_val  shamt
           | is_SRLI  = (let
                            v1 = if (rv == RV32) then (rs1_val .&. 0xffffFFFF) else rs1_val
                          in
                            shiftR  v1  shamt)
           | is_SRAI  = cvt_s64_to_u64  (shiftR  (cvt_u64_to_s64  rs1_val)  shamt)

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP: ADD, SUB, SLT, SLTU, XOR, OR, AND, SLL, SRL, SRA

opcode_OP :: InstrField
opcode_OP = 0x33    -- 7'b_01_100_11

funct3_ADD  :: InstrField;    funct3_ADD  = 0x0     -- 3'b_000
funct7_ADD  :: InstrField;    funct7_ADD  = 0x00    -- 7'b_000_0000

funct3_SUB  :: InstrField;    funct3_SUB  = 0x0     -- 3'b_000
funct7_SUB  :: InstrField;    funct7_SUB  = 0x20    -- 7'b_010_0000

funct3_SLT  :: InstrField;    funct3_SLT  = 0x2     -- 3'b_010
funct7_SLT  :: InstrField;    funct7_SLT  = 0x00    -- 7'b_000_0000

funct3_SLTU :: InstrField;    funct3_SLTU = 0x3     -- 3'b_011
funct7_SLTU :: InstrField;    funct7_SLTU = 0x00    -- 7'b_000_0000

funct3_XOR  :: InstrField;    funct3_XOR  = 0x4     -- 3'b_100
funct7_XOR  :: InstrField;    funct7_XOR  = 0x00    -- 7'b_000_0000

funct3_OR   :: InstrField;    funct3_OR   = 0x6     -- 3'b_110
funct7_OR   :: InstrField;    funct7_OR   = 0x00    -- 7'b_000_0000

funct3_AND  :: InstrField;    funct3_AND  = 0x7     -- 3'b_111
funct7_AND  :: InstrField;    funct7_AND  = 0x00    -- 7'b_000_0000

funct3_SLL  :: InstrField;    funct3_SLL  = 0x1     -- 3'b_001
funct7_SLL  :: InstrField;    funct7_SLL  = 0x00    -- 7'b_000_0000

funct3_SRL  :: InstrField;    funct3_SRL  = 0x5     -- 3'b_101
funct7_SRL  :: InstrField;    funct7_SRL  = 0x00    -- 7'b_000_0000

funct3_SRA  :: InstrField;    funct3_SRA  = 0x5     -- 3'b_101
funct7_SRA  :: InstrField;    funct7_SRA  = 0x20    -- 7'b_010_0000

                                                                    -- \begin_latex{spec_ADD_1}
spec_OP :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP    mstate       instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    is_ADD   = ((funct3 == funct3_ADD)  && (funct7 == funct7_ADD))
    is_SUB   = ((funct3 == funct3_SUB)  && (funct7 == funct7_SUB))
                                                                    -- \end_latex{spec_ADD_1}
    is_SLT   = ((funct3 == funct3_SLT)  && (funct7 == funct7_SLT))
    is_SLTU  = ((funct3 == funct3_SLTU) && (funct7 == funct7_SLTU))
    is_XOR   = ((funct3 == funct3_XOR)  && (funct7 == funct7_XOR))
    is_OR    = ((funct3 == funct3_OR)   && (funct7 == funct7_OR))
    is_AND   = ((funct3 == funct3_AND)  && (funct7 == funct7_AND))
    is_SLL   = ((funct3 == funct3_SLL)  && (funct7 == funct7_SLL))
    is_SRL   = ((funct3 == funct3_SRL)  && (funct7 == funct7_SRL))
    is_SRA   = ((funct3 == funct3_SRA)  && (funct7 == funct7_SRA))  -- \begin_latex{spec_ADD_2}
    is_legal = ((opcode == opcode_OP)
                && (is_ADD
                    || is_SUB
                    || is_SLT                                       -- \end_latex{spec_ADD_2}
                    || is_SLTU
                    || is_XOR
                    || is_OR
                    || is_AND
                    || is_SLL
                    || is_SRL
                    || is_SRA
                   ))
                                                                    -- \begin_latex{spec_ADD_3}
    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2
    rv      = mstate_rv_read   mstate                               -- \end_latex{spec_ADD_3}

    shamt   = cvt_u64_to_Int (if (rv == RV32) then (rs2_val .&. 0x1F) else (rs2_val .&. 0x3F))

                                                                    -- \begin_latex{spec_ADD_4}
    rd_val | is_ADD  = cvt_s64_to_u64  ((cvt_u64_to_s64  rs1_val) + (cvt_u64_to_s64  rs2_val))
           | is_SUB  = cvt_s64_to_u64  ((cvt_u64_to_s64  rs1_val) - (cvt_u64_to_s64  rs2_val))
           | is_SLT  = if ((cvt_u64_to_s64  rs1_val) < (cvt_u64_to_s64  rs2_val)) then 1 else 0
           | is_SLTU = if (rs1_val < rs2_val) then 1 else 0
                                                                    -- \end_latex{spec_ADD_4}
           | is_XOR  = xor  rs1_val  rs2_val
           | is_OR   = rs1_val  .|.  rs2_val
           | is_AND  = rs1_val  .&.  rs2_val
           | is_SLL  = shiftL  rs1_val  shamt
           | is_SRL  = (let
                           v1 = if (rv == RV32) then (rs1_val .&. 0xffffFFFF) else rs1_val
                        in
                           shiftR  v1  shamt)
           | is_SRA  = cvt_s64_to_u64  (shiftR  (cvt_u64_to_s64  rs1_val)  shamt)

                                                                    -- \begin_latex{spec_ADD_5}
    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)
                                                                    -- \end_latex{spec_ADD_5}
-- ================================================================
-- MISC_MEM: FENCE, FENCE.I
-- These are technically architectural 'no-ops', but they can modify
-- hidden micro-arch state that affects future memory ops

opcode_MISC_MEM :: InstrField
opcode_MISC_MEM  = 0x0F    -- 7'b_00_011_11

funct3_FENCE   :: InstrField;    funct3_FENCE   = 0x0      -- 3'b_000
funct3_FENCE_I :: InstrField;    funct3_FENCE_I = 0x1      -- 3'b_001

spec_MISC_MEM :: Machine_State -> Instr -> (Bool, Machine_State)
spec_MISC_MEM    mstate       instr =
  let
    -- Instr fields: R-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type  instr

    (msbs4, pred, succ) = i_imm12_fields_for_FENCE  imm12

    -- Decode check
    is_FENCE   = ((funct3 == funct3_FENCE)   && (rd == 0) && (rs1 == 0) && (msbs4 == 0))
    is_FENCE_I = ((funct3 == funct3_FENCE_I) && (rd == 0) && (rs1 == 0) && (imm12 == 0))
    is_legal   = ((opcode == opcode_MISC_MEM)
                  && (is_FENCE
                      || is_FENCE_I))

    -- Semantics
    mstate1 | is_FENCE   = mstate_mem_fence    mstate
            | is_FENCE_I = mstate_mem_fence_i  mstate

    mstate2 = finish_pc_plus_4  mstate1
  in
    (is_legal, mstate2)

-- ================================================================
-- SYSTEM:
--    PRIV:  ECALL, EBREAK, MRET, SRET, URET, WFI
--    other: CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI

opcode_SYSTEM :: InstrField
opcode_SYSTEM = 0x73    -- 7'b_11_100_11

-- SYSTEM sub-opcodes
funct3_PRIV   :: InstrField;    funct3_PRIV   = 0x0     -- 3'b_000

-- ----------------
-- SYSTEM.PRIV.ECALL

funct12_ECALL    :: InstrField;    funct12_ECALL    = 0x000    -- 12'b_0000_0000_0000

spec_SYSTEM_ECALL :: Machine_State -> Instr -> (Bool, Machine_State)
spec_SYSTEM_ECALL    mstate       instr =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct12 == funct12_ECALL)
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    priv = mstate_priv_read  mstate
    exc_code | priv == m_Priv_Level = exc_code_ECall_from_M
             | priv == s_Priv_Level = exc_code_ECall_from_S
             | priv == u_Priv_Level = exc_code_ECall_from_U
             | True                 = error ("Illegal priv " ++ show (priv))
    tval = 0

    mstate1 = finish_trap  mstate  exc_code  tval
  in
    (is_legal, mstate1)

-- ----------------
-- SYSTEM.PRIV.MRET/SRET/URET

funct12_URET     :: InstrField;    funct12_URET     = 0x002    -- 12'b_0000_0000_0010
funct12_SRET     :: InstrField;    funct12_SRET     = 0x102    -- 12'b_0001_0000_0010
funct12_MRET     :: InstrField;    funct12_MRET     = 0x302    -- 12'b_0011_0000_0010

spec_SYSTEM_xRET :: Machine_State -> Instr -> (Bool, Machine_State)
spec_SYSTEM_xRET    mstate       instr =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    priv   = mstate_priv_read  mstate

    -- Decode check
    is_MRET = (funct12 == funct12_MRET)
    is_SRET = (funct12 == funct12_SRET)
    is_URET = (funct12 == funct12_URET)
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && ((is_MRET && (priv == m_Priv_Level))
                     || (is_SRET && (priv >= s_Priv_Level))
                     || (is_URET && (priv >= u_Priv_Level)))
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    mstatus   = mstate_csr_read  mstate  csr_addr_mstatus
    tsr_fault = (is_SRET && (priv == s_Priv_Level) && (testBit  mstatus  mstatus_tsr_bitpos))
    (mpp,spp,mpie,spie,upie,mie,sie,uie) = mstatus_stack_fields  mstatus
    rv        = mstate_rv_read   mstate
    misa      = mstate_csr_read  mstate  csr_addr_misa

    mstate3   = if (tsr_fault)
                then
                  let tval = zeroExtend_u32_to_u64  instr    -- TODO: may need extend or truncate?
                  in
                    finish_trap  mstate  exc_code_illegal_instruction  tval

                else
                  let
                    -- New 'previous-priv' is U if supported, else M
                    new_pp = if (misa_flag  misa  'U') then u_Priv_Level else m_Priv_Level

                    -- New priv, and new priv stack
                    (priv',mpp',spp')
                      -- From M
                      | (priv == m_Priv_Level) && is_MRET && (mpp == m_Priv_Level) = (m_Priv_Level, new_pp, spp)
                      | (priv == m_Priv_Level) && is_MRET && (mpp == s_Priv_Level) = (s_Priv_Level, new_pp, spp)
                      | (priv == m_Priv_Level) && is_MRET && (mpp == u_Priv_Level) = (u_Priv_Level, new_pp, spp)

                      | (priv == m_Priv_Level) && is_SRET && (spp == s_Priv_Level) = (s_Priv_Level, new_pp, spp)
                      | (priv == m_Priv_Level) && is_SRET && (spp == u_Priv_Level) = (u_Priv_Level, new_pp, spp)

                      | (priv == m_Priv_Level) && is_URET                          = (u_Priv_Level, new_pp, spp)

                      -- From S
                      | (priv == s_Priv_Level) && is_SRET && (spp == s_Priv_Level) = (s_Priv_Level, mpp, new_pp)
                      | (priv == s_Priv_Level) && is_SRET && (spp == u_Priv_Level) = (u_Priv_Level, mpp, new_pp)

                      | (priv == s_Priv_Level) && is_URET                          = (u_Priv_Level, mpp, new_pp)

                      -- From U
                      | (priv == u_Priv_Level) && is_URET                          = (u_Priv_Level, mpp, spp)

                    -- New interrupt-enable stack in new mstatus
                    (mpie',spie',upie',mie',sie',uie') | is_MRET = (   1, spie, upie, mpie,  sie,   uie)
                                                       | is_SRET = (mpie,    1, upie,  mie, spie,   uie)
                                                       | is_URET = (mpie, spie,    1,  mie,  sie,  upie)
                    mstatus' = mstatus_upd_stack_fields  mstatus  (mpp',spp',mpie',spie',upie',mie',sie',uie')

                    -- New PC
                    pc1 | is_MRET = mstate_csr_read  mstate  csr_addr_mepc
                        | is_SRET = mstate_csr_read  mstate  csr_addr_sepc
                        | is_URET = mstate_csr_read  mstate  csr_addr_uepc
                    pc2 | (rv == RV32) = (pc1 .&. 0xFFFFFFFF)
                        | True         = pc1

                    -- Update arch state
                    mstate1 = mstate_csr_write   mstate   csr_addr_mstatus  mstatus'
                    mstate2 = mstate_priv_write  mstate1  priv'
                  in
                    finish_pc  mstate2  pc2
  in
    (is_legal, mstate3)

-- ----------------
-- SYSTEM.PRIV.EBREAK

funct12_EBREAK   :: InstrField;    funct12_EBREAK   = 0x001    -- 12'b_0000_0000_0001

spec_SYSTEM_EBREAK :: Machine_State -> Instr -> (Bool, Machine_State)
spec_SYSTEM_EBREAK    mstate       instr =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct12 == funct12_EBREAK)
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    exc_code = exc_code_breakpoint
    tval     = mstate_pc_read  mstate

    mstate1 = finish_trap  mstate  exc_code  tval
  in
    (is_legal, mstate1)

-- ----------------
-- SYSTEM.PRIV.WFI

funct12_WFI :: InstrField;    funct12_WFI = 0x105    -- 12'b_0001_0000_0101

spec_SYSTEM_WFI :: Machine_State -> Instr -> (Bool, Machine_State)
spec_SYSTEM_WFI    mstate       instr =
  let
    -- Instr fields: I-type
    (funct12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    priv   = mstate_priv_read  mstate

    -- Decode check    TODO: if priv is u_Priv_Level, misa.N must be set (supporting user-mode interrupts)
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct12 == funct12_WFI)
                && (rs1 == 0)
                && (rd == 0))

    -- Semantics
    -- If mstatus.tw is set, illegal instruction trap after bounded timeout
    --     (here, the timeout is 0)
    -- Otherwise it's functionally a no-op
    --     Optionally: pause here until interrupt
    --     TODO: set run_state to WFI, and wait in the outer fetch-execute loop
    mstatus    = mstate_csr_read   mstate  csr_addr_mstatus
    tw_bit_set = testBit  mstatus  mstatus_tw_bitpos
    mstate1    = if (tw_bit_set)
                 then
                   let tval = zeroExtend_u32_to_u64  instr    -- TODO: may need extend or truncate?
                   in
                     finish_trap  mstate  exc_code_illegal_instruction  tval
                 else
                   finish_pc_plus_4  mstate
  in
    (is_legal, mstate1)

-- ----------------
-- SYSTEM.PRIV.SFENCE.VM

funct7_SFENCE_VM :: InstrField;    funct7_SFENCE_VM = 0x09     --  7'b_000_1001

spec_SYSTEM_SFENCE_VM :: Machine_State -> Instr -> (Bool, Machine_State)
spec_SYSTEM_SFENCE_VM    mstate       instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type   instr

    priv   = mstate_priv_read  mstate

    -- Decode check
    is_legal = ((opcode == opcode_SYSTEM)
                && (funct3 == funct3_PRIV)
                && (funct7 == funct7_SFENCE_VM)
                && (rd == 0)
                && (priv >= s_Priv_Level))    -- TODO: allowed in m_Priv_Level?

    -- Semantics
    -- Functionally a no-op, but can change micro-arch state to affect future mem ops
    rs1_val   = mstate_gpr_read  mstate  rs1
    rs2_val   = mstate_gpr_read  mstate  rs2
    mstatus   = mstate_csr_read  mstate  csr_addr_mstatus
    tvm_fault = testBit  mstatus  mstatus_tvm_bitpos

    mstate2   = if (tvm_fault)
                then
                  let tval = zeroExtend_u32_to_u64  instr    -- TODO: may need extend or truncate?
                  in
                    finish_trap  mstate  exc_code_illegal_instruction  tval
                else
                  let
                    mstate1 = mstate_mem_sfence_vm  mstate  rs1_val  rs2_val
                  in
                    finish_pc_plus_4  mstate1
  in
    (is_legal, mstate2)

-- ----------------
-- SYSTEM.not PRIV: CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI

funct3_CSRRW  :: InstrField;    funct3_CSRRW  = 0x1     -- 3'b_001
funct3_CSRRWI :: InstrField;    funct3_CSRRWI = 0x5     -- 3'b_101

spec_SYSTEM_CSRRW :: Machine_State -> Instr -> (Bool, Machine_State)
spec_SYSTEM_CSRRW    mstate       instr =
  let
    -- Instr fields: I-Type
    (csr_addr, rs1, funct3, rd, opcode) = ifields_I_type   instr
    zimm = rs1

    -- Decode check
    is_CSRRW  = (funct3 == funct3_CSRRW)
    is_CSRRWI = (funct3 == funct3_CSRRWI)
    is_legal  = ((opcode == opcode_SYSTEM)
                 && (is_CSRRW
                     || is_CSRRWI))

    -- Semantics
    priv       = mstate_priv_read  mstate
    permission = mstate_csr_read_permission  mstate  priv  csr_addr

    legal2 = (permission == CSR_Permission_RW)

    -- Read CSR only if rd is not 0
    old_csr_val = if (rd /= 0) then
                    mstate_csr_read  mstate  csr_addr
                  else
                    0    -- arbitrary; will be discarded (rd==0)
    rs1_val     = mstate_gpr_read  mstate  rs1

    new_csr_val | is_CSRRW  = rs1_val
                | is_CSRRWI = zeroExtend_u32_to_u64  rs1

    rd_val      = old_csr_val

    mstate1 = if legal2 then
                let mstate_a = mstate_csr_write  mstate  csr_addr  new_csr_val
                in
                  finish_rd_and_pc_plus_4  mstate_a  rd  rd_val
              else
                let tval = zeroExtend_u32_to_u64  instr    -- TODO: may need extend or truncate?
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    (is_legal, mstate1)

funct3_CSRRS  :: InstrField;    funct3_CSRRS  = 0x2     -- 3'b_010
funct3_CSRRC  :: InstrField;    funct3_CSRRC  = 0x3     -- 3'b_011
funct3_CSRRSI :: InstrField;    funct3_CSRRSI = 0x6     -- 3'b_110
funct3_CSRRCI :: InstrField;    funct3_CSRRCI = 0x7     -- 3'b_111

spec_SYSTEM_CSRR_S_C :: Machine_State -> Instr -> (Bool, Machine_State)
spec_SYSTEM_CSRR_S_C    mstate       instr =
  let
    -- Instr fields: I-Type
    (csr_addr, rs1, funct3, rd, opcode) = ifields_I_type   instr
    zimm = rs1

    -- Decode check
    is_CSRRS  = (funct3 == funct3_CSRRS)
    is_CSRRC  = (funct3 == funct3_CSRRC)
    is_CSRRSI = (funct3 == funct3_CSRRSI)
    is_CSRRCI = (funct3 == funct3_CSRRCI)
    is_legal  = ((opcode == opcode_SYSTEM)
                 && (is_CSRRS
                     || is_CSRRC
                     || is_CSRRSI
                     || is_CSRRCI))

    -- Semantics
    priv       = mstate_priv_read  mstate
    permission = mstate_csr_read_permission  mstate  priv  csr_addr

    legal2 | (permission == CSR_Permission_None) = False
           | (permission == CSR_Permission_RO)   = (rs1 == 0)
           | (permission == CSR_Permission_RW)   = True

    old_csr_val = mstate_csr_read  mstate  csr_addr
    rs1_val     = mstate_gpr_read  mstate  rs1

    new_csr_val | is_CSRRS  = old_csr_val .|. rs1_val
                | is_CSRRC  = old_csr_val .&. (complement rs1_val)
                | is_CSRRSI = old_csr_val .|. zeroExtend_u32_to_u64  rs1
                | is_CSRRCI = old_csr_val .&. (complement (zeroExtend_u32_to_u64  rs1))
    rd_val      = old_csr_val

    mstate1 = if legal2 then
                -- Write CSR only if rs1/zimm is not 0
                let mstate_a | (rs1 /= 0) = mstate_csr_write  mstate  csr_addr  new_csr_val
                             | True       = mstate
                in
                  finish_rd_and_pc_plus_4  mstate_a  rd  rd_val
              else
                let tval = zeroExtend_u32_to_u64  instr    -- TODO: may need extend or truncate?
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    (is_legal, mstate1)

-- ================================================================
-- OP: 'M' Extension: MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU

-- ----------------
-- OP: MUL, MULH, MULHSU, MULHU

funct3_MUL    :: InstrField;    funct3_MUL    = 0x0     -- 3'b_000
funct7_MUL    :: InstrField;    funct7_MUL    = 0x01    -- 7'b_000_0001

funct3_MULH   :: InstrField;    funct3_MULH   = 0x1     -- 3'b_001
funct7_MULH   :: InstrField;    funct7_MULH   = 0x01    -- 7'b_000_0001

funct3_MULHSU :: InstrField;    funct3_MULHSU = 0x2     -- 3'b_010
funct7_MULHSU :: InstrField;    funct7_MULHSU = 0x01    -- 7'b_000_0001

funct3_MULHU  :: InstrField;    funct3_MULHU  = 0x3     -- 3'b_011
funct7_MULHU  :: InstrField;    funct7_MULHU  = 0x01    -- 7'b_000_0001

spec_OP_MUL :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_MUL    mstate       instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    is_legal =
      ((opcode == opcode_OP)
       && ((   (funct3 == funct3_MUL)     && (funct7 == funct7_MUL))
           || ((funct3 == funct3_MULH)    && (funct7 == funct7_MULH))
           || ((funct3 == funct3_MULHSU)  && (funct7 == funct7_MULHSU))
           || ((funct3 == funct3_MULHU)   && (funct7 == funct7_MULHU))
          ))

    -- Semantics
    rv      = mstate_rv_read    mstate
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2

    -- Convert inputs to unbounded integers (sign- or zero-extending the msb)
    u1_i, u2_i, s1_i, s2_i :: Integer
    u1_i = fromIntegral  (if rv == RV32 then (rs1_val .&. 0xffffFFFF) else rs1_val)
    s1_i = fromIntegral  (cvt_u64_to_s64  rs1_val)
    u2_i = fromIntegral  (if rv == RV32 then (rs2_val .&. 0xffffFFFF) else rs2_val)
    s2_i = fromIntegral  (cvt_u64_to_s64  rs2_val)
    prod_i | (funct3 == funct3_MUL)    = s1_i * s2_i
           | (funct3 == funct3_MULH)   = s1_i * s2_i
           | (funct3 == funct3_MULHU)  = u1_i * u2_i
           | (funct3 == funct3_MULHSU) = s1_i * u2_i

    -- Pick out relevant XLEN bits
    result_i | (funct3 == funct3_MUL)    = prod_i
             | ((   funct3 == funct3_MULH)
                || (funct3 == funct3_MULHU)
                || (funct3 == funct3_MULHSU)) = shiftR  prod_i  xlen

    -- Convert back from unbounded integer to unsigned word
    rd_val :: Word64
    rd_val = fromIntegral  result_i


    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ----------------
-- OP: DIV, DIVU

funct3_DIV    :: InstrField;    funct3_DIV    = 0x4     -- 3'b_100
funct7_DIV    :: InstrField;    funct7_DIV    = 0x01    -- 7'b_000_0001

funct3_DIVU   :: InstrField;    funct3_DIVU   = 0x5     -- 3'b_101
funct7_DIVU   :: InstrField;    funct7_DIVU   = 0x01    -- 7'b_000_0001

spec_OP_DIV :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_DIV    mstate       instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    is_legal =
      ((opcode == opcode_OP)
       && ((   (funct3 == funct3_DIV)  && (funct7 == funct7_DIV))
           || ((funct3 == funct3_DIVU) && (funct7 == funct7_DIVU))
          ))

    -- Semantics
    rv      = mstate_rv_read  mstate
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2

    -- Signed versions of inputs
    rs1_val_s = cvt_u64_to_s64  rs1_val
    rs2_val_s = cvt_u64_to_s64  rs2_val

    rd_val | (funct3 == funct3_DIV)  = cvt_s64_to_u64 (if (rs2_val == 0)
                                                       then -1
                                                       else
                                                         if ((rs1_val_s == minBound)
                                                             && (rs2_val_s == -1)) then
                                                           rs1_val_s
                                                         else
                                                           quot  rs1_val_s  rs2_val_s)
           | (funct3 == funct3_DIVU) = if (rv == RV32) then
                                         let
                                           v1_u32 = trunc_u64_to_u32  rs1_val
                                           v2_u32 = trunc_u64_to_u32  rs2_val
                                           z_u32  = if (v2_u32 == 0) then maxBound
                                                    else  div  v1_u32  v2_u32
                                         in
                                           signExtend_u32_to_u64  z_u32
                                       else
                                         if (rs2_val == 0) then maxBound
                                         else div  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ----------------
-- OP: REM, REMU

funct3_REM    :: InstrField;    funct3_REM    = 0x6     -- 3'b_110
funct7_REM    :: InstrField;    funct7_REM    = 0x01    -- 7'b_000_0001

funct3_REMU   :: InstrField;    funct3_REMU   = 0x7     -- 3'b_111
funct7_REMU   :: InstrField;    funct7_REMU   = 0x01    -- 7'b_000_0001

spec_OP_REM :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_REM    mstate       instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    is_legal =
      ((opcode == opcode_OP)
       && ((   (funct3 == funct3_REM)     && (funct7 == funct7_REM))
           || ((funct3 == funct3_REMU)    && (funct7 == funct7_REMU))
          ))

    -- Semantics
    rv      = mstate_rv_read  mstate
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2

    -- Signed versions of inputs
    rs1_val_s = cvt_u64_to_s64  rs1_val
    rs2_val_s = cvt_u64_to_s64  rs2_val

    rd_val | (funct3 == funct3_REM)  = cvt_s64_to_u64 (if (rs2_val == 0)
                                                       then rs1_val_s
                                                       else
                                                         if ((rs1_val_s == minBound)
                                                             && (rs2_val_s == -1)) then
                                                           0
                                                         else
                                                           rem  rs1_val_s  rs2_val_s)
           | (funct3 == funct3_REMU) = if (rs2_val == 0) then rs1_val
                                       else rem  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-IMM-32: ADDIW, SLLIW, SRLIW, SRAIW

opcode_OP_IMM_32 :: InstrField
opcode_OP_IMM_32 = 0x1B    -- 7'b_00_110_11

funct3_ADDIW :: InstrField;    funct3_ADDIW = 0x0     -- 3'b_000

funct3_SLLIW :: InstrField;    funct3_SLLIW = 0x1     -- 3'b_001
funct7_SLLIW :: InstrField;    funct7_SLLIW = 0x00    -- 7'b_0000000

funct3_SRLIW :: InstrField;    funct3_SRLIW = 0x5     -- 3'b_101
funct7_SRLIW :: InstrField;    funct7_SRLIW = 0x00    -- 7'b_0000000

funct3_SRAIW :: InstrField;    funct3_SRAIW = 0x5     -- 3'b_101
funct7_SRAIW :: InstrField;    funct7_SRAIW = 0x20    -- 7'b_0100000

spec_OP_IMM_32 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_IMM_32    mstate       instr =
  let
    -- Instr fields: R-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type  instr
    (funct7, shamt_5) = i_imm12_fields_7_5  imm12

    -- Decode check
    rv       = mstate_rv_read  mstate
    is_ADDIW = (    funct3 == funct3_ADDIW)
    is_SLLIW = ((funct3 == funct3_SLLIW) && (funct7 == funct7_SLLIW))
    is_SRLIW = ((funct3 == funct3_SRLIW) && (funct7 == funct7_SRLIW))
    is_SRAIW = ((funct3 == funct3_SRAIW) && (funct7 == funct7_SRAIW))
    is_legal = ((rv == RV64)
                && (opcode == opcode_OP_IMM_32)
                && (is_ADDIW
                    || is_SLLIW
                    || is_SRLIW
                    || is_SRAIW
                   ))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1

    u1_32 = trunc_u64_to_u32  rs1_val
    u2_32 = signExtend_bit_in_u32  imm12  12

    shamt = cvt_u32_to_Int shamt_5

    rd_val_32 | is_ADDIW = cvt_s32_to_u32  ((cvt_u32_to_s32  u1_32) + (cvt_u32_to_s32  u2_32))
              | is_SLLIW = shiftL  u1_32  shamt
              | is_SRLIW = shiftR  u1_32  shamt
              | is_SRAIW = cvt_s32_to_u32  (shiftR  (cvt_u32_to_s32  u1_32)  shamt)

    rd_val = signExtend_u32_to_u64  rd_val_32

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-32: for RV64: ADDW, SUBW, SLLW, SRLW, SRAW

opcode_OP_32 :: InstrField
opcode_OP_32     = 0x3B    -- 7'b_01_110_11

funct3_ADDW  :: InstrField;    funct3_ADDW  = 0x0     --- 3'b_000
funct7_ADDW  :: InstrField;    funct7_ADDW  = 0x00    --- 7'b_000_0000

funct3_SUBW  :: InstrField;    funct3_SUBW  = 0x0     --- 3'b_000
funct7_SUBW  :: InstrField;    funct7_SUBW  = 0x20    --- 7'b_010_0000

funct3_SLLW  :: InstrField;    funct3_SLLW  = 0x1     --- 3'b_001
funct7_SLLW  :: InstrField;    funct7_SLLW  = 0x00    --- 7'b_000_0000

funct3_SRLW  :: InstrField;    funct3_SRLW  = 0x5     --- 3'b_101
funct7_SRLW  :: InstrField;    funct7_SRLW  = 0x00    --- 7'b_000_0000

funct3_SRAW  :: InstrField;    funct3_SRAW  = 0x5     --- 3'b_101
funct7_SRAW  :: InstrField;    funct7_SRAW  = 0x20    --- 7'b_010_0000

spec_OP_32 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_32    mstate       instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    rv      = mstate_rv_read  mstate
    is_ADDW = ((funct3 == funct3_ADDW) && (funct7 == funct7_ADDW))
    is_SUBW = ((funct3 == funct3_SUBW) && (funct7 == funct7_SUBW))
    is_SLLW = ((funct3 == funct3_SLLW) && (funct7 == funct7_SLLW))
    is_SRLW = ((funct3 == funct3_SRLW) && (funct7 == funct7_SRLW))
    is_SRAW = ((funct3 == funct3_SRAW) && (funct7 == funct7_SRAW))
    is_legal = ((rv == RV64)
                && (opcode == opcode_OP_32)
                && (is_ADDW
                    || is_SUBW
                    || is_SLLW
                    || is_SRLW
                    || is_SRAW))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2

    u1_32 = trunc_u64_to_u32  rs1_val
    u2_32 = trunc_u64_to_u32  rs2_val

    shamt = cvt_u64_to_Int (rs2_val .&. 0x1F)

    rd_val_32 | is_ADDW = cvt_s32_to_u32  ((cvt_u32_to_s32  u1_32) + (cvt_u32_to_s32  u2_32))
              | is_SUBW = cvt_s32_to_u32  ((cvt_u32_to_s32  u1_32) - (cvt_u32_to_s32  u2_32))
              | is_SLLW = shiftL  u1_32  shamt
              | is_SRLW = shiftR  u1_32  shamt
              | is_SRAW = cvt_s32_to_u32 (shiftR  (cvt_u32_to_s32  u1_32)  shamt)

    rd_val = signExtend_u32_to_u64  rd_val_32

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-32: 'M' Extension for RV64: MULW, DIVW, DIVUW, REMW, REMUW

funct3_MULW  :: InstrField;    funct3_MULW  = 0x0     --- 3'b_000
funct7_MULW  :: InstrField;    funct7_MULW  = 0x01    --- 7'b_000_0001

funct3_DIVW  :: InstrField;    funct3_DIVW  = 0x4     --- 3'b_100
funct7_DIVW  :: InstrField;    funct7_DIVW  = 0x01    --- 7'b_000_0001

funct3_DIVUW :: InstrField;    funct3_DIVUW = 0x5     --- 3'b_101
funct7_DIVUW :: InstrField;    funct7_DIVUW = 0x01    --- 7'b_000_0001

funct3_REMW  :: InstrField;    funct3_REMW  = 0x6     --- 3'b_110
funct7_REMW  :: InstrField;    funct7_REMW  = 0x01    --- 7'b_000_0001

funct3_REMUW :: InstrField;    funct3_REMUW = 0x7     --- 3'b_111
funct7_REMUW :: InstrField;    funct7_REMUW = 0x01    --- 7'b_000_0001

spec_OP_32_M :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_32_M    mstate       instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr

    -- Decode check
    rv       = mstate_rv_read  mstate
    is_MULW  = ((funct3 == funct3_MULW)   && (funct7 == funct7_MULW))
    is_DIVW  = ((funct3 == funct3_DIVW)   && (funct7 == funct7_DIVW))
    is_DIVUW = ((funct3 == funct3_DIVUW)  && (funct7 == funct7_DIVUW))
    is_REMW  = ((funct3 == funct3_REMW)   && (funct7 == funct7_REMW))
    is_REMUW = ((funct3 == funct3_REMUW)  && (funct7 == funct7_REMUW))
    is_legal = ((rv == RV64)
                && (opcode == opcode_OP_32)
                && (is_MULW
                    || is_DIVW
                    || is_DIVUW
                    || is_REMW
                    || is_REMUW))

    -- Semantics
    rs1_val = mstate_gpr_read  mstate  rs1
    rs2_val = mstate_gpr_read  mstate  rs2

    -- Convert 32 lsbs of inputs to unbounded integers (sign- or zero-extending bit 31)
    u1_32, u2_32 :: Word32
    u1_32 = trunc_u64_to_u32  rs1_val
    u2_32 = trunc_u64_to_u32  rs2_val

    s1_32, s2_32 :: Int32
    s1_32 = cvt_u32_to_s32  u1_32
    s2_32 = cvt_u32_to_s32  u2_32

    rd_val_32 | is_MULW  = cvt_s32_to_u32  (s1_32 * s2_32)
              | is_DIVW  = cvt_s32_to_u32  (if (u2_32 == 0) then
                                              -1
                                            else if (s1_32 == minBound) && (s2_32 == -1) then
                                                   s1_32
                                                 else
                                                   quot  s1_32  s2_32)
              | is_DIVUW = if (u2_32 == 0) then maxBound
                           else div  u1_32  u2_32
              | is_REMW  = cvt_s32_to_u32  (if (u2_32 == 0) then
                                              s1_32
                                            else if (s1_32 == minBound) && (s2_32 == -1) then
                                                   0
                                                 else
                                                   rem  s1_32  s2_32)
              | is_REMUW = if (u2_32 == 0) then
                             u1_32
                           else
                             rem  u1_32  u2_32

    rd_val = signExtend_u32_to_u64  rd_val_32

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- 'A' Extension

opcode_AMO :: InstrField
opcode_AMO     = 0x2F    -- 7'b_01_011_11

-- Note: these are duplicates of defs in Mem_Ops.hs
funct3_AMO_W   :: InstrField;    funct3_AMO_W   = 0x2     -- 3'b010
funct3_AMO_D   :: InstrField;    funct3_AMO_D   = 0x3     -- 3'b011

msbs5_AMO_LR   :: InstrField;    msbs5_AMO_LR   = 0x02    -- 5'b00010;
msbs5_AMO_SC   :: InstrField;    msbs5_AMO_SC   = 0x03    -- 5'b00011;
msbs5_AMO_ADD  :: InstrField;    msbs5_AMO_ADD  = 0x00    -- 5'b00000;
msbs5_AMO_SWAP :: InstrField;    msbs5_AMO_SWAP = 0x01    -- 5'b00001;
msbs5_AMO_XOR  :: InstrField;    msbs5_AMO_XOR  = 0x04    -- 5'b00100;
msbs5_AMO_AND  :: InstrField;    msbs5_AMO_AND  = 0x0C    -- 5'b01100;
msbs5_AMO_OR   :: InstrField;    msbs5_AMO_OR   = 0x08    -- 5'b01000;
msbs5_AMO_MIN  :: InstrField;    msbs5_AMO_MIN  = 0x10    -- 5'b10000;
msbs5_AMO_MAX  :: InstrField;    msbs5_AMO_MAX  = 0x14    -- 5'b10100;
msbs5_AMO_MINU :: InstrField;    msbs5_AMO_MINU = 0x18    -- 5'b11000;
msbs5_AMO_MAXU :: InstrField;    msbs5_AMO_MAXU = 0x1C    -- 5'b11100;

spec_AMO :: Machine_State -> Instr -> (Bool, Machine_State)
spec_AMO  mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, funct3, rd, opcode) = ifields_R_type  instr
    (msbs5, aq, rl) = r_funct7_fields_for_AMO  funct7

    -- Decode check
    rv       = mstate_rv_read  mstate
    is_legal = ((opcode == opcode_AMO)
                && ((funct3 == funct3_AMO_W)
                    || ((rv == RV64) && (funct3 == funct3_AMO_D)))
                && ((  (msbs5 == msbs5_AMO_LR) && (rs2 == 0))
                    || (msbs5 == msbs5_AMO_SC)
                    || (msbs5 == msbs5_AMO_ADD)
                    || (msbs5 == msbs5_AMO_SWAP)
                    || (msbs5 == msbs5_AMO_XOR)
                    || (msbs5 == msbs5_AMO_AND)
                    || (msbs5 == msbs5_AMO_OR)
                    || (msbs5 == msbs5_AMO_MIN)
                    || (msbs5 == msbs5_AMO_MAX)
                    || (msbs5 == msbs5_AMO_MINU)
                    || (msbs5 == msbs5_AMO_MAXU)))

    -- Semantics
    rs2_val = mstate_gpr_read  mstate  rs2

    --     Compute effective address
    eaddr1  = mstate_gpr_read  mstate  rs1
    eaddr2  = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)

    --     If Virtual Mem is active, translate to a physical addr
    is_instr = False
    is_read  = False
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  eaddr2
                         else
                           (Mem_Result_Ok  eaddr2, mstate)

    --     If no trap due to Virtual Mem translation, do AMO op in memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   eaddr2_pa ->
                             mstate_mem_amo  mstate1  eaddr2_pa  funct3  msbs5  aq  rl  rs2_val

    --     Finally: finish with trap, or finish with loading Rd with AMO result
    mstate3 = case result2 of
                Mem_Result_Err exc_code ->
                  finish_trap  mstate2  exc_code  eaddr2

                Mem_Result_Ok  d_u64    ->
                  let rd_val | (funct3 == funct3_AMO_W) = signExtend  d_u64  32
                             | (funct3 == funct3_AMO_D) = d_u64
                  in
                    finish_rd_and_pc_plus_4  mstate2  rd  rd_val
  in
    (is_legal, mstate3)

-- ================================================================
-- Common ways to finish an instruction.
-- Each opcode of course does something unique, but they all finish with
-- a few common actions:
--     - updating register Rd
--     - updating the PC with PC+4 or a new PC
--     - updating a CSR
--     - upating the MINSTRET register (number of instructions retired)
-- These functions capture those standard finishes.

-- Update RD, increment PC by 4, increment INSTRET                \begin_latex{finish_rd_and_pc_plus_4}
finish_rd_and_pc_plus_4 :: Machine_State -> GPR_Addr -> Word64 -> Machine_State
finish_rd_and_pc_plus_4  mstate  rd  rd_val =
  let mstate1 = mstate_gpr_write  mstate  rd  rd_val
      pc      = mstate_pc_read    mstate1
      mstate2 = mstate_pc_write   mstate1  (pc + 4)
      mstate3 = incr_minstret     mstate2
  in
    mstate3
                                                               -- \end_latex{finish_rd_and_pc_plus_4}
-- Update RD, update PC to new value, increment INSTRET
finish_rd_and_pc :: Machine_State -> GPR_Addr -> Word64 -> Word64 -> Machine_State
finish_rd_and_pc  mstate  rd  rd_val  new_pc =
  let
    mstate1 = mstate_gpr_write  mstate  rd  rd_val
    mstate2 = mstate_pc_write  mstate1  new_pc
    mstate3 = incr_minstret  mstate2
  in
    mstate3

-- Increment PC by 4, increment INSTRET
finish_pc_plus_4 :: Machine_State -> Machine_State
finish_pc_plus_4  mstate =
  let
    pc      = mstate_pc_read  mstate
    mstate1 = mstate_pc_write  mstate  (pc + 4)
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Update PC to new value
finish_pc :: Machine_State -> Word64 -> Machine_State
finish_pc  mstate  new_pc =
  let
    mstate1 = mstate_pc_write  mstate  new_pc
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Trap with given exception code and trap value
finish_trap :: Machine_State -> Exc_Code -> Word64 -> Machine_State
finish_trap  mstate  exc_code  tval =
  let
    mstate1 = mstate_upd_on_trap  mstate  False  exc_code  tval
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Every completed instruction increments minstret
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

mstate_upd_on_trap :: Machine_State -> Bool -> Exc_Code -> Word64 -> Machine_State
mstate_upd_on_trap  mstate  is_interrupt  exc_code  tval =
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
-- Executing one instruction

-- The spec is organized as a collection of functions.
-- Some functions specify just one kind of instruction (e.g., LUI).
-- Some functions specify a small family of related instructions (e.g., BRANCH)
-- Each function has the following type:

type Instr_Spec   = Machine_State -> Instr   -> (Bool, Machine_State)
type Instr_C_Spec = Machine_State -> Instr_C -> (Bool, Machine_State)

-- The first argument is a machine state (which contains the architectural state).
-- The second argument is an instruction, a 32-bit word.
-- It returns a 2-tuple (x,y) where:
--   x is True if this instruction is handled by this function
-- and
--   y is the transformed architecture state due executing this instruction, if x is True,
--     and irrelevant otherwise.

-- The following is a list of all the specification functions defined below.

instr_specs :: [(Instr_Spec, String)]
instr_specs = [(spec_LUI,               "LUI"),
               (spec_AUIPC,             "AUIPC"),
               (spec_JAL,               "JAL"),
               (spec_JALR,              "JALR"),
               (spec_BRANCH,            "BRANCH"),
               (spec_LOAD,              "LOAD"),
               (spec_STORE,             "STORE"),
               (spec_OP_IMM,            "OP_IMM"),
               (spec_OP,                "OP"),
               (spec_MISC_MEM,          "MISC_MEM"),
               (spec_SYSTEM_ECALL,      "SYSTEM_ECALL"),
               (spec_SYSTEM_xRET,       "SYSTEM_xRET"),
               (spec_SYSTEM_EBREAK,     "SYSTEM_EBREAK"),
               (spec_SYSTEM_WFI,        "SYSTEM_WFI"),
               (spec_SYSTEM_SFENCE_VM,  "SYSTEM_SFENCE_VM"),
               (spec_SYSTEM_CSRRW,      "SYSTEM_CSRRW"),
               (spec_SYSTEM_CSRR_S_C,   "SYSTEM_CSRR_S_C"),
               (spec_OP_MUL,            "OP_MUL"),
               (spec_OP_DIV,            "OP_DIV"),
               (spec_OP_REM,            "OP_REM"),
               (spec_OP_IMM_32,         "OP_IMM_32"),
               (spec_OP_32,             "OP_32"),
               (spec_OP_32_M,           "OP_32_M"),
               (spec_AMO,               "AMO") ]

instr_C_specs :: [(Instr_C_Spec, String)]
instr_C_specs = []    -- TODO: no C instructions implemented yet

-- 'exec_instr' takes a machine state and a 32-bit instruction and
-- returns a new machine state after executing that instruction.  It
-- attempts all the specs in 'instr_specs' and, if none of them apply,
-- performs an illegal-instruction trap.

exec_instr :: Machine_State -> Instr -> (Machine_State, String)
exec_instr  mstate  instr =
  let
    tryall []                  = (let
                                     tval = zeroExtend_u32_to_u64  instr    -- TODO: may need extend or truncate?
                                  in
                                    (finish_trap  mstate  exc_code_illegal_instruction  tval,
                                     "NONE"))

    tryall ((spec,name):specs) = (let
                                     (success, mstate1) = spec  mstate  instr
                                  in
                                     (if success then
                                        (mstate1, name)
                                      else
                                        tryall  specs))
  in
    tryall  instr_specs

-- 'exec_instr_C' takes a machine state and a 16-bit compressed instruction and
-- returns a new machine state after executing that instruction.  It
-- attempts all the specs in 'instr_C_specs' and, if none of them apply,
-- performs an illegal-instruction trap.

exec_instr_C :: Machine_State -> Instr_C -> (Machine_State, String)
exec_instr_C  mstate  instr =
  let
    tryall []                  = (let
                                     tval = zeroExtend_u16_to_u64  instr    -- TODO: may need extend or truncate?
                                  in
                                    (finish_trap  mstate  exc_code_illegal_instruction  tval, "NONE"))

    tryall ((spec,name):specs) = (let
                                     (success, mstate1) = spec  mstate  instr
                                  in
                                     if success then (mstate1, name)
                                     else
                                       tryall  specs)
  in
    tryall  instr_C_specs

-- ================================================================
-- Take interrupt                                                     \begin_latex{take_interrupt}

take_interrupt_if_any :: Machine_State -> (Bool, Machine_State)
take_interrupt_if_any  mstate =
  let                                                              -- \end_latex{take_interrupt}
    mstatus = mstate_csr_read  mstate  csr_addr_mstatus
    mip     = mstate_csr_read  mstate  csr_addr_mip
    mie     = mstate_csr_read  mstate  csr_addr_mie

    priv    = mstate_priv_read  mstate

    tval    = 0
  in
    case (fn_interrupt_pending  mstatus  mip  mie  priv) of
      Nothing        -> (False, mstate)
      Just  exc_code -> (True,  mstate_upd_on_trap  mstate  True  exc_code  tval)

-- ================================================================

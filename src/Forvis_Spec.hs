-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Forvis_Spec where

-- ================================================================
-- Specification of all RISC-V instructions.

-- ================================================================
-- Haskell lib imports

import Data.Bits    -- For bit-wise 'and' (.&.) etc.
import Data.Int     -- For Intxx type (signed fixed-width ints)

-- Local imports

import Bit_Utils
import FP_Bit_Utils
import ALU
import Arch_Defs
import Machine_State
import CSR_File
import Address_Map
import Virtual_Mem
import SoftFloat
-- ================================================================ \begin_latex{instr_fetch}
-- Instruction fetch
-- This function attempts an insruction fetch based on the current PC.

-- It first attempts to read 2 bytes only, in case the next
-- instruction is a 'C' (compressed) instruction. This may trap; if
-- not, we can decide if it's a C instruction, and read the next 2
-- bytes if it is not a C instruction; this, too, may trap.

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
      -- 32b instructions only
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
    is_instr = True
    is_read  = True
    funct3   = if (n_bytes == 4) then  funct3_LW  else  funct3_LH

    --     If Virtual Mem is active, translate pc to a physical addr
    (result1, mstate1) = if (fn_vm_is_active  mstate  is_instr) then
                           vm_translate  mstate  is_instr  is_read  va
                         else
                           (Mem_Result_Ok  va, mstate)

    --     If no trap due to Virtual Mem translation, read 2 bytes from memory
    (result2, mstate2) = case result1 of
                           Mem_Result_Err  exc_code -> (result1, mstate1)
                           Mem_Result_Ok   pa ->
                             mstate_mem_read   mstate1  exc_code_instr_access_fault  funct3  pa
  in
    (result2, mstate2)

-- ================================================================
-- LUI

opcode_LUI = 0x37 :: InstrField   -- 7'b_01_101_11

spec_LUI :: Machine_State -> Instr -> (Bool, Machine_State)
spec_LUI    mstate       instr =
  let
    -- Instr fields: U-type
    (imm20, rd, opcode) = ifields_U_type  instr

    -- Decode check
    is_legal = (opcode == opcode_LUI)

    -- Semantics
    xlen   = mstate_xlen_read  mstate
    imm32  = shiftL  imm20  12
    rd_val = sign_extend  32  xlen  imm32

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- AUIPC

opcode_AUIPC = 0x17 :: InstrField   -- 7'b_00_101_11

spec_AUIPC :: Machine_State -> Instr -> (Bool, Machine_State)
spec_AUIPC    mstate       instr =
  let
    -- Instr fields: U-type
    (imm20, rd, opcode) = ifields_U_type  instr

    -- Decode check
    is_legal = (opcode == opcode_AUIPC)

    -- Semantics
    xlen     = mstate_xlen_read  mstate
    pc       = mstate_pc_read  mstate
    imm32    = shiftL  imm20  12
    s_offset = sign_extend  32  xlen  imm32
    rd_val   = alu_add  xlen  pc  s_offset

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- JAL

opcode_JAL = 0x6F :: InstrField    -- 7'b_11_011_11

spec_JAL :: Machine_State -> Instr -> (Bool, Machine_State)
spec_JAL    mstate       instr =
  let
    -- Instr fields: J-type
    (imm20, rd, opcode) = ifields_J_type  instr

    -- Decode check
    is_legal = (opcode == opcode_JAL)

    -- Semantics
    xlen   = mstate_xlen_read  mstate
    pc     = mstate_pc_read    mstate
    rd_val = pc + 4

    imm21    = shiftL  imm20  1                -- offset imm20 is in multiples of 2 bytes
    s_offset = sign_extend  21  xlen  imm21
    new_pc   = alu_add  xlen  pc  s_offset
    aligned  = ((new_pc .&. 0x3) == 0)

    mstate1  = if aligned
               then
                 finish_rd_and_pc  mstate  rd  rd_val  new_pc
               else
                 finish_trap  mstate  exc_code_instr_addr_misaligned  new_pc
  in
    (is_legal, mstate1)

-- ================================================================
-- JALR

opcode_JALR = 0x67 :: InstrField    -- 7'b_11_001_11

spec_JALR :: Machine_State -> Instr -> (Bool, Machine_State)
spec_JALR    mstate       instr =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    is_legal = (opcode == opcode_JALR)

    -- Semantics
    xlen    = mstate_xlen_read  mstate
    pc      = mstate_pc_read    mstate
    rd_val  = pc + 4

    s_offset = sign_extend  12  xlen  imm12

    rs1_val = mstate_gpr_read  mstate  rs1

    new_pc  = alu_add  xlen  rs1_val  s_offset
    new_pc' = clearBit  new_pc  0
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

opcode_BRANCH = 0x63 :: InstrField    -- 7'b_11_000_11

funct3_BEQ  = 0x0 :: InstrField     -- 3'b_000
funct3_BNE  = 0x1 :: InstrField     -- 3'b_001
funct3_BLT  = 0x4 :: InstrField     -- 3'b_100
funct3_BGE  = 0x5 :: InstrField     -- 3'b_101
funct3_BLTU = 0x6 :: InstrField     -- 3'b_110
funct3_BGEU = 0x7 :: InstrField     -- 3'b_111

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
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read   mstate  rs1
    rs2_val = mstate_gpr_read   mstate  rs2

    taken | is_BEQ  = alu_eq   xlen  rs1_val  rs2_val
          | is_BNE  = alu_ne   xlen  rs1_val  rs2_val
          | is_BLT  = alu_lt   xlen  rs1_val  rs2_val
          | is_BGE  = alu_ge   xlen  rs1_val  rs2_val
          | is_BLTU = alu_ltu  xlen  rs1_val  rs2_val
          | is_BGEU = alu_geu  xlen  rs1_val  rs2_val

    pc      = mstate_pc_read  mstate

    imm13    = shiftL  imm12  1                -- since offset imm12 is in multiples of 2 bytes
    s_offset = sign_extend  13  xlen  imm13

    target  = alu_add  xlen  pc  s_offset
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

opcode_LOAD = 0x03 :: InstrField    -- 7'b_00_000_11

-- Note: these are duplicates of defs in Mem_Ops.hs
funct3_LB  = 0x0 :: InstrField     -- 3'b_000
funct3_LH  = 0x1 :: InstrField     -- 3'b_001
funct3_LW  = 0x2 :: InstrField     -- 3'b_010
funct3_LD  = 0x3 :: InstrField     -- 3'b_011
funct3_LBU = 0x4 :: InstrField     -- 3'b_100
funct3_LHU = 0x5 :: InstrField     -- 3'b_101
funct3_LWU = 0x6 :: InstrField     -- 3'b_110

spec_LOAD :: Machine_State -> Instr -> (Bool, Machine_State)
spec_LOAD    mstate       instr =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
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
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
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

                Mem_Result_Ok  d    ->
                  let rd_val | is_LB = sign_extend  8   xlen  d
                             | is_LH = sign_extend  16  xlen  d
                             | is_LW = sign_extend  32  xlen  d
                             | True  = d
                  in
                    finish_rd_and_pc_plus_4  mstate2  rd  rd_val
  in
    (is_legal, mstate3)

-- ================================================================
-- STORE:
--    RV32: SB, SH, SW
--    RV64: SD

opcode_STORE = 0x23 :: InstrField    -- 7'b_01_000_11

-- Note: these are duplicates of defs in Mem_Ops.hs
funct3_SB = 0x0 :: InstrField     -- 3'b_000
funct3_SH = 0x1 :: InstrField     -- 3'b_001
funct3_SW = 0x2 :: InstrField     -- 3'b_010
funct3_SD = 0x3 :: InstrField     -- 3'b_011

spec_STORE :: Machine_State -> Instr -> (Bool, Machine_State)
spec_STORE    mstate       instr =
  let
    -- Instr fields: S-type
    (imm12, rs2, rs1, funct3, opcode) = ifields_S_type  instr

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
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
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
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

opcode_OP_IMM = 0x13 :: InstrField    -- 7'b_00_100_11

funct3_ADDI  = 0x0 :: InstrField      -- 3'b_000
funct3_SLTI  = 0x2 :: InstrField      -- 3'b_010
funct3_SLTIU = 0x3 :: InstrField      -- 3'b_011
funct3_XORI  = 0x4 :: InstrField      -- 3'b_100
funct3_ORI   = 0x6 :: InstrField      -- 3'b_110
funct3_ANDI  = 0x7 :: InstrField      -- 3'b_111
funct3_SLLI  = 0x1 :: InstrField      -- 3'b_001
funct3_SRLI  = 0x5 :: InstrField      -- 3'b_101
funct3_SRAI  = 0x5 :: InstrField      -- 3'b_101

-- OP_IMM.SLLI/SRLI/SRAI for RV64
msbs7_SLLI  = 0x00 :: InstrField     -- 7'b_0000000
msbs7_SRLI  = 0x00 :: InstrField     -- 7'b_0000000
msbs7_SRAI  = 0x20 :: InstrField     -- 7'b_0100000

-- OP_IMM.SLLI/SRLI/SRAI for RV64
msbs6_SLLI  = 0x00 :: InstrField     -- 6'b_000000
msbs6_SRLI  = 0x00 :: InstrField     -- 6'b_000000
msbs6_SRAI  = 0x10 :: InstrField     -- 6'b_010000


spec_OP_IMM :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_IMM    mstate       instr =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr
    (msbs7, shamt5) = i_imm12_fields_7_5  imm12
    (msbs6, shamt6) = i_imm12_fields_6_6  imm12

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
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

    s_imm12 = sign_extend  12  xlen  imm12

    rd_val | is_ADDI  = alu_add   xlen  rs1_val  s_imm12
           | is_SLTI  = alu_slt   xlen  rs1_val  s_imm12
           | is_SLTIU = alu_sltu  xlen  rs1_val  s_imm12
           | is_XORI  = alu_xor   xlen  rs1_val  s_imm12
           | is_ORI   = alu_or    xlen  rs1_val  s_imm12
           | is_ANDI  = alu_and   xlen  rs1_val  s_imm12
           | is_SLLI  = alu_sll   xlen  rs1_val  imm12
           | is_SRLI  = alu_srl   xlen  rs1_val  imm12
           | is_SRAI  = alu_sra   xlen  rs1_val  imm12

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP: ADD, SUB, SLT, SLTU, XOR, OR, AND, SLL, SRL, SRA

opcode_OP = 0x33 :: InstrField    -- 7'b_01_100_11

funct3_ADD  = 0x0 :: InstrField     -- 3'b_000
funct7_ADD  = 0x00 :: InstrField    -- 7'b_000_0000

funct3_SUB  = 0x0 :: InstrField     -- 3'b_000
funct7_SUB  = 0x20 :: InstrField    -- 7'b_010_0000

funct3_SLT  = 0x2 :: InstrField     -- 3'b_010
funct7_SLT  = 0x00 :: InstrField    -- 7'b_000_0000

funct3_SLTU = 0x3 :: InstrField     -- 3'b_011
funct7_SLTU = 0x00 :: InstrField    -- 7'b_000_0000

funct3_XOR  = 0x4 :: InstrField     -- 3'b_100
funct7_XOR  = 0x00 :: InstrField    -- 7'b_000_0000

funct3_OR   = 0x6 :: InstrField     -- 3'b_110
funct7_OR   = 0x00 :: InstrField    -- 7'b_000_0000

funct3_AND  = 0x7 :: InstrField     -- 3'b_111
funct7_AND  = 0x00 :: InstrField    -- 7'b_000_0000

funct3_SLL  = 0x1 :: InstrField     -- 3'b_001
funct7_SLL  = 0x00 :: InstrField    -- 7'b_000_0000

funct3_SRL  = 0x5 :: InstrField     -- 3'b_101
funct7_SRL  = 0x00 :: InstrField    -- 7'b_000_0000

funct3_SRA  = 0x5 :: InstrField     -- 3'b_101
funct7_SRA  = 0x20 :: InstrField    -- 7'b_010_0000

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
    xlen    = mstate_xlen_read  mstate                              -- \end_latex{spec_ADD_3}
    rs1_val = mstate_gpr_read   mstate  rs1
    rs2_val = mstate_gpr_read   mstate  rs2
                                                                    -- \begin_latex{spec_ADD_4}
    rd_val | is_ADD  = alu_add  xlen  rs1_val  rs2_val
           | is_SUB  = alu_sub  xlen  rs1_val  rs2_val
           | is_SLT  = alu_slt  xlen  rs1_val  rs2_val
           | is_SLTU = alu_sltu xlen  rs1_val  rs2_val
                                                                    -- \end_latex{spec_ADD_4}
           | is_XOR  = alu_xor  xlen  rs1_val  rs2_val
           | is_OR   = alu_or   xlen  rs1_val  rs2_val
           | is_AND  = alu_and  xlen  rs1_val  rs2_val
           | is_SLL  = alu_sll  xlen  rs1_val  rs2_val
           | is_SRL  = alu_srl  xlen  rs1_val  rs2_val
           | is_SRA  = alu_sra  xlen  rs1_val  rs2_val

                                                                    -- \begin_latex{spec_ADD_5}
    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)
                                                                    -- \end_latex{spec_ADD_5}
-- ================================================================
-- MISC_MEM: FENCE, FENCE.I
-- These are technically architectural 'no-ops', but they can modify
-- hidden micro-arch state that affects future memory ops

opcode_MISC_MEM  = 0x0F :: InstrField    -- 7'b_00_011_11

funct3_FENCE   = 0x0 :: InstrField      -- 3'b_000
funct3_FENCE_I = 0x1 :: InstrField      -- 3'b_001

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

opcode_SYSTEM = 0x73 :: InstrField    -- 7'b_11_100_11

-- SYSTEM sub-opcodes
funct3_PRIV = 0x0 :: InstrField     -- 3'b_000

-- ----------------
-- SYSTEM.PRIV.ECALL

funct12_ECALL = 0x000 :: InstrField    -- 12'b_0000_0000_0000

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

funct12_URET     = 0x002 :: InstrField    -- 12'b_0000_0000_0010
funct12_SRET     = 0x102 :: InstrField    -- 12'b_0001_0000_0010
funct12_MRET     = 0x302 :: InstrField    -- 12'b_0011_0000_0010

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
                  let tval = instr
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

funct12_EBREAK   = 0x001 :: InstrField    -- 12'b_0000_0000_0001

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

funct12_WFI = 0x105 :: InstrField    -- 12'b_0001_0000_0101

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
    --     Optionally: pause here in WFI state until interrupt
    mstatus    = mstate_csr_read   mstate  csr_addr_mstatus
    tw_bit_set = testBit  mstatus  mstatus_tw_bitpos
    mstate1    = if (tw_bit_set)
                 then
                   let tval = instr
                   in
                     finish_trap  mstate  exc_code_illegal_instruction  tval
                 else
                   let
                     mstate' = mstate_run_state_write  mstate  Run_State_WFI
                   in
                     finish_pc_plus_4  mstate'
  in
    (is_legal, mstate1)

-- ----------------
-- SYSTEM.PRIV.SFENCE.VM

funct7_SFENCE_VM = 0x09 :: InstrField    --  7'b_000_1001

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
                  let tval = instr
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

funct3_CSRRW  = 0x1 :: InstrField     -- 3'b_001
funct3_CSRRWI = 0x5 :: InstrField     -- 3'b_101

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
                    if (csr_addr == csr_addr_time) then
                      let
                        -- CSR TIME is a read-only shadow of MTIME,
                        -- which is actually a memory-mapped location,
                        -- not a CSR
                        mtime = mstate_mem_read_mtime  mstate
                      in
                        mtime
                    else
                      mstate_csr_read  mstate  csr_addr
                  else
                    0    -- arbitrary; will be discarded (rd==0)

    rs1_val     = mstate_gpr_read  mstate  rs1

    new_csr_val | is_CSRRW  = rs1_val
                | is_CSRRWI = rs1

    rd_val      = old_csr_val

    mstate1 = if legal2 then
                -- FCSR consists of two sub-CSRs, there is a special function
                -- to handle writes to FCSR
                let
                  mstate_a = (
                    if (csr_addr == csr_addr_fcsr) then
                      mstate_fcsr_write  mstate  csr_addr  new_csr_val 
                    else
                      mstate_csr_write  mstate  csr_addr  new_csr_val)
                in
                  finish_rd_and_pc_plus_4  mstate_a  rd  rd_val
              else
                let tval = instr
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    (is_legal, mstate1)

funct3_CSRRS  = 0x2 :: InstrField     -- 3'b_010
funct3_CSRRC  = 0x3 :: InstrField     -- 3'b_011
funct3_CSRRSI = 0x6 :: InstrField     -- 3'b_110
funct3_CSRRCI = 0x7 :: InstrField     -- 3'b_111

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
                | is_CSRRSI = old_csr_val .|. rs1
                | is_CSRRCI = old_csr_val .&. (complement rs1)
    rd_val      = old_csr_val

    mstate1 = if legal2 then
                -- Write CSR only if rs1/zimm is not 0
                let mstate_a | (rs1 /= 0) = mstate_csr_write  mstate  csr_addr  new_csr_val
                             | True       = mstate
                in
                  finish_rd_and_pc_plus_4  mstate_a  rd  rd_val
              else
                let tval = instr
                in
                  finish_trap  mstate  exc_code_illegal_instruction  tval
  in
    (is_legal, mstate1)

-- ================================================================
-- OP: 'M' Extension: MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU

-- ----------------
-- OP: MUL, MULH, MULHSU, MULHU

funct3_MUL    = 0x0 :: InstrField     -- 3'b_000
funct7_MUL    = 0x01 :: InstrField    -- 7'b_000_0001

funct3_MULH   = 0x1 :: InstrField     -- 3'b_001
funct7_MULH   = 0x01 :: InstrField    -- 7'b_000_0001

funct3_MULHSU = 0x2 :: InstrField     -- 3'b_010
funct7_MULHSU = 0x01 :: InstrField    -- 7'b_000_0001

funct3_MULHU  = 0x3 :: InstrField     -- 3'b_011
funct7_MULHU  = 0x01 :: InstrField    -- 7'b_000_0001

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

    rd_val | (funct3 == funct3_MUL)    = alu_mul     xlen  rs1_val  rs2_val
           | (funct3 == funct3_MULH)   = alu_mulh    xlen  rs1_val  rs2_val
           | (funct3 == funct3_MULHU)  = alu_mulhu   xlen  rs1_val  rs2_val
           | (funct3 == funct3_MULHSU) = alu_mulhsu  xlen  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ----------------
-- OP: DIV, DIVU

funct3_DIV    = 0x4 :: InstrField     -- 3'b_100
funct7_DIV    = 0x01 :: InstrField    -- 7'b_000_0001

funct3_DIVU   = 0x5 :: InstrField     -- 3'b_101
funct7_DIVU   = 0x01 :: InstrField    -- 7'b_000_0001

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
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read   mstate  rs1
    rs2_val = mstate_gpr_read   mstate  rs2

    rd_val | (funct3 == funct3_DIV)  = alu_div   xlen  rs1_val  rs2_val
           | (funct3 == funct3_DIVU) = alu_divu  xlen  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ----------------
-- OP: REM, REMU

funct3_REM    = 0x6 :: InstrField     -- 3'b_110
funct7_REM    = 0x01 :: InstrField    -- 7'b_000_0001

funct3_REMU   = 0x7 :: InstrField     -- 3'b_111
funct7_REMU   = 0x01 :: InstrField    -- 7'b_000_0001

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
    xlen    = mstate_xlen_read  mstate
    rs1_val = mstate_gpr_read   mstate  rs1
    rs2_val = mstate_gpr_read   mstate  rs2

    rd_val | (funct3 == funct3_REM)  = alu_rem   xlen  rs1_val  rs2_val
           | (funct3 == funct3_REMU) = alu_remu  xlen  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-IMM-32: ADDIW, SLLIW, SRLIW, SRAIW

opcode_OP_IMM_32 = 0x1B :: InstrField    -- 7'b_00_110_11

funct3_ADDIW = 0x0 :: InstrField     -- 3'b_000

funct3_SLLIW = 0x1 :: InstrField     -- 3'b_001
funct7_SLLIW = 0x00 :: InstrField    -- 7'b_0000000

funct3_SRLIW = 0x5 :: InstrField     -- 3'b_101
funct7_SRLIW = 0x00 :: InstrField    -- 7'b_0000000

funct3_SRAIW = 0x5 :: InstrField     -- 3'b_101
funct7_SRAIW = 0x20 :: InstrField    -- 7'b_0100000

spec_OP_IMM_32 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_OP_IMM_32    mstate       instr =
  let
    -- Instr fields: R-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type  instr
    (funct7, shamt_5) = i_imm12_fields_7_5  imm12

    -- Decode check
    rv       = mstate_rv_read    mstate
    xlen     = mstate_xlen_read  mstate
    is_ADDIW = ( funct3 == funct3_ADDIW)
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

    rd_val | is_ADDIW = alu_addw  rs1_val  (sign_extend  12  xlen  imm12)
           | is_SLLIW = alu_sllw  rs1_val  shamt_5
           | is_SRLIW = alu_srlw  rs1_val  shamt_5
           | is_SRAIW = alu_sraw  rs1_val  shamt_5

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-32: for RV64: ADDW, SUBW, SLLW, SRLW, SRAW

opcode_OP_32     = 0x3B :: InstrField    -- 7'b_01_110_11

funct3_ADDW  = 0x0  :: InstrField    --- 3'b_000
funct7_ADDW  = 0x00 :: InstrField    --- 7'b_000_0000

funct3_SUBW  = 0x0  :: InstrField    --- 3'b_000
funct7_SUBW  = 0x20 :: InstrField    --- 7'b_010_0000

funct3_SLLW  = 0x1  :: InstrField    --- 3'b_001
funct7_SLLW  = 0x00 :: InstrField    --- 7'b_000_0000

funct3_SRLW  = 0x5  :: InstrField    --- 3'b_101
funct7_SRLW  = 0x00 :: InstrField    --- 7'b_000_0000

funct3_SRAW  = 0x5  :: InstrField    --- 3'b_101
funct7_SRAW  = 0x20 :: InstrField    --- 7'b_010_0000

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

    rd_val | is_ADDW = alu_addw  rs1_val  rs2_val
           | is_SUBW = alu_subw  rs1_val  rs2_val
           | is_SLLW = alu_sllw  rs1_val  rs2_val
           | is_SRLW = alu_srlw  rs1_val  rs2_val
           | is_SRAW = alu_sraw  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- OP-32: 'M' Extension for RV64: MULW, DIVW, DIVUW, REMW, REMUW

funct3_MULW  = 0x0  :: InstrField    --- 3'b_000
funct7_MULW  = 0x01 :: InstrField    --- 7'b_000_0001

funct3_DIVW  = 0x4  :: InstrField    --- 3'b_100
funct7_DIVW  = 0x01 :: InstrField    --- 7'b_000_0001

funct3_DIVUW = 0x5  :: InstrField    --- 3'b_101
funct7_DIVUW = 0x01 :: InstrField    --- 7'b_000_0001

funct3_REMW  = 0x6  :: InstrField    --- 3'b_110
funct7_REMW  = 0x01 :: InstrField    --- 7'b_000_0001

funct3_REMUW = 0x7  :: InstrField    --- 3'b_111
funct7_REMUW = 0x01 :: InstrField    --- 7'b_000_0001

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

    rd_val | is_MULW  = alu_mulw   rs1_val  rs2_val
           | is_DIVW  = alu_divw   rs1_val  rs2_val
           | is_DIVUW = alu_divuw  rs1_val  rs2_val
           | is_REMW  = alu_remw   rs1_val  rs2_val
           | is_REMUW = alu_remuw  rs1_val  rs2_val

    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val
  in
    (is_legal, mstate1)

-- ================================================================
-- 'A' Extension

opcode_AMO     = 0x2F :: InstrField    -- 7'b_01_011_11

-- Note: these are duplicates of defs in Mem_Ops.hs
funct3_AMO_W   = 0x2 :: InstrField     -- 3'b010
funct3_AMO_D   = 0x3 :: InstrField     -- 3'b011

msbs5_AMO_LR   = 0x02 :: InstrField    -- 5'b00010;
msbs5_AMO_SC   = 0x03 :: InstrField    -- 5'b00011;
msbs5_AMO_ADD  = 0x00 :: InstrField    -- 5'b00000;
msbs5_AMO_SWAP = 0x01 :: InstrField    -- 5'b00001;
msbs5_AMO_XOR  = 0x04 :: InstrField    -- 5'b00100;
msbs5_AMO_AND  = 0x0C :: InstrField    -- 5'b01100;
msbs5_AMO_OR   = 0x08 :: InstrField    -- 5'b01000;
msbs5_AMO_MIN  = 0x10 :: InstrField    -- 5'b10000;
msbs5_AMO_MAX  = 0x14 :: InstrField    -- 5'b10100;
msbs5_AMO_MINU = 0x18 :: InstrField    -- 5'b11000;
msbs5_AMO_MAXU = 0x1C :: InstrField    -- 5'b11100;

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

                Mem_Result_Ok  x        ->
                  finish_rd_and_pc_plus_4  mstate2  rd  x
  in
    (is_legal, mstate3)

-- ================================================================
-- 'F' and 'D' extensions (floating point)
-- ================================================================
-- FD_LOAD
--    SP: FLW
--    DP: FLD

opcode_FD_LOAD = 0x07   :: InstrField  -- 7'b_00_001_11
funct3_FD_LW   = 0x2    :: InstrField  -- 3'b_010
funct3_FD_LD   = 0x3    :: InstrField  -- 3'b_011

spec_FD_LOAD            :: Machine_State -> Instr -> (Bool, Machine_State)
spec_FD_LOAD  mstate  instr =
  let
    -- Instr fields: I-type
    (imm12, rs1, funct3, rd, opcode) = ifields_I_type   instr

    -- Decode check
    rv         = mstate_rv_read  mstate
    xlen       = mstate_xlen_read  mstate
    misa       = (mstate_csr_read mstate  csr_addr_misa)
    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_LW      = (funct3 == funct3_FD_LW)
    is_LD      = (funct3 == funct3_FD_LD)
    is_legal   = (   (opcode == opcode_FD_LOAD)
                  && (is_F)
                  && (is_LW || (is_LD && is_D)))

    -- Semantics
    --     Compute effective address
    rs1_val = mstate_gpr_read  mstate  rs1
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
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
                  finish_frd_and_pc_plus_4  mstate2  rd  d_u64  is_LW
  in
    (is_legal, mstate3)

-- ================================================================
-- FD_STORE
--    SP: FSW
--    DP: FSD

-- Note: these are duplicates of defs in Mem_Ops.hs
opcode_FD_STORE   = 0x27   :: InstrField  -- 7'b_01_001_11
funct3_FD_SW      = 0x2    :: InstrField  -- 3'b_010
funct3_FD_SD      = 0x3    :: InstrField  -- 3'b_011

spec_FD_STORE              :: Machine_State -> Instr -> (Bool, Machine_State)
spec_FD_STORE  mstate  instr =
  let
    -- Instr fields: S-type
    (imm12, rs2, rs1, funct3, opcode) = ifields_S_type  instr

    -- Decode check
    rv         = mstate_rv_read  mstate
    xlen     = mstate_xlen_read  mstate
    misa       = (mstate_csr_read mstate  csr_addr_misa)
    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_SW      = (funct3 == funct3_FD_SW)
    is_SD      = (funct3 == funct3_FD_SD)
    is_legal   = (   (opcode == opcode_FD_STORE)
                  && (is_F)
                  && (is_SW || (is_SD && is_D)))

    -- Semantics
    -- For SW, the upper bits are to be ignored
    rs2_val = mstate_fpr_read  mstate  rs2   -- store value

    --     Compute effective address
    rs1_val = mstate_gpr_read  mstate  rs1    -- address base
    s_imm12 = sign_extend  12  xlen  imm12
    eaddr1  = alu_add  xlen  rs1_val  s_imm12
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
-- FD Opcodes
-- Opcode (duplicate from Arch_Defs)
opcode_FD_OP      = 0x53   :: InstrField  -- 7'b_10_100_11

funct7_FADD_D     = 0x1    :: InstrField  -- 7'b_00_000_01
funct7_FSUB_D     = 0x5    :: InstrField  -- 7'b_00_001_01
funct7_FMUL_D     = 0x9    :: InstrField  -- 7'b_00_010_01
funct7_FDIV_D     = 0xD    :: InstrField  -- 7'b_01_011_01
funct7_FSQRT_D    = 0x2D   :: InstrField  -- 7'b_00_000_01
funct7_FCMP_D     = 0x51   :: InstrField  -- 7'b_10_100_01
funct7_FMIN_D     = 0x15   :: InstrField  -- 7'b_00_101_01
funct7_FMAX_D     = 0x15   :: InstrField  -- 7'b_00_101_01
funct7_FSGNJ_D    = 0x11   :: InstrField  -- 7'b_00_100_01

funct7_FADD_S     = 0x0    :: InstrField  -- 7'b_00_000_00
funct7_FSUB_S     = 0x4    :: InstrField  -- 7'b_00_001_00
funct7_FMUL_S     = 0x8    :: InstrField  -- 7'b_00_010_00
funct7_FDIV_S     = 0xC    :: InstrField  -- 7'b_01_011_00
funct7_FSQRT_S    = 0x2C   :: InstrField  -- 7'b_00_000_00
funct7_FCMP_S     = 0x50   :: InstrField  -- 7'b_10_100_00
funct7_FMIN_S     = 0x14   :: InstrField  -- 7'b_00_101_01
funct7_FMAX_S     = 0x14   :: InstrField  -- 7'b_00_101_01
funct7_FSGNJ_S    = 0x10   :: InstrField  -- 7'b_00_100_00

funct7_FCVT_W_S   = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_WU_S  = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_S_W   = 0x68   :: InstrField  -- 7'b_11_010_00
funct7_FCVT_S_WU  = 0x68   :: InstrField  -- 7'b_11_010_00

funct7_FCVT_L_S   = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_LU_S  = 0x60   :: InstrField  -- 7'b_11_000_00
funct7_FCVT_S_L   = 0x68   :: InstrField  -- 7'b_11_010_00
funct7_FCVT_S_LU  = 0x68   :: InstrField  -- 7'b_11_010_00

funct7_FCVT_S_D   = 0x20   :: InstrField  -- 7'b_01_000_00
funct7_FCVT_D_S   = 0x21   :: InstrField  -- 7'b_01_000_01
funct7_FCVT_W_D   = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_WU_D  = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_D_W   = 0x69   :: InstrField  -- 7'b_11_010_01
funct7_FCVT_D_WU  = 0x69   :: InstrField  -- 7'b_11_010_01

funct7_FCVT_L_D   = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_LU_D  = 0x61   :: InstrField  -- 7'b_11_000_01
funct7_FCVT_D_L   = 0x69   :: InstrField  -- 7'b_11_010_00
funct7_FCVT_D_LU  = 0x69   :: InstrField  -- 7'b_11_010_00


-- 'D' extensions OPs: FADD, FSUB, FMUL, FDIV, FMIN, FMAX, FSQRT
spec_D_OP                  :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_OP                  mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FADD_D  = (funct7 == funct7_FADD_D)
    is_FSUB_D  = (funct7 == funct7_FSUB_D)
    is_FMUL_D  = (funct7 == funct7_FMUL_D)
    is_FDIV_D  = (funct7 == funct7_FDIV_D)
    is_FSQRT_D = (funct7 == funct7_FSQRT_D)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FADD_D
                    || is_FSUB_D
                    || is_FMUL_D
                    || is_FDIV_D
                    || is_FSQRT_D)
                && (is_F && is_D)
                && rmIsLegal)

    -- Semantics
    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)
    rs2_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs2)

    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rm_val  = frm_to_RoundingMode frmVal

    -- Do the operations using the softfloat functions
    fpuRes | is_FADD_D  = f64Add  rm_val  rs1_val  rs2_val
           | is_FSUB_D  = f64Sub  rm_val  rs1_val  rs2_val
           | is_FMUL_D  = f64Mul  rm_val  rs1_val  rs2_val
           | is_FDIV_D  = f64Div  rm_val  rs1_val  rs2_val
           | is_FSQRT_D = f64Sqrt rm_val  rs1_val

    -- Extract the results and the flags
    rd_val = extractRdDPResult fpuRes
    fflags = extractFFlagsDPResult fpuRes

    mstate1 = finish_frd_fflags_and_pc_plus_4 mstate rd rd_val fflags False
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FSGNJ, FSGNJN, FSGNJX
spec_D_FSGNJ               :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_FSGNJ               mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)

    is_F          = (misa_flag misa 'F')
    is_D          = (misa_flag misa 'D')

    is_FSGNJ_D    = (funct7 == funct7_FSGNJ_D) && (rm == 0x0)
    is_FSGNJN_D   = (funct7 == funct7_FSGNJ_D) && (rm == 0x1)
    is_FSGNJX_D   = (funct7 == funct7_FSGNJ_D) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FSGNJ_D
                    || is_FSGNJN_D
                    || is_FSGNJX_D)
                && (is_F && is_D))

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the components of the source values
    (s1, e1, m1) = extractFromDP  rs1_val
    (s2, e2, m2) = extractFromDP  rs2_val

    rd_val | is_FSGNJ_D    = composeDP   s2             e1  m1
           | is_FSGNJN_D   = composeDP  (xor  s2  0x1)  e1  m1
           | is_FSGNJX_D   = composeDP  (xor  s2  s1)   e1  m1


    -- No exceptions are signalled by these operations
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  0x0  False
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FCVT
spec_D_FCVT                :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_FCVT                mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)
    rv            = mstate_rv_read  mstate

    is_F          = (misa_flag misa 'F')
    is_D          = (misa_flag misa 'D')
    
    is_FCVT_W_D   =    (funct7 == funct7_FCVT_W_D)
                    && (rs2 == 0)
    is_FCVT_WU_D  =    (funct7 == funct7_FCVT_WU_D)
                    && (rs2 == 1)
    is_FCVT_L_D   =    (funct7 == funct7_FCVT_L_D)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_LU_D  =    (funct7 == funct7_FCVT_LU_D)
                    && (rs2 == 3)
                    && (rv == RV64)
    is_FCVT_D_W   =    (funct7 == funct7_FCVT_D_W)
                    && (rs2 == 0)
    is_FCVT_D_WU  =    (funct7 == funct7_FCVT_D_WU)
                    && (rs2 == 1)
    is_FCVT_D_L   =    (funct7 == funct7_FCVT_D_L)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_D_LU  =    (funct7 == funct7_FCVT_D_LU)
                    && (rs2 == 3)
                    && (rv == RV64)
    is_FCVT_D_S   =    (funct7 == funct7_FCVT_D_S)
                    && (rs2 == 0)
    is_FCVT_S_D   =    (funct7 == funct7_FCVT_S_D)
                    && (rs2 == 1)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FCVT_W_D 
                    || is_FCVT_WU_D
                    || is_FCVT_L_D 
                    || is_FCVT_LU_D
                    || is_FCVT_D_W 
                    || is_FCVT_D_WU
                    || is_FCVT_D_L 
                    || is_FCVT_D_LU
                    || is_FCVT_D_S
                    || is_FCVT_S_D) 
                && (is_F && is_D)
                && rmIsLegal)

    destInGPR   =    is_FCVT_W_D
                  || is_FCVT_WU_D
                  || is_FCVT_L_D
                  || is_FCVT_LU_D

    -- Semantics
    xlen        | (rv == RV64) = 64
                | (rv == RV32) = 32

    frs1_val    = mstate_fpr_read  mstate  rs1
    frs1_val_sp = unboxSP  (mstate_fpr_read  mstate  rs1)
    grs1_val    = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)

    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rm_val  = frm_to_RoundingMode frmVal

    -- Do the operations using the softfloat functions where a FPR is the dest
    frdVal | is_FCVT_D_L   = extractRdDPResult  (i64ToF64   rm_val  (cvt_Integer_to_Int64   grs1_val))
           | is_FCVT_D_LU  = extractRdDPResult  (ui64ToF64  rm_val  (cvt_Integer_to_Word64  grs1_val))
           | is_FCVT_D_W   = extractRdDPResult  (i32ToF64   rm_val  (cvt_Integer_to_Int32   grs1_val))
           | is_FCVT_D_WU  = extractRdDPResult  (ui32ToF64  rm_val  (cvt_Integer_to_Word32  grs1_val))
           | is_FCVT_D_S   = extractRdDPResult  (f32ToF64   rm_val  (cvt_Integer_to_Word32  frs1_val_sp))
           | is_FCVT_S_D   = extractRdSPResult  (f64ToF32   rm_val  (cvt_Integer_to_Word64  frs1_val))

    -- Do the operations using the softfloat functions where a GPR is the dest
    grdVal | is_FCVT_L_D   = extractRdLResult   (f64ToI64   rm_val  (cvt_Integer_to_Word64  frs1_val))
           | is_FCVT_LU_D  = extractRdLUResult  (f64ToUi64  rm_val  (cvt_Integer_to_Word64  frs1_val))
           | is_FCVT_W_D   = extractRdWResult   (f64ToI32   rm_val  (cvt_Integer_to_Word64  frs1_val))
           | is_FCVT_WU_D  = extractRdWUResult  (f64ToUi32  rm_val  (cvt_Integer_to_Word64  frs1_val))

    -- Extract the flags for the operations which update FPR
    fflags | is_FCVT_D_L   = extractFFlagsDPResult  (i64ToF64   rm_val  (cvt_Integer_to_Int64   grs1_val))
           | is_FCVT_D_LU  = extractFFlagsDPResult  (ui64ToF64  rm_val  (cvt_Integer_to_Word64  grs1_val))
           | is_FCVT_D_W   = extractFFlagsDPResult  (i32ToF64   rm_val  (cvt_Integer_to_Int32   grs1_val))
           | is_FCVT_D_WU  = extractFFlagsDPResult  (ui32ToF64  rm_val  (cvt_Integer_to_Word32  grs1_val))
           | is_FCVT_D_S   = extractFFlagsDPResult  (f32ToF64   rm_val  (cvt_Integer_to_Word32  frs1_val_sp))
           | is_FCVT_S_D   = extractFFlagsSPResult  (f64ToF32   rm_val  (cvt_Integer_to_Word64  frs1_val))
           | is_FCVT_L_D   = extractFFlagsLResult   (f64ToI64   rm_val  (cvt_Integer_to_Word64  frs1_val))
           | is_FCVT_LU_D  = extractFFlagsLUResult  (f64ToUi64  rm_val  (cvt_Integer_to_Word64  frs1_val))
           | is_FCVT_W_D   = extractFFlagsWResult   (f64ToI32   rm_val  (cvt_Integer_to_Word64  frs1_val))
           | is_FCVT_WU_D  = extractFFlagsWUResult  (f64ToUi32  rm_val  (cvt_Integer_to_Word64  frs1_val))

    mstate1 = if (destInGPR) then
                finish_grd_fflags_and_pc_plus_4  mstate  rd  grdVal  fflags
              else
                finish_frd_fflags_and_pc_plus_4  mstate  rd  frdVal  fflags  is_FCVT_S_D
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FMIN, FMAX
spec_D_MIN                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_MIN                 mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMIN_D  = (funct7 == funct7_FMIN_D) && (rm == 0x0)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMIN_D
                && (is_F && is_D))

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs1_lt_rs2, fflags) = f64IsLE  rs1_val  rs2_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = f64IsSNaN     rs1_val
    rs2IsSNaN = f64IsSNaN     rs2_val

    rs1IsQNaN = f64IsQNaN     rs1_val
    rs2IsQNaN = f64IsQNaN     rs2_val

    rs1IsPos0 = f64IsPosZero  rs1_val
    rs2IsPos0 = f64IsPosZero  rs2_val

    rs1IsNeg0 = f64IsNegZero  rs1_val
    rs2IsNeg0 = f64IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN64
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN64
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs1_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs2_val
           | rs1_lt_rs2                = rs1_val
           | (not rs1_lt_rs2)          = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  False
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FEQ, FLT, FLE
spec_D_CMP                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_CMP                 mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FLE_D   = (funct7 == funct7_FCMP_D) && (rm == 0x0)
    is_FLT_D   = (funct7 == funct7_FCMP_D) && (rm == 0x1)
    is_FEQ_D   = (funct7 == funct7_FCMP_D) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (is_FEQ_D || is_FLT_D || is_FLE_D)
                && (is_F && is_D))

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) | (is_FEQ_D) = f64IsEQQ  rs1_val  rs2_val
                          | (is_FLT_D) = f64IsLT   rs1_val  rs2_val  False
                          | (is_FLE_D) = f64IsLE   rs1_val  rs2_val  False

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = f64IsSNaN     rs1_val
    rs2IsSNaN = f64IsSNaN     rs2_val

    rs1IsQNaN = f64IsQNaN     rs1_val
    rs2IsQNaN = f64IsQNaN     rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags
  in
    (is_legal, mstate1)


-- 'D' extensions OPs:  FMAX
spec_D_MAX                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_MAX                 mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMIN_D  = (funct7 == funct7_FMIN_D) && (rm == 0x0)
    is_FMAX_D  = (funct7 == funct7_FMAX_D) && (rm == 0x1)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMAX_D
                && (is_F && is_D))

    -- Semantics
    rs1_val = mstate_fpr_read  mstate  rs1
    rs2_val = mstate_fpr_read  mstate  rs2

    -- Extract the result of the operation and the flags
    (rs2_lt_rs1, fflags) = f64IsLE rs2_val  rs1_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = f64IsSNaN     rs1_val
    rs2IsSNaN = f64IsSNaN     rs2_val

    rs1IsQNaN = f64IsQNaN     rs1_val
    rs2IsQNaN = f64IsQNaN     rs2_val

    rs1IsPos0 = f64IsPosZero  rs1_val
    rs2IsPos0 = f64IsPosZero  rs2_val

    rs1IsNeg0 = f64IsNegZero  rs1_val
    rs2IsNeg0 = f64IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN64
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN64
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs2_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs1_val
           | rs2_lt_rs1 = rs1_val
           | (not rs2_lt_rs1) = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  False
  in
    (is_legal, mstate1)


-- ================================================================
-- 'D' extensions OPs: FMADD, FMSUB, FNMADD, FNMSUB, 
opcode_FMADD_OP   = 0x43   :: InstrField  -- 7'b_10_000_11
opcode_FMSUB_OP   = 0x47   :: InstrField  -- 7'b_10_001_11
opcode_FNMSUB_OP  = 0x4B   :: InstrField  -- 7'b_10_010_11
opcode_FNMADD_OP  = 0x4F   :: InstrField  -- 7'b_10_011_11
spec_D_FMOP                :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_FMOP                mstate  instr =
  let
    -- Instr fields: R4-type
    (rs3, funct2, rs2, rs1, rm, rd, opcode) = ifields_R4_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMADD_D  = (opcode == opcode_FMADD_OP)  && (funct2 == 0x1)
    is_FMSUB_D  = (opcode == opcode_FMSUB_OP)  && (funct2 == 0x1)
    is_FNMADD_D = (opcode == opcode_FNMADD_OP) && (funct2 == 0x1)
    is_FNMSUB_D = (opcode == opcode_FNMSUB_OP) && (funct2 == 0x1)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (is_F && is_D)
                && (   is_FMADD_D
                    || is_FMSUB_D
                    || is_FNMADD_D
                    || is_FNMSUB_D)
                && rmIsLegal)

    -- Semantics
    rs1_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs1)
    rs2_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs2)
    rs3_val = cvt_Integer_to_Word64  (mstate_fpr_read  mstate  rs3)

    neg_rs1_val = cvt_Integer_to_Word64  (negateD  (mstate_fpr_read  mstate  rs1))
    neg_rs2_val = cvt_Integer_to_Word64  (negateD  (mstate_fpr_read  mstate  rs2))
    neg_rs3_val = cvt_Integer_to_Word64  (negateD  (mstate_fpr_read  mstate  rs3))

    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rm_val  = frm_to_RoundingMode frmVal

    -- Extract the result of the operation and the flags
    fpuRes | is_FMADD_D    = f64MulAdd  rm_val  rs1_val      rs2_val  rs3_val
           | is_FMSUB_D    = f64MulAdd  rm_val  rs1_val      rs2_val  neg_rs3_val
           | is_FNMSUB_D   = f64MulAdd  rm_val  neg_rs1_val  rs2_val  rs3_val
           | is_FNMADD_D   = f64MulAdd  rm_val  neg_rs1_val  rs2_val  neg_rs3_val

    rd_val = extractRdDPResult  fpuRes
    fflags = extractFFlagsDPResult  fpuRes

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  False
  in
    (is_legal, mstate1)


-- RV64-'D' extension Ops: FMV.D.X and FMV.X.D
funct7_FMV_X_D    = 0x71   :: InstrField  -- 7'b_11_100_01
funct7_FMV_D_X    = 0x79   :: InstrField  -- 7'b_11_110_01
spec_D_FMV                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_FMV                 mstate  instr = 
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_FMV_X_D = (funct7 == funct7_FMV_X_D)
    is_FMV_D_X = (funct7 == funct7_FMV_D_X)
    rmIsLegal  = (rm == 0x0)
    rv         = mstate_rv_read  mstate

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FMV_X_D
                    || is_FMV_D_X)
                && (is_F && is_D)
                && (rv == RV64)
                && rmIsLegal)

    -- Semantics
    frs1_val = mstate_fpr_read  mstate  rs1
    grs1_val = mstate_gpr_read  mstate  rs1

    mstate1  = if (is_FMV_X_D) then
                 finish_rd_and_pc_plus_4  mstate  rd  frs1_val
               else
                 finish_frd_and_pc_plus_4  mstate  rd  grs1_val  False
  in
    (is_legal, mstate1)


-- 'D' extension Ops: FCLASS
funct7_FCLASS_D  = 0x71 :: InstrField  -- 7'b_11_100_01
spec_D_FCLASS           :: Machine_State -> Instr -> (Bool, Machine_State)
spec_D_FCLASS           mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_FCLASS  = (funct7 == funct7_FCLASS_D)
    rmIsLegal  = (rm == 0x1)
    is_legal   = (   (opcode == opcode_FD_OP)
                  && is_FCLASS
                  && (is_F && is_D)
                  && rmIsLegal)

    -- Semantics
    frs1_val = mstate_fpr_read  mstate  rs1
    
    -- Classify the frs1_val
    is_NegInf     = f64IsNegInf        frs1_val
    is_NegNorm    = f64IsNegNorm       frs1_val
    is_NegSubNorm = f64IsNegSubNorm    frs1_val
    is_NegZero    = f64IsNegZero       frs1_val
    is_PosZero    = f64IsPosZero       frs1_val
    is_PosSubNorm = f64IsPosSubNorm    frs1_val
    is_PosNorm    = f64IsPosNorm       frs1_val
    is_PosInf     = f64IsPosInf        frs1_val
    is_SNaN       = f64IsSNaN          frs1_val
    is_QNaN       = f64IsQNaN          frs1_val

    -- Form the rd based on the above clasification
    rd_val  = 0x0 :: Integer
    rd_val' | is_NegInf       = rd_val .|. shiftL  1  fclass_negInf_bitpos
            | is_NegNorm      = rd_val .|. shiftL  1  fclass_negNorm_bitpos
            | is_NegSubNorm   = rd_val .|. shiftL  1  fclass_negSubNorm_bitpos
            | is_NegZero      = rd_val .|. shiftL  1  fclass_negZero_bitpos
            | is_PosZero      = rd_val .|. shiftL  1  fclass_posZero_bitpos
            | is_PosSubNorm   = rd_val .|. shiftL  1  fclass_posSubNorm_bitpos
            | is_PosNorm      = rd_val .|. shiftL  1  fclass_posNorm_bitpos
            | is_PosInf       = rd_val .|. shiftL  1  fclass_posInf_bitpos
            | is_SNaN         = rd_val .|. shiftL  1  fclass_SNaN_bitpos
            | is_QNaN         = rd_val .|. shiftL  1  fclass_QNaN_bitpos
    
    -- No exceptions are signalled by this operation
    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val'
  in
    (is_legal, mstate1)


-- 'F' extensions OPs: FADD, FSUB, FMUL, FDIV, FSQRT
spec_F_OP                  :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_OP                  mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FADD_S  = (funct7 == funct7_FADD_S)
    is_FSUB_S  = (funct7 == funct7_FSUB_S)
    is_FMUL_S  = (funct7 == funct7_FMUL_S)
    is_FDIV_S  = (funct7 == funct7_FDIV_S)
    is_FSQRT_S = (funct7 == funct7_FSQRT_S)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FADD_S
                    || is_FSUB_S
                    || is_FMUL_S
                    || is_FDIV_S
                    || is_FSQRT_S)
                && is_F
                && rmIsLegal)

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  mstate  rs1))
    rs2_val = cvt_Integer_to_Word32  (unboxSP  (mstate_fpr_read  mstate  rs2))

    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rm_val  = frm_to_RoundingMode  frmVal

    -- Extract the result of the operation and the flags
    fpuRes | is_FADD_S  = f32Add  rm_val  rs1_val  rs2_val
           | is_FSUB_S  = f32Sub  rm_val  rs1_val  rs2_val
           | is_FMUL_S  = f32Mul  rm_val  rs1_val  rs2_val
           | is_FDIV_S  = f32Div  rm_val  rs1_val  rs2_val
           | is_FSQRT_S = f32Sqrt rm_val  rs1_val

    rd_val = extractRdSPResult fpuRes
    fflags = extractFFlagsSPResult fpuRes

    mstate1 = finish_frd_fflags_and_pc_plus_4 mstate rd rd_val fflags True
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FSGNJ, FSGNJN, FSGNJX
spec_F_FSGNJ               :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_FSGNJ               mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)

    is_F          = (misa_flag misa 'F')

    is_FSGNJ_S    = (funct7 == funct7_FSGNJ_S) && (rm == 0x0)
    is_FSGNJN_S   = (funct7 == funct7_FSGNJ_S) && (rm == 0x1)
    is_FSGNJX_S   = (funct7 == funct7_FSGNJ_S) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FSGNJ_S
                    || is_FSGNJN_S
                    || is_FSGNJX_S)
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

    -- Extract the components of the source values
    (s1, e1, m1) = extractFromSP  rs1_val
    (s2, e2, m2) = extractFromSP  rs2_val

    rd_val | is_FSGNJ_S    = composeSP   s2             e1  m1
           | is_FSGNJN_S   = composeSP  (xor  s2  0x1)  e1  m1
           | is_FSGNJX_S   = composeSP  (xor  s2  s1)   e1  m1


    -- No exceptions are signalled by these operations
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  0x0  True
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FCVT
spec_F_FCVT                :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_FCVT                mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa          = (mstate_csr_read mstate  csr_addr_misa)
    rv            = mstate_rv_read  mstate

    is_F          = (misa_flag misa 'F')
    
    is_FCVT_W_S   =    (funct7 == funct7_FCVT_W_S)
                    && (rs2 == 0)
    is_FCVT_WU_S  =    (funct7 == funct7_FCVT_WU_S)
                    && (rs2 == 1)
    is_FCVT_L_S   =    (funct7 == funct7_FCVT_L_S)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_LU_S  =    (funct7 == funct7_FCVT_LU_S)
                    && (rs2 == 3)
                    && (rv == RV64)
    is_FCVT_S_W   =    (funct7 == funct7_FCVT_S_W)
                    && (rs2 == 0)
    is_FCVT_S_WU  =    (funct7 == funct7_FCVT_S_WU)
                    && (rs2 == 1)
    is_FCVT_S_L   =    (funct7 == funct7_FCVT_S_L)
                    && (rs2 == 2)
                    && (rv == RV64)
    is_FCVT_S_LU  =    (funct7 == funct7_FCVT_S_LU)
                    && (rs2 == 3)
                    && (rv == RV64)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FCVT_W_S 
                    || is_FCVT_WU_S
                    || is_FCVT_L_S 
                    || is_FCVT_LU_S
                    || is_FCVT_S_W 
                    || is_FCVT_S_WU
                    || is_FCVT_S_L 
                    || is_FCVT_S_LU)
                && (is_F)
                && rmIsLegal)

    destInGPR   =    is_FCVT_W_S
                  || is_FCVT_WU_S
                  || is_FCVT_L_S
                  || is_FCVT_LU_S

    -- Semantics
    xlen        | (rv == RV64) = 64
                | (rv == RV32) = 32

    frs1_val    = unboxSP  (mstate_fpr_read  mstate  rs1)
    grs1_val    = cvt_2s_comp_to_Integer  xlen  (mstate_gpr_read  mstate  rs1)

    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rm_val  = frm_to_RoundingMode frmVal

    -- Do the operations using the softfloat functions where a FPR is the dest
    frdVal | is_FCVT_S_L   = extractRdSPResult  (i64ToF32   rm_val  (cvt_Integer_to_Int64   grs1_val))
           | is_FCVT_S_LU  = extractRdSPResult  (ui64ToF32  rm_val  (cvt_Integer_to_Word64  grs1_val))
           | is_FCVT_S_W   = extractRdSPResult  (i32ToF32   rm_val  (cvt_Integer_to_Int32   grs1_val))
           | is_FCVT_S_WU  = extractRdSPResult  (ui32ToF32  rm_val  (cvt_Integer_to_Word32  grs1_val))

    -- Do the operations using the softfloat functions where a GPR is the dest
    grdVal | is_FCVT_L_S   = extractRdLResult   (f32ToI64   rm_val  (cvt_Integer_to_Word32  frs1_val))
           | is_FCVT_LU_S  = extractRdLUResult  (f32ToUi64  rm_val  (cvt_Integer_to_Word32  frs1_val))
           | is_FCVT_W_S   = extractRdWResult   (f32ToI32   rm_val  (cvt_Integer_to_Word32  frs1_val))
           | is_FCVT_WU_S  = extractRdWUResult  (f32ToUi32  rm_val  (cvt_Integer_to_Word32  frs1_val))

    -- Extract the flags for the operations which update FPR
    fflags | is_FCVT_S_L   = extractFFlagsSPResult  (i64ToF32   rm_val  (cvt_Integer_to_Int64   grs1_val))
           | is_FCVT_S_LU  = extractFFlagsSPResult  (ui64ToF32  rm_val  (cvt_Integer_to_Word64  grs1_val))
           | is_FCVT_S_W   = extractFFlagsSPResult  (i32ToF32   rm_val  (cvt_Integer_to_Int32   grs1_val))
           | is_FCVT_S_WU  = extractFFlagsSPResult  (ui32ToF32  rm_val  (cvt_Integer_to_Word32  grs1_val))
           | is_FCVT_L_S   = extractFFlagsLResult   (f32ToI64   rm_val  (cvt_Integer_to_Word32  frs1_val))
           | is_FCVT_LU_S  = extractFFlagsLUResult  (f32ToUi64  rm_val  (cvt_Integer_to_Word32  frs1_val))
           | is_FCVT_W_S   = extractFFlagsWResult   (f32ToI32   rm_val  (cvt_Integer_to_Word32  frs1_val))
           | is_FCVT_WU_S  = extractFFlagsWUResult  (f32ToUi32  rm_val  (cvt_Integer_to_Word32  frs1_val))

    mstate1 = if (destInGPR) then
                finish_grd_fflags_and_pc_plus_4  mstate  rd  grdVal  fflags
              else
                finish_frd_fflags_and_pc_plus_4  mstate rd frdVal fflags True
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FMIN
spec_F_MIN                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_MIN                 mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')

    is_FMIN_S  = (funct7 == funct7_FMIN_S) && (rm == 0x0)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMIN_S
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

    -- Extract the result of the operation and the flags
    (rs1_lt_rs2, fflags) = f32IsLE  rs1_val  rs2_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = f32IsSNaN  rs1_val
    rs2IsSNaN = f32IsSNaN  rs2_val

    rs1IsQNaN = f32IsQNaN  rs1_val
    rs2IsQNaN = f32IsQNaN  rs2_val

    rs1IsPos0 = f32IsPosZero  rs1_val
    rs2IsPos0 = f32IsPosZero  rs2_val

    rs1IsNeg0 = f32IsNegZero  rs1_val
    rs2IsNeg0 = f32IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN32
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN32
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs1_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs2_val
           | rs1_lt_rs2                = rs1_val
           | (not rs1_lt_rs2)          = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  True
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FEQ, FLT, FLE
spec_F_CMP                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_CMP                 mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')

    is_FLE_S   = (funct7 == funct7_FCMP_S) && (rm == 0x0)
    is_FLT_S   = (funct7 == funct7_FCMP_S) && (rm == 0x1)
    is_FEQ_S   = (funct7 == funct7_FCMP_S) && (rm == 0x2)

    is_legal = (   (opcode == opcode_FD_OP)
                && (is_FEQ_S || is_FLT_S || is_FLE_S)
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

    -- Extract the result of the operation and the flags
    (rs1_cmp_rs2, fflags) | (is_FEQ_S) = f32IsEQQ  rs1_val  rs2_val
                          | (is_FLT_S) = f32IsLT   rs1_val  rs2_val  False
                          | (is_FLE_S) = f32IsLE   rs1_val  rs2_val  False

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = f32IsSNaN  rs1_val
    rs2IsSNaN = f32IsSNaN  rs2_val

    rs1IsQNaN = f32IsQNaN  rs1_val
    rs2IsQNaN = f32IsQNaN  rs2_val

    rd_val | (rs1IsSNaN || rs2IsSNaN)  = 0
           | (rs1IsQNaN || rs2IsQNaN)  = 0
           | rs1_cmp_rs2               = 1
           | (not rs1_cmp_rs2)         = 0

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_grd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags
  in
    (is_legal, mstate1)


-- 'F' extensions OPs:  FMAX
spec_F_MAX                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_MAX                 mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')

    is_FMAX_S  = (funct7 == funct7_FMAX_S) && (rm == 0x1)

    is_legal = (   (opcode == opcode_FD_OP)
                && is_FMAX_S
                && (is_F))

    -- Semantics
    -- Check if the values are correctly NaN-Boxed. If they are correctly
    -- NaN-boxed, the lower 32-bits will be used as rs1 and rs2 values. If they
    -- are not correctly NaN-boxed, the value will be treated as "32-bit
    -- canonical NaN"
    rs1_val = unboxSP (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP (mstate_fpr_read  mstate  rs2)

    -- Extract the result of the operation and the flags
    (rs2_lt_rs1, fflags) = f32IsLE  rs2_val  rs1_val  True

    -- Check if either rs1 or rs2 is a s-NaN or a q-NaN
    rs1IsSNaN = f32IsSNaN  rs1_val
    rs2IsSNaN = f32IsSNaN  rs2_val

    rs1IsQNaN = f32IsQNaN  rs1_val
    rs2IsQNaN = f32IsQNaN  rs2_val

    rs1IsPos0 = f32IsPosZero  rs1_val
    rs2IsPos0 = f32IsPosZero  rs2_val

    rs1IsNeg0 = f32IsNegZero  rs1_val
    rs2IsNeg0 = f32IsNegZero  rs2_val

    rd_val | (rs1IsSNaN && rs2IsSNaN)  = canonicalNaN32
           | rs1IsSNaN                 = rs2_val
           | rs2IsSNaN                 = rs1_val
           | (rs1IsQNaN && rs2IsQNaN)  = canonicalNaN32
           | rs1IsQNaN                 = rs2_val
           | rs2IsQNaN                 = rs1_val
           | (rs1IsNeg0 && rs2IsPos0)  = rs2_val
           | (rs2IsNeg0 && rs1IsPos0)  = rs1_val
           | rs2_lt_rs1                = rs1_val
           | (not rs2_lt_rs1)          = rs2_val

    -- Exceptions are signalled by these operations only if one of the arguments
    -- is a SNaN. This is a quiet operation
    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  True
  in
    (is_legal, mstate1)


-- ================================================================
-- 'F' extensions OPs: FMADD, FMSUB, FNMADD, FNMSUB, 
spec_F_FMOP                :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_FMOP                mstate  instr =
  let
    -- Instr fields: R4-type
    (rs3, funct2, rs2, rs1, rm, rd, opcode) = ifields_R4_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')

    is_FMADD_S  = (opcode == opcode_FMADD_OP)  && (funct2 == 0)
    is_FMSUB_S  = (opcode == opcode_FMSUB_OP)  && (funct2 == 0)
    is_FNMADD_S = (opcode == opcode_FNMADD_OP) && (funct2 == 0)
    is_FNMSUB_S = (opcode == opcode_FNMSUB_OP) && (funct2 == 0)

    (frmVal, rmIsLegal) = rounding_mode_check  rm  (mstate_csr_read  mstate  csr_addr_frm)

    is_legal = (   (is_F)
                && (   is_FMADD_S
                    || is_FMSUB_S
                    || is_FNMADD_S
                    || is_FNMSUB_S)
                && rmIsLegal)

    -- Semantics
    rs1_val = unboxSP  (mstate_fpr_read  mstate  rs1)
    rs2_val = unboxSP  (mstate_fpr_read  mstate  rs2)
    rs3_val = unboxSP  (mstate_fpr_read  mstate  rs3)

    rs1_val_32 = cvt_Integer_to_Word32  rs1_val
    rs2_val_32 = cvt_Integer_to_Word32  rs2_val
    rs3_val_32 = cvt_Integer_to_Word32  rs3_val

    neg_rs1_val_32 = cvt_Integer_to_Word32  (negateS  rs1_val)
    neg_rs2_val_32 = cvt_Integer_to_Word32  (negateS  rs2_val)
    neg_rs3_val_32 = cvt_Integer_to_Word32  (negateS  rs3_val)

    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rm_val  = frm_to_RoundingMode frmVal

    -- Extract the result of the operation and the flags
    fpuRes | is_FMADD_S    = f32MulAdd  rm_val  rs1_val_32      rs2_val_32  rs3_val_32
           | is_FMSUB_S    = f32MulAdd  rm_val  rs1_val_32      rs2_val_32  neg_rs3_val_32
           | is_FNMSUB_S   = f32MulAdd  rm_val  neg_rs1_val_32  rs2_val_32  rs3_val_32
           | is_FNMADD_S   = f32MulAdd  rm_val  neg_rs1_val_32  rs2_val_32  neg_rs3_val_32

    rd_val = extractRdSPResult  fpuRes
    fflags = extractFFlagsSPResult  fpuRes

    mstate1 = finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  True
  in
    (is_legal, mstate1)


-- RV32/64 - 'F' extension Ops: FMV.W.X and FMV.X.W
funct7_FMV_X_W    = 0x70   :: InstrField  -- 7'b_11_100_00
funct7_FMV_W_X    = 0x78   :: InstrField  -- 7'b_11_110_00
spec_F_FMV                 :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_FMV                 mstate  instr = 
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_D       = (misa_flag misa 'D')
    is_FMV_X_W = (funct7 == funct7_FMV_X_W)
    is_FMV_W_X = (funct7 == funct7_FMV_W_X)
    rmIsLegal  = (rm == 0x0)
    rv         = mstate_rv_read  mstate

    is_legal = (   (opcode == opcode_FD_OP)
                && (   is_FMV_X_W
                    || is_FMV_W_X)
                && (is_F)
                && rmIsLegal)

    -- Semantics
    frs1_val = mstate_fpr_read  mstate  rs1
    grs1_val = mstate_gpr_read  mstate  rs1

    -- FMV_X_W
    -- GPR value is sign-extended version of lower 32-bits of FPR contents
    frs1_val' = sign_extend  32  64  (bitSlice frs1_val  31  0)

    mstate1  = if (is_FMV_X_W) then
                 finish_rd_and_pc_plus_4  mstate  rd  frs1_val'
               else
                 finish_frd_and_pc_plus_4  mstate  rd  grs1_val  True
  in
    (is_legal, mstate1)


-- 'F' extension Ops: FCLASS
funct7_FCLASS_F = 0x70  :: InstrField  -- 7'b_11_100_01
spec_F_FCLASS           :: Machine_State -> Instr -> (Bool, Machine_State)
spec_F_FCLASS           mstate  instr =
  let
    -- Instr fields: R-type
    (funct7, rs2, rs1, rm, rd, opcode) = ifields_R_type   instr

    -- Decode and legality check
    misa       = (mstate_csr_read mstate  csr_addr_misa)

    is_F       = (misa_flag misa 'F')
    is_FCLASS  = (funct7 == funct7_FCLASS_F)
    rmIsLegal  = (rm == 0x1)
    is_legal   = (   (opcode == opcode_FD_OP)
                  && is_FCLASS
                  && (is_F)
                  && rmIsLegal)

    -- Semantics
    frs1_val = unboxSP  (mstate_fpr_read  mstate  rs1)
    
    -- Classify the frs1_val
    is_NegInf     = f32IsNegInf        frs1_val
    is_NegNorm    = f32IsNegNorm       frs1_val
    is_NegSubNorm = f32IsNegSubNorm    frs1_val
    is_NegZero    = f32IsNegZero       frs1_val
    is_PosZero    = f32IsPosZero       frs1_val
    is_PosSubNorm = f32IsPosSubNorm    frs1_val
    is_PosNorm    = f32IsPosNorm       frs1_val
    is_PosInf     = f32IsPosInf        frs1_val
    is_SNaN       = f32IsSNaN          frs1_val
    is_QNaN       = f32IsQNaN          frs1_val

    -- Form the rd based on the above clasification
    rd_val  = 0x0 :: Integer
    rd_val' | is_NegInf       = rd_val .|. shiftL  1  fclass_negInf_bitpos
            | is_NegNorm      = rd_val .|. shiftL  1  fclass_negNorm_bitpos
            | is_NegSubNorm   = rd_val .|. shiftL  1  fclass_negSubNorm_bitpos
            | is_NegZero      = rd_val .|. shiftL  1  fclass_negZero_bitpos
            | is_PosZero      = rd_val .|. shiftL  1  fclass_posZero_bitpos
            | is_PosSubNorm   = rd_val .|. shiftL  1  fclass_posSubNorm_bitpos
            | is_PosNorm      = rd_val .|. shiftL  1  fclass_posNorm_bitpos
            | is_PosInf       = rd_val .|. shiftL  1  fclass_posInf_bitpos
            | is_SNaN         = rd_val .|. shiftL  1  fclass_SNaN_bitpos
            | is_QNaN         = rd_val .|. shiftL  1  fclass_QNaN_bitpos
    
    -- No exceptions are signalled by this operation
    mstate1 = finish_rd_and_pc_plus_4  mstate  rd  rd_val'
  in
    (is_legal, mstate1)


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
{-# INLINE finish_rd_and_pc_plus_4 #-}
finish_rd_and_pc_plus_4 :: Machine_State -> GPR_Addr -> Integer -> Machine_State
finish_rd_and_pc_plus_4  mstate  rd  rd_val =
  let mstate1 = mstate_gpr_write  mstate  rd  rd_val
      pc      = mstate_pc_read    mstate1
      mstate2 = mstate_pc_write   mstate1  (pc + 4)
      mstate3 = incr_minstret     mstate2
  in
    mstate3
                                                               -- \end_latex{finish_rd_and_pc_plus_4}

-- Update GPR.RD, CSR.FFlags, increment PC by 4, increment INSTRET       \begin_latex{finish_grd_fflags_and_pc_plus_4}
{-# INLINE finish_grd_fflags_and_pc_plus_4 #-}
finish_grd_fflags_and_pc_plus_4 :: Machine_State -> GPR_Addr -> Integer -> Integer -> Machine_State
finish_grd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  =
  let
    mstate1 = mstate_fcsr_fflags_update  mstate  fflags
    mstate2 = finish_rd_and_pc_plus_4  mstate1  rd  rd_val
  in
    mstate2
                                                               -- \end_latex{finish_grd_fflags_and_pc_plus_4}

-- Update RD, update PC to new value, increment INSTRET
{-# INLINE finish_rd_and_pc #-}
finish_rd_and_pc :: Machine_State -> GPR_Addr -> Integer -> Integer -> Machine_State
finish_rd_and_pc  mstate  rd  rd_val  new_pc =
  let
    mstate1 = mstate_gpr_write  mstate  rd  rd_val
    mstate2 = mstate_pc_write  mstate1  new_pc
    mstate3 = incr_minstret  mstate2
  in
    mstate3


-- Increment PC by 4, increment INSTRET
{-# INLINE finish_pc_plus_4 #-}
finish_pc_plus_4 :: Machine_State -> Machine_State
finish_pc_plus_4  mstate =
  let
    pc      = mstate_pc_read  mstate
    mstate1 = mstate_pc_write  mstate  (pc + 4)
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Update PC to new value
{-# INLINE finish_pc #-}
finish_pc :: Machine_State -> Integer -> Machine_State
finish_pc  mstate  new_pc =
  let
    mstate1 = mstate_pc_write  mstate  new_pc
    mstate2 = incr_minstret  mstate1
  in
    mstate2

-- Update FPU.RD, CSR.FFlags, increment PC by 4, increment INSTRET       \begin_latex{finish_frd_fflags_and_pc_plus_4}
{-# INLINE finish_frd_fflags_and_pc_plus_4 #-}
finish_frd_fflags_and_pc_plus_4 :: Machine_State -> FPR_Addr -> Integer -> Integer -> Bool -> Machine_State
finish_frd_fflags_and_pc_plus_4  mstate  rd  rd_val  fflags  is_n_lt_FLEN =
  let
    mstate1 = mstate_fcsr_fflags_update  mstate  fflags
    mstate2 = finish_frd_and_pc_plus_4  mstate1  rd  rd_val  is_n_lt_FLEN
  in
    mstate2
                                                               -- \end_latex{finish_frd_and_pc_plus_4}

-- Update FPU.FRD, increment PC by 4, increment INSTRET               \begin_latex{finish_frd_and_pc_plus_4}
{-# INLINE finish_frd_and_pc_plus_4 #-}
finish_frd_and_pc_plus_4 :: Machine_State -> FPR_Addr -> Integer -> Bool -> Machine_State
finish_frd_and_pc_plus_4  mstate  rd  rd_val  is_n_lt_FLEN =
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
finish_trap  mstate  exc_code  tval =
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
mstate_upd_on_trap :: Machine_State -> Bool -> Exc_Code -> Integer -> Machine_State
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
               (spec_AMO,               "AMO"),
               (spec_FD_LOAD,           "FD_LOAD"),
               (spec_FD_STORE,          "FD_STORE"),
               (spec_D_OP,              "FPU_D_OP"),
               (spec_D_FCVT,            "FPU_D_CONVERT"),
               (spec_D_FCLASS,          "FPU_D_CLASS"),
               (spec_D_FSGNJ,           "FPU_D_SIGN_CHANGE"),
               (spec_D_CMP,             "FPU_D_COMPARE"),
               (spec_D_MAX,             "FPU_D_MAX"),
               (spec_D_MIN,             "FPU_D_MIN"),
               (spec_D_FMOP,            "FPU_D_MULTIPLY_ACCUMULATE"),
               (spec_D_FMV,             "FPU_D_REG_MOVE"),
               (spec_F_OP,              "FPU_F_OP"),
               (spec_F_FCVT,            "FPU_F_CONVERT"),
               (spec_F_FCLASS,          "FPU_F_CLASS"),
               (spec_F_FSGNJ,           "FPU_F_SIGN_CHANGE"),
               (spec_F_CMP,             "FPU_F_COMPARE"),
               (spec_F_MIN,             "FPU_F_MIN"),
               (spec_F_MAX,             "FPU_F_MAX"),
               (spec_F_FMOP,            "FPU_F_MULTIPLY_ACCUMULATE"),
               (spec_F_FMV,             "FPU_F_REG_MOVE")]

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
                                     tval = instr
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
                                     tval = instr
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
-- Take interrupt if interrupts pending and enabled                   \begin_latex{take_interrupt}

{-# INLINE take_interrupt_if_any #-}
take_interrupt_if_any :: Machine_State -> (Maybe Exc_Code, Machine_State)
take_interrupt_if_any  mstate =
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
-- Check if an interrupt is pending to resume from WFI state

-- Note: this is a weaker condition than the condition of actually
-- taking an interrupt since it is unaffected by MSTATUS.MIE/SIE/UIE
-- and MIDELEG and MEDELEG.

mstate_wfi_resume :: Machine_State -> Bool
mstate_wfi_resume  mstate =
  let
    mip     = mstate_csr_read  mstate  csr_addr_mip
    mie     = mstate_csr_read  mstate  csr_addr_mie
    resume  = ((mip .&. mie) /= 0)
  in
    resume

-- ================================================================

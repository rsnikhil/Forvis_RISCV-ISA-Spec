module ExecuteInstr (executeInstr) where

-- ================================================================
-- This module contains the execution semantics for each RISC-V instruction
-- Each clause below pattern-matches on a different instruction opcode
-- and specifies its semantics.

-- ================================================================
-- Standard Haskell imports

import System.IO
import Data.Int
import Data.List
import Data.Word
import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

import BitManipulation
import ArchState64
import ArchDefs64
import Decode
import CSRFile

-- ================================================================
-- Each opcode of course does something unique, but they all end with
-- a few common actions:
--     - updating the PC with either PC+4 or a new PC
--     - upating the MINSTRET register (number of instructions retired)
--     - updating a CSR
-- These 'exec_end_...' functions encapsulate those standard endings.

-- Every completed instruction increments minstret
incr_minstret :: ArchState64 -> IO (ArchState64)
incr_minstret  astate = do
  let minstret = get_ArchState64_csr  astate  csr_addr_minstret
  set_ArchState64_csr  astate  csr_addr_minstret  (minstret+1)

-- Most common ending: optionally update Rd; incr PC by 4; increment MINSTRET
exec_end_common :: ArchState64 -> Maybe (Register, WordXLEN) -> IO ArchState64
exec_end_common  astate  m_rd_rdval = do
  astate1 <- case m_rd_rdval of
               Just (rd, rd_val) -> set_ArchState64_gpr  astate  rd  rd_val
               Nothing           -> return astate
  let pc   = get_ArchState64_PC  astate1
  astate2 <- set_ArchState64_PC  astate1  (pc + 4)
  incr_minstret  astate2

-- Ending for control transfers: store saved PC in Rd; set PC to new PC; increment MINSTRET
exec_end_jump :: ArchState64 -> Register -> WordXLEN -> WordXLEN -> IO ArchState64
exec_end_jump  astate  rd  save_PC  target_PC = do
  if ((mod  target_PC  4) /= 0)
    then raiseException  astate  0  0
    else do
      astate1 <- set_ArchState64_gpr  astate  rd  save_PC
      astate2 <- set_ArchState64_PC   astate1  target_PC
      incr_minstret astate2

-- Ending for BRANCH instrs: PC = if taken then newPC else PC+4; increment MINSTRET
exec_end_branch :: ArchState64 -> WordXLEN -> Bool -> WordXLEN -> IO ArchState64
exec_end_branch  astate  pc  taken  target_PC = do
  if (taken && (mod target_PC 4 /= 0))
    then
      raiseException  astate  0  0
    else do
      let nextPC = if taken then target_PC else pc + 4
      astate1 <- set_ArchState64_PC  astate  nextPC
      incr_minstret  astate1

-- Ending on traps
-- TODO: Currently stopping execution; should trap instead
exec_end_trap :: ArchState64 -> TrapCause -> IO ArchState64
exec_end_trap  astate  cause = do
  astate1 <- set_ArchState64_csr   astate  csr_addr_mcause  (mk_mcause_from_TrapCause  cause)
  astate2 <- set_ArchState64_stop  astate  (if (cause == TrapCause_Breakpoint) then Stop_Break else Stop_Other)
  return astate2

-- ================================================================
-- 'executeInstr' takes current arch state and a decoded instruction
-- and returns a new arch state after executing that instruction.

executeInstr :: ArchState64 -> Instruction -> IO ArchState64

-- ================================================================
-- RV32I instructions

-- Immediate constants: LUI AUIPC

executeInstr  astate  (Lui rd imm20) = do
  let rd_val = imm20
  exec_end_common  astate  (Just (rd, rd_val))
  
executeInstr  astate  (Auipc rd imm20) = do
  let pc     = get_ArchState64_PC  astate
      rd_val = cvt_s_to_u (imm20 + (cvt_u_to_s  pc))
  exec_end_common  astate  (Just (rd, rd_val))

-- Jumps : JAL JALR

executeInstr  astate  (Jal rd jimm20) = do
  let pc        = get_ArchState64_PC  astate
      save_PC   = pc + 4
      target_PC = cvt_s_to_u  ((cvt_u_to_s  pc) + jimm20)
  exec_end_jump  astate  rd  save_PC  target_PC

executeInstr  astate (Jalr rd rs1 oimm12) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      pc        = get_ArchState64_PC  astate
      save_PC   = pc + 4
      target_PC = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + oimm12)
  exec_end_jump  astate  rd  save_PC  target_PC

-- Branches: BEQ BNE BLT BGE BLTU BGEU

executeInstr  astate  (Beq rs1 rs2 sbimm12) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      pc        = get_ArchState64_PC  astate
      target_PC = cvt_s_to_u  ((cvt_u_to_s  pc) + sbimm12)
  exec_end_branch  astate  pc  (rs2_val == rs2_val)  target_PC

executeInstr  astate  (Bne rs1 rs2 sbimm12) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      pc        = get_ArchState64_PC  astate
      target_PC = cvt_s_to_u  ((cvt_u_to_s  pc) + sbimm12)
  exec_end_branch  astate  pc  (rs1_val /= rs2_val)  target_PC

executeInstr  astate  (Blt rs1 rs2 sbimm12) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs1_val_s = cvt_u_to_s  rs1_val
      rs2_val   = get_ArchState64_gpr  astate  rs2
      rs2_val_s = cvt_u_to_s  rs2_val
      pc        = get_ArchState64_PC  astate
      target_PC = cvt_s_to_u  ((cvt_u_to_s  pc) + sbimm12)
  exec_end_branch  astate  pc  (rs1_val_s < rs2_val_s)  target_PC

executeInstr  astate  (Bge rs1 rs2 sbimm12) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs1_val_s = cvt_u_to_s  rs1_val
      rs2_val   = get_ArchState64_gpr  astate  rs2
      rs2_val_s = cvt_u_to_s  rs2_val
      pc        = get_ArchState64_PC  astate
      target_PC = cvt_s_to_u  ((cvt_u_to_s  pc) + sbimm12)
  exec_end_branch  astate  pc  (rs1_val_s >= rs2_val_s)  target_PC

executeInstr  astate  (Bltu rs1 rs2 sbimm12) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      pc        = get_ArchState64_PC  astate
      target_PC = cvt_s_to_u  ((cvt_u_to_s  pc) + sbimm12)
  exec_end_branch  astate  pc  (rs1_val < rs2_val)  target_PC

executeInstr  astate  (Bgeu rs1 rs2 sbimm12) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      pc        = get_ArchState64_PC  astate
      target_PC = cvt_s_to_u  ((cvt_u_to_s  pc) + sbimm12)
  exec_end_branch  astate  pc  (rs1_val >= rs2_val)  target_PC

-- Loads: LB LH LU LBU LHU

executeInstr  astate  (Lb rd rs1 oimm12) = do
  let rs1_val           = get_ArchState64_gpr  astate  rs1
      eaddr             = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + oimm12)
      (result, astate') = get_ArchState64_mem8  astate  eaddr
  case result of
    LoadResult_Err cause -> exec_end_trap  astate'  cause
    LoadResult_Ok  u8    ->
      do
        let rd_val = signExtend_u8_to_u  u8
        exec_end_common  astate'  (Just (rd, rd_val))

executeInstr  astate  (Lh rd rs1 oimm12) = do
  let rs1_val           = get_ArchState64_gpr  astate  rs1
      eaddr             = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + oimm12)
      (result, astate') = get_ArchState64_mem16  astate  eaddr
  case result of
    LoadResult_Err cause -> exec_end_trap  astate'  cause
    LoadResult_Ok  u16   ->
      do
        let rd_val = signExtend_u16_to_u  u16
        exec_end_common  astate'  (Just (rd, rd_val))

executeInstr  astate  (Lw rd rs1 oimm12) = do
  let rs1_val           = get_ArchState64_gpr  astate  rs1
      eaddr             = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + oimm12)
      (result, astate') = get_ArchState64_mem32  astate  eaddr
  case result of
    LoadResult_Err cause -> exec_end_trap  astate'  cause
    LoadResult_Ok  u32   ->
      do
        let rd_val = signExtend_u32_to_u  u32
        exec_end_common  astate'  (Just (rd, rd_val))

executeInstr  astate  (Lbu rd rs1 oimm12) = do
  let rs1_val           = get_ArchState64_gpr  astate  rs1
      eaddr             = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + oimm12)
      (result, astate') = get_ArchState64_mem8  astate  eaddr
  case result of
    LoadResult_Err cause -> exec_end_trap  astate'  cause
    LoadResult_Ok  u8    ->
      do
        let rd_val = zeroExtend_u8_to_u  u8
        exec_end_common  astate'  (Just (rd, rd_val))

executeInstr  astate  (Lhu rd rs1 oimm12) = do
  let rs1_val           = get_ArchState64_gpr  astate  rs1
      eaddr             = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + oimm12)
      (result, astate') = get_ArchState64_mem16  astate  eaddr
  case result of
    LoadResult_Err cause -> exec_end_trap  astate'  cause
    LoadResult_Ok  u16   ->
      do
        let rd_val = zeroExtend_u16_to_u  u16
        exec_end_common  astate'  (Just (rd, rd_val))

-- Stores: SB SH SW

executeInstr  astate  (Sb rs1 rs2 simm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      eaddr   = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + simm12)
      u8      = trunc_u_to_u8  rs2_val
  astate1 <- set_ArchState64_mem8  astate  eaddr  u8
  exec_end_common  astate1  Nothing

executeInstr  astate  (Sh rs1 rs2 simm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      eaddr   = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + simm12)
      u16     = trunc_u_to_u16  rs2_val
  astate1 <- set_ArchState64_mem16  astate  eaddr  u16
  exec_end_common  astate1  Nothing

executeInstr  astate  (Sw rs1 rs2 simm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      eaddr   = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + simm12)
      u32     = trunc_u_to_u32  rs2_val
  astate1 <- set_ArchState64_mem32  astate  eaddr  u32
  exec_end_common  astate1  Nothing

-- ALU register immediate: ADDI SLTI SLTIU XORI ORI ANDI SLLI SRLI SRAI

executeInstr  astate  (Addi rd rs1 imm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + imm12)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Slti rd rs1 imm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = if (cvt_u_to_s  rs1_val) < imm12 then 1 else 0
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sltiu rd rs1 imm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = if rs1_val < (cvt_s_to_u  imm12) then 1 else 0
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Xori rd rs1 imm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = xor rs1_val (cvt_s_to_u  imm12)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Ori rd rs1 imm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = rs1_val .|. (cvt_s_to_u  imm12)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Andi rd rs1 imm12) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = rs1_val .&. (cvt_s_to_u  imm12)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Slli rd rs1 shamt6) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = shiftL  rs1_val  shamt6
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Srli rd rs1 shamt6) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = shiftR  rs1_val  shamt6
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Srai rd rs1 shamt6) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rd_val  = cvt_s_to_u  (shiftR  (cvt_u_to_s  rs1_val)  shamt6)
  exec_end_common  astate  (Just (rd, rd_val))

-- ALU register-register: ADD SUB SLL SLT SLTU SRL SRA XOR OR AND

executeInstr  astate  (Add rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = cvt_s_to_u  ((cvt_u_to_s  rs1_val) + cvt_u_to_s  (rs2_val))
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sub rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = cvt_s_to_u  ((cvt_u_to_s  rs1_val) - cvt_u_to_s  (rs2_val))
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sll rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      shamt :: Int
      shamt   = cvt_u_to_Int  (rs2_val .&. (if xlen==32 then 0x1F else 0x3F))
      rd_val  = shiftL  rs1_val  shamt
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Slt rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = if (cvt_u_to_s  rs1_val) < cvt_u_to_s  (rs2_val) then 1 else 0
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sltu rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = if rs1_val < rs2_val then 1 else 0
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Srl rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      shamt :: Int
      shamt   = cvt_u_to_Int  (rs2_val .&. (if xlen==32 then 0x1F else 0x3F))
      rd_val  = shiftR  rs1_val  shamt
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sra rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      shamt :: Int
      shamt   = cvt_u_to_Int  (rs2_val .&. (if xlen==32 then 0x1F else 0x3F))
      rd_val  = cvt_s_to_u  (shiftR  (cvt_u_to_s  rs1_val)  shamt)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Xor rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = xor  rs1_val  rs2_val
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Or rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = rs1_val  .|.  rs2_val
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (And rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = rs1_val  .&.  rs2_val
  exec_end_common  astate  (Just (rd, rd_val))

-- Memory Model: FENCE FENCE.I

-- TODO: currently a no-op; fix up
executeInstr  astate  (Fence  pred  succ) = do
  exec_end_common  astate  Nothing

-- TODO: currently a no-op; fix up
executeInstr  astate  Fence_i = do
  exec_end_common  astate  Nothing

-- ECALL
-- TODO: trap
executeInstr  astate  Ecall = do
  putStrLn ("Ecall; STOPPING")
  set_ArchState64_stop  astate  Stop_Other

-- EBREAK
executeInstr  astate  Ebreak = do
  putStrLn ("Ebreak; STOPPING")
  exec_end_trap  astate  TrapCause_Breakpoint

-- CSRRx: CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI

executeInstr  astate  (Csrrw rd rs1 csr12) = do
  let csr_val = if (rd /= Rg_x0) then
                  get_ArchState64_csr  astate  csr12
                else
                  0    -- arbitrary; will be discarded (rd==0)
      rs1_val = get_ArchState64_gpr  astate  rs1
  astate1 <- set_ArchState64_csr  astate  csr12  rs1_val
  exec_end_common  astate1  (Just (rd, csr_val))

executeInstr  astate  (Csrrs rd rs1 csr12) = do
  let csr_val = get_ArchState64_csr  astate  csr12
  astate1 <- if (rs1 /= Rg_x0) then do
               let rs1_val = get_ArchState64_gpr  astate  rs1
                   new_csr_val = csr_val  .|.  rs1_val
               set_ArchState64_csr  astate  csr12  new_csr_val
             else
               return astate
  exec_end_common  astate1  (Just (rd, csr_val))

executeInstr  astate  (Csrrc rd rs1 csr12) = do
  let csr_val = get_ArchState64_csr  astate  csr12
  astate1 <- if (rs1 /= Rg_x0) then do
               let rs1_val = get_ArchState64_gpr  astate  rs1
                   new_csr_val = csr_val  .&.  (complement  rs1_val)
               set_ArchState64_csr  astate  csr12  new_csr_val
             else
               return astate
  exec_end_common  astate1  (Just (rd, csr_val))

executeInstr  astate  (Csrrwi rd zimm csr12) = do
  let csr_val = if (rd /= Rg_x0) then
                  get_ArchState64_csr  astate  csr12
                else
                  0    -- arbitrary; will be discarded (rd==0)
  astate1 <- set_ArchState64_csr  astate  csr12  zimm
  exec_end_common  astate1  (Just (rd, csr_val))

executeInstr  astate  (Csrrsi rd zimm csr12) = do
  let csr_val = get_ArchState64_csr  astate  csr12
  astate1 <- if (zimm /= 0) then do
               let new_csr_val = csr_val  .|.  zimm
               set_ArchState64_csr  astate  csr12  new_csr_val
             else
               return astate
  exec_end_common  astate1  (Just (rd, csr_val))

executeInstr  astate  (Csrrci rd zimm csr12) = do
  let csr_val = get_ArchState64_csr  astate  csr12
  astate1 <- if (zimm /= 0) then do
               let new_csr_val = csr_val  .&.  (complement  zimm)
               set_ArchState64_csr  astate  csr12  new_csr_val
             else
               return astate
  exec_end_common  astate1  (Just (rd, csr_val))

-- ================================================================
-- RV64I instructions

-- Loads: LWU LD

executeInstr  astate  (Lwu rd rs1 oimm12) = do
  let rs1_val_u64       = get_ArchState64_gpr  astate  rs1
      eaddr_u64         = cvt_s_to_u  ((cvt_u_to_s  rs1_val_u64) + oimm12)
      (result, astate') = get_ArchState64_mem32  astate  eaddr_u64
  case result of
    LoadResult_Err cause -> exec_end_trap  astate'  cause
    LoadResult_Ok  u32   ->
      do
        let rd_val = zeroExtend_u32_to_u64  u32
        exec_end_common  astate'  (Just (rd, rd_val))

executeInstr  astate  (Ld rd rs1 oimm12) = do
  let rs1_val_u64       = get_ArchState64_gpr  astate  rs1
      eaddr_u64         = cvt_s_to_u  ((cvt_u_to_s  rs1_val_u64) + oimm12)
      (result, astate') = get_ArchState64_mem64  astate  eaddr_u64
  case result of
    LoadResult_Err cause -> exec_end_trap  astate'  cause
    LoadResult_Ok  u64   ->
      do
        let rd_val = u64
        exec_end_common  astate'  (Just (rd, rd_val))

-- Stores: SD

executeInstr  astate  (Sd rs1 rs2 simm12) = do
  let rs1_val_u64 = get_ArchState64_gpr  astate  rs1
      rs2_val_u64 = get_ArchState64_gpr  astate  rs2
      eaddr_u64   = cvt_s_to_u  ((cvt_u_to_s  rs1_val_u64) + simm12)
  astate1 <- set_ArchState64_mem64  astate  eaddr_u64  rs2_val_u64
  exec_end_common  astate1  Nothing

-- ALU Register-Immediate: ADDIW SLLIW SRLIW SRAIW

executeInstr  astate  (Addiw rd rs1 imm12_s64) = do
  let rs1_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs1)
      imm12_s32   = trunc_s64_to_s32  imm12_s64
      sum_s32     = rs1_val_s32 + imm12_s32
      rd_val      = signExtend_s32_to_u64  sum_s32
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Slliw rd rs1 shamt5) = do
  let rs1_val_u32 = trunc_u64_to_u32  (get_ArchState64_gpr  astate  rs1)
      rd_val      = signExtend_u32_to_u64 (shiftL  rs1_val_u32  shamt5)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Srliw rd rs1 shamt5) = do
  let rs1_val_u32 = trunc_u64_to_u32  (get_ArchState64_gpr  astate  rs1)
      rd_val      = signExtend_u32_to_u64 (shiftR  rs1_val_u32  shamt5)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sraiw rd rs1 shamt5) = do
  let rs1_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs1)
      rd_val      = signExtend_s32_to_u64 (shiftR  rs1_val_s32  shamt5)
  exec_end_common  astate  (Just (rd, rd_val))

-- ALU register and register: ADDW SUBW SLLW SRLW SRAW

executeInstr  astate  (Addw rd rs1 rs2) = do
  let rs1_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs1)
      rs2_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs2)
      rd_val      = signExtend_s32_to_u64 (rs1_val_s32 + rs2_val_s32)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Subw rd rs1 rs2) = do
  let rs1_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs1)
      rs2_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs2)
      rd_val      = signExtend_s32_to_u64 (rs1_val_s32 - rs2_val_s32)
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sllw rd rs1 rs2) = do
  let rs1_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs1)
      rs2_val_u64 = get_ArchState64_gpr  astate  rs2
      shamt      :: Int
      shamt       = fromIntegral (rs2_val_u64 .&. 0x1F)
      result_s32  = shiftL  rs1_val_s32  shamt
      rd_val      = signExtend_s32_to_u64  result_s32
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Srlw rd rs1 rs2) = do
  let rs1_val_u32 = trunc_u64_to_u32  (get_ArchState64_gpr  astate  rs1)
      rs2_val_u64 = get_ArchState64_gpr  astate  rs2
      shamt       = fromIntegral (rs2_val_u64 .&. 0x1F)
      result_u32  = shiftR  rs1_val_u32  shamt
      rd_val      = signExtend_u32_to_u64  result_u32
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Sraw rd rs1 rs2) = do
  let rs1_val_s32 = trunc_u64_to_s32  (get_ArchState64_gpr  astate  rs1)
      rs2_val_u64 = get_ArchState64_gpr  astate  rs2
      shamt       = fromIntegral (rs2_val_u64 .&. 0x1F)
      result_s32  = shiftR  rs1_val_s32  shamt
      rd_val      = signExtend_s32_to_u64  result_s32
  exec_end_common  astate  (Just (rd, rd_val))

-- ================================================================
-- 'M' extension in RV32 and RV64 (integer multiply and divide)

executeInstr  astate  (Mul rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      rd_val  = cvt_s_to_u  ((cvt_u_to_s  rs1_val) * cvt_u_to_s  (rs2_val))
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Mulh rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      v1_i, v2_i, prod_i :: Integer    -- unbounded integers
      v1_i   = fromIntegral (cvt_u_to_s  rs1_val)    -- signed
      v2_i   = fromIntegral (cvt_u_to_s  rs2_val)    -- signed
      prod_i = v1_i * v2_i
      rd_val :: WordXLEN
      rd_val = cvt_s_to_u (fromIntegral (bitSlice  prod_i  xlen  (xlen+xlen)))
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Mulhu rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      v1_i, v2_i, prod_i :: Integer    -- unbounded integers
      v1_i   = fromIntegral  rs1_val    -- unsigned
      v2_i   = fromIntegral  rs2_val    -- unsigned
      prod_i = v1_i * v2_i
      rd_val :: WordXLEN
      rd_val = cvt_s_to_u (fromIntegral (bitSlice  prod_i  xlen  (xlen+xlen)))
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Mulhsu rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      v1_i, v2_i, prod_i :: Integer    -- unbounded integers
      v1_i   = fromIntegral (cvt_u_to_s  rs1_val)    -- signed
      v2_i   = fromIntegral  rs2_val                 -- unsigned
      prod_i = v1_i * v2_i
      rd_val :: WordXLEN
      rd_val = cvt_s_to_u (fromIntegral (bitSlice  prod_i  xlen  (xlen+xlen)))
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Div rd rs1 rs2) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      rs1_val_s = cvt_u_to_s  rs1_val
      rs2_val_s = cvt_u_to_s  rs2_val
      rd_val_s  = if (rs2_val_s == 0) then -1
                  else if (rs1_val_s == minBound) && (rs2_val_s == -1) then rs1_val_s
                       else quot  rs1_val_s  rs2_val_s
      rd_val    = cvt_s_to_u  rd_val_s
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Divu rd rs1 rs2) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      rd_val    = if (rs2_val == 0) then maxBound
                  else div  rs1_val  rs2_val
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Rem rd rs1 rs2) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      rs1_val_s = cvt_u_to_s  rs1_val
      rs2_val_s = cvt_u_to_s  rs2_val
      rd_val_s  = if (rs2_val_s == 0) then rs1_val_s
                  else if (rs1_val_s == minBound) && (rs2_val_s == -1) then 0
                       else rem  rs1_val_s  rs2_val_s
      rd_val    = cvt_s_to_u  rd_val_s
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Remu rd rs1 rs2) = do
  let rs1_val   = get_ArchState64_gpr  astate  rs1
      rs2_val   = get_ArchState64_gpr  astate  rs2
      rd_val    = if (rs2_val == 0) then rs1_val
                  else rem  rs1_val  rs2_val
  exec_end_common  astate  (Just (rd, rd_val))

-- ================================================================
-- 'M' extension in RV64 only (integer multiply and divide)

executeInstr  astate  (Mulw rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      v1_i, v2_i, prod_i :: Integer    -- unbounded integers
      v1_i   = fromIntegral (trunc_u64_to_s32  rs1_val)    -- signed
      v2_i   = fromIntegral (trunc_u64_to_s32  rs2_val)    -- signed
      prod_i = v1_i * v2_i
      rd_val :: WordXLEN
      rd_val = cvt_s_to_u (fromIntegral (bitSlice  prod_i  xlen  (xlen+xlen)))
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Divw rd rs1 rs2) = do
  let rs1_val  = get_ArchState64_gpr  astate  rs1
      rs2_val  = get_ArchState64_gpr  astate  rs2
      v1_s32, v2_s32, quot_s32 :: Int32
      v1_s32   = trunc_u64_to_s32  rs1_val
      v2_s32   = trunc_u64_to_s32  rs2_val
      quot_s32 = if (v2_s32 == 0) then -1
                 else if (v1_s32 == minBound) && (v2_s32 == -1) then v1_s32
                      else quot  v1_s32  v2_s32
      quot_u32 :: Word32
      quot_u32 = fromIntegral  quot_s32
      rd_val   = signExtend_u32_to_u64  quot_u32
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Divuw rd rs1 rs2) = do
  let rs1_val  = get_ArchState64_gpr  astate  rs1
      rs2_val  = get_ArchState64_gpr  astate  rs2
      v1_u32, v2_u32, quot_u32 :: Word32
      v1_u32   = trunc_u64_to_u32  rs1_val
      v2_u32   = trunc_u64_to_u32  rs2_val
      quot_u32 = if (v2_u32 == 0) then maxBound
                 else div  v1_u32  v2_u32
      rd_val   = signExtend_u32_to_u64  quot_u32
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Remw rd rs1 rs2) = do
  let rs1_val = get_ArchState64_gpr  astate  rs1
      rs2_val = get_ArchState64_gpr  astate  rs2
      v1_s32, v2_s32, rem_s32 :: Int32
      v1_s32  = trunc_u64_to_s32  rs1_val
      v2_s32  = trunc_u64_to_s32  rs2_val
      rem_s32 = if (v2_s32 == 0) then v1_s32
                else if (v1_s32 == minBound) && (v2_s32 == -1) then 0
                     else rem  v1_s32  v2_s32
      rem_u32 :: Word32
      rem_u32 = fromIntegral  rem_s32
      rd_val  = signExtend_u32_to_u64  rem_u32
  exec_end_common  astate  (Just (rd, rd_val))

executeInstr  astate  (Remuw rd rs1 rs2) = do
  let rs1_val  = get_ArchState64_gpr  astate  rs1
      rs2_val  = get_ArchState64_gpr  astate  rs2
      v1_u32, v2_u32, rem_u32 :: Word32
      v1_u32   = trunc_u64_to_u32  rs1_val
      v2_u32   = trunc_u64_to_u32  rs2_val
      rem_u32 = if (v2_u32 == 0) then v1_u32
                 else rem  v1_u32  v2_u32
      rd_val = signExtend_u32_to_u64  rem_u32
  exec_end_common  astate  (Just (rd, rd_val))

-- ================================================================
-- Invalid instructions
-- TODO: trap to trap handler; for now, just stop

executeInstr  astate  IllegalInstruction = do
  putStrLn "  ILLEGAL INSTRUCTION; STOPPING"
  set_ArchState64_stop  astate  Stop_Other

-- ================================================================
-- We should never reach here; the above clauses should handle all the
-- variants of the 'Instruction' type.

executeInstr  astate  instr = do
  putStrLn ("  INTERNAL ERROR: UNIMPLEMENTED: " ++ (show instr) ++ "; STOPPING")
  set_ArchState64_stop  astate  Stop_Other

-- ================================================================
-- TODO: raiseException is just a placeholder for now; fix up

raiseException :: ArchState64 -> Int -> Int -> IO ArchState64
raiseException  astate  x  y = do
  putStrLn ("raiseException: x= " ++ show x ++ " y= " ++ show y ++ "; STOPPING")
  set_ArchState64_stop  astate  Stop_Other

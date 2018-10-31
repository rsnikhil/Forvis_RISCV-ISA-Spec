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
import Arch_Defs
import Machine_State
import Virtual_Mem
import Forvis_Spec_Finish_Instr     -- Canonical ways for finish an instruction

-- User-level instructions
import Forvis_Spec_I                -- 'I' (Base) instruction set
import Forvis_Spec_M                -- Extension 'M' (Integer Multiply/Divide)
import Forvis_Spec_A                -- Extension 'A' (Atomic Memory Ops (AMO))
import Forvis_Spec_FD               -- Extensions 'F' and 'D' (single- and double-precision floating point)
import Forvis_Spec_C                -- Extension 'C' (Compressed 16-bit instrs)

-- Privileged Architecture instructions
import Forvis_Spec_Priv

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
-- Executing one instruction

-- The spec is organized as a collection of functions.
-- Some functions specify just one kind of instruction (e.g., LUI).
-- Some functions specify a small family of related instructions (e.g., BRANCH)
-- Each function has the following type:

type Instr_Spec   = Machine_State -> Instr   -> Bool -> (Bool, Machine_State)
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
instr_C_specs = [(spec_C_LWSP,      "C_LWSP"),
                 (spec_C_LDSP,      "C_LDSP"),
                 -- (spec_C_LQSP,      "C_LQSP"),      Uncomment when we do RV128
                 -- (spec_C_FLWSP,     "C_FLWSP"),     Uncomment when we do floating point
                 -- (spec_C_FLDSP,     "C_FLDSP"),     Uncomment when we do floating point
                 (spec_C_SWSP,      "C_SWSP"),
                 (spec_C_SDSP,      "C_SDSP"),
                 -- (spec_C_SQSP,      "C_SQSP"),      Uncomment when we do RV128
                 -- (spec_C_FSWSP,     "C_FSWSP"),     Uncomment when we do floating point
                 -- (spec_C_FSDSP,     "C_FSDSP"),     Uncomment when we do floating point
                 (spec_C_LW,        "C_LW"),
                 (spec_C_LD,        "C_LD"),
                 -- (spec_C_LQ,        "C_LQ"),        Uncomment when we do RV128
                 -- (spec_C_FLW,       "C_FLW"),       Uncomment when we do floating point
                 -- (spec_C_FLD,       "C_FLD"),       Uncomment when we do floating point
                 (spec_C_SW,        "C_SW"),
                 (spec_C_SD,        "C_SD"),
                 -- (spec_C_SQ,        "C_SQ"),        Uncomment when we do RV128
                 -- (spec_C_FSW,       "C_FSW"),       Uncomment when we do floating point
                 -- (spec_C_FSD,       "C_FSD"),       Uncomment when we do floating point
                 (spec_C_J,         "C_J"),
                 (spec_C_JAL,       "C_JAL"),
                 (spec_C_JR,        "C_JR"),
                 (spec_C_JALR,      "C_JALR"),
                 (spec_C_BEQZ,      "C_BEQZ"),
                 (spec_C_BNEZ,      "C_BNEZ"),
                 (spec_C_LI,        "C_LI"),
                 (spec_C_LUI,       "C_LUI"),
                 (spec_C_ADDI,      "C_ADDI"),
                 (spec_C_NOP,       "C_NOP"),
                 (spec_C_ADDIW,     "C_ADDIW"),
                 (spec_C_ADDI16SP,  "C_ADDI16SP"),
                 (spec_C_ADDI4SPN,  "C_ADDI4SPN"),
                 (spec_C_SLLI,      "C_SLLI"),
                 (spec_C_SRLI,      "C_SRLI"),
                 (spec_C_SRAI,      "C_SRAI"),
                 (spec_C_ANDI,      "C_ANDI"),
                 (spec_C_MV,        "C_MV"),
                 (spec_C_ADD,       "C_ADD"),
                 (spec_C_AND,       "C_AND"),
                 (spec_C_OR,        "C_OR"),
                 (spec_C_XOR,       "C_XOR"),
                 (spec_C_SUB,       "C_SUB"),
                 (spec_C_ADDW,      "C_ADDW"),
                 (spec_C_SUBW,      "C_SUBW"),
                 (spec_C_EBREAK,    "C_EBREAK")
                ]

-- 'exec_instr' takes a machine state and a 32-bit instruction and
-- returns a new machine state after executing that instruction.  It
-- attempts all the specs in 'instr_specs' and, if none of them apply,
-- performs an illegal-instruction trap.

exec_instr :: Machine_State -> Instr -> (Machine_State, String)
exec_instr    mstate           instr =
  let
    tryall []                  = (let
                                     tval = instr
                                  in
                                    (finish_trap  mstate  exc_code_illegal_instruction  tval,
                                     "NONE"))

    tryall ((spec,name):specs) = (let
                                     is_C = False
                                     (success, mstate1) = spec  mstate  instr  is_C
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
exec_instr_C    mstate           instr =
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

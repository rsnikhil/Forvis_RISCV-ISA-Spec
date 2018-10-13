-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Arch_Defs where

-- ================================================================
-- This module has basic, RISC-V definitions (such RV32 vs. RV64,
-- bit-fields of instructions and standard CSRs, etc.) on which the
-- rest of the architectural state and operations depend.

-- ================================================================
-- Standard Haskell imports

import Data.Bits
import Data.Char

-- Project imports

import Bit_Utils

-- ================================================================
-- Major architectural parameters                  -- \begin_latex{RV}

data RV = RV32
        | RV64
        deriving (Eq, Show)
                                                   -- \end_latex{RV}
-- ================================================================
-- Predicate to decide whether the arg may be a 'C' (Compressed)
-- instruction or not ('C' instrs have 2 lsbs not equal to 2'b11)

{-# INLINE is_instr_C #-}
is_instr_C :: Integer -> Bool
is_instr_C  u16 = ((u16 .&. 0x3) /= 0x3)

-- ================================================================
-- Instructions and instruction fields                \begin_latex{Instr}
-- These are just synonyms of 'Integer', for readability

type Instr   = Integer
type Instr_C = Integer

type InstrField = Integer

-- General-purpose registers

type GPR_Addr = Integer
type GPR_Val  = Integer

-- Floating-point registers

type FPR_Addr = Integer

-- CSRs

type CSR_Addr = Integer
                                                   -- \end_latex{Instr}
-- ================================================================
-- Functions to extract instruction fields            \begin_latex{Instr_Field_Functions}

{-# INLINE ifield_opcode #-}
ifield_opcode  :: Instr -> InstrField
ifield_opcode  instr = bitSlice instr  6  0

{-# INLINE ifield_funct3 #-}
ifield_funct3  :: Instr -> InstrField
ifield_funct3  instr = bitSlice instr  14  12

{-# INLINE ifield_rd #-}
ifield_rd      :: Instr -> InstrField
ifield_rd      instr = bitSlice instr  11  7

{-# INLINE ifield_rs1 #-}
ifield_rs1     :: Instr -> InstrField
ifield_rs1     instr = bitSlice instr  19  15
                                                   -- \end_latex{Instr_Field_Functions}
{-# INLINE ifield_rs2 #-}
ifield_rs2     :: Instr -> InstrField
ifield_rs2     instr = bitSlice instr  24  20

{-# INLINE ifield_rs3 #-}
ifield_rs3     :: Instr -> InstrField
ifield_rs3     instr = bitSlice instr  31  27    -- for FMADD, FMSUB, FNMSUB

{-# INLINE ifield_funct10 #-}
ifield_funct10 :: Instr -> InstrField
ifield_funct10 instr = (shift (bitSlice instr  31  25) 3) .|. (bitSlice instr  14  12)

{-# INLINE ifields_R_type #-}
ifields_R_type :: Instr -> (InstrField, InstrField, InstrField, InstrField, InstrField, InstrField)
ifields_R_type  instr =
  let
    funct7 = bitSlice instr  31  25
    rs2    = ifield_rs2     instr
    rs1    = ifield_rs1     instr
    funct3 = ifield_funct3  instr
    rd     = ifield_rd      instr
    opcode = ifield_opcode  instr
  in
    (funct7, rs2, rs1, funct3, rd, opcode)

{-# INLINE ifields_R4_type #-}
ifields_R4_type :: Instr -> (InstrField, InstrField, InstrField, InstrField, InstrField, InstrField, InstrField)
ifields_R4_type  instr =
  let
    rs3    = ifield_rs3     instr
    funct2 = ifield_funct2  instr
    rs2    = ifield_rs2     instr
    rs1    = ifield_rs1     instr
    funct3 = ifield_funct3  instr
    rd     = ifield_rd      instr
    opcode = ifield_opcode  instr
  in
    (rs3, funct2, rs2, rs1, funct3, rd, opcode)

{-# INLINE ifields_I_type #-}
ifields_I_type :: Instr -> (InstrField, InstrField, InstrField, InstrField, InstrField)
ifields_I_type  instr =
  let
    imm12  = bitSlice instr  31  20
    rs1    = ifield_rs1     instr
    funct3 = ifield_funct3  instr
    rd     = ifield_rd      instr
    opcode = ifield_opcode  instr
  in
    (imm12, rs1, funct3, rd, opcode)

{-# INLINE ifields_S_type #-}
ifields_S_type :: Instr -> (InstrField, InstrField, InstrField, InstrField, InstrField)
ifields_S_type  instr =
  let
    imm12  = ( shift (bitSlice instr  31  25) 5 .|.
               shift (bitSlice instr 11  7)  0)
    rs2    = ifield_rs2     instr
    rs1    = ifield_rs1     instr
    funct3 = ifield_funct3  instr
    opcode = ifield_opcode  instr
  in
    (imm12, rs2, rs1, funct3, opcode)

                                                -- \begin_latex{ifields_B_type}
{-# INLINE ifields_B_type #-}
ifields_B_type :: Instr -> (InstrField, InstrField, InstrField, InstrField, InstrField)
ifields_B_type  instr =
  let imm12  = ( shift (bitSlice instr  31  31) 11  .|.
                 shift (bitSlice instr   7   7) 10  .|.
                 shift (bitSlice instr  30  25)  4  .|.
                 shift (bitSlice instr  11   8)  0  )
      rs2    = ifield_rs2     instr
      rs1    = ifield_rs1     instr
      funct3 = ifield_funct3  instr
      opcode = ifield_opcode  instr
  in
    (imm12, rs2, rs1, funct3, opcode)
                                                -- \end_latex{ifields_B_type}

{-# INLINE ifields_U_type #-}
ifields_U_type :: Instr -> (InstrField, InstrField, InstrField)
ifields_U_type  instr =
  let
    imm20  = bitSlice       instr  31  12
    rd     = ifield_rd      instr
    opcode = ifield_opcode  instr
  in
    (imm20, rd, opcode)

{-# INLINE ifields_J_type #-}
ifields_J_type :: Instr -> (InstrField, InstrField, InstrField)
ifields_J_type  instr =
  let
    imm20  = ( shift (bitSlice instr  31  31) 19  .|.
               shift (bitSlice instr  19  12) 11  .|.
               shift (bitSlice instr  20  20) 10  .|.
               shift (bitSlice instr  30  21)  0  )
    rd     = ifield_rd      instr
    opcode = ifield_opcode  instr
  in
    (imm20, rd, opcode)

-- I-type imm12 fields for shift instrs

{-# INLINE i_imm12_fields_7_5 #-}
i_imm12_fields_7_5 :: InstrField -> (InstrField, InstrField)
i_imm12_fields_7_5  imm12 = (bitSlice  imm12  11  5,
                             bitSlice  imm12   4  0)

{-# INLINE i_imm12_fields_6_6 #-}
i_imm12_fields_6_6 :: InstrField -> (InstrField, InstrField)
i_imm12_fields_6_6  imm12 = (bitSlice  imm12  11  6,
                             bitSlice  imm12   5  0)

-- I-type imm12 fields for FENCE
{-# INLINE i_imm12_fields_for_FENCE #-}
i_imm12_fields_for_FENCE :: InstrField -> (InstrField, InstrField, InstrField)
i_imm12_fields_for_FENCE  imm12 = (bitSlice  imm12  11  8,
                                   bitSlice  imm12   7  4,
                                   bitSlice  imm12   3  0)

-- R-type funct7 fields for AMO
{-# INLINE r_funct7_fields_for_AMO #-}
r_funct7_fields_for_AMO :: InstrField -> (InstrField, InstrField, InstrField)
r_funct7_fields_for_AMO  funct7 = (bitSlice  funct7  6  2,
                                   bitSlice  funct7  1  1,
                                   bitSlice  funct7  0  0)

-- ================================================================
-- Exception Codes                                 \begin_latex{exception_codes_A}

type Exc_Code = Integer

exc_code_u_software_interrupt      =  0 :: Exc_Code
exc_code_s_software_interrupt      =  1 :: Exc_Code
                                                -- \end_latex{exception_codes_A}
exc_code_m_software_interrupt      =  3 :: Exc_Code
exc_code_u_timer_interrupt         =  4 :: Exc_Code
exc_code_s_timer_interrupt         =  5 :: Exc_Code
exc_code_m_timer_interrupt         =  7 :: Exc_Code
exc_code_u_external_interrupt      =  8 :: Exc_Code
exc_code_s_external_interrupt      =  9 :: Exc_Code
exc_code_m_external_interrupt      = 11 :: Exc_Code

                                                -- \begin_latex{exception_codes_B}
exc_code_instr_addr_misaligned     =  0 :: Exc_Code
exc_code_instr_access_fault        =  1 :: Exc_Code
                                                -- \end_latex{exception_codes_B}
exc_code_illegal_instruction       =  2 :: Exc_Code
exc_code_breakpoint                =  3 :: Exc_Code
exc_code_load_addr_misaligned      =  4 :: Exc_Code
exc_code_load_access_fault         =  5 :: Exc_Code
exc_code_store_AMO_addr_misaligned =  6 :: Exc_Code
exc_code_store_AMO_access_fault    =  7 :: Exc_Code
exc_code_ECall_from_U              =  8 :: Exc_Code
exc_code_ECall_from_S              =  9 :: Exc_Code
exc_code_ECall_from_M              = 11 :: Exc_Code
exc_code_Instruction_Page_Fault    = 12 :: Exc_Code
exc_code_Load_Page_Fault           = 13 :: Exc_Code
exc_code_Store_AMO_Page_Fault      = 15 :: Exc_Code

-- ================
-- For debugging

show_interrupt_exc_code :: Exc_Code -> String
show_interrupt_exc_code  ec  | (ec == exc_code_u_software_interrupt) = "exc_code_u_software_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_s_software_interrupt) = "exc_code_s_software_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_m_software_interrupt) = "exc_code_m_software_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_u_timer_interrupt)    = "exc_code_u_timer_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_s_timer_interrupt)    = "exc_code_s_timer_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_m_timer_interrupt)    = "exc_code_m_timer_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_u_external_interrupt) = "exc_code_u_external_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_s_external_interrupt) = "exc_code_s_external_interrupt"
show_interrupt_exc_code  ec  | (ec == exc_code_m_external_interrupt) = "exc_code_m_external_interrupt"


show_trap_exc_code :: Exc_Code -> String
show_trap_exc_code  ec  | (ec == exc_code_instr_addr_misaligned)     = "exc_code_instr_addr_misaligned"
show_trap_exc_code  ec  | (ec == exc_code_instr_access_fault)        = "exc_code_instr_access_fault"
show_trap_exc_code  ec  | (ec == exc_code_illegal_instruction)       = "exc_code_illegal_instruction"
show_trap_exc_code  ec  | (ec == exc_code_breakpoint)                = "exc_code_breakpoint"
show_trap_exc_code  ec  | (ec == exc_code_load_addr_misaligned)      = "exc_code_load_addr_misaligned"
show_trap_exc_code  ec  | (ec == exc_code_load_access_fault)         = "exc_code_load_access_fault"
show_trap_exc_code  ec  | (ec == exc_code_store_AMO_addr_misaligned) = "exc_code_store_AMO_addr_misaligned"
show_trap_exc_code  ec  | (ec == exc_code_store_AMO_access_fault)    = "exc_code_store_AMO_access_fault"
show_trap_exc_code  ec  | (ec == exc_code_ECall_from_U)              = "exc_code_ECall_from_U"
show_trap_exc_code  ec  | (ec == exc_code_ECall_from_S)              = "exc_code_ECall_from_S"
show_trap_exc_code  ec  | (ec == exc_code_ECall_from_M)              = "exc_code_ECall_from_M"
show_trap_exc_code  ec  | (ec == exc_code_Instruction_Page_Fault)    = "exc_code_Instruction_Page_Fault"
show_trap_exc_code  ec  | (ec == exc_code_Load_Page_Fault)           = "exc_code_Load_Page_Fault"
show_trap_exc_code  ec  | (ec == exc_code_Store_AMO_Page_Fault)      = "exc_code_Store_AMO_Page_Fault"

-- ================================================================
-- Memory access results                           \begin_latex{Mem_Result}
-- Either Ok with value, or Err with an exception code

data Mem_Result = Mem_Result_Ok   Integer
                | Mem_Result_Err  Exc_Code
                deriving (Show)
                                                -- \end_latex{Mem_Result}
-- ================================================================
-- Privilege levels                                \begin_latex{Priv_Level}
-- Machine, Supervisor and User

type Priv_Level = InstrField

m_Priv_Level  = 3 :: Priv_Level
s_Priv_Level  = 1 :: Priv_Level
u_Priv_Level  = 0 :: Priv_Level

                                                -- \end_latex{Priv_Level}
-- ================================================================
-- User-Level CSR addresses

csr_addr_ustatus    = 0x000 :: CSR_Addr
csr_addr_uie        = 0x004 :: CSR_Addr
csr_addr_utvec      = 0x005 :: CSR_Addr

csr_addr_uscratch   = 0x040 :: CSR_Addr
csr_addr_uepc       = 0x041 :: CSR_Addr
csr_addr_ucause     = 0x042 :: CSR_Addr
csr_addr_utval      = 0x043 :: CSR_Addr
csr_addr_uip        = 0x044 :: CSR_Addr

csr_addr_fflags     = 0x001 :: CSR_Addr
csr_addr_frm        = 0x002 :: CSR_Addr
csr_addr_fcsr       = 0x003 :: CSR_Addr

csr_addr_cycle      = 0xC00 :: CSR_Addr
csr_addr_time       = 0xC01 :: CSR_Addr
csr_addr_instret    = 0xC02 :: CSR_Addr

csr_addr_cycleh     = 0xC80 :: CSR_Addr
csr_addr_timeh      = 0xC81 :: CSR_Addr
csr_addr_instreth   = 0xC82 :: CSR_Addr

-- TODO: hpmcounterN, hpmcounterNh

-- The following list is in the order printed by print_CSR_File()

u_csr_addrs_and_names :: [(CSR_Addr, String)]
u_csr_addrs_and_names  =
  [ (csr_addr_ustatus,    "ustatus"),
    (csr_addr_uie,        "uie"),
    (csr_addr_uip,        "uip"),
    (csr_addr_time,       "time"),
    (csr_addr_timeh,      "timeh"),

    (csr_addr_utvec,      "utvec"),
    (csr_addr_uepc,       "uepc"),
    (csr_addr_ucause,     "ucause"),
    (csr_addr_utval,      "utval"),
    (csr_addr_uscratch,   "uscratch"),

    (csr_addr_cycle,      "cycle"),
    (csr_addr_cycleh,     "cycleh"),
    (csr_addr_instret,    "instret"),
    (csr_addr_instreth,   "instreth"),

    (csr_addr_fflags,     "fflags"),
    (csr_addr_frm,        "frm"),
    (csr_addr_fcsr,       "fcsr") ]

-- ================================================================
-- Supervisor-Level CSR addresses

csr_addr_sstatus    = 0x100 :: CSR_Addr
csr_addr_sedeleg    = 0x102 :: CSR_Addr
csr_addr_sideleg    = 0x103 :: CSR_Addr
csr_addr_sie        = 0x104 :: CSR_Addr
csr_addr_stvec      = 0x105 :: CSR_Addr
csr_addr_scounteren = 0x106 :: CSR_Addr

csr_addr_sscratch   = 0x140 :: CSR_Addr
csr_addr_sepc       = 0x141 :: CSR_Addr
csr_addr_scause     = 0x142 :: CSR_Addr
csr_addr_stval      = 0x143 :: CSR_Addr
csr_addr_sip        = 0x144 :: CSR_Addr

csr_addr_satp       = 0x180 :: CSR_Addr

-- The following list is in the order printed by print_CSR_File()

s_csr_addrs_and_names :: [(CSR_Addr, String)]
s_csr_addrs_and_names  =
  [ (csr_addr_sstatus,    "sstatus"),
    (csr_addr_sie,        "sie"),
    (csr_addr_sip,        "sip"),
    (csr_addr_sedeleg,    "sedeleg"),
    (csr_addr_sideleg,    "sideleg"),

    (csr_addr_stvec,      "stvec"),
    (csr_addr_sepc,       "sepc"),
    (csr_addr_scause,     "scause"),
    (csr_addr_stval,      "stval"),
    (csr_addr_sscratch,   "sscratch"),

    (csr_addr_satp,       "satp"),
    (csr_addr_scounteren, "scounteren") ]

-- ================================================================
-- Machine-Level CSR addresses

csr_addr_mvendorid  = 0xF11 :: CSR_Addr
csr_addr_marchid    = 0xF12 :: CSR_Addr
csr_addr_mimpid     = 0xF13 :: CSR_Addr
csr_addr_mhartid    = 0xF14 :: CSR_Addr

csr_addr_mstatus    = 0x300 :: CSR_Addr
csr_addr_misa       = 0x301 :: CSR_Addr
csr_addr_medeleg    = 0x302 :: CSR_Addr
csr_addr_mideleg    = 0x303 :: CSR_Addr
csr_addr_mie        = 0x304 :: CSR_Addr
csr_addr_mtvec      = 0x305 :: CSR_Addr
csr_addr_mcounteren = 0x306 :: CSR_Addr

csr_addr_mscratch   = 0x340 :: CSR_Addr
csr_addr_mepc       = 0x341 :: CSR_Addr
csr_addr_mcause     = 0x342 :: CSR_Addr
csr_addr_mtval      = 0x343 :: CSR_Addr
csr_addr_mip        = 0x344 :: CSR_Addr

-- TODO: pmpcfgN, pmpaddrN

csr_addr_mcycle     = 0xB00 :: CSR_Addr
csr_addr_minstret   = 0xB02 :: CSR_Addr

-- TODO: mhpmcounterN

csr_addr_mcycleh    = 0xB80 :: CSR_Addr
csr_addr_minstreth  = 0xB82 :: CSR_Addr

-- TODO: mhpmcounterNh

csr_addr_tselect    = 0x7A0 :: CSR_Addr
csr_addr_data1      = 0x7A1 :: CSR_Addr
csr_addr_data2      = 0x7A2 :: CSR_Addr
csr_addr_data3      = 0x7A3 :: CSR_Addr

csr_addr_dcsr       = 0x7B0 :: CSR_Addr
csr_addr_dpc        = 0x7B1 :: CSR_Addr
csr_addr_dscratch   = 0x7B2 :: CSR_Addr

-- The following list is in the order printed by print_CSR_File()

m_csr_addrs_and_names :: [(CSR_Addr, String)]
m_csr_addrs_and_names  =
  [ (csr_addr_mstatus,    "mstatus"),
    (csr_addr_mie,        "mie"),
    (csr_addr_mip,        "mip"),
    (csr_addr_medeleg,    "medeleg"),
    (csr_addr_mideleg,    "mideleg"),

    (csr_addr_mtvec,      "mtvec"),
    (csr_addr_mepc,       "mepc"),
    (csr_addr_mcause,     "mcause"),
    (csr_addr_mtval,      "mtval"),
    (csr_addr_mscratch,   "mscratch"),

    (csr_addr_minstret,   "minstret"),
    (csr_addr_minstreth,  "minstreth"),
    (csr_addr_mcycle,     "mcycle"),
    (csr_addr_mcycleh,    "mcycleh"),
    (csr_addr_mcounteren, "mcounteren"),

    (csr_addr_mvendorid,  "mvendorid"),
    (csr_addr_marchid,    "marchid"),
    (csr_addr_mimpid,     "mimpid"),
    (csr_addr_mhartid,    "mhartid"),
    (csr_addr_misa,       "misa"),

    (csr_addr_tselect,    "tselect"),
    (csr_addr_data1,      "data1"),
    (csr_addr_data2,      "data2"),
    (csr_addr_data3,      "data3"),
    (csr_addr_dcsr,       "dcsr"),

    (csr_addr_dpc,        "dpc"),
    (csr_addr_dscratch,   "dscratch")
  ]

-- ================================================================
-- MISA bit fields

-- Test whether a particular MISA 'letter' bit (A-Z) is set

{-# INLINE misa_flag #-}
misa_flag :: Integer -> Char -> Bool
misa_flag  misa  letter | isAsciiUpper  letter = (((shiftR  misa  ((ord letter) - (ord 'A'))) .&. 1) == 1)
misa_flag  misa  letter | isAsciiLower  letter = (((shiftR  misa  ((ord letter) - (ord 'a'))) .&. 1) == 1)
                        | otherwise            = False

-- Codes for MXL, SXL, UXL
xl_rv32  = 1 :: Integer
xl_rv64  = 2 :: Integer
xl_rv128 = 3 :: Integer

-- Bit fields
misa_A_bitpos = 0 :: Int
misa_B_bitpos = 1 :: Int
misa_C_bitpos = 2 :: Int
misa_D_bitpos = 3 :: Int

misa_E_bitpos = 4 :: Int
misa_F_bitpos = 5 :: Int
misa_G_bitpos = 6 :: Int
misa_H_bitpos = 7 :: Int

misa_I_bitpos = 8 :: Int
misa_J_bitpos = 9 :: Int
misa_K_bitpos = 10 :: Int
misa_L_bitpos = 11 :: Int

misa_M_bitpos = 12 :: Int
misa_N_bitpos = 13 :: Int
misa_O_bitpos = 14 :: Int
misa_P_bitpos = 15 :: Int

misa_Q_bitpos = 16 :: Int
misa_R_bitpos = 17 :: Int
misa_S_bitpos = 18 :: Int
misa_T_bitpos = 19 :: Int

misa_U_bitpos = 20 :: Int
misa_V_bitpos = 21 :: Int
misa_W_bitpos = 22 :: Int
misa_X_bitpos = 23 :: Int

misa_Y_bitpos = 24 :: Int
misa_Z_bitpos = 25 :: Int

misa_MXL_bitpos_RV32 = 30 :: Int
misa_MXL_bitpos_RV64 = 62 :: Int

-- ================================================================
-- MSTATUS bit fields

mstatus_sd_bitpos_RV64 = 63 :: Int
mstatus_sd_bitpos_RV32 = 31 :: Int

mstatus_sxl_bitpos     = 34 :: Int
mstatus_uxl_bitpos     = 32 :: Int

mstatus_tsr_bitpos     = 22 :: Int
mstatus_tw_bitpos      = 21 :: Int
mstatus_tvm_bitpos     = 20 :: Int

mstatus_mxr_bitpos     = 19 :: Int
mstatus_sum_bitpos     = 18 :: Int
mstatus_mprv_bitpos    = 17 :: Int

mstatus_fs_bitpos      = 15 :: Int
mstatus_xs_bitpos      = 13 :: Int

mstatus_mpp_bitpos     = 11 :: Int
mstatus_spp_bitpos     =  8 :: Int

mstatus_mpie_bitpos    =  7 :: Int
mstatus_spie_bitpos    =  5 :: Int
mstatus_upie_bitpos    =  4 :: Int

mstatus_mie_bitpos     =  3 :: Int
mstatus_sie_bitpos     =  1 :: Int
mstatus_uie_bitpos     =  0 :: Int

-- MSTATUS contains a ``stack'' of ``previous-privilege'' and ``interrupt-enable'' bits

-- Return the stack fields in mstatus

{-# INLINE mstatus_stack_fields #-}
mstatus_stack_fields :: Integer -> (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer)
mstatus_stack_fields  mstatus =
  let
    mpp  = (shiftR  mstatus  mstatus_mpp_bitpos)  .&. 0x3
    spp  = (shiftR  mstatus  mstatus_spp_bitpos)  .&. 0x1
    mpie = (shiftR  mstatus  mstatus_mpie_bitpos) .&. 0x1
    spie = (shiftR  mstatus  mstatus_spie_bitpos) .&. 0x1
    upie = (shiftR  mstatus  mstatus_upie_bitpos) .&. 0x1
    mie  = (shiftR  mstatus  mstatus_mie_bitpos)  .&. 0x1
    sie  = (shiftR  mstatus  mstatus_sie_bitpos)  .&. 0x1
    uie  = (shiftR  mstatus  mstatus_uie_bitpos)  .&. 0x1
  in
    (mpp, spp, mpie, spie, upie, mie, sie, uie)

-- Update the stack fields in mstatus

{-# INLINE mstatus_upd_stack_fields #-}
mstatus_upd_stack_fields :: Integer ->
                            (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer) ->
                            Integer
mstatus_upd_stack_fields  mstatus (mpp, spp, mpie, spie, upie, mie, sie, uie) =
  let
    mstatus_stack_mask :: Integer
    mstatus_stack_mask = 0x1FFF    -- all LSBs up to and including MPP

    mstatus' = ((mstatus .&. (complement  mstatus_stack_mask))
                .|. (shiftL  mpp   mstatus_mpp_bitpos)
                .|. (shiftL  spp   mstatus_spp_bitpos)
                .|. (shiftL  mpie  mstatus_mpie_bitpos)
                .|. (shiftL  spie  mstatus_spie_bitpos)
                .|. (shiftL  upie  mstatus_upie_bitpos)
                .|. (shiftL  mie   mstatus_mie_bitpos)
                .|. (shiftL  sie   mstatus_sie_bitpos)
                .|. (shiftL  uie   mstatus_uie_bitpos))
  in
    mstatus'

-- These masks specify which fields are observed/updated in MSTATUS

mstatus_mask_RV32 :: Integer
mstatus_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                     .|. (shiftL  1  mstatus_tsr_bitpos)
                     .|. (shiftL  1  mstatus_tw_bitpos)
                     .|. (shiftL  1  mstatus_tvm_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)
                     .|. (shiftL  1  mstatus_mprv_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  3  mstatus_mpp_bitpos)
                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_mpie_bitpos)
                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_mie_bitpos)
                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))

mstatus_mask_RV64 :: Integer
mstatus_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                     -- .|. (shiftL  3  mstatus_sxl_bitpos)    -- TODO: this is not-writable implementation choice
                     -- .|. (shiftL  3  mstatus_uxl_bitpos)    -- TODO: this is not-writable implementation choice

                     .|. (shiftL  1  mstatus_tsr_bitpos)
                     .|. (shiftL  1  mstatus_tw_bitpos)
                     .|. (shiftL  1  mstatus_tvm_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)
                     .|. (shiftL  1  mstatus_mprv_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  3  mstatus_mpp_bitpos)
                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_mpie_bitpos)
                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_mie_bitpos)
                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))


-- SSTATUS is a ``view'' of MSTATUS, masking in/out certain fields

sstatus_mask_RV32 :: Integer
sstatus_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))

sstatus_mask_RV64 :: Integer
sstatus_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                     .|. (shiftL  3  mstatus_uxl_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_spp_bitpos)

                     .|. (shiftL  1  mstatus_spie_bitpos)
                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_sie_bitpos)
                     .|. (shiftL  1  mstatus_uie_bitpos))

-- USTATUS is a ``view'' of MSTATUS, masking in/out certain fields
-- TODO: find out what should be in ustatus (the v1.10 spec doc does not specify)

ustatus_mask_RV32 :: Integer
ustatus_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_uie_bitpos))

ustatus_mask_RV64 :: Integer
ustatus_mask_RV64 = ((    shiftL  1  mstatus_sd_bitpos_RV64)

                     .|. (shiftL  3  mstatus_uxl_bitpos)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_uie_bitpos))

-- ================================================================
-- Trap Vectors (mtvec, stvec, utvec) have
--    a 'mode' in bits [1:0]
--    a 'base' in bits [xlen-1:2]
-- MTVEC, STVEC and UTVEC have the same format
--     (for Machine, Supervisor, User privilege levels)

{-# INLINE tvec_mode #-}
tvec_mode :: Integer -> Integer
tvec_mode  tvec = (tvec .&. 3)

tvec_mode_DIRECT   = 0 :: Integer
tvec_mode_VECTORED = 1 :: Integer

{-# INLINE tvec_base #-}
tvec_base :: Integer -> Integer
tvec_base  tvec = shiftL (shiftR  tvec  2) 2

-- ================================================================
-- MIP bit fields (Machine privilege level Interrupt Pending)
-- MIE bit fields (Machine privilege level Interrupt Enable)

mip_usip_bitpos =  0 :: Int
mip_ssip_bitpos =  1 :: Int
mip_msip_bitpos =  3 :: Int

mip_utip_bitpos =  4 :: Int
mip_stip_bitpos =  5 :: Int
mip_mtip_bitpos =  7 :: Int

mip_ueip_bitpos =  8 :: Int
mip_seip_bitpos =  9 :: Int
mip_meip_bitpos = 11 :: Int

-- SIP is a ``view'' of MIP for Supervisor privilege level
-- SIE is a ``view'' of MIE for Supervisor privilege level, with the same mask

sip_mask :: Integer
sip_mask = ((    shiftL 1 mip_seip_bitpos)
            .|. (shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_stip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_ssip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- UIP is a ``view'' of MIP for User privilege level
-- UIE is a ``view'' of MIE for User privilege level, with the same mask

uip_mask :: Integer
uip_mask = ((    shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- ================================================================
-- Function from mstatus, mip, mie and current privilege values to
--     whether or not an interrupt is pending,
-- and if so, the corresponding exception code

{-# INLINE fn_interrupt_pending #-}
fn_interrupt_pending :: Integer ->                    -- MISA
                        Integer ->                    -- MSTATUS
                        Integer ->                    -- MIP
                        Integer ->                    -- MIE
                        Integer ->                    -- MIDELEG
                        Integer ->                    -- SIDELEG
                        Priv_Level -> Maybe Exc_Code
fn_interrupt_pending  misa  mstatus  mip  mie  mideleg  sideleg  priv =
  let
    fn_interrupt_i_pending :: Exc_Code -> Bool
    fn_interrupt_i_pending  ec =
      let
        i = (fromIntegral ec :: Int)
        intr_pending = ((testBit  mip  i) && (testBit  mie  i))
        handler_priv = if (testBit  mideleg  i) then
                         if (misa_flag  misa  'U') then
                           if (misa_flag  misa  'S') then
                             -- System with M, S, U
                             if (testBit  sideleg  i) then
                               if (misa_flag  misa 'N') then
                                 -- M->S->U delegation
                                 u_Priv_Level
                               else
                                 -- TODO: Error: SIDELEG [i] should not be 1 if MISA.N is 0
                                 m_Priv_Level
                             else
                               -- M->S delegation
                               s_Priv_Level
                           else
                             -- System with M, U
                             if (misa_flag  misa  'N') then
                               -- M->U delegation
                               u_Priv_Level
                             else
                               -- TODO: Error: MIDELEG [i] should not be 1 if MISA.N is 0
                               m_Priv_Level
                         else
                           -- Error: System with M only; MIDELEG [i] should not be 1
                           m_Priv_Level
                       else
                         -- no delegation
                         m_Priv_Level

        xie :: Bool
        xie | (priv == u_Priv_Level) = (testBit  mstatus  mstatus_uie_bitpos)
            | (priv == s_Priv_Level) = (testBit  mstatus  mstatus_sie_bitpos)
            | (priv == m_Priv_Level) = (testBit  mstatus  mstatus_mie_bitpos)

        glob_enabled :: Bool
        glob_enabled = ((priv < handler_priv) || ((priv == handler_priv) && xie))
      in
        (intr_pending && glob_enabled)

    -- Check all interrupts in the following decreasing priority order
    m_ec | fn_interrupt_i_pending (exc_code_m_external_interrupt) = Just exc_code_m_external_interrupt
         | fn_interrupt_i_pending (exc_code_m_software_interrupt) = Just exc_code_m_software_interrupt
         | fn_interrupt_i_pending (exc_code_m_timer_interrupt)    = Just exc_code_m_timer_interrupt
         | fn_interrupt_i_pending (exc_code_s_external_interrupt) = Just exc_code_s_external_interrupt
         | fn_interrupt_i_pending (exc_code_s_software_interrupt) = Just exc_code_s_software_interrupt
         | fn_interrupt_i_pending (exc_code_s_timer_interrupt)    = Just exc_code_s_timer_interrupt
         | fn_interrupt_i_pending (exc_code_u_external_interrupt) = Just exc_code_u_external_interrupt
         | fn_interrupt_i_pending (exc_code_u_software_interrupt) = Just exc_code_u_software_interrupt
         | fn_interrupt_i_pending (exc_code_u_timer_interrupt)    = Just exc_code_u_timer_interrupt
         | True                                                   = Nothing
  in
    m_ec

-- ================================================================
-- MCAUSE bit fields

mcause_interrupt_bitpos_RV32 = 31 :: Int
mcause_interrupt_bitpos_RV64 = 63 :: Int

-- Constructor: make an MCAUSE value depending interrupt or trap, and exception code

{-# INLINE mkCause #-}
mkCause :: RV -> Bool -> Exc_Code -> Integer
mkCause  rv  interrupt_not_trap  exc_code =
  let
    msb | interrupt_not_trap && (rv == RV32) = shiftL  1  mcause_interrupt_bitpos_RV32
        | interrupt_not_trap && (rv == RV64) = shiftL  1  mcause_interrupt_bitpos_RV64
        | otherwise                          = 0
  in
    (msb .|. exc_code)

-- ================================================================
-- ISA-F and ISA-D related definitions
-- FCSR bit fields
-- Bit fields
fcsr_nx_bitpos          = 0 :: Int
fcsr_uf_bitpos          = 1 :: Int
fcsr_of_bitpos          = 2 :: Int
fcsr_dz_bitpos          = 3 :: Int
fcsr_nv_bitpos          = 4 :: Int

-- The flags with only one of the bits set
nxFlag = (shiftL  1  fcsr_nx_bitpos) :: Word64
ufFlag = (shiftL  1  fcsr_uf_bitpos) :: Word64
ofFlag = (shiftL  1  fcsr_of_bitpos) :: Word64
dzFlag = (shiftL  1  fcsr_dz_bitpos) :: Word64
nvFlag = (shiftL  1  fcsr_nv_bitpos) :: Word64

-- 7:5
frm_bitpos              = 5 :: Int

-- Extract the fflags field from fcsr
fcsr_fflags    :: Word64 -> Word64 
fcsr_fflags    fcsr = bitSlice fcsr 4 0

-- Extract the frm field from fcsr
fcsr_frm       :: Word64 -> Word64 
fcsr_frm       fcsr = bitSlice fcsr 7 5

-- Check if a frm value in the FCSR.FRM is valid
fcsr_frm_valid :: InstrField -> Bool
fcsr_frm_valid   frm  = (   (frm /= 0x5)
                         && (frm /= 0x6)
                         && (frm /= 0x7))

-- Check if a frm value in the instr is valid
instr_frm_valid:: InstrField -> Bool
instr_frm_valid   frm = (   (frm /= 0x5)
                         && (frm /= 0x6))

-- Bit positions for the mask describing the class of FP value (table 8.5 v2.2)
fclass_negInf_bitpos       = 0 :: Int
fclass_negNorm_bitpos      = 1 :: Int
fclass_negSubNorm_bitpos   = 2 :: Int
fclass_negZero_bitpos      = 3 :: Int
fclass_posZero_bitpos      = 4 :: Int
fclass_posSubNorm_bitpos   = 5 :: Int
fclass_posNorm_bitpos      = 6 :: Int
fclass_posInf_bitpos       = 7 :: Int
fclass_SNaN_bitpos         = 8 :: Int
fclass_QNaN_bitpos         = 9 :: Int

-- Create a FFLAGS word to accrue into the CSR
form_fflags_word  :: Bool -> Bool -> Bool -> Bool -> Bool -> Word64
form_fflags_word  nxf  uff  off  dzf  nvf  =
  let
    fflags = (    (if (nxf) then (shiftL  1  fcsr_nx_bitpos) else 0)
              .|. (if (uff) then (shiftL  1  fcsr_uf_bitpos) else 0)
              .|. (if (off) then (shiftL  1  fcsr_of_bitpos) else 0)
              .|. (if (dzf) then (shiftL  1  fcsr_dz_bitpos) else 0)
              .|. (if (nvf) then (shiftL  1  fcsr_nv_bitpos) else 0))
  in
    fflags



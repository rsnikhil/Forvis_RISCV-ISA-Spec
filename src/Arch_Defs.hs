-- See LICENSE for license details

module Arch_Defs where

-- ================================================================
-- This module has basic, RISC-V definitions (such RV32 vs. RV64,
-- bit-fields of instructions and standard CSRs, etc.) on which the
-- rest of the architectural state and operations depend.

-- ================================================================
-- Standard Haskell imports

import Data.Word    -- for Word8/16/32/64 (unsigned)
import Data.Int     -- for Int8/16/32/64 (signed)
import Data.Bits
import Data.Char

-- Project imports

import Bit_Manipulation

-- ================================================================
-- Major architectural parameters                  -- \begin_latex{RV}

data RV = RV32
        | RV64
        deriving (Eq, Show)
                                                   -- \end_latex{RV}
-- ================================================================
-- Predicate to decide whether a u16 may be a 'C' (Compressed)
-- instruction or not ('C' instrs have 2 lsbs not equal to 2'b11)

is_instr_C :: Word16 -> Bool
is_instr_C  u16 = ((u16 .&. 0x3) /= 0x3)

-- ================================================================
-- Instructions and instruction fields                \begin_latex{Instr}

type Instr   = Word32
type Instr_C = Word16

type InstrField = Word32

-- General-purpose registers

type GPR_Addr = InstrField

-- CSRs

type CSR_Addr = InstrField
                                                   -- \end_latex{Instr}
-- ================================================================
-- Functions to extract instruction fields            \begin_latex{Instr_Field_Functions}

ifield_opcode  :: Instr -> InstrField
ifield_opcode  instr = bitSlice instr  6  0

ifield_funct3  :: Instr -> InstrField
ifield_funct3  instr = bitSlice instr  14  12

ifield_rd      :: Instr -> InstrField
ifield_rd      instr = bitSlice instr  11  7

ifield_rs1     :: Instr -> InstrField
ifield_rs1     instr = bitSlice instr  19  15
                                                   -- \end_latex{Instr_Field_Functions}
ifield_rs2     :: Instr -> InstrField
ifield_rs2     instr = bitSlice instr  24  20
ifield_rs3     :: Instr -> InstrField
ifield_rs3     instr = bitSlice instr  31  27    -- for FMADD, FMSUB, FNMSUB

ifield_funct10 :: Instr -> InstrField
ifield_funct10 instr = (shift (bitSlice instr  31  25) 3) .|. (bitSlice instr  14  12)

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

ifields_U_type :: Instr -> (InstrField, InstrField, InstrField)
ifields_U_type  instr =
  let
    imm20  = bitSlice       instr  31  12
    rd     = ifield_rd      instr
    opcode = ifield_opcode  instr
  in
    (imm20, rd, opcode)

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

i_imm12_fields_7_5 :: InstrField -> (InstrField, InstrField)
i_imm12_fields_7_5  imm12 = (bitSlice  imm12  11  5,
                             bitSlice  imm12   4  0)

i_imm12_fields_6_6 :: InstrField -> (InstrField, InstrField)
i_imm12_fields_6_6  imm12 = (bitSlice  imm12  11  6,
                             bitSlice  imm12   5  0)

-- I-type imm12 fields for FENCE
i_imm12_fields_for_FENCE :: InstrField -> (InstrField, InstrField, InstrField)
i_imm12_fields_for_FENCE  imm12 = (bitSlice  imm12  11  8,
                                   bitSlice  imm12   7  4,
                                   bitSlice  imm12   3  0)

-- R-type funct7 fields for AMO
r_funct7_fields_for_AMO :: InstrField -> (InstrField, InstrField, InstrField)
r_funct7_fields_for_AMO  funct7 = (bitSlice  funct7  6  2,
                                   bitSlice  funct7  1  1,
                                   bitSlice  funct7  0  0)

-- ================================================================
-- Exception Codes                                 \begin_latex{exception_codes_A}

type Exc_Code = Word64

exc_code_u_software_interrupt      :: Exc_Code;    exc_code_u_software_interrupt      =  0;
exc_code_s_software_interrupt      :: Exc_Code;    exc_code_s_software_interrupt      =  1;
                                                -- \end_latex{exception_codes_A}
exc_code_m_software_interrupt      :: Exc_Code;    exc_code_m_software_interrupt      =  3;
exc_code_u_timer_interrupt         :: Exc_Code;    exc_code_u_timer_interrupt         =  4;
exc_code_s_timer_interrupt         :: Exc_Code;    exc_code_s_timer_interrupt         =  5;
exc_code_m_timer_interrupt         :: Exc_Code;    exc_code_m_timer_interrupt         =  7;
exc_code_u_external_interrupt      :: Exc_Code;    exc_code_u_external_interrupt      =  8;
exc_code_s_external_interrupt      :: Exc_Code;    exc_code_s_external_interrupt      =  9;
exc_code_m_external_interrupt      :: Exc_Code;    exc_code_m_external_interrupt      = 11;

                                                -- \begin_latex{exception_codes_B}
exc_code_instr_addr_misaligned     :: Exc_Code;    exc_code_instr_addr_misaligned     =  0;
exc_code_instr_access_fault        :: Exc_Code;    exc_code_instr_access_fault        =  1;
                                                -- \end_latex{exception_codes_B}
exc_code_illegal_instruction       :: Exc_Code;    exc_code_illegal_instruction       =  2;
exc_code_breakpoint                :: Exc_Code;    exc_code_breakpoint                =  3;
exc_code_load_addr_misaligned      :: Exc_Code;    exc_code_load_addr_misaligned      =  4;
exc_code_load_access_fault         :: Exc_Code;    exc_code_load_access_fault         =  5;
exc_code_store_AMO_addr_misaligned :: Exc_Code;    exc_code_store_AMO_addr_misaligned =  6;
exc_code_store_AMO_access_fault    :: Exc_Code;    exc_code_store_AMO_access_fault    =  7;
exc_code_ECall_from_U              :: Exc_Code;    exc_code_ECall_from_U              =  8;
exc_code_ECall_from_S              :: Exc_Code;    exc_code_ECall_from_S              =  9;
exc_code_ECall_from_M              :: Exc_Code;    exc_code_ECall_from_M              = 11;
exc_code_Instruction_Page_Fault    :: Exc_Code;    exc_code_Instruction_Page_Fault    = 12;
exc_code_Load_Page_Fault           :: Exc_Code;    exc_code_Load_Page_Fault           = 13;
exc_code_Store_AMO_Page_Fault      :: Exc_Code;    exc_code_Store_AMO_Page_Fault      = 15;

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

data Mem_Result = Mem_Result_Ok   Word64
                | Mem_Result_Err  Exc_Code
                deriving (Show)
                                                -- \end_latex{Mem_Result}
-- ================================================================
-- Privilege levels                                \begin_latex{Priv_Level}
-- Machine, Supervisor and User

type Priv_Level = InstrField

m_Priv_Level :: Priv_Level;    m_Priv_Level  = 3
s_Priv_Level :: Priv_Level;    s_Priv_Level  = 1
u_Priv_Level :: Priv_Level;    u_Priv_Level  = 0

                                                -- \end_latex{Priv_Level}
-- ================================================================
-- User-Level CSR addresses

csr_addr_ustatus    :: CSR_Addr;    csr_addr_ustatus    = 0x000
csr_addr_uie        :: CSR_Addr;    csr_addr_uie        = 0x004
csr_addr_utvec      :: CSR_Addr;    csr_addr_utvec      = 0x005

csr_addr_uscratch   :: CSR_Addr;    csr_addr_uscratch   = 0x040
csr_addr_uepc       :: CSR_Addr;    csr_addr_uepc       = 0x041
csr_addr_ucause     :: CSR_Addr;    csr_addr_ucause     = 0x042
csr_addr_utval      :: CSR_Addr;    csr_addr_utval      = 0x043
csr_addr_uip        :: CSR_Addr;    csr_addr_uip        = 0x044

csr_addr_fflags     :: CSR_Addr;    csr_addr_fflags     = 0x001
csr_addr_frm        :: CSR_Addr;    csr_addr_frm        = 0x002
csr_addr_fcsr       :: CSR_Addr;    csr_addr_fcsr       = 0x003

csr_addr_cycle      :: CSR_Addr;    csr_addr_cycle      = 0xC00
csr_addr_time       :: CSR_Addr;    csr_addr_time       = 0xC01
csr_addr_instret    :: CSR_Addr;    csr_addr_instret    = 0xC02

csr_addr_cycleh     :: CSR_Addr;    csr_addr_cycleh     = 0xC80
csr_addr_timeh      :: CSR_Addr;    csr_addr_timeh      = 0xC81
csr_addr_instreth   :: CSR_Addr;    csr_addr_instreth   = 0xC82

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

csr_addr_sstatus    :: CSR_Addr;    csr_addr_sstatus    = 0x100
csr_addr_sedeleg    :: CSR_Addr;    csr_addr_sedeleg    = 0x102
csr_addr_sideleg    :: CSR_Addr;    csr_addr_sideleg    = 0x103
csr_addr_sie        :: CSR_Addr;    csr_addr_sie        = 0x104
csr_addr_stvec      :: CSR_Addr;    csr_addr_stvec      = 0x105
csr_addr_scounteren :: CSR_Addr;    csr_addr_scounteren = 0x106

csr_addr_sscratch   :: CSR_Addr;    csr_addr_sscratch   = 0x140
csr_addr_sepc       :: CSR_Addr;    csr_addr_sepc       = 0x141
csr_addr_scause     :: CSR_Addr;    csr_addr_scause     = 0x142
csr_addr_stval      :: CSR_Addr;    csr_addr_stval      = 0x143
csr_addr_sip        :: CSR_Addr;    csr_addr_sip        = 0x144

csr_addr_satp       :: CSR_Addr;    csr_addr_satp       = 0x180

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

csr_addr_mvendorid  :: CSR_Addr;    csr_addr_mvendorid  = 0xF11
csr_addr_marchid    :: CSR_Addr;    csr_addr_marchid    = 0xF12
csr_addr_mimpid     :: CSR_Addr;    csr_addr_mimpid     = 0xF13
csr_addr_mhartid    :: CSR_Addr;    csr_addr_mhartid    = 0xF14

csr_addr_mstatus    :: CSR_Addr;    csr_addr_mstatus    = 0x300
csr_addr_misa       :: CSR_Addr;    csr_addr_misa       = 0x301
csr_addr_medeleg    :: CSR_Addr;    csr_addr_medeleg    = 0x302
csr_addr_mideleg    :: CSR_Addr;    csr_addr_mideleg    = 0x303
csr_addr_mie        :: CSR_Addr;    csr_addr_mie        = 0x304
csr_addr_mtvec      :: CSR_Addr;    csr_addr_mtvec      = 0x305
csr_addr_mcounteren :: CSR_Addr;    csr_addr_mcounteren = 0x306

csr_addr_mscratch   :: CSR_Addr;    csr_addr_mscratch   = 0x340
csr_addr_mepc       :: CSR_Addr;    csr_addr_mepc       = 0x341
csr_addr_mcause     :: CSR_Addr;    csr_addr_mcause     = 0x342
csr_addr_mtval      :: CSR_Addr;    csr_addr_mtval      = 0x343
csr_addr_mip        :: CSR_Addr;    csr_addr_mip        = 0x344

-- TODO: pmpcfgN, pmpaddrN

csr_addr_mcycle     :: CSR_Addr;    csr_addr_mcycle     = 0xB00
csr_addr_minstret   :: CSR_Addr;    csr_addr_minstret   = 0xB02

-- TODO: mhpmcounterN

csr_addr_mcycleh    :: CSR_Addr;    csr_addr_mcycleh    = 0xB80
csr_addr_minstreth  :: CSR_Addr;    csr_addr_minstreth  = 0xB82

-- TODO: mhpmcounterNh

csr_addr_tselect    :: CSR_Addr;    csr_addr_tselect    = 0x7A0
csr_addr_data1      :: CSR_Addr;    csr_addr_data1      = 0x7A1
csr_addr_data2      :: CSR_Addr;    csr_addr_data2      = 0x7A2
csr_addr_data3      :: CSR_Addr;    csr_addr_data3      = 0x7A3

csr_addr_dcsr       :: CSR_Addr;    csr_addr_dcsr       = 0x7B0
csr_addr_dpc        :: CSR_Addr;    csr_addr_dpc        = 0x7B1
csr_addr_dscratch   :: CSR_Addr;    csr_addr_dscratch   = 0x7B2

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

misa_flag :: Word64 -> Char -> Bool
misa_flag  misa  letter | isAsciiUpper  letter = (((shiftR  misa  ((ord letter) - (ord 'A'))) .&. 1) == 1)
misa_flag  misa  letter | isAsciiLower  letter = (((shiftR  misa  ((ord letter) - (ord 'a'))) .&. 1) == 1)
                        | otherwise            = False

-- Codes for MXL, SXL, UXL
xl_rv32  :: Word64;    xl_rv32  = 1
xl_rv64  :: Word64;    xl_rv64  = 2
xl_rv128 :: Word64;    xl_rv128 = 3

-- Bit fields
misa_A_bitpos :: Int;    misa_A_bitpos = 0
misa_B_bitpos :: Int;    misa_B_bitpos = 1
misa_C_bitpos :: Int;    misa_C_bitpos = 2
misa_D_bitpos :: Int;    misa_D_bitpos = 3

misa_E_bitpos :: Int;    misa_E_bitpos = 4
misa_F_bitpos :: Int;    misa_F_bitpos = 5
misa_G_bitpos :: Int;    misa_G_bitpos = 6
misa_H_bitpos :: Int;    misa_H_bitpos = 7

misa_I_bitpos :: Int;    misa_I_bitpos = 8
misa_J_bitpos :: Int;    misa_J_bitpos = 9
misa_K_bitpos :: Int;    misa_K_bitpos = 10
misa_L_bitpos :: Int;    misa_L_bitpos = 11

misa_M_bitpos :: Int;    misa_M_bitpos = 12
misa_N_bitpos :: Int;    misa_N_bitpos = 13
misa_O_bitpos :: Int;    misa_O_bitpos = 14
misa_P_bitpos :: Int;    misa_P_bitpos = 15

misa_Q_bitpos :: Int;    misa_Q_bitpos = 16
misa_R_bitpos :: Int;    misa_R_bitpos = 17
misa_S_bitpos :: Int;    misa_S_bitpos = 18
misa_T_bitpos :: Int;    misa_T_bitpos = 19

misa_U_bitpos :: Int;    misa_U_bitpos = 20
misa_V_bitpos :: Int;    misa_V_bitpos = 21
misa_W_bitpos :: Int;    misa_W_bitpos = 22
misa_X_bitpos :: Int;    misa_X_bitpos = 23

misa_Y_bitpos :: Int;    misa_Y_bitpos = 24
misa_Z_bitpos :: Int;    misa_Z_bitpos = 25

misa_MXL_bitpos_RV32 :: Int;    misa_MXL_bitpos_RV32 = 30
misa_MXL_bitpos_RV64 :: Int;    misa_MXL_bitpos_RV64 = 62

-- ================================================================
-- MSTATUS bit fields

mstatus_sd_bitpos_RV64  :: Int;  mstatus_sd_bitpos_RV64 = 63
mstatus_sd_bitpos_RV32  :: Int;  mstatus_sd_bitpos_RV32 = 31

mstatus_sxl_bitpos      :: Int;  mstatus_sxl_bitpos     = 34
mstatus_uxl_bitpos      :: Int;  mstatus_uxl_bitpos     = 32

mstatus_tsr_bitpos      :: Int;  mstatus_tsr_bitpos     = 22
mstatus_tw_bitpos       :: Int;  mstatus_tw_bitpos      = 21
mstatus_tvm_bitpos      :: Int;  mstatus_tvm_bitpos     = 20

mstatus_mxr_bitpos      :: Int;  mstatus_mxr_bitpos     = 19
mstatus_sum_bitpos      :: Int;  mstatus_sum_bitpos     = 18
mstatus_mprv_bitpos     :: Int;  mstatus_mprv_bitpos    = 17

mstatus_xs_bitpos       :: Int;  mstatus_fs_bitpos      = 15
mstatus_fs_bitpos       :: Int;  mstatus_xs_bitpos      = 13

mstatus_mpp_bitpos      :: Int;  mstatus_mpp_bitpos     = 11
mstatus_spp_bitpos      :: Int;  mstatus_spp_bitpos     =  8

mstatus_mpie_bitpos     :: Int;  mstatus_mpie_bitpos    =  7
mstatus_spie_bitpos     :: Int;  mstatus_spie_bitpos    =  5
mstatus_upie_bitpos     :: Int;  mstatus_upie_bitpos    =  4

mstatus_mie_bitpos      :: Int;  mstatus_mie_bitpos     =  3
mstatus_sie_bitpos      :: Int;  mstatus_sie_bitpos     =  1
mstatus_uie_bitpos      :: Int;  mstatus_uie_bitpos     =  0

-- MSTATUS contains a ``stack'' of ``previous-privilege'' and ``interrupt-enable'' bits

-- Return the stack fields in mstatus

mstatus_stack_fields :: Word64 -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
mstatus_stack_fields  mstatus =
  let
    mpp  = trunc_u64_to_u32  (shiftR  mstatus  mstatus_mpp_bitpos)  .&. 0x3
    spp  = trunc_u64_to_u32  (shiftR  mstatus  mstatus_spp_bitpos)  .&. 0x1
    mpie = trunc_u64_to_u32  (shiftR  mstatus  mstatus_mpie_bitpos) .&. 0x1
    spie = trunc_u64_to_u32  (shiftR  mstatus  mstatus_spie_bitpos) .&. 0x1
    upie = trunc_u64_to_u32  (shiftR  mstatus  mstatus_upie_bitpos) .&. 0x1
    mie  = trunc_u64_to_u32  (shiftR  mstatus  mstatus_mie_bitpos)  .&. 0x1
    sie  = trunc_u64_to_u32  (shiftR  mstatus  mstatus_sie_bitpos)  .&. 0x1
    uie  = trunc_u64_to_u32  (shiftR  mstatus  mstatus_uie_bitpos)  .&. 0x1
  in
    (mpp, spp, mpie, spie, upie, mie, sie, uie)

-- Update the stack fields in mstatus

mstatus_upd_stack_fields :: Word64 ->
                            (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32) ->
                            Word64
mstatus_upd_stack_fields  mstatus (mpp, spp, mpie, spie, upie, mie, sie, uie) =
  let
    mstatus_stack_mask :: Word64
    mstatus_stack_mask = 0x1FFF    -- all LSBs up to and including MPP

    mstatus' = ((mstatus .&. (complement  mstatus_stack_mask))
                .|. (shiftL  (zeroExtend_u32_to_u64  mpp)   mstatus_mpp_bitpos)
                .|. (shiftL  (zeroExtend_u32_to_u64  spp)   mstatus_spp_bitpos)
                .|. (shiftL  (zeroExtend_u32_to_u64  mpie)  mstatus_mpie_bitpos)
                .|. (shiftL  (zeroExtend_u32_to_u64  spie)  mstatus_spie_bitpos)
                .|. (shiftL  (zeroExtend_u32_to_u64  upie)  mstatus_upie_bitpos)
                .|. (shiftL  (zeroExtend_u32_to_u64  mie)   mstatus_mie_bitpos)
                .|. (shiftL  (zeroExtend_u32_to_u64  sie)   mstatus_sie_bitpos)
                .|. (shiftL  (zeroExtend_u32_to_u64  uie)   mstatus_uie_bitpos))
  in
    mstatus'

-- These masks specify which fields are observed/updated in MSTATUS

mstatus_mask_RV32 :: Word64
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

mstatus_mask_RV64 :: Word64
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

sstatus_mask_RV32 :: Word64
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

sstatus_mask_RV64 :: Word64
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

ustatus_mask_RV32 :: Word64
ustatus_mask_RV32 = ((    shiftL  1  mstatus_sd_bitpos_RV32)

                     .|. (shiftL  1  mstatus_mxr_bitpos)
                     .|. (shiftL  1  mstatus_sum_bitpos)

                     .|. (shiftL  3  mstatus_xs_bitpos)
                     .|. (shiftL  3  mstatus_fs_bitpos)

                     .|. (shiftL  1  mstatus_upie_bitpos)

                     .|. (shiftL  1  mstatus_uie_bitpos))

ustatus_mask_RV64 :: Word64
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

tvec_mode :: Word64 -> Word64
tvec_mode  tvec = (tvec .&. 3)

tvec_mode_DIRECT   :: Word64;  tvec_mode_DIRECT   = 0
tvec_mode_VECTORED :: Word64;  tvec_mode_VECTORED = 1

tvec_base :: Word64 -> Word64
tvec_base  tvec = shiftL (shiftR  tvec  2) 2

-- ================================================================
-- MIP bit fields (Machine privilege level Interrupt Pending)
-- MIE bit fields (Machine privilege level Interrupt Enable)

mip_usip_bitpos :: Int;  mip_usip_bitpos =  0
mip_ssip_bitpos :: Int;  mip_ssip_bitpos =  1
mip_msip_bitpos :: Int;  mip_msip_bitpos =  3

mip_utip_bitpos :: Int;  mip_utip_bitpos =  4
mip_stip_bitpos :: Int;  mip_stip_bitpos =  5
mip_mtip_bitpos :: Int;  mip_mtip_bitpos =  7

mip_ueip_bitpos :: Int;  mip_ueip_bitpos =  8
mip_seip_bitpos :: Int;  mip_seip_bitpos =  9
mip_meip_bitpos :: Int;  mip_meip_bitpos = 11

-- SIP is a ``view'' of MIP for Supervisor privilege level
-- SIE is a ``view'' of MIE for Supervisor privilege level, with the same mask

sip_mask :: Word64
sip_mask = ((    shiftL 1 mip_seip_bitpos)
            .|. (shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_stip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_ssip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- UIP is a ``view'' of MIP for User privilege level
-- UIE is a ``view'' of MIE for User privilege level, with the same mask

uip_mask :: Word64
uip_mask = ((    shiftL 1 mip_ueip_bitpos)
            .|. (shiftL 1 mip_utip_bitpos)
            .|. (shiftL 1 mip_usip_bitpos))

-- ================================================================
-- Function from mstatus, mip, mie and current privilege values to
--     whether or not an interrupt is pending,
-- and if so, the corresponding exception code

fn_interrupt_pending :: Word64 -> Word64 -> Word64 -> Priv_Level -> Maybe Exc_Code

fn_interrupt_pending  mstatus  mip  mie  priv =
  let
    -- From mie and mip, find which interrupts are pending
    mi_p_and_e = (mip .&. mie)
    (meip, seip, ueip) = (testBit  mi_p_and_e  mip_meip_bitpos,
                          testBit  mi_p_and_e  mip_seip_bitpos,
                          testBit  mi_p_and_e  mip_ueip_bitpos)

    (mtip, stip, utip) = (testBit  mi_p_and_e  mip_mtip_bitpos,
                          testBit  mi_p_and_e  mip_stip_bitpos,
                          testBit  mi_p_and_e  mip_utip_bitpos)

    (msip, ssip, usip) = (testBit  mi_p_and_e  mip_msip_bitpos,
                          testBit  mi_p_and_e  mip_ssip_bitpos,
                          testBit  mi_p_and_e  mip_usip_bitpos)

    -- Priotitize 'external' > 'software' > 'timer'
    exc_code_m | meip = exc_code_m_external_interrupt
               | msip = exc_code_m_software_interrupt
               | mtip = exc_code_m_timer_interrupt

    exc_code_s | seip = exc_code_s_external_interrupt
               | ssip = exc_code_s_software_interrupt
               | stip = exc_code_s_timer_interrupt

    exc_code_u | ueip = exc_code_u_external_interrupt
               | usip = exc_code_u_software_interrupt
               | utip = exc_code_u_timer_interrupt

    mstatus_mie = testBit  mstatus  mstatus_mie_bitpos
    mstatus_sie = testBit  mstatus  mstatus_sie_bitpos
    mstatus_uie = testBit  mstatus  mstatus_uie_bitpos

    m_exc_code | ((priv == m_Priv_Level) && mstatus_mie && (meip || mtip || msip)) = Just exc_code_m
               | ((priv == m_Priv_Level) && mstatus_mie && (seip || stip || ssip)) = Just exc_code_s
               | ((priv == m_Priv_Level) && mstatus_mie && (ueip || utip || usip)) = Just exc_code_u

               -- TODO: the following should use 'sstatus', not 'mstatus'?
               | ((priv == s_Priv_Level) && mstatus_sie && (seip || stip || ssip)) = Just exc_code_s
               | ((priv == s_Priv_Level) && mstatus_sie && (ueip || utip || usip)) = Just exc_code_u

               -- TODO: the following should use 'ustatus', not 'mstatus'?
               | ((priv == u_Priv_Level) && mstatus_uie && (ueip || utip || usip)) = Just exc_code_u

               | True                                                              = Nothing
  in
    m_exc_code

-- ================================================================
-- MCAUSE bit fields

mcause_interrupt_bitpos_RV32 :: Int; mcause_interrupt_bitpos_RV32 = 31
mcause_interrupt_bitpos_RV64 :: Int; mcause_interrupt_bitpos_RV64 = 63

-- Constructor: make an MCAUSE value depending interrupt or trap, and exception code

mkCause :: RV -> Bool -> Exc_Code -> Word64
mkCause  rv  interrupt_not_trap  exc_code =
  let
    msb | interrupt_not_trap && (rv == RV32) = shiftL  1  mcause_interrupt_bitpos_RV32
        | interrupt_not_trap && (rv == RV64) = shiftL  1  mcause_interrupt_bitpos_RV64
        | otherwise                          = 0
  in
    (msb .|. exc_code)

-- ================================================================

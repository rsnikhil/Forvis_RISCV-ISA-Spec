(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Converted imports: *)

Require Bit_Manipulation.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require Import GHC.Char.
Require Import GHC.Err.
Require Import GHC.Num.
Require Import GHC.Real.
Import Data.Bits.Notations.

(* Converted type declarations: *)

Inductive RV : Type := RV32 : RV |  RV64 : RV.

Definition Instr_C :=
  N%type.

Definition InstrField :=
  N%type.

Definition Priv_Level :=
  InstrField%type.

Definition Instr :=
  N%type.

Definition GPR_Addr :=
  InstrField%type.

Definition Exc_Code :=
  N%type.

Inductive Mem_Result : Type
  := Mem_Result_Ok : N -> Mem_Result
  |  Mem_Result_Err : Exc_Code -> Mem_Result.

Definition CSR_Addr :=
  InstrField%type.

Instance Default__RV : Default RV := Build_Default _ RV32.
(* Midamble *)

Require Import GHC.Err.

Instance Mem_Result_Default : Default Mem_Result :=
  Build_Default _ (Mem_Result_Err default).


(* Converted value declarations: *)

(* Skipping instance Show__Mem_Result of class Show *)

(* Skipping instance Show__RV of class Show *)

Local Definition Eq___RV_op_zeze__ : RV -> RV -> bool :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | RV32, RV32 => true
    | RV64, RV64 => true
    | _, _ => false
    end.

Local Definition Eq___RV_op_zsze__ : RV -> RV -> bool :=
  fun x y => negb (Eq___RV_op_zeze__ x y).

Program Instance Eq___RV : Eq_ RV :=
  fun _ k =>
    k {| op_zeze____ := Eq___RV_op_zeze__ ; op_zsze____ := Eq___RV_op_zsze__ |}.

Definition csr_addr_cycle :=
  fromInteger 3072 : CSR_Addr.

Definition csr_addr_cycleh :=
  fromInteger 3200 : CSR_Addr.

Definition csr_addr_data1 :=
  fromInteger 1953 : CSR_Addr.

Definition csr_addr_data2 :=
  fromInteger 1954 : CSR_Addr.

Definition csr_addr_data3 :=
  fromInteger 1955 : CSR_Addr.

Definition csr_addr_dcsr :=
  fromInteger 1968 : CSR_Addr.

Definition csr_addr_dpc :=
  fromInteger 1969 : CSR_Addr.

Definition csr_addr_dscratch :=
  fromInteger 1970 : CSR_Addr.

Definition csr_addr_fcsr :=
  fromInteger 3 : CSR_Addr.

Definition csr_addr_fflags :=
  fromInteger 1 : CSR_Addr.

Definition csr_addr_frm :=
  fromInteger 2 : CSR_Addr.

Definition csr_addr_instret :=
  fromInteger 3074 : CSR_Addr.

Definition csr_addr_instreth :=
  fromInteger 3202 : CSR_Addr.

Definition csr_addr_marchid :=
  fromInteger 3858 : CSR_Addr.

Definition csr_addr_mcause :=
  fromInteger 834 : CSR_Addr.

Definition csr_addr_mcounteren :=
  fromInteger 774 : CSR_Addr.

Definition csr_addr_mcycle :=
  fromInteger 2816 : CSR_Addr.

Definition csr_addr_mcycleh :=
  fromInteger 2944 : CSR_Addr.

Definition csr_addr_medeleg :=
  fromInteger 770 : CSR_Addr.

Definition csr_addr_mepc :=
  fromInteger 833 : CSR_Addr.

Definition csr_addr_mhartid :=
  fromInteger 3860 : CSR_Addr.

Definition csr_addr_mideleg :=
  fromInteger 771 : CSR_Addr.

Definition csr_addr_mie :=
  fromInteger 772 : CSR_Addr.

Definition csr_addr_mimpid :=
  fromInteger 3859 : CSR_Addr.

Definition csr_addr_minstret :=
  fromInteger 2818 : CSR_Addr.

Definition csr_addr_minstreth :=
  fromInteger 2946 : CSR_Addr.

Definition csr_addr_mip :=
  fromInteger 836 : CSR_Addr.

Definition csr_addr_misa :=
  fromInteger 769 : CSR_Addr.

Definition csr_addr_mscratch :=
  fromInteger 832 : CSR_Addr.

Definition csr_addr_mstatus :=
  fromInteger 768 : CSR_Addr.

Definition csr_addr_mtval :=
  fromInteger 835 : CSR_Addr.

Definition csr_addr_mtvec :=
  fromInteger 773 : CSR_Addr.

Definition csr_addr_mvendorid :=
  fromInteger 3857 : CSR_Addr.

Definition csr_addr_satp :=
  fromInteger 384 : CSR_Addr.

Definition csr_addr_scause :=
  fromInteger 322 : CSR_Addr.

Definition csr_addr_scounteren :=
  fromInteger 262 : CSR_Addr.

Definition csr_addr_sedeleg :=
  fromInteger 258 : CSR_Addr.

Definition csr_addr_sepc :=
  fromInteger 321 : CSR_Addr.

Definition csr_addr_sideleg :=
  fromInteger 259 : CSR_Addr.

Definition csr_addr_sie :=
  fromInteger 260 : CSR_Addr.

Definition csr_addr_sip :=
  fromInteger 324 : CSR_Addr.

Definition csr_addr_sscratch :=
  fromInteger 320 : CSR_Addr.

Definition csr_addr_sstatus :=
  fromInteger 256 : CSR_Addr.

Definition csr_addr_stval :=
  fromInteger 323 : CSR_Addr.

Definition csr_addr_stvec :=
  fromInteger 261 : CSR_Addr.

Definition s_csr_addrs_and_names : list (CSR_Addr * String)%type :=
  cons (pair csr_addr_sstatus (GHC.Base.hs_string__ "sstatus")) (cons (pair
                                                                       csr_addr_sie (GHC.Base.hs_string__ "sie")) (cons
                                                                       (pair csr_addr_sip (GHC.Base.hs_string__ "sip"))
                                                                       (cons (pair csr_addr_sedeleg
                                                                                   (GHC.Base.hs_string__ "sedeleg"))
                                                                             (cons (pair csr_addr_sideleg
                                                                                         (GHC.Base.hs_string__
                                                                                          "sideleg")) (cons (pair
                                                                                                             csr_addr_stvec
                                                                                                             (GHC.Base.hs_string__
                                                                                                              "stvec"))
                                                                                                            (cons (pair
                                                                                                                   csr_addr_sepc
                                                                                                                   (GHC.Base.hs_string__
                                                                                                                    "sepc"))
                                                                                                                  (cons
                                                                                                                   (pair
                                                                                                                    csr_addr_scause
                                                                                                                    (GHC.Base.hs_string__
                                                                                                                     "scause"))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     csr_addr_stval
                                                                                                                     (GHC.Base.hs_string__
                                                                                                                      "stval"))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      csr_addr_sscratch
                                                                                                                      (GHC.Base.hs_string__
                                                                                                                       "sscratch"))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       csr_addr_satp
                                                                                                                       (GHC.Base.hs_string__
                                                                                                                        "satp"))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        csr_addr_scounteren
                                                                                                                        (GHC.Base.hs_string__
                                                                                                                         "scounteren"))
                                                                                                                       nil))))))))))).

Definition csr_addr_time :=
  fromInteger 3073 : CSR_Addr.

Definition csr_addr_timeh :=
  fromInteger 3201 : CSR_Addr.

Definition csr_addr_tselect :=
  fromInteger 1952 : CSR_Addr.

Definition m_csr_addrs_and_names : list (CSR_Addr * String)%type :=
  cons (pair csr_addr_mstatus (GHC.Base.hs_string__ "mstatus")) (cons (pair
                                                                       csr_addr_mie (GHC.Base.hs_string__ "mie")) (cons
                                                                       (pair csr_addr_mip (GHC.Base.hs_string__ "mip"))
                                                                       (cons (pair csr_addr_medeleg
                                                                                   (GHC.Base.hs_string__ "medeleg"))
                                                                             (cons (pair csr_addr_mideleg
                                                                                         (GHC.Base.hs_string__
                                                                                          "mideleg")) (cons (pair
                                                                                                             csr_addr_mtvec
                                                                                                             (GHC.Base.hs_string__
                                                                                                              "mtvec"))
                                                                                                            (cons (pair
                                                                                                                   csr_addr_mepc
                                                                                                                   (GHC.Base.hs_string__
                                                                                                                    "mepc"))
                                                                                                                  (cons
                                                                                                                   (pair
                                                                                                                    csr_addr_mcause
                                                                                                                    (GHC.Base.hs_string__
                                                                                                                     "mcause"))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     csr_addr_mtval
                                                                                                                     (GHC.Base.hs_string__
                                                                                                                      "mtval"))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      csr_addr_mscratch
                                                                                                                      (GHC.Base.hs_string__
                                                                                                                       "mscratch"))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       csr_addr_minstret
                                                                                                                       (GHC.Base.hs_string__
                                                                                                                        "minstret"))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        csr_addr_minstreth
                                                                                                                        (GHC.Base.hs_string__
                                                                                                                         "minstreth"))
                                                                                                                       (cons
                                                                                                                        (pair
                                                                                                                         csr_addr_mcycle
                                                                                                                         (GHC.Base.hs_string__
                                                                                                                          "mcycle"))
                                                                                                                        (cons
                                                                                                                         (pair
                                                                                                                          csr_addr_mcycleh
                                                                                                                          (GHC.Base.hs_string__
                                                                                                                           "mcycleh"))
                                                                                                                         (cons
                                                                                                                          (pair
                                                                                                                           csr_addr_mcounteren
                                                                                                                           (GHC.Base.hs_string__
                                                                                                                            "mcounteren"))
                                                                                                                          (cons
                                                                                                                           (pair
                                                                                                                            csr_addr_mvendorid
                                                                                                                            (GHC.Base.hs_string__
                                                                                                                             "mvendorid"))
                                                                                                                           (cons
                                                                                                                            (pair
                                                                                                                             csr_addr_marchid
                                                                                                                             (GHC.Base.hs_string__
                                                                                                                              "marchid"))
                                                                                                                            (cons
                                                                                                                             (pair
                                                                                                                              csr_addr_mimpid
                                                                                                                              (GHC.Base.hs_string__
                                                                                                                               "mimpid"))
                                                                                                                             (cons
                                                                                                                              (pair
                                                                                                                               csr_addr_mhartid
                                                                                                                               (GHC.Base.hs_string__
                                                                                                                                "mhartid"))
                                                                                                                              (cons
                                                                                                                               (pair
                                                                                                                                csr_addr_misa
                                                                                                                                (GHC.Base.hs_string__
                                                                                                                                 "misa"))
                                                                                                                               (cons
                                                                                                                                (pair
                                                                                                                                 csr_addr_tselect
                                                                                                                                 (GHC.Base.hs_string__
                                                                                                                                  "tselect"))
                                                                                                                                (cons
                                                                                                                                 (pair
                                                                                                                                  csr_addr_data1
                                                                                                                                  (GHC.Base.hs_string__
                                                                                                                                   "data1"))
                                                                                                                                 (cons
                                                                                                                                  (pair
                                                                                                                                   csr_addr_data2
                                                                                                                                   (GHC.Base.hs_string__
                                                                                                                                    "data2"))
                                                                                                                                  (cons
                                                                                                                                   (pair
                                                                                                                                    csr_addr_data3
                                                                                                                                    (GHC.Base.hs_string__
                                                                                                                                     "data3"))
                                                                                                                                   (cons
                                                                                                                                    (pair
                                                                                                                                     csr_addr_dcsr
                                                                                                                                     (GHC.Base.hs_string__
                                                                                                                                      "dcsr"))
                                                                                                                                    (cons
                                                                                                                                     (pair
                                                                                                                                      csr_addr_dpc
                                                                                                                                      (GHC.Base.hs_string__
                                                                                                                                       "dpc"))
                                                                                                                                     (cons
                                                                                                                                      (pair
                                                                                                                                       csr_addr_dscratch
                                                                                                                                       (GHC.Base.hs_string__
                                                                                                                                        "dscratch"))
                                                                                                                                      nil)))))))))))))))))))))))))).

Definition csr_addr_ucause :=
  fromInteger 66 : CSR_Addr.

Definition csr_addr_uepc :=
  fromInteger 65 : CSR_Addr.

Definition csr_addr_uie :=
  fromInteger 4 : CSR_Addr.

Definition csr_addr_uip :=
  fromInteger 68 : CSR_Addr.

Definition csr_addr_uscratch :=
  fromInteger 64 : CSR_Addr.

Definition csr_addr_ustatus :=
  fromInteger 0 : CSR_Addr.

Definition csr_addr_utval :=
  fromInteger 67 : CSR_Addr.

Definition csr_addr_utvec :=
  fromInteger 5 : CSR_Addr.

Definition u_csr_addrs_and_names : list (CSR_Addr * String)%type :=
  cons (pair csr_addr_ustatus (GHC.Base.hs_string__ "ustatus")) (cons (pair
                                                                       csr_addr_uie (GHC.Base.hs_string__ "uie")) (cons
                                                                       (pair csr_addr_uip (GHC.Base.hs_string__ "uip"))
                                                                       (cons (pair csr_addr_time (GHC.Base.hs_string__
                                                                                    "time")) (cons (pair csr_addr_timeh
                                                                                                         (GHC.Base.hs_string__
                                                                                                          "timeh"))
                                                                                                   (cons (pair
                                                                                                          csr_addr_utvec
                                                                                                          (GHC.Base.hs_string__
                                                                                                           "utvec"))
                                                                                                         (cons (pair
                                                                                                                csr_addr_uepc
                                                                                                                (GHC.Base.hs_string__
                                                                                                                 "uepc"))
                                                                                                               (cons
                                                                                                                (pair
                                                                                                                 csr_addr_ucause
                                                                                                                 (GHC.Base.hs_string__
                                                                                                                  "ucause"))
                                                                                                                (cons
                                                                                                                 (pair
                                                                                                                  csr_addr_utval
                                                                                                                  (GHC.Base.hs_string__
                                                                                                                   "utval"))
                                                                                                                 (cons
                                                                                                                  (pair
                                                                                                                   csr_addr_uscratch
                                                                                                                   (GHC.Base.hs_string__
                                                                                                                    "uscratch"))
                                                                                                                  (cons
                                                                                                                   (pair
                                                                                                                    csr_addr_cycle
                                                                                                                    (GHC.Base.hs_string__
                                                                                                                     "cycle"))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     csr_addr_cycleh
                                                                                                                     (GHC.Base.hs_string__
                                                                                                                      "cycleh"))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      csr_addr_instret
                                                                                                                      (GHC.Base.hs_string__
                                                                                                                       "instret"))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       csr_addr_instreth
                                                                                                                       (GHC.Base.hs_string__
                                                                                                                        "instreth"))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        csr_addr_fflags
                                                                                                                        (GHC.Base.hs_string__
                                                                                                                         "fflags"))
                                                                                                                       (cons
                                                                                                                        (pair
                                                                                                                         csr_addr_frm
                                                                                                                         (GHC.Base.hs_string__
                                                                                                                          "frm"))
                                                                                                                        (cons
                                                                                                                         (pair
                                                                                                                          csr_addr_fcsr
                                                                                                                          (GHC.Base.hs_string__
                                                                                                                           "fcsr"))
                                                                                                                         nil)))))))))))))))).

Definition exc_code_ECall_from_M :=
  fromInteger 11 : Exc_Code.

Definition exc_code_ECall_from_S :=
  fromInteger 9 : Exc_Code.

Definition exc_code_ECall_from_U :=
  fromInteger 8 : Exc_Code.

Definition exc_code_Instruction_Page_Fault :=
  fromInteger 12 : Exc_Code.

Definition exc_code_Load_Page_Fault :=
  fromInteger 13 : Exc_Code.

Definition exc_code_Store_AMO_Page_Fault :=
  fromInteger 15 : Exc_Code.

Definition exc_code_breakpoint :=
  fromInteger 3 : Exc_Code.

Definition exc_code_illegal_instruction :=
  fromInteger 2 : Exc_Code.

Definition exc_code_instr_access_fault :=
  fromInteger 1 : Exc_Code.

Definition exc_code_instr_addr_misaligned :=
  fromInteger 0 : Exc_Code.

Definition exc_code_load_access_fault :=
  fromInteger 5 : Exc_Code.

Definition exc_code_load_addr_misaligned :=
  fromInteger 4 : Exc_Code.

Definition exc_code_m_external_interrupt :=
  fromInteger 11 : Exc_Code.

Definition exc_code_m_software_interrupt :=
  fromInteger 3 : Exc_Code.

Definition exc_code_m_timer_interrupt :=
  fromInteger 7 : Exc_Code.

Definition exc_code_s_external_interrupt :=
  fromInteger 9 : Exc_Code.

Definition exc_code_s_software_interrupt :=
  fromInteger 1 : Exc_Code.

Definition exc_code_s_timer_interrupt :=
  fromInteger 5 : Exc_Code.

Definition exc_code_store_AMO_access_fault :=
  fromInteger 7 : Exc_Code.

Definition exc_code_store_AMO_addr_misaligned :=
  fromInteger 6 : Exc_Code.

Definition show_trap_exc_code : Exc_Code -> String :=
  fun arg_0__ =>
    let 'ec := arg_0__ in
    if (ec == exc_code_instr_addr_misaligned) : bool
    then GHC.Base.hs_string__ "exc_code_instr_addr_misaligned" else
    let 'ec := arg_0__ in
    if (ec == exc_code_instr_access_fault) : bool
    then GHC.Base.hs_string__ "exc_code_instr_access_fault" else
    let 'ec := arg_0__ in
    if (ec == exc_code_illegal_instruction) : bool
    then GHC.Base.hs_string__ "exc_code_illegal_instruction" else
    let 'ec := arg_0__ in
    if (ec == exc_code_breakpoint) : bool
    then GHC.Base.hs_string__ "exc_code_breakpoint" else
    let 'ec := arg_0__ in
    if (ec == exc_code_load_addr_misaligned) : bool
    then GHC.Base.hs_string__ "exc_code_load_addr_misaligned" else
    let 'ec := arg_0__ in
    if (ec == exc_code_load_access_fault) : bool
    then GHC.Base.hs_string__ "exc_code_load_access_fault" else
    let 'ec := arg_0__ in
    if (ec == exc_code_store_AMO_addr_misaligned) : bool
    then GHC.Base.hs_string__ "exc_code_store_AMO_addr_misaligned" else
    let 'ec := arg_0__ in
    if (ec == exc_code_store_AMO_access_fault) : bool
    then GHC.Base.hs_string__ "exc_code_store_AMO_access_fault" else
    let 'ec := arg_0__ in
    if (ec == exc_code_ECall_from_U) : bool
    then GHC.Base.hs_string__ "exc_code_ECall_from_U" else
    let 'ec := arg_0__ in
    if (ec == exc_code_ECall_from_S) : bool
    then GHC.Base.hs_string__ "exc_code_ECall_from_S" else
    let 'ec := arg_0__ in
    if (ec == exc_code_ECall_from_M) : bool
    then GHC.Base.hs_string__ "exc_code_ECall_from_M" else
    let 'ec := arg_0__ in
    if (ec == exc_code_Instruction_Page_Fault) : bool
    then GHC.Base.hs_string__ "exc_code_Instruction_Page_Fault" else
    let 'ec := arg_0__ in
    if (ec == exc_code_Load_Page_Fault) : bool
    then GHC.Base.hs_string__ "exc_code_Load_Page_Fault" else
    let 'ec := arg_0__ in
    if (ec == exc_code_Store_AMO_Page_Fault) : bool
    then GHC.Base.hs_string__ "exc_code_Store_AMO_Page_Fault" else
    patternFailure.

Definition exc_code_u_external_interrupt :=
  fromInteger 8 : Exc_Code.

Definition exc_code_u_software_interrupt :=
  fromInteger 0 : Exc_Code.

Definition exc_code_u_timer_interrupt :=
  fromInteger 4 : Exc_Code.

Definition show_interrupt_exc_code : Exc_Code -> String :=
  fun arg_0__ =>
    let 'ec := arg_0__ in
    if (ec == exc_code_u_software_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_u_software_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_s_software_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_s_software_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_m_software_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_m_software_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_u_timer_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_u_timer_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_s_timer_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_s_timer_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_m_timer_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_m_timer_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_u_external_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_u_external_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_s_external_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_s_external_interrupt" else
    let 'ec := arg_0__ in
    if (ec == exc_code_m_external_interrupt) : bool
    then GHC.Base.hs_string__ "exc_code_m_external_interrupt" else
    patternFailure.

Definition i_imm12_fields_6_6 : InstrField -> (InstrField * InstrField)%type :=
  fun imm12 =>
    pair (Bit_Manipulation.bitSlice imm12 (fromInteger 11) (fromInteger 6))
         (Bit_Manipulation.bitSlice imm12 (fromInteger 5) (fromInteger 0)).

Definition i_imm12_fields_7_5 : InstrField -> (InstrField * InstrField)%type :=
  fun imm12 =>
    pair (Bit_Manipulation.bitSlice imm12 (fromInteger 11) (fromInteger 5))
         (Bit_Manipulation.bitSlice imm12 (fromInteger 4) (fromInteger 0)).

Definition i_imm12_fields_for_FENCE
   : InstrField -> (InstrField * InstrField * InstrField)%type :=
  fun imm12 =>
    pair (pair (Bit_Manipulation.bitSlice imm12 (fromInteger 11) (fromInteger 8))
               (Bit_Manipulation.bitSlice imm12 (fromInteger 7) (fromInteger 4)))
         (Bit_Manipulation.bitSlice imm12 (fromInteger 3) (fromInteger 0)).

Definition ifield_funct10 : Instr -> InstrField :=
  fun instr =>
    (Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 31) (fromInteger
                                                                        25)) (fromInteger 3)) Data.Bits..|.(**)
    (Bit_Manipulation.bitSlice instr (fromInteger 14) (fromInteger 12)).

Definition ifield_funct3 : Instr -> InstrField :=
  fun instr => Bit_Manipulation.bitSlice instr (fromInteger 14) (fromInteger 12).

Definition ifield_opcode : Instr -> InstrField :=
  fun instr => Bit_Manipulation.bitSlice instr (fromInteger 6) (fromInteger 0).

Definition ifield_rd : Instr -> InstrField :=
  fun instr => Bit_Manipulation.bitSlice instr (fromInteger 11) (fromInteger 7).

Definition ifields_J_type
   : Instr -> (InstrField * InstrField * InstrField)%type :=
  fun instr =>
    let opcode := ifield_opcode instr in
    let rd := ifield_rd instr in
    let imm20 :=
      (((Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 31)
                          (fromInteger 31)) (fromInteger 19) Data.Bits..|.(**)
         Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 19) (fromInteger
                                                                            12)) (fromInteger 11)) Data.Bits..|.(**)
        Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 20) (fromInteger
                                                                           20)) (fromInteger 10)) Data.Bits..|.(**)
       Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 30) (fromInteger
                                                                          21)) (fromInteger 0)) in
    pair (pair imm20 rd) opcode.

Definition ifields_U_type
   : Instr -> (InstrField * InstrField * InstrField)%type :=
  fun instr =>
    let opcode := ifield_opcode instr in
    let rd := ifield_rd instr in
    let imm20 :=
      Bit_Manipulation.bitSlice instr (fromInteger 31) (fromInteger 12) in
    pair (pair imm20 rd) opcode.

Definition ifield_rs1 : Instr -> InstrField :=
  fun instr => Bit_Manipulation.bitSlice instr (fromInteger 19) (fromInteger 15).

Definition ifields_I_type
   : Instr ->
     (InstrField * InstrField * InstrField * InstrField * InstrField)%type :=
  fun instr =>
    let opcode := ifield_opcode instr in
    let rd := ifield_rd instr in
    let funct3 := ifield_funct3 instr in
    let rs1 := ifield_rs1 instr in
    let imm12 :=
      Bit_Manipulation.bitSlice instr (fromInteger 31) (fromInteger 20) in
    pair (pair (pair (pair imm12 rs1) funct3) rd) opcode.

Definition ifield_rs2 : Instr -> InstrField :=
  fun instr => Bit_Manipulation.bitSlice instr (fromInteger 24) (fromInteger 20).

Definition ifields_B_type
   : Instr ->
     (InstrField * InstrField * InstrField * InstrField * InstrField)%type :=
  fun instr =>
    let opcode := ifield_opcode instr in
    let funct3 := ifield_funct3 instr in
    let rs1 := ifield_rs1 instr in
    let rs2 := ifield_rs2 instr in
    let imm12 :=
      (((Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 31)
                          (fromInteger 31)) (fromInteger 11) Data.Bits..|.(**)
         Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 7) (fromInteger
                                                                           7)) (fromInteger 10)) Data.Bits..|.(**)
        Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 30) (fromInteger
                                                                           25)) (fromInteger 4)) Data.Bits..|.(**)
       Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 11) (fromInteger
                                                                          8)) (fromInteger 0)) in
    pair (pair (pair (pair imm12 rs2) rs1) funct3) opcode.

Definition ifields_R_type
   : Instr ->
     (InstrField * InstrField * InstrField * InstrField * InstrField *
      InstrField)%type :=
  fun instr =>
    let opcode := ifield_opcode instr in
    let rd := ifield_rd instr in
    let funct3 := ifield_funct3 instr in
    let rs1 := ifield_rs1 instr in
    let rs2 := ifield_rs2 instr in
    let funct7 :=
      Bit_Manipulation.bitSlice instr (fromInteger 31) (fromInteger 25) in
    pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode.

Definition ifields_S_type
   : Instr ->
     (InstrField * InstrField * InstrField * InstrField * InstrField)%type :=
  fun instr =>
    let opcode := ifield_opcode instr in
    let funct3 := ifield_funct3 instr in
    let rs1 := ifield_rs1 instr in
    let rs2 := ifield_rs2 instr in
    let imm12 :=
      (Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 31) (fromInteger
                                                                          25)) (fromInteger 5) Data.Bits..|.(**)
       Data.Bits.shift (Bit_Manipulation.bitSlice instr (fromInteger 11) (fromInteger
                                                                          7)) (fromInteger 0)) in
    pair (pair (pair (pair imm12 rs2) rs1) funct3) opcode.

Definition ifield_rs3 : Instr -> InstrField :=
  fun instr => Bit_Manipulation.bitSlice instr (fromInteger 31) (fromInteger 27).

Definition is_instr_C : N -> bool :=
  fun u16 => ((u16 Data.Bits..&.(**) fromInteger 3) /= fromInteger 3).

Definition m_Priv_Level :=
  fromInteger 3 : Priv_Level.

Definition mcause_interrupt_bitpos_RV32 :=
  fromInteger 31 : Int.

Definition mcause_interrupt_bitpos_RV64 :=
  fromInteger 63 : Int.

Definition mkCause : RV -> bool -> Exc_Code -> N :=
  fun rv interrupt_not_trap exc_code =>
    let msb :=
      if andb interrupt_not_trap (rv == RV32) : bool
      then Data.Bits.shiftL (fromInteger 1) mcause_interrupt_bitpos_RV32 else
      if andb interrupt_not_trap (rv == RV64) : bool
      then Data.Bits.shiftL (fromInteger 1) mcause_interrupt_bitpos_RV64 else
      fromInteger 0 in
    (msb Data.Bits..|.(**) exc_code).

Definition mip_meip_bitpos :=
  fromInteger 11 : Int.

Definition mip_msip_bitpos :=
  fromInteger 3 : Int.

Definition mip_mtip_bitpos :=
  fromInteger 7 : Int.

Definition mip_seip_bitpos :=
  fromInteger 9 : Int.

Definition mip_ssip_bitpos :=
  fromInteger 1 : Int.

Definition mip_stip_bitpos :=
  fromInteger 5 : Int.

Definition mip_ueip_bitpos :=
  fromInteger 8 : Int.

Definition mip_usip_bitpos :=
  fromInteger 0 : Int.

Definition mip_utip_bitpos :=
  fromInteger 4 : Int.

Definition uip_mask : N :=
  (((Data.Bits.shiftL (fromInteger 1) mip_ueip_bitpos) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mip_utip_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mip_usip_bitpos)).

Definition sip_mask : N :=
  ((((((Data.Bits.shiftL (fromInteger 1) mip_seip_bitpos) Data.Bits..|.(**)
       (Data.Bits.shiftL (fromInteger 1) mip_ueip_bitpos)) Data.Bits..|.(**)
      (Data.Bits.shiftL (fromInteger 1) mip_stip_bitpos)) Data.Bits..|.(**)
     (Data.Bits.shiftL (fromInteger 1) mip_utip_bitpos)) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mip_ssip_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mip_usip_bitpos)).

Definition misa_A_bitpos :=
  fromInteger 0 : Int.

Definition misa_B_bitpos :=
  fromInteger 1 : Int.

Definition misa_C_bitpos :=
  fromInteger 2 : Int.

Definition misa_D_bitpos :=
  fromInteger 3 : Int.

Definition misa_E_bitpos :=
  fromInteger 4 : Int.

Definition misa_F_bitpos :=
  fromInteger 5 : Int.

Definition misa_G_bitpos :=
  fromInteger 6 : Int.

Definition misa_H_bitpos :=
  fromInteger 7 : Int.

Definition misa_I_bitpos :=
  fromInteger 8 : Int.

Definition misa_J_bitpos :=
  fromInteger 9 : Int.

Definition misa_K_bitpos :=
  fromInteger 10 : Int.

Definition misa_L_bitpos :=
  fromInteger 11 : Int.

Definition misa_MXL_bitpos_RV32 :=
  fromInteger 30 : Int.

Definition misa_MXL_bitpos_RV64 :=
  fromInteger 62 : Int.

Definition misa_M_bitpos :=
  fromInteger 12 : Int.

Definition misa_N_bitpos :=
  fromInteger 13 : Int.

Definition misa_O_bitpos :=
  fromInteger 14 : Int.

Definition misa_P_bitpos :=
  fromInteger 15 : Int.

Definition misa_Q_bitpos :=
  fromInteger 16 : Int.

Definition misa_R_bitpos :=
  fromInteger 17 : Int.

Definition misa_S_bitpos :=
  fromInteger 18 : Int.

Definition misa_T_bitpos :=
  fromInteger 19 : Int.

Definition misa_U_bitpos :=
  fromInteger 20 : Int.

Definition misa_V_bitpos :=
  fromInteger 21 : Int.

Definition misa_W_bitpos :=
  fromInteger 22 : Int.

Definition misa_X_bitpos :=
  fromInteger 23 : Int.

Definition misa_Y_bitpos :=
  fromInteger 24 : Int.

Definition misa_Z_bitpos :=
  fromInteger 25 : Int.

Definition misa_flag : N -> Char -> bool :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | misa, letter =>
        let j_3__ :=
          match arg_0__, arg_1__ with
          | misa, letter =>
              (((Data.Bits.shiftR misa ((ord letter) - (ord (GHC.Char.hs_char__ "a"))))
                Data.Bits..&.(**)
                fromInteger 1) ==
               fromInteger 1)
          end in
        (((Data.Bits.shiftR misa ((ord letter) - (ord (GHC.Char.hs_char__ "A"))))
          Data.Bits..&.(**)
          fromInteger 1) ==
         fromInteger 1)
    end.

Definition mstatus_fs_bitpos :=
  fromInteger 15 : Int.

Definition mstatus_mie_bitpos :=
  fromInteger 3 : Int.

Definition mstatus_mpie_bitpos :=
  fromInteger 7 : Int.

Definition mstatus_mpp_bitpos :=
  fromInteger 11 : Int.

Definition mstatus_mprv_bitpos :=
  fromInteger 17 : Int.

Definition mstatus_mxr_bitpos :=
  fromInteger 19 : Int.

Definition mstatus_sd_bitpos_RV32 :=
  fromInteger 31 : Int.

Definition mstatus_sd_bitpos_RV64 :=
  fromInteger 63 : Int.

Definition mstatus_sie_bitpos :=
  fromInteger 1 : Int.

Definition mstatus_spie_bitpos :=
  fromInteger 5 : Int.

Definition mstatus_spp_bitpos :=
  fromInteger 8 : Int.

Definition mstatus_sum_bitpos :=
  fromInteger 18 : Int.

Definition mstatus_sxl_bitpos :=
  fromInteger 34 : Int.

Definition mstatus_tsr_bitpos :=
  fromInteger 22 : Int.

Definition mstatus_tvm_bitpos :=
  fromInteger 20 : Int.

Definition mstatus_tw_bitpos :=
  fromInteger 21 : Int.

Definition mstatus_uie_bitpos :=
  fromInteger 0 : Int.

Definition mstatus_upie_bitpos :=
  fromInteger 4 : Int.

Definition mstatus_stack_fields : N -> (N * N * N * N * N * N * N * N)%type :=
  fun mstatus =>
    let uie :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_uie_bitpos)
      Data.Bits..&.(**)
      fromInteger 1 in
    let sie :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_sie_bitpos)
      Data.Bits..&.(**)
      fromInteger 1 in
    let mie :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_mie_bitpos)
      Data.Bits..&.(**)
      fromInteger 1 in
    let upie :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_upie_bitpos)
      Data.Bits..&.(**)
      fromInteger 1 in
    let spie :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_spie_bitpos)
      Data.Bits..&.(**)
      fromInteger 1 in
    let mpie :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_mpie_bitpos)
      Data.Bits..&.(**)
      fromInteger 1 in
    let spp :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_spp_bitpos)
      Data.Bits..&.(**)
      fromInteger 1 in
    let mpp :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR mstatus mstatus_mpp_bitpos)
      Data.Bits..&.(**)
      fromInteger 3 in
    pair (pair (pair (pair (pair (pair (pair mpp spp) mpie) spie) upie) mie) sie)
         uie.

Definition mstatus_upd_stack_fields
   : N -> (N * N * N * N * N * N * N * N)%type -> N :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | mstatus
    , pair (pair (pair (pair (pair (pair (pair mpp spp) mpie) spie) upie) mie) sie)
    uie =>
        let mstatus_stack_mask : N := fromInteger 8191 in
        let mstatus' :=
          (((((((((mstatus Data.Bits..&.(**) (Data.Bits.complement mstatus_stack_mask))
                  Data.Bits..|.(**)
                  (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 mpp)
                   mstatus_mpp_bitpos)) Data.Bits..|.(**)
                 (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 spp)
                  mstatus_spp_bitpos)) Data.Bits..|.(**)
                (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 mpie)
                 mstatus_mpie_bitpos)) Data.Bits..|.(**)
               (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 spie)
                mstatus_spie_bitpos)) Data.Bits..|.(**)
              (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 upie)
               mstatus_upie_bitpos)) Data.Bits..|.(**)
             (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 mie)
              mstatus_mie_bitpos)) Data.Bits..|.(**)
            (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 sie)
             mstatus_sie_bitpos)) Data.Bits..|.(**)
           (Data.Bits.shiftL (Bit_Manipulation.zeroExtend_u32_to_u64 uie)
            mstatus_uie_bitpos)) in
        mstatus'
    end.

Definition mstatus_uxl_bitpos :=
  fromInteger 32 : Int.

Definition mstatus_xs_bitpos :=
  fromInteger 13 : Int.

Definition mstatus_mask_RV32 : N :=
  (((((((((((((((((Data.Bits.shiftL (fromInteger 1) mstatus_sd_bitpos_RV32)
                  Data.Bits..|.(**)
                  (Data.Bits.shiftL (fromInteger 1) mstatus_tsr_bitpos)) Data.Bits..|.(**)
                 (Data.Bits.shiftL (fromInteger 1) mstatus_tw_bitpos)) Data.Bits..|.(**)
                (Data.Bits.shiftL (fromInteger 1) mstatus_tvm_bitpos)) Data.Bits..|.(**)
               (Data.Bits.shiftL (fromInteger 1) mstatus_mxr_bitpos)) Data.Bits..|.(**)
              (Data.Bits.shiftL (fromInteger 1) mstatus_sum_bitpos)) Data.Bits..|.(**)
             (Data.Bits.shiftL (fromInteger 1) mstatus_mprv_bitpos)) Data.Bits..|.(**)
            (Data.Bits.shiftL (fromInteger 3) mstatus_xs_bitpos)) Data.Bits..|.(**)
           (Data.Bits.shiftL (fromInteger 3) mstatus_fs_bitpos)) Data.Bits..|.(**)
          (Data.Bits.shiftL (fromInteger 3) mstatus_mpp_bitpos)) Data.Bits..|.(**)
         (Data.Bits.shiftL (fromInteger 1) mstatus_spp_bitpos)) Data.Bits..|.(**)
        (Data.Bits.shiftL (fromInteger 1) mstatus_mpie_bitpos)) Data.Bits..|.(**)
       (Data.Bits.shiftL (fromInteger 1) mstatus_spie_bitpos)) Data.Bits..|.(**)
      (Data.Bits.shiftL (fromInteger 1) mstatus_upie_bitpos)) Data.Bits..|.(**)
     (Data.Bits.shiftL (fromInteger 1) mstatus_mie_bitpos)) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mstatus_sie_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mstatus_uie_bitpos)).

Definition mstatus_mask_RV64 : N :=
  (((((((((((((((((Data.Bits.shiftL (fromInteger 1) mstatus_sd_bitpos_RV64)
                  Data.Bits..|.(**)
                  (Data.Bits.shiftL (fromInteger 1) mstatus_tsr_bitpos)) Data.Bits..|.(**)
                 (Data.Bits.shiftL (fromInteger 1) mstatus_tw_bitpos)) Data.Bits..|.(**)
                (Data.Bits.shiftL (fromInteger 1) mstatus_tvm_bitpos)) Data.Bits..|.(**)
               (Data.Bits.shiftL (fromInteger 1) mstatus_mxr_bitpos)) Data.Bits..|.(**)
              (Data.Bits.shiftL (fromInteger 1) mstatus_sum_bitpos)) Data.Bits..|.(**)
             (Data.Bits.shiftL (fromInteger 1) mstatus_mprv_bitpos)) Data.Bits..|.(**)
            (Data.Bits.shiftL (fromInteger 3) mstatus_xs_bitpos)) Data.Bits..|.(**)
           (Data.Bits.shiftL (fromInteger 3) mstatus_fs_bitpos)) Data.Bits..|.(**)
          (Data.Bits.shiftL (fromInteger 3) mstatus_mpp_bitpos)) Data.Bits..|.(**)
         (Data.Bits.shiftL (fromInteger 1) mstatus_spp_bitpos)) Data.Bits..|.(**)
        (Data.Bits.shiftL (fromInteger 1) mstatus_mpie_bitpos)) Data.Bits..|.(**)
       (Data.Bits.shiftL (fromInteger 1) mstatus_spie_bitpos)) Data.Bits..|.(**)
      (Data.Bits.shiftL (fromInteger 1) mstatus_upie_bitpos)) Data.Bits..|.(**)
     (Data.Bits.shiftL (fromInteger 1) mstatus_mie_bitpos)) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mstatus_sie_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mstatus_uie_bitpos)).

Definition sstatus_mask_RV32 : N :=
  ((((((((((Data.Bits.shiftL (fromInteger 1) mstatus_sd_bitpos_RV32)
           Data.Bits..|.(**)
           (Data.Bits.shiftL (fromInteger 1) mstatus_mxr_bitpos)) Data.Bits..|.(**)
          (Data.Bits.shiftL (fromInteger 1) mstatus_sum_bitpos)) Data.Bits..|.(**)
         (Data.Bits.shiftL (fromInteger 3) mstatus_xs_bitpos)) Data.Bits..|.(**)
        (Data.Bits.shiftL (fromInteger 3) mstatus_fs_bitpos)) Data.Bits..|.(**)
       (Data.Bits.shiftL (fromInteger 1) mstatus_spp_bitpos)) Data.Bits..|.(**)
      (Data.Bits.shiftL (fromInteger 1) mstatus_spie_bitpos)) Data.Bits..|.(**)
     (Data.Bits.shiftL (fromInteger 1) mstatus_upie_bitpos)) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mstatus_sie_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mstatus_uie_bitpos)).

Definition sstatus_mask_RV64 : N :=
  (((((((((((Data.Bits.shiftL (fromInteger 1) mstatus_sd_bitpos_RV64)
            Data.Bits..|.(**)
            (Data.Bits.shiftL (fromInteger 3) mstatus_uxl_bitpos)) Data.Bits..|.(**)
           (Data.Bits.shiftL (fromInteger 1) mstatus_mxr_bitpos)) Data.Bits..|.(**)
          (Data.Bits.shiftL (fromInteger 1) mstatus_sum_bitpos)) Data.Bits..|.(**)
         (Data.Bits.shiftL (fromInteger 3) mstatus_xs_bitpos)) Data.Bits..|.(**)
        (Data.Bits.shiftL (fromInteger 3) mstatus_fs_bitpos)) Data.Bits..|.(**)
       (Data.Bits.shiftL (fromInteger 1) mstatus_spp_bitpos)) Data.Bits..|.(**)
      (Data.Bits.shiftL (fromInteger 1) mstatus_spie_bitpos)) Data.Bits..|.(**)
     (Data.Bits.shiftL (fromInteger 1) mstatus_upie_bitpos)) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mstatus_sie_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mstatus_uie_bitpos)).

Definition ustatus_mask_RV32 : N :=
  (((((((Data.Bits.shiftL (fromInteger 1) mstatus_sd_bitpos_RV32)
        Data.Bits..|.(**)
        (Data.Bits.shiftL (fromInteger 1) mstatus_mxr_bitpos)) Data.Bits..|.(**)
       (Data.Bits.shiftL (fromInteger 1) mstatus_sum_bitpos)) Data.Bits..|.(**)
      (Data.Bits.shiftL (fromInteger 3) mstatus_xs_bitpos)) Data.Bits..|.(**)
     (Data.Bits.shiftL (fromInteger 3) mstatus_fs_bitpos)) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mstatus_upie_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mstatus_uie_bitpos)).

Definition ustatus_mask_RV64 : N :=
  ((((((((Data.Bits.shiftL (fromInteger 1) mstatus_sd_bitpos_RV64)
         Data.Bits..|.(**)
         (Data.Bits.shiftL (fromInteger 3) mstatus_uxl_bitpos)) Data.Bits..|.(**)
        (Data.Bits.shiftL (fromInteger 1) mstatus_mxr_bitpos)) Data.Bits..|.(**)
       (Data.Bits.shiftL (fromInteger 1) mstatus_sum_bitpos)) Data.Bits..|.(**)
      (Data.Bits.shiftL (fromInteger 3) mstatus_xs_bitpos)) Data.Bits..|.(**)
     (Data.Bits.shiftL (fromInteger 3) mstatus_fs_bitpos)) Data.Bits..|.(**)
    (Data.Bits.shiftL (fromInteger 1) mstatus_upie_bitpos)) Data.Bits..|.(**)
   (Data.Bits.shiftL (fromInteger 1) mstatus_uie_bitpos)).

Definition r_funct7_fields_for_AMO
   : InstrField -> (InstrField * InstrField * InstrField)%type :=
  fun funct7 =>
    pair (pair (Bit_Manipulation.bitSlice funct7 (fromInteger 6) (fromInteger 2))
               (Bit_Manipulation.bitSlice funct7 (fromInteger 1) (fromInteger 1)))
         (Bit_Manipulation.bitSlice funct7 (fromInteger 0) (fromInteger 0)).

Definition s_Priv_Level :=
  fromInteger 1 : Priv_Level.

Definition tvec_base : N -> N :=
  fun tvec =>
    Data.Bits.shiftL (Data.Bits.shiftR tvec (fromInteger 2)) (fromInteger 2).

Definition tvec_mode : N -> N :=
  fun tvec => (tvec Data.Bits..&.(**) fromInteger 3).

Definition tvec_mode_DIRECT :=
  fromInteger 0 : N.

Definition tvec_mode_VECTORED :=
  fromInteger 1 : N.

Definition u_Priv_Level :=
  fromInteger 0 : Priv_Level.

Definition fn_interrupt_pending
   : N -> N -> N -> N -> N -> N -> Priv_Level -> option Exc_Code :=
  fun misa mstatus mip mie mideleg sideleg priv =>
    let fn_interrupt_i_pending : Exc_Code -> bool :=
      fun ec =>
        let xie : bool :=
          if (priv == u_Priv_Level) : bool
          then (Data.Bits.testBit mstatus mstatus_uie_bitpos) else
          if (priv == s_Priv_Level) : bool
          then (Data.Bits.testBit mstatus mstatus_sie_bitpos) else
          if (priv == m_Priv_Level) : bool
          then (Data.Bits.testBit mstatus mstatus_mie_bitpos) else
          patternFailure in
        let i := (fromIntegral ec : Int) in
        let intr_pending :=
          (andb (Data.Bits.testBit mip i) (Data.Bits.testBit mie i)) in
        let handler_priv :=
          if (Data.Bits.testBit mideleg i) : bool
          then if (misa_flag misa (GHC.Char.hs_char__ "U")) : bool
               then if (misa_flag misa (GHC.Char.hs_char__ "S")) : bool
                    then if (Data.Bits.testBit sideleg i) : bool
                         then if (misa_flag misa (GHC.Char.hs_char__ "N")) : bool
                              then u_Priv_Level
                              else m_Priv_Level
                         else s_Priv_Level
                    else if (misa_flag misa (GHC.Char.hs_char__ "N")) : bool
                         then u_Priv_Level
                         else m_Priv_Level
               else m_Priv_Level
          else m_Priv_Level in
        let glob_enabled : bool :=
          (orb (priv < handler_priv) (andb (priv == handler_priv) xie)) in
        (andb intr_pending glob_enabled) in
    let m_ec :=
      if fn_interrupt_i_pending (exc_code_m_external_interrupt) : bool
      then Some exc_code_m_external_interrupt else
      if fn_interrupt_i_pending (exc_code_m_software_interrupt) : bool
      then Some exc_code_m_software_interrupt else
      if fn_interrupt_i_pending (exc_code_m_timer_interrupt) : bool
      then Some exc_code_m_timer_interrupt else
      if fn_interrupt_i_pending (exc_code_s_external_interrupt) : bool
      then Some exc_code_s_external_interrupt else
      if fn_interrupt_i_pending (exc_code_s_software_interrupt) : bool
      then Some exc_code_s_software_interrupt else
      if fn_interrupt_i_pending (exc_code_s_timer_interrupt) : bool
      then Some exc_code_s_timer_interrupt else
      if fn_interrupt_i_pending (exc_code_u_external_interrupt) : bool
      then Some exc_code_u_external_interrupt else
      if fn_interrupt_i_pending (exc_code_u_software_interrupt) : bool
      then Some exc_code_u_software_interrupt else
      if fn_interrupt_i_pending (exc_code_u_timer_interrupt) : bool
      then Some exc_code_u_timer_interrupt else
      None in
    m_ec.

Definition xl_rv128 :=
  fromInteger 3 : N.

Definition xl_rv32 :=
  fromInteger 1 : N.

Definition xl_rv64 :=
  fromInteger 2 : N.

(* External variables:
     Build_Default Char Default Eq_ Int N None Some String andb bool cons false
     fromInteger fromIntegral list negb nil op_zeze__ op_zeze____ op_zl__ op_zm__
     op_zsze__ op_zsze____ op_zt__ option orb ord pair patternFailure true
     Bit_Manipulation.bitSlice Bit_Manipulation.trunc_u64_to_u32
     Bit_Manipulation.zeroExtend_u32_to_u64 Data.Bits.complement
     Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__ Data.Bits.shift Data.Bits.shiftL
     Data.Bits.shiftR Data.Bits.testBit
*)

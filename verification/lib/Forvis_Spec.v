(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Converted imports: *)

Require Import Arch_Defs.
Require Bit_Manipulation.
Require Import CSR_File.
Require Import Coq.Init.Datatypes.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require GHC.Enum.
Require Import GHC.Err.
Require Import GHC.Num.
Require Import GHC.Real.
Require Import Machine_State.
Require Import Virtual_Mem.
Import Data.Bits.Notations.

(* Converted type declarations: *)

Definition Instr_Spec :=
  (Machine_State -> Instr -> (bool * Machine_State)%type)%type.

Definition Instr_C_Spec :=
  (Machine_State -> Instr_C -> (bool * Machine_State)%type)%type.

Inductive Fetch_Result : Type
  := Fetch_Trap : Exc_Code -> Fetch_Result
  |  Fetch_C : N -> Fetch_Result
  |  Fetch : N -> Fetch_Result.
(* Converted value declarations: *)

(* Skipping instance Show__Fetch_Result of class Show *)

Definition funct12_EBREAK :=
  fromInteger 1 : InstrField.

Definition funct12_ECALL :=
  fromInteger 0 : InstrField.

Definition funct12_MRET :=
  fromInteger 770 : InstrField.

Definition funct12_SRET :=
  fromInteger 258 : InstrField.

Definition funct12_URET :=
  fromInteger 2 : InstrField.

Definition funct12_WFI :=
  fromInteger 261 : InstrField.

Definition funct3_ADD :=
  fromInteger 0 : InstrField.

Definition funct3_ADDI :=
  fromInteger 0 : InstrField.

Definition funct3_ADDIW :=
  fromInteger 0 : InstrField.

Definition funct3_ADDW :=
  fromInteger 0 : InstrField.

Definition funct3_AMO_D :=
  fromInteger 3 : InstrField.

Definition funct3_AMO_W :=
  fromInteger 2 : InstrField.

Definition funct3_AND :=
  fromInteger 7 : InstrField.

Definition funct3_ANDI :=
  fromInteger 7 : InstrField.

Definition funct3_BEQ :=
  fromInteger 0 : InstrField.

Definition funct3_BGE :=
  fromInteger 5 : InstrField.

Definition funct3_BGEU :=
  fromInteger 7 : InstrField.

Definition funct3_BLT :=
  fromInteger 4 : InstrField.

Definition funct3_BLTU :=
  fromInteger 6 : InstrField.

Definition funct3_BNE :=
  fromInteger 1 : InstrField.

Definition funct3_CSRRC :=
  fromInteger 3 : InstrField.

Definition funct3_CSRRCI :=
  fromInteger 7 : InstrField.

Definition funct3_CSRRS :=
  fromInteger 2 : InstrField.

Definition funct3_CSRRSI :=
  fromInteger 6 : InstrField.

Definition funct3_CSRRW :=
  fromInteger 1 : InstrField.

Definition funct3_CSRRWI :=
  fromInteger 5 : InstrField.

Definition funct3_DIV :=
  fromInteger 4 : InstrField.

Definition funct3_DIVU :=
  fromInteger 5 : InstrField.

Definition funct3_DIVUW :=
  fromInteger 5 : InstrField.

Definition funct3_DIVW :=
  fromInteger 4 : InstrField.

Definition funct3_FENCE :=
  fromInteger 0 : InstrField.

Definition funct3_FENCE_I :=
  fromInteger 1 : InstrField.

Definition funct3_LB :=
  fromInteger 0 : InstrField.

Definition funct3_LBU :=
  fromInteger 4 : InstrField.

Definition funct3_LD :=
  fromInteger 3 : InstrField.

Definition funct3_LH :=
  fromInteger 1 : InstrField.

Definition funct3_LHU :=
  fromInteger 5 : InstrField.

Definition funct3_LW :=
  fromInteger 2 : InstrField.

Definition read_n_instr_bytes
   : Machine_State -> Int -> N -> (Mem_Result * Machine_State)%type :=
  fun mstate n_bytes va =>
    let funct3 :=
      if (n_bytes == fromInteger 4) : bool
      then funct3_LW
      else funct3_LH in
    let is_read := true in
    let is_instr := true in
    let 'pair result1 mstate1 := (if (fn_vm_is_active mstate is_instr) : bool
                                    then vm_translate mstate is_instr is_read va
                                    else pair (Mem_Result_Ok va) mstate) in
    let 'pair result2 mstate2 := (match result1 with
                                    | Mem_Result_Err exc_code => pair result1 mstate1
                                    | Mem_Result_Ok pa =>
                                        mstate_mem_read mstate1 exc_code_instr_access_fault funct3 pa
                                    end) in
    pair result2 mstate2.

Definition funct3_LWU :=
  fromInteger 6 : InstrField.

Definition funct3_MUL :=
  fromInteger 0 : InstrField.

Definition funct3_MULH :=
  fromInteger 1 : InstrField.

Definition funct3_MULHSU :=
  fromInteger 2 : InstrField.

Definition funct3_MULHU :=
  fromInteger 3 : InstrField.

Definition funct3_MULW :=
  fromInteger 0 : InstrField.

Definition funct3_OR :=
  fromInteger 6 : InstrField.

Definition funct3_ORI :=
  fromInteger 6 : InstrField.

Definition funct3_PRIV :=
  fromInteger 0 : InstrField.

Definition funct3_REM :=
  fromInteger 6 : InstrField.

Definition funct3_REMU :=
  fromInteger 7 : InstrField.

Definition funct3_REMUW :=
  fromInteger 7 : InstrField.

Definition funct3_REMW :=
  fromInteger 6 : InstrField.

Definition funct3_SB :=
  fromInteger 0 : InstrField.

Definition funct3_SD :=
  fromInteger 3 : InstrField.

Definition funct3_SH :=
  fromInteger 1 : InstrField.

Definition funct3_SLL :=
  fromInteger 1 : InstrField.

Definition funct3_SLLI :=
  fromInteger 1 : InstrField.

Definition funct3_SLLIW :=
  fromInteger 1 : InstrField.

Definition funct3_SLLW :=
  fromInteger 1 : InstrField.

Definition funct3_SLT :=
  fromInteger 2 : InstrField.

Definition funct3_SLTI :=
  fromInteger 2 : InstrField.

Definition funct3_SLTIU :=
  fromInteger 3 : InstrField.

Definition funct3_SLTU :=
  fromInteger 3 : InstrField.

Definition funct3_SRA :=
  fromInteger 5 : InstrField.

Definition funct3_SRAI :=
  fromInteger 5 : InstrField.

Definition funct3_SRAIW :=
  fromInteger 5 : InstrField.

Definition funct3_SRAW :=
  fromInteger 5 : InstrField.

Definition funct3_SRL :=
  fromInteger 5 : InstrField.

Definition funct3_SRLI :=
  fromInteger 5 : InstrField.

Definition funct3_SRLIW :=
  fromInteger 5 : InstrField.

Definition funct3_SRLW :=
  fromInteger 5 : InstrField.

Definition funct3_SUB :=
  fromInteger 0 : InstrField.

Definition funct3_SUBW :=
  fromInteger 0 : InstrField.

Definition funct3_SW :=
  fromInteger 2 : InstrField.

Definition funct3_XOR :=
  fromInteger 4 : InstrField.

Definition funct3_XORI :=
  fromInteger 4 : InstrField.

Definition funct7_ADD :=
  fromInteger 0 : InstrField.

Definition funct7_ADDW :=
  fromInteger 0 : InstrField.

Definition funct7_AND :=
  fromInteger 0 : InstrField.

Definition funct7_DIV :=
  fromInteger 1 : InstrField.

Definition funct7_DIVU :=
  fromInteger 1 : InstrField.

Definition funct7_DIVUW :=
  fromInteger 1 : InstrField.

Definition funct7_DIVW :=
  fromInteger 1 : InstrField.

Definition funct7_MUL :=
  fromInteger 1 : InstrField.

Definition funct7_MULH :=
  fromInteger 1 : InstrField.

Definition funct7_MULHSU :=
  fromInteger 1 : InstrField.

Definition funct7_MULHU :=
  fromInteger 1 : InstrField.

Definition funct7_MULW :=
  fromInteger 1 : InstrField.

Definition funct7_OR :=
  fromInteger 0 : InstrField.

Definition funct7_REM :=
  fromInteger 1 : InstrField.

Definition funct7_REMU :=
  fromInteger 1 : InstrField.

Definition funct7_REMUW :=
  fromInteger 1 : InstrField.

Definition funct7_REMW :=
  fromInteger 1 : InstrField.

Definition funct7_SFENCE_VM :=
  fromInteger 9 : InstrField.

Definition funct7_SLL :=
  fromInteger 0 : InstrField.

Definition funct7_SLLIW :=
  fromInteger 0 : InstrField.

Definition funct7_SLLW :=
  fromInteger 0 : InstrField.

Definition funct7_SLT :=
  fromInteger 0 : InstrField.

Definition funct7_SLTU :=
  fromInteger 0 : InstrField.

Definition funct7_SRA :=
  fromInteger 32 : InstrField.

Definition funct7_SRAIW :=
  fromInteger 32 : InstrField.

Definition funct7_SRAW :=
  fromInteger 32 : InstrField.

Definition funct7_SRL :=
  fromInteger 0 : InstrField.

Definition funct7_SRLIW :=
  fromInteger 0 : InstrField.

Definition funct7_SRLW :=
  fromInteger 0 : InstrField.

Definition funct7_SUB :=
  fromInteger 32 : InstrField.

Definition funct7_SUBW :=
  fromInteger 32 : InstrField.

Definition funct7_XOR :=
  fromInteger 0 : InstrField.

Definition incr_minstret : Machine_State -> Machine_State :=
  fun mstate =>
    let minstret := mstate_csr_read mstate csr_addr_minstret in
    let mstate1 :=
      mstate_csr_write mstate csr_addr_minstret (minstret + fromInteger 1) in
    mstate1.

Definition finish_rd_and_pc_plus_4
   : Machine_State -> GPR_Addr -> N -> Machine_State :=
  fun mstate rd rd_val =>
    let mstate1 := mstate_gpr_write mstate rd rd_val in
    let pc := mstate_pc_read mstate1 in
    let mstate2 := mstate_pc_write mstate1 (pc + fromInteger 4) in
    let mstate3 := incr_minstret mstate2 in mstate3.

Definition finish_rd_and_pc
   : Machine_State -> GPR_Addr -> N -> N -> Machine_State :=
  fun mstate rd rd_val new_pc =>
    let mstate1 := mstate_gpr_write mstate rd rd_val in
    let mstate2 := mstate_pc_write mstate1 new_pc in
    let mstate3 := incr_minstret mstate2 in mstate3.

Definition finish_pc_plus_4 : Machine_State -> Machine_State :=
  fun mstate =>
    let pc := mstate_pc_read mstate in
    let mstate1 := mstate_pc_write mstate (pc + fromInteger 4) in
    let mstate2 := incr_minstret mstate1 in mstate2.

Definition finish_pc : Machine_State -> N -> Machine_State :=
  fun mstate new_pc =>
    let mstate1 := mstate_pc_write mstate new_pc in
    let mstate2 := incr_minstret mstate1 in mstate2.

Definition instr_C_specs : list (Instr_C_Spec * String)%type :=
  nil.

Definition msbs5_AMO_ADD :=
  fromInteger 0 : InstrField.

Definition msbs5_AMO_AND :=
  fromInteger 12 : InstrField.

Definition msbs5_AMO_LR :=
  fromInteger 2 : InstrField.

Definition msbs5_AMO_MAX :=
  fromInteger 20 : InstrField.

Definition msbs5_AMO_MAXU :=
  fromInteger 28 : InstrField.

Definition msbs5_AMO_MIN :=
  fromInteger 16 : InstrField.

Definition msbs5_AMO_MINU :=
  fromInteger 24 : InstrField.

Definition msbs5_AMO_OR :=
  fromInteger 8 : InstrField.

Definition msbs5_AMO_SC :=
  fromInteger 3 : InstrField.

Definition msbs5_AMO_SWAP :=
  fromInteger 1 : InstrField.

Definition msbs5_AMO_XOR :=
  fromInteger 4 : InstrField.

Definition msbs6_SLLI :=
  fromInteger 0 : InstrField.

Definition msbs6_SRAI :=
  fromInteger 16 : InstrField.

Definition msbs6_SRLI :=
  fromInteger 0 : InstrField.

Definition msbs7_SLLI :=
  fromInteger 0 : InstrField.

Definition msbs7_SRAI :=
  fromInteger 32 : InstrField.

Definition msbs7_SRLI :=
  fromInteger 0 : InstrField.

Definition mstate_upd_on_trap
   : Machine_State -> bool -> Exc_Code -> N -> Machine_State :=
  fun mstate is_interrupt exc_code tval =>
    let vector_offset := exc_code * fromInteger 4 in
    let mstatus := mstate_csr_read mstate csr_addr_mstatus in
    let pc := mstate_pc_read mstate in
    let priv := mstate_priv_read mstate in
    let new_priv :=
      (let j := (fromIntegral exc_code) : Int in
       let sideleg := mstate_csr_read mstate csr_addr_sideleg in
       let sedeleg := mstate_csr_read mstate csr_addr_sedeleg in
       let s_delegating :=
         Data.Bits.testBit (if is_interrupt : bool then sideleg else sedeleg) j in
       let mideleg := mstate_csr_read mstate csr_addr_mideleg in
       let medeleg := mstate_csr_read mstate csr_addr_medeleg in
       let m_delegating :=
         Data.Bits.testBit (if is_interrupt : bool then mideleg else medeleg) j in
       let misa := mstate_csr_read mstate csr_addr_misa in
       let misa_s := misa_flag misa (GHC.Char.hs_char__ "S") in
       let deleg_m_to_s := andb (priv < m_Priv_Level) (andb misa_s m_delegating) in
       let misa_n := misa_flag misa (GHC.Char.hs_char__ "N") in
       let deleg_s_to_u :=
         andb (priv == u_Priv_Level) (andb misa_s (andb misa_n s_delegating)) in
       let deleg_m_to_u :=
         andb (priv == u_Priv_Level) (andb (negb misa_s) (andb misa_n m_delegating)) in
       if deleg_m_to_s : bool
       then if deleg_s_to_u : bool
            then u_Priv_Level
            else s_Priv_Level
       else if deleg_m_to_u : bool
            then u_Priv_Level
            else m_Priv_Level) in
    let 'pair (pair (pair (pair csr_addr_xepc csr_addr_xcause) csr_addr_xtval)
        csr_addr_xtvec) xtvec := (if (new_priv == m_Priv_Level) : bool
                                    then pair (pair (pair (pair csr_addr_mepc csr_addr_mcause) csr_addr_mtval)
                                                    csr_addr_mtvec) (mstate_csr_read mstate csr_addr_mtvec)
                                    else pair (pair (pair (pair csr_addr_sepc csr_addr_scause) csr_addr_stval)
                                                    csr_addr_stvec) (mstate_csr_read mstate csr_addr_stvec)) in
    let pc1 :=
      if andb is_interrupt (tvec_mode (xtvec) == tvec_mode_VECTORED) : bool
      then tvec_base xtvec + vector_offset
      else tvec_base xtvec in
    let mstate1 := mstate_priv_write mstate new_priv in
    let new_mstatus :=
      (let 'pair (pair (pair (pair (pair (pair (pair mpp spp) mpie) spie) upie) mie)
           sie) uie := mstatus_stack_fields mstatus in
       let 'pair mpp' spp' := (if (new_priv == m_Priv_Level) : bool
                               then pair priv spp else
                               if (new_priv == s_Priv_Level) : bool then pair mpp priv else
                               patternFailure) in
       let 'pair (pair (pair (pair (pair mpie' spie') upie') mie') sie') uie' :=
         (if (new_priv == m_Priv_Level) : bool
          then pair (pair (pair (pair (pair mie spie) upie) (fromInteger 0)) sie) uie else
          if (new_priv == s_Priv_Level) : bool
          then pair (pair (pair (pair (pair mpie sie) upie) mie) (fromInteger 0)) uie else
          if (new_priv == u_Priv_Level) : bool
          then pair (pair (pair (pair (pair mpie spie) uie) mie) sie) (fromInteger 0) else
          patternFailure) in
       mstatus_upd_stack_fields mstatus (pair (pair (pair (pair (pair (pair (pair mpp'
                                                                                  spp') mpie') spie') upie') mie') sie')
                                              uie')) in
    let rv := mstate_rv_read mstate in
    let pc2 :=
      if rv == RV64 : bool
      then pc1
      else pc1 Data.Bits..&.(**) fromInteger 4294967295 in
    let mstate2 := mstate_pc_write mstate1 pc2 in
    let mstate3 := mstate_csr_write mstate2 csr_addr_mstatus new_mstatus in
    let mstate4 := mstate_csr_write mstate3 csr_addr_xepc pc in
    let mstate5 :=
      mstate_csr_write mstate4 csr_addr_xcause (mkCause rv is_interrupt exc_code) in
    let mstate6 := mstate_csr_write mstate5 csr_addr_xtval tval in mstate6.

Definition take_interrupt_if_any
   : Machine_State -> (option Exc_Code * Machine_State)%type :=
  fun mstate =>
    let tval := fromInteger 0 in
    let priv := mstate_priv_read mstate in
    let sideleg := mstate_csr_read mstate csr_addr_sideleg in
    let mideleg := mstate_csr_read mstate csr_addr_mideleg in
    let mie := mstate_csr_read mstate csr_addr_mie in
    let mip := mstate_csr_read mstate csr_addr_mip in
    let mstatus := mstate_csr_read mstate csr_addr_mstatus in
    let misa := mstate_csr_read mstate csr_addr_misa in
    let intr_pending :=
      fn_interrupt_pending misa mstatus mip mie mideleg sideleg priv in
    match intr_pending with
    | None => pair intr_pending mstate
    | Some exc_code =>
        let mstate1 := mstate_upd_on_trap mstate true exc_code tval in
        let mstate2 := mstate_run_state_write mstate1 Run_State_Running in
        pair intr_pending mstate2
    end.

Definition finish_trap : Machine_State -> Exc_Code -> N -> Machine_State :=
  fun mstate exc_code tval =>
    let mstate1 := mstate_upd_on_trap mstate false exc_code tval in
    let mstate2 := incr_minstret mstate1 in mstate2.

Definition instr_fetch : Machine_State -> (Fetch_Result * Machine_State)%type :=
  fun mstate =>
    let misa := mstate_csr_read mstate csr_addr_misa in
    let rv := mstate_rv_read mstate in
    let pc :=
      if (rv == RV32) : bool
      then (mstate_pc_read mstate Data.Bits..&.(**) fromInteger 4294967295) else
      if (rv == RV64) : bool then mstate_pc_read mstate else
      patternFailure in
    if (negb (misa_flag misa (GHC.Char.hs_char__ "C"))) : bool
    then let 'pair result1 mstate1 := read_n_instr_bytes mstate (fromInteger 4)
                                        pc in
         match result1 with
         | Mem_Result_Err exc_code =>
             (let tval := pc in
              let mstate2 := finish_trap mstate1 exc_code tval in
              pair (Fetch_Trap exc_code) mstate2)
         | Mem_Result_Ok u64 =>
             (let u32 := Bit_Manipulation.trunc_u64_to_u32 u64 in pair (Fetch u32) mstate1)
         end
    else let 'pair result1 mstate1 := read_n_instr_bytes mstate (fromInteger 2)
                                        pc in
         match result1 with
         | Mem_Result_Err exc_code =>
             (let tval := pc in
              let mstate2 := finish_trap mstate1 exc_code tval in
              pair (Fetch_Trap exc_code) mstate2)
         | Mem_Result_Ok u64_lo =>
             (let u16_lo := Bit_Manipulation.trunc_u64_to_u16 u64_lo in
              if is_instr_C u16_lo : bool
              then pair (Fetch_C u16_lo) mstate1
              else (let 'pair result2 mstate2 := read_n_instr_bytes mstate (fromInteger 2) (pc
                                                                                            +
                                                                                            fromInteger 2) in
                    match result2 with
                    | Mem_Result_Err exc_code =>
                        (let tval := pc + fromInteger 2 in
                         let mstate3 := finish_trap mstate2 exc_code tval in
                         pair (Fetch_Trap exc_code) mstate3)
                    | Mem_Result_Ok u64_hi =>
                        (let u16_hi := Bit_Manipulation.trunc_u64_to_u16 u64_hi in
                         let u32 := Bit_Manipulation.bitconcat_u16_u16_to_u32 u16_lo u16_hi in
                         pair (Fetch u32) mstate2)
                    end))
         end.

Definition exec_instr_C
   : Machine_State -> Instr_C -> (Machine_State * String)%type :=
  fun mstate instr =>
    let fix tryall arg_0__
              := match arg_0__ with
                 | nil =>
                     (let tval := Bit_Manipulation.zeroExtend_u16_to_u64 instr in
                      pair (finish_trap mstate exc_code_illegal_instruction tval)
                           (GHC.Base.hs_string__ "NONE"))
                 | cons (pair spec name) specs =>
                     (let 'pair success mstate1 := spec mstate instr in
                      if success : bool
                      then pair mstate1 name
                      else tryall specs)
                 end in
    tryall instr_C_specs.

Definition mstate_wfi_resume : Machine_State -> bool :=
  fun mstate =>
    let mie := mstate_csr_read mstate csr_addr_mie in
    let mip := mstate_csr_read mstate csr_addr_mip in
    let resume := ((mip Data.Bits..&.(**) mie) /= fromInteger 0) in resume.

Definition opcode_AMO :=
  fromInteger 47 : InstrField.

Definition spec_AMO : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let is_read := false in
    let is_instr := false in
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let 'pair (pair msbs5 aq) rl := r_funct7_fields_for_AMO funct7 in
    let is_legal :=
      (andb (opcode == opcode_AMO) (andb (orb (funct3 == funct3_AMO_W) (andb (rv ==
                                                                              RV64) (funct3 == funct3_AMO_D))) (orb
                                          (andb (msbs5 == msbs5_AMO_LR) (rs2 == fromInteger 0)) (orb (msbs5 ==
                                                                                                      msbs5_AMO_SC) (orb
                                                                                                      (msbs5 ==
                                                                                                       msbs5_AMO_ADD)
                                                                                                      (orb (msbs5 ==
                                                                                                            msbs5_AMO_SWAP)
                                                                                                           (orb (msbs5
                                                                                                                 ==
                                                                                                                 msbs5_AMO_XOR)
                                                                                                                (orb
                                                                                                                 (msbs5
                                                                                                                  ==
                                                                                                                  msbs5_AMO_AND)
                                                                                                                 (orb
                                                                                                                  (msbs5
                                                                                                                   ==
                                                                                                                   msbs5_AMO_OR)
                                                                                                                  (orb
                                                                                                                   (msbs5
                                                                                                                    ==
                                                                                                                    msbs5_AMO_MIN)
                                                                                                                   (orb
                                                                                                                    (msbs5
                                                                                                                     ==
                                                                                                                     msbs5_AMO_MAX)
                                                                                                                    (orb
                                                                                                                     (msbs5
                                                                                                                      ==
                                                                                                                      msbs5_AMO_MINU)
                                                                                                                     (msbs5
                                                                                                                      ==
                                                                                                                      msbs5_AMO_MAXU))))))))))))) in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let eaddr1 := mstate_gpr_read mstate rs1 in
    let eaddr2 :=
      if (rv == RV64) : bool
      then eaddr1
      else (eaddr1 Data.Bits..&.(**) fromInteger 4294967295) in
    let 'pair result1 mstate1 := (if (fn_vm_is_active mstate is_instr) : bool
                                    then vm_translate mstate is_instr is_read eaddr2
                                    else pair (Mem_Result_Ok eaddr2) mstate) in
    let 'pair result2 mstate2 := (match result1 with
                                    | Mem_Result_Err exc_code => pair result1 mstate1
                                    | Mem_Result_Ok eaddr2_pa =>
                                        mstate_mem_amo mstate1 eaddr2_pa funct3 msbs5 aq rl rs2_val
                                    end) in
    let mstate3 :=
      match result2 with
      | Mem_Result_Err exc_code => finish_trap mstate2 exc_code eaddr2
      | Mem_Result_Ok d_u64 =>
          let rd_val :=
            if (funct3 == funct3_AMO_W) : bool
            then Bit_Manipulation.signExtend d_u64 (fromInteger 32) else
            if (funct3 == funct3_AMO_D) : bool then d_u64 else
            patternFailure in
          finish_rd_and_pc_plus_4 mstate2 rd rd_val
      end in
    pair is_legal mstate3.

Definition opcode_AUIPC :=
  fromInteger 23 : InstrField.

Definition spec_AUIPC : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let pc := mstate_pc_read mstate in
    let 'pair (pair imm20 rd) opcode := ifields_U_type instr in
    let is_legal := (opcode == opcode_AUIPC) in
    let x_u32 := Data.Bits.shiftL imm20 (fromInteger 12) in
    let x_u64 := Bit_Manipulation.signExtend_u32_to_u64 x_u32 in
    let rd_val :=
      Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 x_u64) +
                                       (Bit_Manipulation.cvt_u64_to_s64 pc)) in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition opcode_BRANCH :=
  fromInteger 99 : InstrField.

Definition spec_BRANCH
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let misa := mstate_csr_read mstate csr_addr_misa in
    let pc := mstate_pc_read mstate in
    let 'pair (pair (pair (pair imm12 rs2) rs1) funct3) opcode := ifields_B_type
                                                                    instr in
    let is_BEQ := (funct3 == funct3_BEQ) in
    let is_BNE := (funct3 == funct3_BNE) in
    let is_BLT := (funct3 == funct3_BLT) in
    let is_BGE := (funct3 == funct3_BGE) in
    let is_BLTU := (funct3 == funct3_BLTU) in
    let is_BGEU := (funct3 == funct3_BGEU) in
    let is_legal :=
      (andb (opcode == opcode_BRANCH) (orb is_BEQ (orb is_BNE (orb is_BLT (orb is_BGE
                                                                               (orb is_BLTU is_BGEU)))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let taken :=
      if is_BEQ : bool then (rs1_val == rs2_val) else
      if is_BNE : bool then (rs1_val /= rs2_val) else
      if is_BLT : bool
      then (Bit_Manipulation.cvt_u64_to_s64 (rs1_val) <
            Bit_Manipulation.cvt_u64_to_s64 (rs2_val)) else
      if is_BGE : bool
      then (Bit_Manipulation.cvt_u64_to_s64 (rs1_val) >=
            Bit_Manipulation.cvt_u64_to_s64 (rs2_val)) else
      if is_BLTU : bool then (rs1_val < rs2_val) else
      if is_BGEU : bool then (rs1_val >= rs2_val) else
      patternFailure in
    let x_u64 := Bit_Manipulation.zeroExtend_u32_to_u64 imm12 in
    let y_u64 := Data.Bits.shiftL x_u64 (fromInteger 1) in
    let z_u64 := Bit_Manipulation.signExtend y_u64 (fromInteger 13) in
    let target :=
      Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 pc) +
                                       (Bit_Manipulation.cvt_u64_to_s64 z_u64)) in
    let new_pc := if taken : bool then target else pc + fromInteger 4 in
    let aligned :=
      (orb (misa_flag misa (GHC.Char.hs_char__ "C")) ((new_pc Data.Bits..&.(**)
             fromInteger 2) ==
            fromInteger 0)) in
    let mstate1 :=
      if aligned : bool
      then finish_pc mstate new_pc
      else finish_trap mstate exc_code_instr_addr_misaligned new_pc in
    pair is_legal mstate1.

Definition opcode_JAL :=
  fromInteger 111 : InstrField.

Definition spec_JAL : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let pc := mstate_pc_read mstate in
    let rd_val := pc + fromInteger 4 in
    let 'pair (pair imm20 rd) opcode := ifields_J_type instr in
    let is_legal := (opcode == opcode_JAL) in
    let x_u64 := Bit_Manipulation.zeroExtend_u32_to_u64 imm20 in
    let y_u64 := Data.Bits.shiftL x_u64 (fromInteger 1) in
    let z_u64 := Bit_Manipulation.signExtend y_u64 (fromInteger 21) in
    let new_pc :=
      Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 z_u64) +
                                       (Bit_Manipulation.cvt_u64_to_s64 pc)) in
    let aligned := ((new_pc Data.Bits..&.(**) fromInteger 3) == fromInteger 0) in
    let mstate1 :=
      if aligned : bool
      then finish_rd_and_pc mstate rd rd_val new_pc
      else finish_trap mstate exc_code_instr_addr_misaligned new_pc in
    pair is_legal mstate1.

Definition opcode_JALR :=
  fromInteger 103 : InstrField.

Definition spec_JALR : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let pc := mstate_pc_read mstate in
    let rd_val := pc + fromInteger 4 in
    let 'pair (pair (pair (pair imm12 rs1) funct3) rd) opcode := ifields_I_type
                                                                   instr in
    let is_legal := (opcode == opcode_JALR) in
    let x_u64 := Bit_Manipulation.zeroExtend_u32_to_u64 imm12 in
    let y_u64 := Bit_Manipulation.signExtend x_u64 (fromInteger 12) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let new_pc :=
      Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 y_u64) +
                                       (Bit_Manipulation.cvt_u64_to_s64 rs1_val)) in
    let new_pc' := Bit_Manipulation.clear_bit new_pc (fromInteger 0) in
    let aligned := ((new_pc' Data.Bits..&.(**) fromInteger 3) == fromInteger 0) in
    let mstate1 :=
      if aligned : bool
      then finish_rd_and_pc mstate rd rd_val new_pc'
      else finish_trap mstate exc_code_instr_addr_misaligned new_pc' in
    pair is_legal mstate1.

Definition opcode_LOAD :=
  fromInteger 3 : InstrField.

Definition spec_LOAD : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let is_read := true in
    let is_instr := false in
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair imm12 rs1) funct3) rd) opcode := ifields_I_type
                                                                   instr in
    let is_LB := (funct3 == funct3_LB) in
    let is_LH := (funct3 == funct3_LH) in
    let is_LW := (funct3 == funct3_LW) in
    let is_LD := (funct3 == funct3_LD) in
    let is_LBU := (funct3 == funct3_LBU) in
    let is_LHU := (funct3 == funct3_LHU) in
    let is_LWU := (funct3 == funct3_LWU) in
    let is_legal :=
      (andb (opcode == opcode_LOAD) (orb is_LB (orb is_LH (orb is_LW (orb (andb is_LD
                                                                                (rv == RV64)) (orb is_LBU (orb is_LHU
                                                                                                               (andb
                                                                                                                is_LWU
                                                                                                                (rv ==
                                                                                                                 RV64))))))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let x_u64 := Bit_Manipulation.zeroExtend_u32_to_u64 imm12 in
    let y_u64 := Bit_Manipulation.signExtend x_u64 (fromInteger 12) in
    let eaddr1 :=
      Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 y_u64) +
                                       (Bit_Manipulation.cvt_u64_to_s64 rs1_val)) in
    let eaddr2 :=
      if (rv == RV64) : bool
      then eaddr1
      else (eaddr1 Data.Bits..&.(**) fromInteger 4294967295) in
    let 'pair result1 mstate1 := (if (fn_vm_is_active mstate is_instr) : bool
                                    then vm_translate mstate is_instr is_read eaddr2
                                    else pair (Mem_Result_Ok eaddr2) mstate) in
    let 'pair result2 mstate2 := (match result1 with
                                    | Mem_Result_Err exc_code => pair result1 mstate1
                                    | Mem_Result_Ok eaddr2_pa =>
                                        mstate_mem_read mstate1 exc_code_load_access_fault funct3 eaddr2_pa
                                    end) in
    let mstate3 :=
      match result2 with
      | Mem_Result_Err exc_code => finish_trap mstate2 exc_code eaddr2
      | Mem_Result_Ok d_u64 =>
          let rd_val :=
            if is_LB : bool then Bit_Manipulation.signExtend d_u64 (fromInteger 8) else
            if is_LH : bool then Bit_Manipulation.signExtend d_u64 (fromInteger 16) else
            if is_LW : bool then Bit_Manipulation.signExtend d_u64 (fromInteger 32) else
            d_u64 in
          finish_rd_and_pc_plus_4 mstate2 rd rd_val
      end in
    pair is_legal mstate3.

Definition opcode_LOAD_FP :=
  fromInteger 7 : InstrField.

Definition opcode_LUI :=
  fromInteger 55 : InstrField.

Definition spec_LUI : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let 'pair (pair imm20 rd) opcode := ifields_U_type instr in
    let is_legal := (opcode == opcode_LUI) in
    let x_u32 := Data.Bits.shiftL imm20 (fromInteger 12) in
    let rd_val := Bit_Manipulation.signExtend_u32_to_u64 x_u32 in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition opcode_MISC_MEM :=
  fromInteger 15 : InstrField.

Definition spec_MISC_MEM
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let 'pair (pair (pair (pair imm12 rs1) funct3) rd) opcode := ifields_I_type
                                                                   instr in
    let 'pair (pair msbs4 pred) succ := i_imm12_fields_for_FENCE imm12 in
    let is_FENCE :=
      (andb (funct3 == funct3_FENCE) (andb (rd == fromInteger 0) (andb (rs1 ==
                                                                        fromInteger 0) (msbs4 == fromInteger 0)))) in
    let is_FENCE_I :=
      (andb (funct3 == funct3_FENCE_I) (andb (rd == fromInteger 0) (andb (rs1 ==
                                                                          fromInteger 0) (imm12 == fromInteger 0)))) in
    let mstate1 :=
      if is_FENCE : bool then mstate_mem_fence mstate else
      if is_FENCE_I : bool then mstate_mem_fence_i mstate else
      patternFailure in
    let mstate2 := finish_pc_plus_4 mstate1 in
    let is_legal := (andb (opcode == opcode_MISC_MEM) (orb is_FENCE is_FENCE_I)) in
    pair is_legal mstate2.

Definition opcode_OP :=
  fromInteger 51 : InstrField.

Definition spec_OP : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let is_ADD := (andb (funct3 == funct3_ADD) (funct7 == funct7_ADD)) in
    let is_SUB := (andb (funct3 == funct3_SUB) (funct7 == funct7_SUB)) in
    let is_SLT := (andb (funct3 == funct3_SLT) (funct7 == funct7_SLT)) in
    let is_SLTU := (andb (funct3 == funct3_SLTU) (funct7 == funct7_SLTU)) in
    let is_XOR := (andb (funct3 == funct3_XOR) (funct7 == funct7_XOR)) in
    let is_OR := (andb (funct3 == funct3_OR) (funct7 == funct7_OR)) in
    let is_AND := (andb (funct3 == funct3_AND) (funct7 == funct7_AND)) in
    let is_SLL := (andb (funct3 == funct3_SLL) (funct7 == funct7_SLL)) in
    let is_SRL := (andb (funct3 == funct3_SRL) (funct7 == funct7_SRL)) in
    let is_SRA := (andb (funct3 == funct3_SRA) (funct7 == funct7_SRA)) in
    let is_legal :=
      (andb (opcode == opcode_OP) (orb is_ADD (orb is_SUB (orb is_SLT (orb is_SLTU
                                                                           (orb is_XOR (orb is_OR (orb is_AND (orb
                                                                                                        is_SLL (orb
                                                                                                         is_SRL
                                                                                                         is_SRA)))))))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let shamt :=
      Bit_Manipulation.cvt_u64_to_Int (if (rv == RV32) : bool
                                       then (rs2_val Data.Bits..&.(**) fromInteger 31)
                                       else (rs2_val Data.Bits..&.(**) fromInteger 63)) in
    let rd_val :=
      if is_ADD : bool
      then Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 rs1_val)
                                            +
                                            (Bit_Manipulation.cvt_u64_to_s64 rs2_val)) else
      if is_SUB : bool
      then Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 rs1_val)
                                            -
                                            (Bit_Manipulation.cvt_u64_to_s64 rs2_val)) else
      if is_SLT : bool
      then if ((Bit_Manipulation.cvt_u64_to_s64 rs1_val) <
               (Bit_Manipulation.cvt_u64_to_s64 rs2_val)) : bool
           then fromInteger 1
           else fromInteger 0 else
      if is_SLTU : bool
      then if (rs1_val < rs2_val) : bool
           then fromInteger 1
           else fromInteger 0 else
      if is_XOR : bool then Data.Bits.xor rs1_val rs2_val else
      if is_OR : bool then rs1_val Data.Bits..|.(**) rs2_val else
      if is_AND : bool then rs1_val Data.Bits..&.(**) rs2_val else
      if is_SLL : bool then Data.Bits.shiftL rs1_val shamt else
      if is_SRL : bool
      then (let v1 :=
              if (rv == RV32) : bool
              then (rs1_val Data.Bits..&.(**) fromInteger 4294967295)
              else rs1_val in
            Data.Bits.shiftR v1 shamt) else
      if is_SRA : bool
      then Bit_Manipulation.cvt_s64_to_u64 (Data.Bits.shiftR
                                            (Bit_Manipulation.cvt_u64_to_s64 rs1_val) shamt) else
      patternFailure in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition spec_OP_DIV
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let is_legal :=
      (andb (opcode == opcode_OP) (orb (andb (funct3 == funct3_DIV) (funct7 ==
                                              funct7_DIV)) (andb (funct3 == funct3_DIVU) (funct7 == funct7_DIVU)))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let rs1_val_s := Bit_Manipulation.cvt_u64_to_s64 rs1_val in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let rs2_val_s := Bit_Manipulation.cvt_u64_to_s64 rs2_val in
    let rd_val :=
      if (funct3 == funct3_DIV) : bool
      then Bit_Manipulation.cvt_s64_to_u64 (if (rs2_val == fromInteger 0) : bool
                                            then negate (fromInteger 1)
                                            else if (andb (rs1_val_s == GHC.Enum.minBound) (rs2_val_s ==
                                                           negate (fromInteger 1))) : bool
                                                 then rs1_val_s
                                                 else quot rs1_val_s rs2_val_s) else
      if (funct3 == funct3_DIVU) : bool
      then if (rv == RV32) : bool
           then let v2_u32 := Bit_Manipulation.trunc_u64_to_u32 rs2_val in
                let v1_u32 := Bit_Manipulation.trunc_u64_to_u32 rs1_val in
                let z_u32 :=
                  if (v2_u32 == fromInteger 0) : bool
                  then GHC.Enum.maxBound
                  else div v1_u32 v2_u32 in
                Bit_Manipulation.signExtend_u32_to_u64 z_u32
           else if (rs2_val == fromInteger 0) : bool
                then GHC.Enum.maxBound
                else div rs1_val rs2_val else
      patternFailure in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition spec_OP_MUL
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let xlen := mstate_xlen_read mstate in
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let is_legal :=
      (andb (opcode == opcode_OP) (orb (andb (funct3 == funct3_MUL) (funct7 ==
                                              funct7_MUL)) (orb (andb (funct3 == funct3_MULH) (funct7 == funct7_MULH))
                                                                (orb (andb (funct3 == funct3_MULHSU) (funct7 ==
                                                                            funct7_MULHSU)) (andb (funct3 ==
                                                                                                   funct3_MULHU) (funct7
                                                                                                   ==
                                                                                                   funct7_MULHU)))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let u1_i : Z :=
      fromIntegral (if rv == RV32 : bool
                    then (rs1_val Data.Bits..&.(**) fromInteger 4294967295)
                    else rs1_val) in
    let s1_i : Z := fromIntegral (Bit_Manipulation.cvt_u64_to_s64 rs1_val) in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let u2_i : Z :=
      fromIntegral (if rv == RV32 : bool
                    then (rs2_val Data.Bits..&.(**) fromInteger 4294967295)
                    else rs2_val) in
    let s2_i : Z := fromIntegral (Bit_Manipulation.cvt_u64_to_s64 rs2_val) in
    let prod_i :=
      if (funct3 == funct3_MUL) : bool then s1_i * s2_i else
      if (funct3 == funct3_MULH) : bool then s1_i * s2_i else
      if (funct3 == funct3_MULHU) : bool then u1_i * u2_i else
      if (funct3 == funct3_MULHSU) : bool then s1_i * u2_i else
      patternFailure in
    let result_i :=
      if (funct3 == funct3_MUL) : bool then prod_i else
      if (orb (funct3 == funct3_MULH) (orb (funct3 == funct3_MULHU) (funct3 ==
                                            funct3_MULHSU))) : bool
      then Data.Bits.shiftR prod_i xlen else
      patternFailure in
    let rd_val : N := fromIntegral result_i in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition spec_OP_REM
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let is_legal :=
      (andb (opcode == opcode_OP) (orb (andb (funct3 == funct3_REM) (funct7 ==
                                              funct7_REM)) (andb (funct3 == funct3_REMU) (funct7 == funct7_REMU)))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let rs1_val_s := Bit_Manipulation.cvt_u64_to_s64 rs1_val in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let rs2_val_s := Bit_Manipulation.cvt_u64_to_s64 rs2_val in
    let rd_val :=
      if (funct3 == funct3_REM) : bool
      then Bit_Manipulation.cvt_s64_to_u64 (if (rs2_val == fromInteger 0) : bool
                                            then rs1_val_s
                                            else if (andb (rs1_val_s == GHC.Enum.minBound) (rs2_val_s ==
                                                           negate (fromInteger 1))) : bool
                                                 then fromInteger 0
                                                 else rem rs1_val_s rs2_val_s) else
      if (funct3 == funct3_REMU) : bool
      then if (rs2_val == fromInteger 0) : bool
           then rs1_val
           else rem rs1_val rs2_val else
      patternFailure in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition opcode_OP_32 :=
  fromInteger 59 : InstrField.

Definition spec_OP_32 : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let is_ADDW := (andb (funct3 == funct3_ADDW) (funct7 == funct7_ADDW)) in
    let is_SUBW := (andb (funct3 == funct3_SUBW) (funct7 == funct7_SUBW)) in
    let is_SLLW := (andb (funct3 == funct3_SLLW) (funct7 == funct7_SLLW)) in
    let is_SRLW := (andb (funct3 == funct3_SRLW) (funct7 == funct7_SRLW)) in
    let is_SRAW := (andb (funct3 == funct3_SRAW) (funct7 == funct7_SRAW)) in
    let is_legal :=
      (andb (rv == RV64) (andb (opcode == opcode_OP_32) (orb is_ADDW (orb is_SUBW (orb
                                                                           is_SLLW (orb is_SRLW is_SRAW)))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let u1_32 := Bit_Manipulation.trunc_u64_to_u32 rs1_val in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let u2_32 := Bit_Manipulation.trunc_u64_to_u32 rs2_val in
    let shamt :=
      Bit_Manipulation.cvt_u64_to_Int (rs2_val Data.Bits..&.(**) fromInteger 31) in
    let rd_val_32 :=
      if is_ADDW : bool
      then Bit_Manipulation.cvt_s32_to_u32 ((Bit_Manipulation.cvt_u32_to_s32 u1_32) +
                                            (Bit_Manipulation.cvt_u32_to_s32 u2_32)) else
      if is_SUBW : bool
      then Bit_Manipulation.cvt_s32_to_u32 ((Bit_Manipulation.cvt_u32_to_s32 u1_32) -
                                            (Bit_Manipulation.cvt_u32_to_s32 u2_32)) else
      if is_SLLW : bool then Data.Bits.shiftL u1_32 shamt else
      if is_SRLW : bool then Data.Bits.shiftR u1_32 shamt else
      if is_SRAW : bool
      then Bit_Manipulation.cvt_s32_to_u32 (Data.Bits.shiftR
                                            (Bit_Manipulation.cvt_u32_to_s32 u1_32) shamt) else
      patternFailure in
    let rd_val := Bit_Manipulation.signExtend_u32_to_u64 rd_val_32 in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition spec_OP_32_M
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let is_MULW := (andb (funct3 == funct3_MULW) (funct7 == funct7_MULW)) in
    let is_DIVW := (andb (funct3 == funct3_DIVW) (funct7 == funct7_DIVW)) in
    let is_DIVUW := (andb (funct3 == funct3_DIVUW) (funct7 == funct7_DIVUW)) in
    let is_REMW := (andb (funct3 == funct3_REMW) (funct7 == funct7_REMW)) in
    let is_REMUW := (andb (funct3 == funct3_REMUW) (funct7 == funct7_REMUW)) in
    let is_legal :=
      (andb (rv == RV64) (andb (opcode == opcode_OP_32) (orb is_MULW (orb is_DIVW (orb
                                                                           is_DIVUW (orb is_REMW is_REMUW)))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let u1_32 : N := Bit_Manipulation.trunc_u64_to_u32 rs1_val in
    let s1_32 : Z := Bit_Manipulation.cvt_u32_to_s32 u1_32 in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let u2_32 : N := Bit_Manipulation.trunc_u64_to_u32 rs2_val in
    let s2_32 : Z := Bit_Manipulation.cvt_u32_to_s32 u2_32 in
    let rd_val_32 :=
      if is_MULW : bool then Bit_Manipulation.cvt_s32_to_u32 (s1_32 * s2_32) else
      if is_DIVW : bool
      then Bit_Manipulation.cvt_s32_to_u32 (if (u2_32 == fromInteger 0) : bool
                                            then negate (fromInteger 1)
                                            else if andb (s1_32 == GHC.Enum.minBound) (s2_32 ==
                                                          negate (fromInteger 1)) : bool
                                                 then s1_32
                                                 else quot s1_32 s2_32) else
      if is_DIVUW : bool
      then if (u2_32 == fromInteger 0) : bool
           then GHC.Enum.maxBound
           else div u1_32 u2_32 else
      if is_REMW : bool
      then Bit_Manipulation.cvt_s32_to_u32 (if (u2_32 == fromInteger 0) : bool
                                            then s1_32
                                            else if andb (s1_32 == GHC.Enum.minBound) (s2_32 ==
                                                          negate (fromInteger 1)) : bool
                                                 then fromInteger 0
                                                 else rem s1_32 s2_32) else
      if is_REMUW : bool
      then if (u2_32 == fromInteger 0) : bool
           then u1_32
           else rem u1_32 u2_32 else
      patternFailure in
    let rd_val := Bit_Manipulation.signExtend_u32_to_u64 rd_val_32 in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition opcode_OP_IMM :=
  fromInteger 19 : InstrField.

Definition spec_OP_IMM
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair imm12 rs1) funct3) rd) opcode := ifields_I_type
                                                                   instr in
    let 'pair msbs7 shamt5 := i_imm12_fields_7_5 imm12 in
    let 'pair msbs6 shamt6 := i_imm12_fields_6_6 imm12 in
    let shamt :=
      Bit_Manipulation.cvt_u32_to_Int (if (rv == RV32) : bool
                                       then shamt5
                                       else shamt6) in
    let is_ADDI := (funct3 == funct3_ADDI) in
    let is_SLTI := (funct3 == funct3_SLTI) in
    let is_SLTIU := (funct3 == funct3_SLTIU) in
    let is_XORI := (funct3 == funct3_XORI) in
    let is_ORI := (funct3 == funct3_ORI) in
    let is_ANDI := (funct3 == funct3_ANDI) in
    let is_SLLI :=
      (andb (funct3 == funct3_SLLI) (orb (andb (rv == RV32) (msbs7 == msbs7_SLLI))
                                         (andb (rv == RV64) (msbs6 == msbs6_SLLI)))) in
    let is_SRLI :=
      (andb (funct3 == funct3_SRLI) (orb (andb (rv == RV32) (msbs7 == msbs7_SRLI))
                                         (andb (rv == RV64) (msbs6 == msbs6_SRLI)))) in
    let is_SRAI :=
      (andb (funct3 == funct3_SRAI) (orb (andb (rv == RV32) (msbs7 == msbs7_SRAI))
                                         (andb (rv == RV64) (msbs6 == msbs6_SRAI)))) in
    let is_legal :=
      (andb (opcode == opcode_OP_IMM) (orb is_ADDI (orb is_SLTI (orb is_SLTIU (orb
                                                                      is_XORI (orb is_ORI (orb is_ANDI (orb is_SLLI (orb
                                                                                                             is_SRLI
                                                                                                             is_SRAI))))))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let v2_u64 :=
      Bit_Manipulation.signExtend (Bit_Manipulation.zeroExtend_u32_to_u64 imm12)
      (fromInteger 12) in
    let rd_val :=
      if is_ADDI : bool
      then Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 rs1_val)
                                            +
                                            (Bit_Manipulation.cvt_u64_to_s64 v2_u64)) else
      if is_SLTI : bool
      then if (Bit_Manipulation.cvt_u64_to_s64 rs1_val) <
              (Bit_Manipulation.cvt_u64_to_s64 v2_u64) : bool
           then fromInteger 1
           else fromInteger 0 else
      if is_SLTIU : bool
      then if rs1_val < v2_u64 : bool
           then fromInteger 1
           else fromInteger 0 else
      if is_XORI : bool then Data.Bits.xor rs1_val v2_u64 else
      if is_ORI : bool then rs1_val Data.Bits..|.(**) v2_u64 else
      if is_ANDI : bool then rs1_val Data.Bits..&.(**) v2_u64 else
      if is_SLLI : bool then Data.Bits.shiftL rs1_val shamt else
      if is_SRLI : bool
      then (let v1 :=
              if (rv == RV32) : bool
              then (rs1_val Data.Bits..&.(**) fromInteger 4294967295)
              else rs1_val in
            Data.Bits.shiftR v1 shamt) else
      if is_SRAI : bool
      then Bit_Manipulation.cvt_s64_to_u64 (Data.Bits.shiftR
                                            (Bit_Manipulation.cvt_u64_to_s64 rs1_val) shamt) else
      patternFailure in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition opcode_OP_IMM_32 :=
  fromInteger 27 : InstrField.

Definition spec_OP_IMM_32
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair imm12 rs1) funct3) rd) opcode := ifields_I_type
                                                                   instr in
    let 'pair funct7 shamt_5 := i_imm12_fields_7_5 imm12 in
    let shamt := Bit_Manipulation.cvt_u32_to_Int shamt_5 in
    let is_ADDIW := (funct3 == funct3_ADDIW) in
    let is_SLLIW := (andb (funct3 == funct3_SLLIW) (funct7 == funct7_SLLIW)) in
    let is_SRLIW := (andb (funct3 == funct3_SRLIW) (funct7 == funct7_SRLIW)) in
    let is_SRAIW := (andb (funct3 == funct3_SRAIW) (funct7 == funct7_SRAIW)) in
    let is_legal :=
      (andb (rv == RV64) (andb (opcode == opcode_OP_IMM_32) (orb is_ADDIW (orb
                                                                  is_SLLIW (orb is_SRLIW is_SRAIW))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let u1_32 := Bit_Manipulation.trunc_u64_to_u32 rs1_val in
    let u2_32 := Bit_Manipulation.signExtend_bit_in_u32 imm12 (fromInteger 12) in
    let rd_val_32 :=
      if is_ADDIW : bool
      then Bit_Manipulation.cvt_s32_to_u32 ((Bit_Manipulation.cvt_u32_to_s32 u1_32) +
                                            (Bit_Manipulation.cvt_u32_to_s32 u2_32)) else
      if is_SLLIW : bool then Data.Bits.shiftL u1_32 shamt else
      if is_SRLIW : bool then Data.Bits.shiftR u1_32 shamt else
      if is_SRAIW : bool
      then Bit_Manipulation.cvt_s32_to_u32 (Data.Bits.shiftR
                                            (Bit_Manipulation.cvt_u32_to_s32 u1_32) shamt) else
      patternFailure in
    let rd_val := Bit_Manipulation.signExtend_u32_to_u64 rd_val_32 in
    let mstate1 := finish_rd_and_pc_plus_4 mstate rd rd_val in
    pair is_legal mstate1.

Definition opcode_STORE :=
  fromInteger 35 : InstrField.

Definition spec_STORE : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let is_read := false in
    let is_instr := false in
    let rv := mstate_rv_read mstate in
    let 'pair (pair (pair (pair imm12 rs2) rs1) funct3) opcode := ifields_S_type
                                                                    instr in
    let is_SB := (funct3 == funct3_SB) in
    let is_SH := (funct3 == funct3_SH) in
    let is_SW := (funct3 == funct3_SW) in
    let is_SD := (andb (funct3 == funct3_SD) (rv == RV64)) in
    let is_legal :=
      (andb (opcode == opcode_STORE) (orb is_SB (orb is_SH (orb is_SW is_SD)))) in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let x_u64 := Bit_Manipulation.zeroExtend_u32_to_u64 imm12 in
    let y_u64 := Bit_Manipulation.signExtend x_u64 (fromInteger 12) in
    let eaddr1 :=
      Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 rs1_val) +
                                       (Bit_Manipulation.cvt_u64_to_s64 y_u64)) in
    let eaddr2 :=
      if (rv == RV64) : bool
      then eaddr1
      else (eaddr1 Data.Bits..&.(**) fromInteger 4294967295) in
    let 'pair result1 mstate1 := (if (fn_vm_is_active mstate is_instr) : bool
                                    then vm_translate mstate is_instr is_read eaddr2
                                    else pair (Mem_Result_Ok eaddr2) mstate) in
    let 'pair result2 mstate2 := (match result1 with
                                    | Mem_Result_Err exc_code => pair result1 mstate1
                                    | Mem_Result_Ok eaddr2_pa => mstate_mem_write mstate1 funct3 eaddr2_pa rs2_val
                                    end) in
    let mstate3 :=
      match result2 with
      | Mem_Result_Err exc_code => finish_trap mstate2 exc_code eaddr2
      | Mem_Result_Ok _ => finish_pc_plus_4 mstate2
      end in
    pair is_legal mstate3.

Definition opcode_STORE_FP :=
  fromInteger 39 : InstrField.

Definition opcode_SYSTEM :=
  fromInteger 115 : InstrField.

Definition spec_SYSTEM_EBREAK
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let tval := mstate_pc_read mstate in
    let exc_code := exc_code_breakpoint in
    let mstate1 := finish_trap mstate exc_code tval in
    let 'pair (pair (pair (pair funct12 rs1) funct3) rd) opcode := ifields_I_type
                                                                     instr in
    let is_legal :=
      (andb (opcode == opcode_SYSTEM) (andb (funct3 == funct3_PRIV) (andb (funct12 ==
                                                                           funct12_EBREAK) (andb (rs1 == fromInteger 0)
                                                                                                 (rd ==
                                                                                                  fromInteger 0))))) in
    pair is_legal mstate1.

Definition spec_SYSTEM_ECALL
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let tval := fromInteger 0 in
    let priv := mstate_priv_read mstate in
    let exc_code :=
      if priv == m_Priv_Level : bool then exc_code_ECall_from_M else
      if priv == s_Priv_Level : bool then exc_code_ECall_from_S else
      if priv == u_Priv_Level : bool then exc_code_ECall_from_U else
      error (app (GHC.Base.hs_string__ "Illegal priv ") (hs_string__
                  "ELIDED_STRING")) in
    let mstate1 := finish_trap mstate exc_code tval in
    let 'pair (pair (pair (pair funct12 rs1) funct3) rd) opcode := ifields_I_type
                                                                     instr in
    let is_legal :=
      (andb (opcode == opcode_SYSTEM) (andb (funct3 == funct3_PRIV) (andb (funct12 ==
                                                                           funct12_ECALL) (andb (rs1 == fromInteger 0)
                                                                                                (rd ==
                                                                                                 fromInteger 0))))) in
    pair is_legal mstate1.

Definition spec_SYSTEM_CSRRW
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let priv := mstate_priv_read mstate in
    let 'pair (pair (pair (pair csr_addr rs1) funct3) rd) opcode := ifields_I_type
                                                                      instr in
    let zimm := rs1 in
    let is_CSRRW := (funct3 == funct3_CSRRW) in
    let is_CSRRWI := (funct3 == funct3_CSRRWI) in
    let is_legal := (andb (opcode == opcode_SYSTEM) (orb is_CSRRW is_CSRRWI)) in
    let permission := mstate_csr_read_permission mstate priv csr_addr in
    let legal2 := (permission == CSR_Permission_RW) in
    let old_csr_val :=
      if (rd /= fromInteger 0) : bool
      then if (csr_addr == csr_addr_time) : bool
           then let mtime := mstate_mem_read_mtime mstate in mtime
           else mstate_csr_read mstate csr_addr
      else fromInteger 0 in
    let rd_val := old_csr_val in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let new_csr_val :=
      if is_CSRRW : bool then rs1_val else
      if is_CSRRWI : bool then Bit_Manipulation.zeroExtend_u32_to_u64 rs1 else
      patternFailure in
    let mstate1 :=
      if legal2 : bool
      then let mstate_a := mstate_csr_write mstate csr_addr new_csr_val in
           finish_rd_and_pc_plus_4 mstate_a rd rd_val
      else let tval := Bit_Manipulation.zeroExtend_u32_to_u64 instr in
           finish_trap mstate exc_code_illegal_instruction tval in
    pair is_legal mstate1.

Definition spec_SYSTEM_CSRR_S_C
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let priv := mstate_priv_read mstate in
    let 'pair (pair (pair (pair csr_addr rs1) funct3) rd) opcode := ifields_I_type
                                                                      instr in
    let zimm := rs1 in
    let is_CSRRS := (funct3 == funct3_CSRRS) in
    let is_CSRRC := (funct3 == funct3_CSRRC) in
    let is_CSRRSI := (funct3 == funct3_CSRRSI) in
    let is_CSRRCI := (funct3 == funct3_CSRRCI) in
    let is_legal :=
      (andb (opcode == opcode_SYSTEM) (orb is_CSRRS (orb is_CSRRC (orb is_CSRRSI
                                                                       is_CSRRCI)))) in
    let permission := mstate_csr_read_permission mstate priv csr_addr in
    let legal2 :=
      if (permission == CSR_Permission_None) : bool then false else
      if (permission == CSR_Permission_RO) : bool then (rs1 == fromInteger 0) else
      if (permission == CSR_Permission_RW) : bool then true else
      patternFailure in
    let old_csr_val := mstate_csr_read mstate csr_addr in
    let rd_val := old_csr_val in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let new_csr_val :=
      if is_CSRRS : bool then old_csr_val Data.Bits..|.(**) rs1_val else
      if is_CSRRC : bool
      then old_csr_val Data.Bits..&.(**) (Data.Bits.complement rs1_val) else
      if is_CSRRSI : bool
      then old_csr_val Data.Bits..|.(**)
           Bit_Manipulation.zeroExtend_u32_to_u64 rs1 else
      if is_CSRRCI : bool
      then old_csr_val Data.Bits..&.(**)
           (Data.Bits.complement (Bit_Manipulation.zeroExtend_u32_to_u64 rs1)) else
      patternFailure in
    let mstate1 :=
      if legal2 : bool
      then let mstate_a :=
             if (rs1 /= fromInteger 0) : bool
             then mstate_csr_write mstate csr_addr new_csr_val else
             mstate in
           finish_rd_and_pc_plus_4 mstate_a rd rd_val
      else let tval := Bit_Manipulation.zeroExtend_u32_to_u64 instr in
           finish_trap mstate exc_code_illegal_instruction tval in
    pair is_legal mstate1.

Definition spec_SYSTEM_SFENCE_VM
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let mstatus := mstate_csr_read mstate csr_addr_mstatus in
    let tvm_fault := Data.Bits.testBit mstatus mstatus_tvm_bitpos in
    let priv := mstate_priv_read mstate in
    let 'pair (pair (pair (pair (pair funct7 rs2) rs1) funct3) rd) opcode :=
      ifields_R_type instr in
    let is_legal :=
      (andb (opcode == opcode_SYSTEM) (andb (funct3 == funct3_PRIV) (andb (funct7 ==
                                                                           funct7_SFENCE_VM) (andb (rd == fromInteger 0)
                                                                                                   (priv >=
                                                                                                    s_Priv_Level))))) in
    let rs1_val := mstate_gpr_read mstate rs1 in
    let rs2_val := mstate_gpr_read mstate rs2 in
    let mstate2 :=
      if (tvm_fault) : bool
      then let tval := Bit_Manipulation.zeroExtend_u32_to_u64 instr in
           finish_trap mstate exc_code_illegal_instruction tval
      else let mstate1 := mstate_mem_sfence_vm mstate rs1_val rs2_val in
           finish_pc_plus_4 mstate1 in
    pair is_legal mstate2.

Definition spec_SYSTEM_WFI
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let mstatus := mstate_csr_read mstate csr_addr_mstatus in
    let tw_bit_set := Data.Bits.testBit mstatus mstatus_tw_bitpos in
    let mstate1 :=
      if (tw_bit_set) : bool
      then let tval := Bit_Manipulation.zeroExtend_u32_to_u64 instr in
           finish_trap mstate exc_code_illegal_instruction tval
      else let mstate' := mstate_run_state_write mstate Run_State_WFI in
           finish_pc_plus_4 mstate' in
    let priv := mstate_priv_read mstate in
    let 'pair (pair (pair (pair funct12 rs1) funct3) rd) opcode := ifields_I_type
                                                                     instr in
    let is_legal :=
      (andb (opcode == opcode_SYSTEM) (andb (funct3 == funct3_PRIV) (andb (funct12 ==
                                                                           funct12_WFI) (andb (rs1 == fromInteger 0) (rd
                                                                                               ==
                                                                                               fromInteger 0))))) in
    pair is_legal mstate1.

Definition spec_SYSTEM_xRET
   : Machine_State -> Instr -> (bool * Machine_State)%type :=
  fun mstate instr =>
    let misa := mstate_csr_read mstate csr_addr_misa in
    let rv := mstate_rv_read mstate in
    let mstatus := mstate_csr_read mstate csr_addr_mstatus in
    let 'pair (pair (pair (pair (pair (pair (pair mpp spp) mpie) spie) upie) mie)
        sie) uie := mstatus_stack_fields mstatus in
    let priv := mstate_priv_read mstate in
    let 'pair (pair (pair (pair funct12 rs1) funct3) rd) opcode := ifields_I_type
                                                                     instr in
    let is_MRET := (funct12 == funct12_MRET) in
    let is_SRET := (funct12 == funct12_SRET) in
    let tsr_fault :=
      (andb is_SRET (andb (priv == s_Priv_Level) (Data.Bits.testBit mstatus
                           mstatus_tsr_bitpos))) in
    let is_URET := (funct12 == funct12_URET) in
    let mstate3 :=
      if (tsr_fault) : bool
      then let tval := Bit_Manipulation.zeroExtend_u32_to_u64 instr in
           finish_trap mstate exc_code_illegal_instruction tval
      else let pc1 :=
             if is_MRET : bool then mstate_csr_read mstate csr_addr_mepc else
             if is_SRET : bool then mstate_csr_read mstate csr_addr_sepc else
             if is_URET : bool then mstate_csr_read mstate csr_addr_uepc else
             patternFailure in
           let pc2 :=
             if (rv == RV32) : bool then (pc1 Data.Bits..&.(**) fromInteger 4294967295) else
             pc1 in
           let 'pair (pair (pair (pair (pair mpie' spie') upie') mie') sie') uie' :=
             (if is_MRET : bool
              then pair (pair (pair (pair (pair (fromInteger 1) spie) upie) mpie) sie)
                        uie else
              if is_SRET : bool
              then pair (pair (pair (pair (pair mpie (fromInteger 1)) upie) mie) spie)
                        uie else
              if is_URET : bool
              then pair (pair (pair (pair (pair mpie spie) (fromInteger 1)) mie) sie)
                        upie else
              patternFailure) in
           let new_pp :=
             if (misa_flag misa (GHC.Char.hs_char__ "U")) : bool
             then u_Priv_Level
             else m_Priv_Level in
           let 'pair (pair priv' mpp') spp' := (if andb (priv == m_Priv_Level) (andb
                                                         is_MRET (mpp == m_Priv_Level)) : bool
                                                then pair (pair m_Priv_Level new_pp) spp else
                                                if andb (priv == m_Priv_Level) (andb is_MRET (mpp ==
                                                                                      s_Priv_Level)) : bool
                                                then pair (pair s_Priv_Level new_pp) spp else
                                                if andb (priv == m_Priv_Level) (andb is_MRET (mpp ==
                                                                                      u_Priv_Level)) : bool
                                                then pair (pair u_Priv_Level new_pp) spp else
                                                if andb (priv == m_Priv_Level) (andb is_SRET (spp ==
                                                                                      s_Priv_Level)) : bool
                                                then pair (pair s_Priv_Level new_pp) spp else
                                                if andb (priv == m_Priv_Level) (andb is_SRET (spp ==
                                                                                      u_Priv_Level)) : bool
                                                then pair (pair u_Priv_Level new_pp) spp else
                                                if andb (priv == m_Priv_Level) is_URET : bool
                                                then pair (pair u_Priv_Level new_pp) spp else
                                                if andb (priv == s_Priv_Level) (andb is_SRET (spp ==
                                                                                      s_Priv_Level)) : bool
                                                then pair (pair s_Priv_Level mpp) new_pp else
                                                if andb (priv == s_Priv_Level) (andb is_SRET (spp ==
                                                                                      u_Priv_Level)) : bool
                                                then pair (pair u_Priv_Level mpp) new_pp else
                                                if andb (priv == s_Priv_Level) is_URET : bool
                                                then pair (pair u_Priv_Level mpp) new_pp else
                                                if andb (priv == u_Priv_Level) is_URET : bool
                                                then pair (pair u_Priv_Level mpp) spp else
                                                patternFailure) in
           let mstatus' :=
             mstatus_upd_stack_fields mstatus (pair (pair (pair (pair (pair (pair (pair mpp'
                                                                                        spp') mpie') spie') upie') mie')
                                                          sie') uie') in
           let mstate1 := mstate_csr_write mstate csr_addr_mstatus mstatus' in
           let mstate2 := mstate_priv_write mstate1 priv' in finish_pc mstate2 pc2 in
    let is_legal :=
      (andb (opcode == opcode_SYSTEM) (andb (funct3 == funct3_PRIV) (andb (orb (andb
                                                                                is_MRET (priv == m_Priv_Level)) (orb
                                                                                (andb is_SRET (priv >= s_Priv_Level))
                                                                                (andb is_URET (priv >= u_Priv_Level))))
                                                                          (andb (rs1 == fromInteger 0) (rd ==
                                                                                 fromInteger 0))))) in
    pair is_legal mstate3.

Definition instr_specs : list (Instr_Spec * String)%type :=
  cons (pair spec_LUI (GHC.Base.hs_string__ "LUI")) (cons (pair spec_AUIPC
                                                                (GHC.Base.hs_string__ "AUIPC")) (cons (pair spec_JAL
                                                                                                            (GHC.Base.hs_string__
                                                                                                             "JAL"))
                                                                                                      (cons (pair
                                                                                                             spec_JALR
                                                                                                             (GHC.Base.hs_string__
                                                                                                              "JALR"))
                                                                                                            (cons (pair
                                                                                                                   spec_BRANCH
                                                                                                                   (GHC.Base.hs_string__
                                                                                                                    "BRANCH"))
                                                                                                                  (cons
                                                                                                                   (pair
                                                                                                                    spec_LOAD
                                                                                                                    (GHC.Base.hs_string__
                                                                                                                     "LOAD"))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     spec_STORE
                                                                                                                     (GHC.Base.hs_string__
                                                                                                                      "STORE"))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      spec_OP_IMM
                                                                                                                      (GHC.Base.hs_string__
                                                                                                                       "OP_IMM"))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       spec_OP
                                                                                                                       (GHC.Base.hs_string__
                                                                                                                        "OP"))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        spec_MISC_MEM
                                                                                                                        (GHC.Base.hs_string__
                                                                                                                         "MISC_MEM"))
                                                                                                                       (cons
                                                                                                                        (pair
                                                                                                                         spec_SYSTEM_ECALL
                                                                                                                         (GHC.Base.hs_string__
                                                                                                                          "SYSTEM_ECALL"))
                                                                                                                        (cons
                                                                                                                         (pair
                                                                                                                          spec_SYSTEM_xRET
                                                                                                                          (GHC.Base.hs_string__
                                                                                                                           "SYSTEM_xRET"))
                                                                                                                         (cons
                                                                                                                          (pair
                                                                                                                           spec_SYSTEM_EBREAK
                                                                                                                           (GHC.Base.hs_string__
                                                                                                                            "SYSTEM_EBREAK"))
                                                                                                                          (cons
                                                                                                                           (pair
                                                                                                                            spec_SYSTEM_WFI
                                                                                                                            (GHC.Base.hs_string__
                                                                                                                             "SYSTEM_WFI"))
                                                                                                                           (cons
                                                                                                                            (pair
                                                                                                                             spec_SYSTEM_SFENCE_VM
                                                                                                                             (GHC.Base.hs_string__
                                                                                                                              "SYSTEM_SFENCE_VM"))
                                                                                                                            (cons
                                                                                                                             (pair
                                                                                                                              spec_SYSTEM_CSRRW
                                                                                                                              (GHC.Base.hs_string__
                                                                                                                               "SYSTEM_CSRRW"))
                                                                                                                             (cons
                                                                                                                              (pair
                                                                                                                               spec_SYSTEM_CSRR_S_C
                                                                                                                               (GHC.Base.hs_string__
                                                                                                                                "SYSTEM_CSRR_S_C"))
                                                                                                                              (cons
                                                                                                                               (pair
                                                                                                                                spec_OP_MUL
                                                                                                                                (GHC.Base.hs_string__
                                                                                                                                 "OP_MUL"))
                                                                                                                               (cons
                                                                                                                                (pair
                                                                                                                                 spec_OP_DIV
                                                                                                                                 (GHC.Base.hs_string__
                                                                                                                                  "OP_DIV"))
                                                                                                                                (cons
                                                                                                                                 (pair
                                                                                                                                  spec_OP_REM
                                                                                                                                  (GHC.Base.hs_string__
                                                                                                                                   "OP_REM"))
                                                                                                                                 (cons
                                                                                                                                  (pair
                                                                                                                                   spec_OP_IMM_32
                                                                                                                                   (GHC.Base.hs_string__
                                                                                                                                    "OP_IMM_32"))
                                                                                                                                  (cons
                                                                                                                                   (pair
                                                                                                                                    spec_OP_32
                                                                                                                                    (GHC.Base.hs_string__
                                                                                                                                     "OP_32"))
                                                                                                                                   (cons
                                                                                                                                    (pair
                                                                                                                                     spec_OP_32_M
                                                                                                                                     (GHC.Base.hs_string__
                                                                                                                                      "OP_32_M"))
                                                                                                                                    (cons
                                                                                                                                     (pair
                                                                                                                                      spec_AMO
                                                                                                                                      (GHC.Base.hs_string__
                                                                                                                                       "AMO"))
                                                                                                                                     nil))))))))))))))))))))))).

Definition exec_instr
   : Machine_State -> Instr -> (Machine_State * String)%type :=
  fun mstate instr =>
    let fix tryall arg_0__
              := match arg_0__ with
                 | nil =>
                     (let tval := Bit_Manipulation.zeroExtend_u32_to_u64 instr in
                      pair (finish_trap mstate exc_code_illegal_instruction tval)
                           (GHC.Base.hs_string__ "NONE"))
                 | cons (pair spec name) specs =>
                     (let 'pair success mstate1 := spec mstate instr in
                      (if success : bool
                       then pair mstate1 name
                       else tryall specs))
                 end in
    tryall instr_specs.

(* External variables:
     CSR_Permission_None CSR_Permission_RO CSR_Permission_RW Exc_Code GPR_Addr Instr
     InstrField Instr_C Int Machine_State Mem_Result Mem_Result_Err Mem_Result_Ok N
     None RV32 RV64 Run_State_Running Run_State_WFI Some String Z andb app bool cons
     csr_addr_mcause csr_addr_medeleg csr_addr_mepc csr_addr_mideleg csr_addr_mie
     csr_addr_minstret csr_addr_mip csr_addr_misa csr_addr_mstatus csr_addr_mtval
     csr_addr_mtvec csr_addr_scause csr_addr_sedeleg csr_addr_sepc csr_addr_sideleg
     csr_addr_stval csr_addr_stvec csr_addr_time csr_addr_uepc div error
     exc_code_ECall_from_M exc_code_ECall_from_S exc_code_ECall_from_U
     exc_code_breakpoint exc_code_illegal_instruction exc_code_instr_access_fault
     exc_code_instr_addr_misaligned exc_code_load_access_fault false
     fn_interrupt_pending fn_vm_is_active fromInteger fromIntegral hs_string__
     i_imm12_fields_6_6 i_imm12_fields_7_5 i_imm12_fields_for_FENCE ifields_B_type
     ifields_I_type ifields_J_type ifields_R_type ifields_S_type ifields_U_type
     is_instr_C list m_Priv_Level misa_flag mkCause mstate_csr_read
     mstate_csr_read_permission mstate_csr_write mstate_gpr_read mstate_gpr_write
     mstate_mem_amo mstate_mem_fence mstate_mem_fence_i mstate_mem_read
     mstate_mem_read_mtime mstate_mem_sfence_vm mstate_mem_write mstate_pc_read
     mstate_pc_write mstate_priv_read mstate_priv_write mstate_run_state_write
     mstate_rv_read mstate_xlen_read mstatus_stack_fields mstatus_tsr_bitpos
     mstatus_tvm_bitpos mstatus_tw_bitpos mstatus_upd_stack_fields negate negb nil
     op_zeze__ op_zgze__ op_zl__ op_zm__ op_zp__ op_zsze__ op_zt__ option orb pair
     patternFailure quot r_funct7_fields_for_AMO rem s_Priv_Level true tvec_base
     tvec_mode tvec_mode_VECTORED u_Priv_Level vm_translate
     Bit_Manipulation.bitconcat_u16_u16_to_u32 Bit_Manipulation.clear_bit
     Bit_Manipulation.cvt_s32_to_u32 Bit_Manipulation.cvt_s64_to_u64
     Bit_Manipulation.cvt_u32_to_Int Bit_Manipulation.cvt_u32_to_s32
     Bit_Manipulation.cvt_u64_to_Int Bit_Manipulation.cvt_u64_to_s64
     Bit_Manipulation.signExtend Bit_Manipulation.signExtend_bit_in_u32
     Bit_Manipulation.signExtend_u32_to_u64 Bit_Manipulation.trunc_u64_to_u16
     Bit_Manipulation.trunc_u64_to_u32 Bit_Manipulation.zeroExtend_u16_to_u64
     Bit_Manipulation.zeroExtend_u32_to_u64 Data.Bits.complement
     Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__ Data.Bits.shiftL Data.Bits.shiftR
     Data.Bits.testBit Data.Bits.xor GHC.Enum.maxBound GHC.Enum.minBound
*)

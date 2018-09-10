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
Require Coq.Init.Peano.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require Import GHC.Err.
Require Import GHC.Num.
Require GHC.Wf.
Require Import Machine_State.
Require Import Mem_Ops.
Import Data.Bits.Notations.

(* No type declarations to convert. *)
(* Midamble *)


Require Import GHC.Nat.
Require Import Omega.

Ltac simpl_not_zero := match goal with 
  | [ H : GHC.Base.op_zeze__ ?x ?y = false |- _ ] =>
  unfold GHC.Base.op_zeze__ in H;
    unfold Eq_nat in H;
    simpl in H;
  apply Nat.eqb_neq in H end.
Ltac solve_error_sub :=
  unfold error_sub;
  let Hltb := fresh in
  let HeqHltb := fresh in
  match goal with 
    [ |- context[ Nat.ltb ?x (Pos.to_nat 1) ] ] =>
    remember (Nat.ltb x (Pos.to_nat 1)) as Hltb eqn:HeqHltb; 
      destruct Hltb;
      symmetry in HeqHltb;
      try (rewrite Nat.ltb_lt in HeqHltb);
      try (rewrite Nat.ltb_ge in HeqHltb);
      try solve [zify; omega]
  end.

Ltac solve_vm_translate := Tactics.program_simpl; simpl_not_zero; solve_error_sub.
(* Converted value declarations: *)

Definition pte_A : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 6).

Definition pte_D : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 7).

Definition pte_G : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 5).

Definition pte_R : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 1).

Definition pte_U : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 4).

Definition pte_V : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 0).

Definition pte_W : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 2).

Definition pte_X : N -> bool :=
  fun pte => Data.Bits.testBit pte (fromInteger 3).

Definition fn_is_permitted : Priv_Level -> bool -> bool -> N -> N -> bool :=
  fun priv is_instr is_read mstatus pte =>
    let mstatus_sum := Data.Bits.testBit mstatus mstatus_sum_bitpos in
    let priv_ok :=
      if (priv == u_Priv_Level) : bool then (pte_U pte) else
      if (andb (priv == s_Priv_Level) (pte_U pte)) : bool then mstatus_sum else
      true in
    let mstatus_mxr := Data.Bits.testBit mstatus mstatus_mxr_bitpos in
    let r_mxr := (orb (pte_R pte) (andb (pte_X pte) mstatus_mxr)) in
    let access_ok :=
      (orb (andb is_instr (andb is_read (pte_X pte))) (orb (andb (negb is_instr) (andb
                                                                  is_read r_mxr)) (andb (negb is_instr) (andb (negb
                                                                                                               is_read)
                                                                                                              (pte_W
                                                                                                               pte))))) in
    (andb priv_ok access_ok).

Definition satp_fields : RV -> N -> (N * N * N)%type :=
  fun rv satp =>
    if (rv == RV32) : bool
    then (let ppn :=
            Bit_Manipulation.bitSlice satp (fromInteger 21) (fromInteger 0) in
          let asid := Bit_Manipulation.bitSlice satp (fromInteger 30) (fromInteger 22) in
          let mode := Bit_Manipulation.bitSlice satp (fromInteger 31) (fromInteger 31) in
          pair (pair mode asid) ppn) else
    if (rv == RV64) : bool
    then (let ppn :=
            Bit_Manipulation.bitSlice satp (fromInteger 43) (fromInteger 0) in
          let asid := Bit_Manipulation.bitSlice satp (fromInteger 59) (fromInteger 44) in
          let mode := Bit_Manipulation.bitSlice satp (fromInteger 63) (fromInteger 60) in
          pair (pair mode asid) ppn) else
    patternFailure.

Definition sv32 :=
  fromInteger 1 : N.

Definition sv39 :=
  fromInteger 8 : N.

Definition sv48 :=
  fromInteger 9 : N.

Definition va_vpn_J : N -> N -> nat -> N :=
  fun sv va level =>
    if andb (sv == sv32) (level == fromInteger 0) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 21) (fromInteger 12) else
    if andb (sv == sv32) (level == fromInteger 1) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 31) (fromInteger 22) else
    if andb (sv == sv39) (level == fromInteger 0) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 20) (fromInteger 12) else
    if andb (sv == sv39) (level == fromInteger 1) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 29) (fromInteger 21) else
    if andb (sv == sv39) (level == fromInteger 2) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 38) (fromInteger 30) else
    if andb (sv == sv48) (level == fromInteger 0) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 20) (fromInteger 12) else
    if andb (sv == sv48) (level == fromInteger 1) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 29) (fromInteger 21) else
    if andb (sv == sv48) (level == fromInteger 2) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 38) (fromInteger 30) else
    if andb (sv == sv48) (level == fromInteger 3) : bool
    then Bit_Manipulation.bitSlice va (fromInteger 47) (fromInteger 39) else
    patternFailure.

Definition pte_ppn_J : N -> N -> nat -> N :=
  fun arg_0__ arg_1__ arg_2__ =>
    match arg_0__, arg_1__, arg_2__ with
    | sv, pte, num_3__ =>
        let j_10__ :=
          match arg_0__, arg_1__, arg_2__ with
          | sv, pte, num_6__ =>
              let j_7__ :=
                if num_6__ == fromInteger 3 : bool
                then if (sv == sv48) : bool
                     then Bit_Manipulation.bitSlice pte (fromInteger 53) (fromInteger 37) else
                     patternFailure else
                patternFailure in
              let j_8__ :=
                if num_6__ == fromInteger 3 : bool
                then if (sv == sv39) : bool then fromInteger 0 else
                     j_7__ else
                j_7__ in
              if num_6__ == fromInteger 3 : bool
              then if (sv == sv32) : bool then fromInteger 0 else
                   j_8__ else
              j_8__
          end in
        let j_14__ :=
          match arg_0__, arg_1__, arg_2__ with
          | sv, pte, num_5__ =>
              let j_11__ :=
                if num_5__ == fromInteger 2 : bool
                then if (sv == sv48) : bool
                     then Bit_Manipulation.bitSlice pte (fromInteger 36) (fromInteger 28) else
                     j_10__ else
                j_10__ in
              let j_12__ :=
                if num_5__ == fromInteger 2 : bool
                then if (sv == sv39) : bool
                     then Bit_Manipulation.bitSlice pte (fromInteger 53) (fromInteger 28) else
                     j_11__ else
                j_11__ in
              if num_5__ == fromInteger 2 : bool
              then if (sv == sv32) : bool then fromInteger 0 else
                   j_12__ else
              j_12__
          end in
        let j_18__ :=
          match arg_0__, arg_1__, arg_2__ with
          | sv, pte, num_4__ =>
              let j_15__ :=
                if num_4__ == fromInteger 1 : bool
                then if (sv == sv48) : bool
                     then Bit_Manipulation.bitSlice pte (fromInteger 27) (fromInteger 19) else
                     j_14__ else
                j_14__ in
              let j_16__ :=
                if num_4__ == fromInteger 1 : bool
                then if (sv == sv39) : bool
                     then Bit_Manipulation.bitSlice pte (fromInteger 27) (fromInteger 19) else
                     j_15__ else
                j_15__ in
              if num_4__ == fromInteger 1 : bool
              then if (sv == sv32) : bool
                   then Bit_Manipulation.bitSlice pte (fromInteger 31) (fromInteger 20) else
                   j_16__ else
              j_16__
          end in
        let j_19__ :=
          if num_3__ == fromInteger 0 : bool
          then if (sv == sv48) : bool
               then Bit_Manipulation.bitSlice pte (fromInteger 18) (fromInteger 10) else
               j_18__ else
          j_18__ in
        let j_20__ :=
          if num_3__ == fromInteger 0 : bool
          then if (sv == sv39) : bool
               then Bit_Manipulation.bitSlice pte (fromInteger 18) (fromInteger 10) else
               j_19__ else
          j_19__ in
        if num_3__ == fromInteger 0 : bool
        then if (sv == sv32) : bool
             then Bit_Manipulation.bitSlice pte (fromInteger 19) (fromInteger 10) else
             j_20__ else
        j_20__
    end.

Definition fn_is_misaligned_pte_ppn : N -> N -> nat -> bool :=
  fun sv pte leaf_level =>
    if (andb (leaf_level >= fromInteger 1) ((pte_ppn_J sv pte (fromInteger 0)) /=
              fromInteger 0)) : bool
    then true
    else if (andb (leaf_level >= fromInteger 2) ((pte_ppn_J sv pte (fromInteger 1))
                   /=
                   fromInteger 0)) : bool
         then true
         else if (andb (leaf_level >= fromInteger 3) ((pte_ppn_J sv pte (fromInteger 2))
                        /=
                        fromInteger 0)) : bool
              then true
              else false.

Definition mk_ptn_pa_from_pte : N -> N -> N :=
  fun sv pte =>
    let offset := fromInteger 0 in
    let pte_ppn_0 := pte_ppn_J sv pte (fromInteger 0) in
    let pte_ppn_1 := pte_ppn_J sv pte (fromInteger 1) in
    let pte_ppn_2 := pte_ppn_J sv pte (fromInteger 2) in
    let pte_ppn_3 := pte_ppn_J sv pte (fromInteger 3) in
    let pa :=
      if (sv == sv32) : bool
      then (((Data.Bits.shiftL pte_ppn_1 (fromInteger 22)) Data.Bits..|.(**)
             (Data.Bits.shiftL pte_ppn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if (sv == sv39) : bool
      then ((((Data.Bits.shiftL pte_ppn_2 (fromInteger 30)) Data.Bits..|.(**)
              (Data.Bits.shiftL pte_ppn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL pte_ppn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if (sv == sv48) : bool
      then (((((Data.Bits.shiftL pte_ppn_3 (fromInteger 39)) Data.Bits..|.(**)
               (Data.Bits.shiftL pte_ppn_2 (fromInteger 30))) Data.Bits..|.(**)
              (Data.Bits.shiftL pte_ppn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL pte_ppn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      patternFailure in
    pa.

Definition fn_vm_is_active : Machine_State -> bool -> bool :=
  fun mstate is_instr =>
    let mstatus := mstate_csr_read mstate csr_addr_mstatus in
    let mprv := Data.Bits.testBit mstatus mstatus_mprv_bitpos in
    let mpp :=
      Bit_Manipulation.trunc_u64_to_u32 ((Data.Bits.shiftR mstatus mstatus_mpp_bitpos)
                                         Data.Bits..&.(**)
                                         fromInteger 3) in
    let priv := mstate_priv_read mstate in
    let priv' := if (andb mprv (negb is_instr)) : bool then mpp else priv in
    let satp := mstate_csr_read mstate csr_addr_satp in
    let rv := mstate_rv_read mstate in
    let 'pair (pair satp_mode _) _ := satp_fields rv satp in
    let vm_active :=
      if (rv == RV32) : bool
      then (andb (priv' <= s_Priv_Level) (satp_mode == sv32)) else
      if (rv == RV64) : bool
      then (andb (priv' <= s_Priv_Level) (orb (satp_mode == sv39) (satp_mode ==
                                               sv48))) else
      patternFailure in
    vm_active.

Definition va_offset : N -> N :=
  fun va => (va Data.Bits..&.(**) fromInteger 4095).

Definition mk_pa_in_page : N -> N -> N -> nat -> N :=
  fun sv pte va level =>
    let offset := va_offset va in
    let va_vpn_0 := va_vpn_J sv va (fromInteger 0) in
    let va_vpn_1 := va_vpn_J sv va (fromInteger 1) in
    let va_vpn_2 := va_vpn_J sv va (fromInteger 2) in
    let va_vpn_3 := va_vpn_J sv va (fromInteger 3) in
    let pte_ppn_0 := pte_ppn_J sv pte (fromInteger 0) in
    let pte_ppn_1 := pte_ppn_J sv pte (fromInteger 1) in
    let pte_ppn_2 := pte_ppn_J sv pte (fromInteger 2) in
    let pte_ppn_3 := pte_ppn_J sv pte (fromInteger 3) in
    let pa :=
      if andb (sv == sv32) (level == fromInteger 1) : bool
      then (((Data.Bits.shiftL pte_ppn_1 (fromInteger 22)) Data.Bits..|.(**)
             (Data.Bits.shiftL va_vpn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv32) (level == fromInteger 0) : bool
      then (((Data.Bits.shiftL pte_ppn_1 (fromInteger 22)) Data.Bits..|.(**)
             (Data.Bits.shiftL pte_ppn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv39) (level == fromInteger 2) : bool
      then ((((Data.Bits.shiftL pte_ppn_2 (fromInteger 30)) Data.Bits..|.(**)
              (Data.Bits.shiftL va_vpn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL va_vpn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv39) (level == fromInteger 1) : bool
      then ((((Data.Bits.shiftL pte_ppn_2 (fromInteger 30)) Data.Bits..|.(**)
              (Data.Bits.shiftL pte_ppn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL va_vpn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv39) (level == fromInteger 0) : bool
      then ((((Data.Bits.shiftL pte_ppn_2 (fromInteger 30)) Data.Bits..|.(**)
              (Data.Bits.shiftL pte_ppn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL pte_ppn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv48) (level == fromInteger 3) : bool
      then (((((Data.Bits.shiftL pte_ppn_3 (fromInteger 39)) Data.Bits..|.(**)
               (Data.Bits.shiftL va_vpn_2 (fromInteger 30))) Data.Bits..|.(**)
              (Data.Bits.shiftL va_vpn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL va_vpn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv48) (level == fromInteger 2) : bool
      then (((((Data.Bits.shiftL pte_ppn_3 (fromInteger 39)) Data.Bits..|.(**)
               (Data.Bits.shiftL pte_ppn_2 (fromInteger 30))) Data.Bits..|.(**)
              (Data.Bits.shiftL va_vpn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL va_vpn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv48) (level == fromInteger 1) : bool
      then (((((Data.Bits.shiftL pte_ppn_3 (fromInteger 39)) Data.Bits..|.(**)
               (Data.Bits.shiftL pte_ppn_2 (fromInteger 30))) Data.Bits..|.(**)
              (Data.Bits.shiftL pte_ppn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL va_vpn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      if andb (sv == sv48) (level == fromInteger 0) : bool
      then (((((Data.Bits.shiftL pte_ppn_3 (fromInteger 39)) Data.Bits..|.(**)
               (Data.Bits.shiftL pte_ppn_2 (fromInteger 30))) Data.Bits..|.(**)
              (Data.Bits.shiftL pte_ppn_1 (fromInteger 21))) Data.Bits..|.(**)
             (Data.Bits.shiftL pte_ppn_0 (fromInteger 12))) Data.Bits..|.(**)
            offset) else
      patternFailure in
    pa.

Program Definition vm_translate
           : Machine_State -> bool -> bool -> N -> (Mem_Result * Machine_State)%type :=
          fun mstate is_instr is_read va =>
            let satp := mstate_csr_read mstate csr_addr_satp in
            let exc_code_page_fault :=
              if Bool.Sumbool.sumbool_of_bool is_instr
              then exc_code_Instruction_Page_Fault else
              if Bool.Sumbool.sumbool_of_bool is_read then exc_code_Load_Page_Fault else
              exc_code_Store_AMO_Page_Fault in
            let exc_code_access :=
              if Bool.Sumbool.sumbool_of_bool is_instr then exc_code_instr_access_fault else
              if Bool.Sumbool.sumbool_of_bool is_read then exc_code_load_access_fault else
              exc_code_store_AMO_access_fault in
            let mstatus := mstate_csr_read mstate csr_addr_mstatus in
            let mprv := Data.Bits.testBit mstatus mstatus_mprv_bitpos in
            let mpp :=
              Bit_Manipulation.trunc_u64_to_u32 ((Data.Bits.shiftR mstatus mstatus_mpp_bitpos)
                                                 Data.Bits..&.(**)
                                                 fromInteger 3) in
            let priv := mstate_priv_read mstate in
            let priv' :=
              if Bool.Sumbool.sumbool_of_bool (andb mprv (negb is_instr))
              then mpp
              else priv in
            let rv := mstate_rv_read mstate in
            let 'pair (pair sv asid) pt_base_ppn := satp_fields rv satp in
            let 'pair funct3 pte_size_bytes := (if Bool.Sumbool.sumbool_of_bool (sv == sv32)
                                                then pair funct3_LW (fromInteger 4) else
                                                if Bool.Sumbool.sumbool_of_bool (sv == sv39)
                                                then pair funct3_LD (fromInteger 8) else
                                                if Bool.Sumbool.sumbool_of_bool (sv == sv48)
                                                then pair funct3_LD (fromInteger 8) else
                                                patternFailure) in
            let pt_base_addr := (Data.Bits.shiftL pt_base_ppn (fromInteger 12)) in
            let ptw : Machine_State -> N -> nat -> (Mem_Result * Machine_State)%type :=
              GHC.Wf.wfFix3 Coq.Init.Peano.lt (fun mstate ptn_pa level => level) _ (fun mstate
                             ptn_pa
                             level
                             ptw =>
                               let vpn_J := va_vpn_J sv va level in
                               let pte_pa := ptn_pa + (vpn_J * pte_size_bytes) in
                               let 'pair mem_result mstate1 := mstate_mem_read mstate exc_code_access funct3
                                                                 pte_pa in
                               match mem_result with
                               | Mem_Result_Err exc_code => pair mem_result mstate1
                               | Mem_Result_Ok pte =>
                                   let is_bad_pte_A_D :=
                                     (orb (negb (pte_A pte)) (andb (negb is_read) (negb (pte_D pte)))) in
                                   let is_misaligned_pte_ppn := fn_is_misaligned_pte_ppn sv pte level in
                                   let is_permitted := fn_is_permitted priv' is_instr is_read mstatus pte in
                                   let is_valid_pte_R_W := (orb (pte_R pte) (negb (pte_W pte))) in
                                   let is_valid_pte := pte_V pte in
                                   let is_leaf := (orb (pte_R pte) (pte_X pte)) in
                                   if Bool.Sumbool.sumbool_of_bool (orb (negb is_valid_pte) (negb
                                                                         is_valid_pte_R_W))
                                   then pair (Mem_Result_Err exc_code_page_fault) mstate1
                                   else if Bool.Sumbool.sumbool_of_bool (andb is_leaf (orb (negb is_permitted) (orb
                                                                                            is_misaligned_pte_ppn
                                                                                            is_bad_pte_A_D)))
                                        then pair (Mem_Result_Err exc_code_page_fault) mstate1
                                        else if Bool.Sumbool.sumbool_of_bool is_leaf
                                             then let pa := mk_pa_in_page sv pte va level in
                                                  pair (Mem_Result_Ok pa) mstate1
                                             else if Bool.Sumbool.sumbool_of_bool (level == fromInteger 0)
                                                  then pair (Mem_Result_Err exc_code_page_fault) mstate1
                                                  else ptw mstate1 (mk_ptn_pa_from_pte sv pte) (level - fromInteger 1)
                               end) in
            let start_level :=
              if Bool.Sumbool.sumbool_of_bool (sv == sv32) then fromInteger 1 else
              if Bool.Sumbool.sumbool_of_bool (sv == sv39) then fromInteger 2 else
              if Bool.Sumbool.sumbool_of_bool (sv == sv48) then fromInteger 3 else
              patternFailure in
            let 'pair mem_result mstate1 := ptw mstate pt_base_addr start_level in
            pair mem_result mstate1.
Solve Obligations with (solve_vm_translate).

(* External variables:
     Bool.Sumbool.sumbool_of_bool Machine_State Mem_Result Mem_Result_Err
     Mem_Result_Ok N Priv_Level RV RV32 RV64 andb bool csr_addr_mstatus csr_addr_satp
     exc_code_Instruction_Page_Fault exc_code_Load_Page_Fault
     exc_code_Store_AMO_Page_Fault exc_code_instr_access_fault
     exc_code_load_access_fault exc_code_store_AMO_access_fault false fromInteger
     funct3_LD funct3_LW mstate_csr_read mstate_mem_read mstate_priv_read
     mstate_rv_read mstatus_mpp_bitpos mstatus_mprv_bitpos mstatus_mxr_bitpos
     mstatus_sum_bitpos nat negb op_zeze__ op_zgze__ op_zlze__ op_zm__ op_zp__
     op_zsze__ op_zt__ orb pair patternFailure s_Priv_Level true u_Priv_Level
     Bit_Manipulation.bitSlice Bit_Manipulation.trunc_u64_to_u32 Coq.Init.Peano.lt
     Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__ Data.Bits.shiftL Data.Bits.shiftR
     Data.Bits.testBit GHC.Wf.wfFix3
*)

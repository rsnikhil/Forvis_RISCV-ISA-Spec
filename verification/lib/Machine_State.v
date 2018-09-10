(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Converted imports: *)

Require Import Address_Map.
Require Import Arch_Defs.
Require Bit_Manipulation.
Require Import CSR_File.
Require Import Coq.Init.Datatypes.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require Import GHC.Err.
Require Import GHC.Num.
Require Import GPR_File.
Require IO.
Require Import MMIO.
Require Import Mem_Ops.
Require Import Memory.
Require Numeric.
Import Data.Bits.Notations.

(* Converted type declarations: *)

Inductive Run_State : Type
  := Run_State_Running : Run_State
  |  Run_State_WFI : Run_State.

Inductive Machine_State : Type
  := Mk_Machine_State
   : N ->
     GPR_File ->
     CSR_File ->
     Priv_Level ->
     Mem -> MMIO -> list (N * N)%type -> RV -> Int -> Run_State -> Machine_State.

Instance Default__Run_State : Default Run_State :=
  Build_Default _ Run_State_Running.

Definition f_csrs (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ f_csrs _ _ _ _ _ _ _ := arg_0__ in
  f_csrs.

Definition f_gprs (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ f_gprs _ _ _ _ _ _ _ _ := arg_0__ in
  f_gprs.

Definition f_mem (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ _ _ f_mem _ _ _ _ _ := arg_0__ in
  f_mem.

Definition f_mem_addr_ranges (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ _ _ _ _ f_mem_addr_ranges _ _ _ := arg_0__ in
  f_mem_addr_ranges.

Definition f_mmio (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ _ _ _ f_mmio _ _ _ _ := arg_0__ in
  f_mmio.

Definition f_pc (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State f_pc _ _ _ _ _ _ _ _ _ := arg_0__ in
  f_pc.

Definition f_priv (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ _ f_priv _ _ _ _ _ _ := arg_0__ in
  f_priv.

Definition f_run_state (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ _ _ _ _ _ _ _ f_run_state := arg_0__ in
  f_run_state.

Definition f_rv (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ _ _ _ _ _ f_rv _ _ := arg_0__ in
  f_rv.

Definition f_verbosity (arg_0__ : Machine_State) :=
  let 'Mk_Machine_State _ _ _ _ _ _ _ _ f_verbosity _ := arg_0__ in
  f_verbosity.
(* Midamble *)

(* Of course, we should fill in this definition with a suitable value. 
   But that will require defining some additional instances of the 
   Default type class. So, we admit for now. *)

Require Import GHC.Err.

Instance Default_Machine_State : GHC.Err.Default Machine_State.
Admitted.

(* Converted value declarations: *)

(* Skipping instance Show__Run_State of class Show *)

Local Definition Eq___Run_State_op_zeze__ : Run_State -> Run_State -> bool :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | Run_State_Running, Run_State_Running => true
    | Run_State_WFI, Run_State_WFI => true
    | _, _ => false
    end.

Local Definition Eq___Run_State_op_zsze__ : Run_State -> Run_State -> bool :=
  fun x y => negb (Eq___Run_State_op_zeze__ x y).

Program Instance Eq___Run_State : Eq_ Run_State :=
  fun _ k =>
    k {| op_zeze____ := Eq___Run_State_op_zeze__ ;
         op_zsze____ := Eq___Run_State_op_zsze__ |}.

Definition is_supported_addr : Machine_State -> InstrField -> N -> bool :=
  fun mstate funct3 addr =>
    let addr_ranges := f_mem_addr_ranges mstate in
    let size :=
      if (funct3 == funct3_LB) : bool then fromInteger 1 else
      if (funct3 == funct3_LBU) : bool then fromInteger 1 else
      if (funct3 == funct3_LH) : bool then fromInteger 2 else
      if (funct3 == funct3_LHU) : bool then fromInteger 2 else
      if (funct3 == funct3_LW) : bool then fromInteger 4 else
      if (funct3 == funct3_LWU) : bool then fromInteger 4 else
      if (funct3 == funct3_LD) : bool then fromInteger 8 else
      patternFailure in
    let addr_lim := addr + size in
    let fix check arg_9__
              := match arg_9__ with
                 | nil => false
                 | cons (pair astart alim) ranges =>
                     if (andb (addr >= astart) (addr_lim <= alim)) : bool then true else
                     check ranges
                 end in
    check addr_ranges.

Definition mstate_mem_amo
   : Machine_State ->
     N ->
     InstrField ->
     InstrField ->
     InstrField -> InstrField -> N -> (Mem_Result * Machine_State)%type :=
  fun mstate addr funct3 msbs5 aq rl st_val =>
    if negb (is_supported_addr mstate funct3 addr) : bool
    then let load_result := Mem_Result_Err exc_code_store_AMO_access_fault in
         pair load_result mstate
    else if negb (is_IO_addr addr) : bool
         then let 'pair load_result mem' := mem_amo (f_mem mstate) addr funct3 msbs5 aq
                                              rl st_val in
              pair load_result (let 'Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__
                                   f_priv_5__ f_mem_6__ f_mmio_7__ f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__
                                   f_run_state_11__ := mstate in
                    Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ mem' f_mmio_7__
                                     f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__)
         else let 'pair load_result mmio' := mmio_amo (f_mmio mstate) addr funct3 msbs5
                                               aq rl st_val in
              pair load_result (let 'Mk_Machine_State f_pc_15__ f_gprs_16__ f_csrs_17__
                                   f_priv_18__ f_mem_19__ f_mmio_20__ f_mem_addr_ranges_21__ f_rv_22__
                                   f_verbosity_23__ f_run_state_24__ := mstate in
                    Mk_Machine_State f_pc_15__ f_gprs_16__ f_csrs_17__ f_priv_18__ f_mem_19__ mmio'
                                     f_mem_addr_ranges_21__ f_rv_22__ f_verbosity_23__ f_run_state_24__).

Definition mstate_mem_read
   : Machine_State ->
     Exc_Code -> InstrField -> N -> (Mem_Result * Machine_State)%type :=
  fun mstate exc_code_access_fault funct3 addr =>
    if negb (is_supported_addr mstate funct3 addr) : bool
    then let load_result := Mem_Result_Err exc_code_access_fault in
         pair load_result mstate
    else if negb (is_IO_addr addr) : bool
         then let 'pair load_result mem' := mem_read (f_mem mstate) funct3 addr in
              let mstate' :=
                (let 'Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ f_mem_6__
                    f_mmio_7__ f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__ :=
                   mstate in
                 Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ mem' f_mmio_7__
                                  f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__) in
              pair load_result mstate'
         else let 'pair load_result mmio' := mmio_read (f_mmio mstate) funct3 addr in
              pair load_result (let 'Mk_Machine_State f_pc_16__ f_gprs_17__ f_csrs_18__
                                   f_priv_19__ f_mem_20__ f_mmio_21__ f_mem_addr_ranges_22__ f_rv_23__
                                   f_verbosity_24__ f_run_state_25__ := mstate in
                    Mk_Machine_State f_pc_16__ f_gprs_17__ f_csrs_18__ f_priv_19__ f_mem_20__ mmio'
                                     f_mem_addr_ranges_22__ f_rv_23__ f_verbosity_24__ f_run_state_25__).

Definition mstate_mem_write
   : Machine_State -> InstrField -> N -> N -> (Mem_Result * Machine_State)%type :=
  fun mstate funct3 addr val =>
    if negb (is_supported_addr mstate funct3 addr) : bool
    then let load_result := Mem_Result_Err exc_code_store_AMO_access_fault in
         pair load_result mstate
    else if negb (is_IO_addr addr) : bool
         then let 'pair store_result mem' := mem_write (f_mem mstate) funct3 addr val in
              pair store_result (let 'Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__
                                    f_priv_5__ f_mem_6__ f_mmio_7__ f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__
                                    f_run_state_11__ := mstate in
                    Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ mem' f_mmio_7__
                                     f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__)
         else let 'pair store_result mmio' := mmio_write (f_mmio mstate) funct3 addr
                                                val in
              let mstate1 :=
                let 'Mk_Machine_State f_pc_15__ f_gprs_16__ f_csrs_17__ f_priv_18__ f_mem_19__
                   f_mmio_20__ f_mem_addr_ranges_21__ f_rv_22__ f_verbosity_23__
                   f_run_state_24__ := mstate in
                Mk_Machine_State f_pc_15__ f_gprs_16__ f_csrs_17__ f_priv_18__ f_mem_19__ mmio'
                                 f_mem_addr_ranges_21__ f_rv_22__ f_verbosity_23__ f_run_state_24__ in
              pair store_result mstate1.

Definition mkMachine_State
   : RV -> N -> list (N * N)%type -> (list (Int * N)%type) -> Machine_State :=
  fun rv initial_PC addr_ranges addr_byte_list =>
    Mk_Machine_State initial_PC mkGPR_File (mkCSR_File rv) m_Priv_Level (mkMem
                      addr_byte_list) mkMMIO addr_ranges rv (fromInteger 0) Run_State_Running.

Definition mstate_csr_read : Machine_State -> CSR_Addr -> N :=
  fun mstate csr_addr => csr_read (f_rv mstate) (f_csrs mstate) csr_addr.

Definition mstate_csr_read_permission
   : Machine_State -> Priv_Level -> CSR_Addr -> CSR_Permission :=
  fun mstate priv csr_addr => csr_permission (f_csrs mstate) priv csr_addr.

Definition mstate_csr_write : Machine_State -> CSR_Addr -> N -> Machine_State :=
  fun mstate csr_addr value =>
    let csr_file' := csr_write (f_rv mstate) (f_csrs mstate) csr_addr value in
    let mstate' :=
      let 'Mk_Machine_State f_pc_1__ f_gprs_2__ f_csrs_3__ f_priv_4__ f_mem_5__
         f_mmio_6__ f_mem_addr_ranges_7__ f_rv_8__ f_verbosity_9__ f_run_state_10__ :=
        mstate in
      Mk_Machine_State f_pc_1__ f_gprs_2__ csr_file' f_priv_4__ f_mem_5__ f_mmio_6__
                       f_mem_addr_ranges_7__ f_rv_8__ f_verbosity_9__ f_run_state_10__ in
    mstate'.

Definition mstate_gpr_read : Machine_State -> GPR_Addr -> N :=
  fun mstate reg => gpr_read (f_gprs mstate) reg.

Definition mstate_gpr_write : Machine_State -> GPR_Addr -> N -> Machine_State :=
  fun mstate reg val =>
    let gprs := f_gprs mstate in
    let rv := f_rv mstate in
    let val1 :=
      if rv == RV32 : bool then Bit_Manipulation.signExtend val (fromInteger 32) else
      if rv == RV64 : bool then val else
      patternFailure in
    let gprs' := gpr_write gprs reg val1 in
    let mstate' :=
      let 'Mk_Machine_State f_pc_5__ f_gprs_6__ f_csrs_7__ f_priv_8__ f_mem_9__
         f_mmio_10__ f_mem_addr_ranges_11__ f_rv_12__ f_verbosity_13__
         f_run_state_14__ := mstate in
      Mk_Machine_State f_pc_5__ gprs' f_csrs_7__ f_priv_8__ f_mem_9__ f_mmio_10__
                       f_mem_addr_ranges_11__ f_rv_12__ f_verbosity_13__ f_run_state_14__ in
    mstate'.

Definition mstate_mem_all_console_input
   : Machine_State -> (String * String)%type :=
  fun mstate => let mmio := f_mmio mstate in mmio_all_console_input mmio.

Definition mstate_mem_all_console_output
   : Machine_State -> (String * String)%type :=
  fun mstate => let mmio := f_mmio mstate in mmio_all_console_output mmio.

Definition mstate_mem_deq_console_output
   : Machine_State -> (String * Machine_State)%type :=
  fun mstate =>
    let mmio := f_mmio mstate in
    let 'pair console_output mmio' := mmio_deq_console_output mmio in
    let mstate' :=
      if (console_output == GHC.Base.hs_string__ "") : bool
      then mstate
      else let 'Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ f_mem_6__
              f_mmio_7__ f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__ :=
             mstate in
           Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ f_mem_6__ mmio'
                            f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__ in
    pair console_output mstate'.

Definition mstate_mem_enq_console_input
   : Machine_State -> String -> Machine_State :=
  fun mstate s =>
    let mmio := f_mmio mstate in
    let mmio' := mmio_enq_console_input mmio s in
    let mstate' :=
      let 'Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ f_mem_6__
         f_mmio_7__ f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__ :=
        mstate in
      Mk_Machine_State f_pc_2__ f_gprs_3__ f_csrs_4__ f_priv_5__ f_mem_6__ mmio'
                       f_mem_addr_ranges_8__ f_rv_9__ f_verbosity_10__ f_run_state_11__ in
    mstate'.

Definition mstate_mem_fence : Machine_State -> Machine_State :=
  fun mstate => mstate.

Definition mstate_mem_fence_i : Machine_State -> Machine_State :=
  fun mstate => mstate.

Definition mstate_mem_num_entries : Machine_State -> Int :=
  fun mstate => let mem := f_mem mstate in mem_num_entries mem.

Definition mstate_mem_read_mtime : Machine_State -> N :=
  fun mstate => mmio_read_mtime (f_mmio mstate).

Definition mstate_mem_sfence_vm : Machine_State -> N -> N -> Machine_State :=
  fun mstate rs1_val rs2_val => mstate.

Definition mstate_mem_tick : Machine_State -> Machine_State :=
  fun mstate =>
    let mmio := f_mmio mstate in
    let mmio1 := mmio_tick_mtime mmio in
    let 'pair (pair eip_new tip_new) sip_new := mmio_has_interrupts mmio1 in
    let csrs := f_csrs mstate in
    let rv := f_rv mstate in
    let csrs1 :=
      (let mcycle := csr_read rv csrs csr_addr_mcycle in
       let csrs' := csr_write rv csrs csr_addr_mcycle (mcycle + fromInteger 1) in
       csrs') in
    let mip_old := csr_read rv csrs1 csr_addr_mip in
    let eip_old := Data.Bits.testBit mip_old mip_meip_bitpos in
    let tip_old := Data.Bits.testBit mip_old mip_mtip_bitpos in
    let sip_old := Data.Bits.testBit mip_old mip_msip_bitpos in
    let csrs2 :=
      if (andb (eip_new == eip_old) (andb (tip_new == tip_old) (sip_new ==
                                           sip_old))) : bool
      then csrs1
      else (let mip1 :=
              if (eip_new) : bool
              then (mip_old Data.Bits..|.(**)
                    (Data.Bits.shiftL (fromInteger 1) mip_meip_bitpos))
              else (mip_old Data.Bits..&.(**)
                    (Data.Bits.complement (Data.Bits.shiftL (fromInteger 1) mip_meip_bitpos))) in
            let mip2 :=
              if (tip_new) : bool
              then (mip1 Data.Bits..|.(**) (Data.Bits.shiftL (fromInteger 1) mip_mtip_bitpos))
              else (mip1 Data.Bits..&.(**)
                    (Data.Bits.complement (Data.Bits.shiftL (fromInteger 1) mip_mtip_bitpos))) in
            let mip3 :=
              if (sip_new) : bool
              then (mip2 Data.Bits..|.(**) (Data.Bits.shiftL (fromInteger 1) mip_msip_bitpos))
              else (mip2 Data.Bits..&.(**)
                    (Data.Bits.complement (Data.Bits.shiftL (fromInteger 1) mip_msip_bitpos))) in
            csr_write rv csrs1 csr_addr_mip mip3) in
    let mstate1 :=
      let 'Mk_Machine_State f_pc_16__ f_gprs_17__ f_csrs_18__ f_priv_19__ f_mem_20__
         f_mmio_21__ f_mem_addr_ranges_22__ f_rv_23__ f_verbosity_24__
         f_run_state_25__ := mstate in
      Mk_Machine_State f_pc_16__ f_gprs_17__ csrs2 f_priv_19__ f_mem_20__ mmio1
                       f_mem_addr_ranges_22__ f_rv_23__ f_verbosity_24__ f_run_state_25__ in
    mstate1.

Definition mstate_pc_read : Machine_State -> N :=
  fun mstate => f_pc mstate.

Definition mstate_pc_write : Machine_State -> N -> Machine_State :=
  fun mstate val =>
    let 'Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__
       f_mmio_5__ f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ f_run_state_9__ :=
      mstate in
    Mk_Machine_State val f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__ f_mmio_5__
                     f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ f_run_state_9__.

Definition mstate_print : String -> Machine_State -> IO.IO unit :=
  fun indent mstate =>
    let run_state := f_run_state mstate in
    let rv := f_rv mstate in
    let priv := f_priv mstate in
    let csrs := f_csrs mstate in
    let gprs := f_gprs mstate in
    let pc := f_pc mstate in
    IO.putStrLn (app indent (app (hs_string__ "ELIDED_STRING") (app
                                  (GHC.Base.hs_string__ " pc:") (app (Numeric.showHex pc (GHC.Base.hs_string__
                                                                                          " priv:")) (hs_string__
                                                                      "ELIDED_STRING"))))) >>
    (print_GPR_File indent gprs >>
     (print_CSR_File indent rv csrs >>
      IO.putStrLn (app indent (hs_string__ "ELIDED_STRING")))).

Definition mstate_priv_read : Machine_State -> Priv_Level :=
  fun mstate => f_priv mstate.

Definition mstate_priv_write : Machine_State -> Priv_Level -> Machine_State :=
  fun mstate priv =>
    let 'Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__
       f_mmio_5__ f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ f_run_state_9__ :=
      mstate in
    Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ priv f_mem_4__ f_mmio_5__
                     f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ f_run_state_9__.

Definition mstate_run_state_read : Machine_State -> Run_State :=
  fun mstate => f_run_state mstate.

Definition mstate_run_state_write
   : Machine_State -> Run_State -> Machine_State :=
  fun mstate run_state =>
    let 'Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__
       f_mmio_5__ f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ f_run_state_9__ :=
      mstate in
    Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__ f_mmio_5__
                     f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ run_state.

Definition mstate_rv_read : Machine_State -> RV :=
  fun mstate => f_rv mstate.

Definition mstate_rv_write : Machine_State -> RV -> Machine_State :=
  fun mstate rv =>
    let 'Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__
       f_mmio_5__ f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ f_run_state_9__ :=
      mstate in
    Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__ f_mmio_5__
                     f_mem_addr_ranges_6__ rv f_verbosity_8__ f_run_state_9__.

Definition mstate_verbosity_read : Machine_State -> Int :=
  fun mstate => f_verbosity mstate.

Definition mstate_verbosity_write : Machine_State -> Int -> Machine_State :=
  fun mstate verbosity =>
    let 'Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__
       f_mmio_5__ f_mem_addr_ranges_6__ f_rv_7__ f_verbosity_8__ f_run_state_9__ :=
      mstate in
    Mk_Machine_State f_pc_0__ f_gprs_1__ f_csrs_2__ f_priv_3__ f_mem_4__ f_mmio_5__
                     f_mem_addr_ranges_6__ f_rv_7__ verbosity f_run_state_9__.

Definition mstate_xlen_read : Machine_State -> Int :=
  fun mstate =>
    if f_rv mstate == RV32 : bool then fromInteger 32 else
    if f_rv mstate == RV64 : bool then fromInteger 64 else
    patternFailure.

(* External variables:
     Build_Default CSR_Addr CSR_File CSR_Permission Default Eq_ Exc_Code GPR_Addr
     GPR_File InstrField Int MMIO Mem Mem_Result Mem_Result_Err N Priv_Level RV RV32
     RV64 String andb app bool cons csr_addr_mcycle csr_addr_mip csr_permission
     csr_read csr_write exc_code_store_AMO_access_fault false fromInteger funct3_LB
     funct3_LBU funct3_LD funct3_LH funct3_LHU funct3_LW funct3_LWU gpr_read
     gpr_write hs_string__ is_IO_addr list m_Priv_Level mem_amo mem_num_entries
     mem_read mem_write mip_meip_bitpos mip_msip_bitpos mip_mtip_bitpos mkCSR_File
     mkGPR_File mkMMIO mkMem mmio_all_console_input mmio_all_console_output mmio_amo
     mmio_deq_console_output mmio_enq_console_input mmio_has_interrupts mmio_read
     mmio_read_mtime mmio_tick_mtime mmio_write negb nil op_zeze__ op_zeze____
     op_zgze__ op_zgzg__ op_zlze__ op_zp__ op_zsze____ op_zt__ pair patternFailure
     print_CSR_File print_GPR_File true unit Bit_Manipulation.signExtend
     Data.Bits.complement Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__
     Data.Bits.shiftL Data.Bits.testBit IO.IO IO.putStrLn Numeric.showHex
*)

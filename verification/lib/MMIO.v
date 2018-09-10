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
Require Import Bit_Manipulation.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require GHC.DeferredFix.
Require Import GHC.Err.
Require Import GHC.Num.
Require Import Mem_Ops.
Require Import UART.
Import Data.Bits.Notations.

(* Converted type declarations: *)

Inductive MMIO : Type := Mk_MMIO : N -> N -> bool -> N -> UART_NS16550A -> MMIO.

Definition f_msip (arg_0__ : MMIO) :=
  let 'Mk_MMIO _ _ _ f_msip _ := arg_0__ in
  f_msip.

Definition f_mtime (arg_0__ : MMIO) :=
  let 'Mk_MMIO f_mtime _ _ _ _ := arg_0__ in
  f_mtime.

Definition f_mtimecmp (arg_0__ : MMIO) :=
  let 'Mk_MMIO _ f_mtimecmp _ _ _ := arg_0__ in
  f_mtimecmp.

Definition f_mtip (arg_0__ : MMIO) :=
  let 'Mk_MMIO _ _ f_mtip _ _ := arg_0__ in
  f_mtip.

Definition f_uart (arg_0__ : MMIO) :=
  let 'Mk_MMIO _ _ _ _ f_uart := arg_0__ in
  f_uart.
(* Midamble *)

Require Import GHC.Err.

Instance MMIO_Default : Default MMIO :=
  Build_Default _ (Mk_MMIO default default default default default).


(* Converted value declarations: *)

Definition mkMMIO : MMIO :=
  Mk_MMIO (fromInteger 1) (fromInteger 0) false (fromInteger 0) mkUART.

Definition mmio_all_console_input : MMIO -> (String * String)%type :=
  fun mmio => let uart := f_uart mmio in uart_all_input uart.

Definition mmio_all_console_output : MMIO -> (String * String)%type :=
  fun mmio => let uart := f_uart mmio in uart_all_output uart.

Definition mmio_amo
   : MMIO ->
     N ->
     InstrField ->
     InstrField -> InstrField -> InstrField -> N -> (Mem_Result * MMIO)%type :=
  fun mmio addr funct3 msbs5 aq rl stv_d =>
    if (negb (is_AMO_aligned funct3 addr)) : bool
    then pair (Mem_Result_Err exc_code_store_AMO_addr_misaligned) mmio
    else if (andb (addr /= addr_msip) (addr /= addr_mtimecmp)) : bool
         then pair (Mem_Result_Err exc_code_store_AMO_access_fault) mmio
         else let omv_d :=
                if (addr == addr_msip) : bool
                then f_msip mmio
                else if (addr == addr_mtimecmp) : bool
                     then f_mtimecmp mmio
                     else if (addr == addr_mtime) : bool
                          then f_mtime mmio
                          else fromInteger 0 in
              let omv_w0 := trunc_u64_to_u32 omv_d in
              let omv_w1 := trunc_u64_to_u32 (Data.Bits.shiftR omv_d (fromInteger 32)) in
              let ldv :=
                if (msbs5 == msbs5_AMO_SC) : bool then fromInteger 1 else
                if (funct3 == funct3_AMO_W) : bool
                then bitconcat_u32_u32_to_u64 (fromInteger 0) omv_w0 else
                if (funct3 == funct3_AMO_D) : bool then omv_d else
                patternFailure in
              let stv_w1 := trunc_u64_to_u32 (Data.Bits.shiftR stv_d (fromInteger 32)) in
              let stv_w0 := trunc_u64_to_u32 stv_d in
              let 'pair nmv_w1 nmv_w0 := (if (msbs5 == msbs5_AMO_SC) : bool
                                          then pair stv_w1 stv_w0 else
                                          if (msbs5 == msbs5_AMO_SWAP) : bool then pair stv_w1 stv_w0 else
                                          if (msbs5 == msbs5_AMO_ADD) : bool
                                          then (if (funct3 == funct3_AMO_W) : bool
                                                then let z_w :=
                                                       cvt_s32_to_u32 ((cvt_u32_to_s32 omv_w0) +
                                                                       (cvt_u32_to_s32 stv_w0)) in
                                                     pair (fromInteger 0) z_w
                                                else let z_d :=
                                                       cvt_s64_to_u64 ((cvt_u64_to_s64 omv_d) +
                                                                       (cvt_u64_to_s64 stv_d)) in
                                                     pair (trunc_u64_to_u32 (Data.Bits.shiftR z_d (fromInteger 32)))
                                                          (trunc_u64_to_u32 z_d)) else
                                          if (msbs5 == msbs5_AMO_AND) : bool
                                          then pair (omv_w1 Data.Bits..&.(**) stv_w1) (omv_w0 Data.Bits..&.(**)
                                                     stv_w0) else
                                          if (msbs5 == msbs5_AMO_OR) : bool
                                          then pair (omv_w1 Data.Bits..|.(**) stv_w1) (omv_w0 Data.Bits..|.(**)
                                                     stv_w0) else
                                          if (msbs5 == msbs5_AMO_XOR) : bool
                                          then pair (Data.Bits.xor omv_w1 stv_w1) (Data.Bits.xor omv_w0 stv_w0) else
                                          if (msbs5 == msbs5_AMO_MAX) : bool
                                          then (if (funct3 == funct3_AMO_W) : bool
                                                then let z_w :=
                                                       if ((cvt_u32_to_s32 omv_w0) > (cvt_u32_to_s32 stv_w0)) : bool
                                                       then omv_w0
                                                       else stv_w0 in
                                                     pair (fromInteger 0) z_w
                                                else if ((cvt_u64_to_s64 omv_d) > (cvt_u64_to_s64 stv_d)) : bool
                                                     then pair omv_w1 omv_w0
                                                     else pair stv_w1 stv_w0) else
                                          if (msbs5 == msbs5_AMO_MIN) : bool
                                          then (if (funct3 == funct3_AMO_W) : bool
                                                then let z_w :=
                                                       if ((cvt_u32_to_s32 omv_w0) < (cvt_u32_to_s32 stv_w0)) : bool
                                                       then omv_w0
                                                       else stv_w0 in
                                                     pair (fromInteger 0) z_w
                                                else if ((cvt_u64_to_s64 omv_d) < (cvt_u64_to_s64 stv_d)) : bool
                                                     then pair omv_w1 omv_w0
                                                     else pair stv_w1 stv_w0) else
                                          if (msbs5 == msbs5_AMO_MAXU) : bool
                                          then (if (funct3 == funct3_AMO_W) : bool
                                                then let z_w := if (omv_w0 > stv_w0) : bool then omv_w0 else stv_w0 in
                                                     pair (fromInteger 0) z_w
                                                else if (omv_d > stv_d) : bool
                                                     then pair omv_w1 omv_w0
                                                     else pair stv_w1 stv_w0) else
                                          if (msbs5 == msbs5_AMO_MINU) : bool
                                          then (if (funct3 == funct3_AMO_W) : bool
                                                then let z_w := if (omv_w0 < stv_w0) : bool then omv_w0 else stv_w0 in
                                                     pair (fromInteger 0) z_w
                                                else if (omv_d < stv_d) : bool
                                                     then pair omv_w1 omv_w0
                                                     else pair stv_w1 stv_w0) else
                                          patternFailure) in
              let nmv_d := bitconcat_u32_u32_to_u64 nmv_w1 nmv_w0 in
              let mmio' :=
                if (msbs5 == msbs5_AMO_LR) : bool then mmio else
                if (msbs5 == msbs5_AMO_SC) : bool then mmio else
                if (addr == addr_mtimecmp) : bool
                then let 'Mk_MMIO f_mtime_25__ f_mtimecmp_26__ f_mtip_27__ f_msip_28__
                        f_uart_29__ := mmio in
                     Mk_MMIO f_mtime_25__ nmv_d false f_msip_28__ f_uart_29__
                else if (addr == addr_msip) : bool
                     then let 'Mk_MMIO f_mtime_32__ f_mtimecmp_33__ f_mtip_34__ f_msip_35__
                             f_uart_36__ := mmio in
                          Mk_MMIO f_mtime_32__ f_mtimecmp_33__ f_mtip_34__ nmv_d f_uart_36__
                     else mmio in
              pair (Mem_Result_Ok ldv) mmio'.

Definition mmio_deq_console_output : MMIO -> (String * MMIO)%type :=
  fun mmio =>
    let uart := f_uart mmio in
    let 'pair s uart' := uart_deq_output uart in
    let mmio' :=
      if (s == GHC.Base.hs_string__ "") : bool
      then mmio
      else let 'Mk_MMIO f_mtime_2__ f_mtimecmp_3__ f_mtip_4__ f_msip_5__ f_uart_6__ :=
             mmio in
           Mk_MMIO f_mtime_2__ f_mtimecmp_3__ f_mtip_4__ f_msip_5__ uart' in
    pair s mmio'.

Definition mmio_enq_console_input : MMIO -> String -> MMIO :=
  fun mmio s =>
    let uart := f_uart mmio in
    let uart' := uart_enq_input uart s in
    let mmio' :=
      let 'Mk_MMIO f_mtime_2__ f_mtimecmp_3__ f_mtip_4__ f_msip_5__ f_uart_6__ :=
        mmio in
      Mk_MMIO f_mtime_2__ f_mtimecmp_3__ f_mtip_4__ f_msip_5__ uart' in
    mmio'.

Definition mmio_has_interrupts : MMIO -> (bool * bool * bool)%type :=
  fun mmio =>
    let sip := ((f_msip mmio) /= fromInteger 0) in
    let tip := f_mtip mmio in
    let eip := uart_has_interrupt (f_uart mmio) in pair (pair eip tip) sip.

Definition mmio_read : MMIO -> InstrField -> N -> (Mem_Result * MMIO)%type :=
  fun mmio funct3 addr =>
    if (addr == addr_mtime) : bool
    then let mtime := f_mtime mmio in pair (Mem_Result_Ok mtime) mmio
    else if (addr == addr_mtimecmp) : bool
         then let mtimecmp := f_mtimecmp mmio in pair (Mem_Result_Ok mtimecmp) mmio
         else if (addr == addr_msip) : bool
              then let msip := f_msip mmio in pair (Mem_Result_Ok msip) mmio
              else if (andb (addr_base_UART <= addr) (addr <
                             (addr_base_UART + addr_size_UART))) : bool
                   then let uart := f_uart mmio in
                        let 'pair v uart' := uart_read uart (addr - addr_base_UART) in
                        let v_u64 := zeroExtend_u8_to_u64 v in
                        let mmio' :=
                          let 'Mk_MMIO f_mtime_6__ f_mtimecmp_7__ f_mtip_8__ f_msip_9__ f_uart_10__ :=
                            mmio in
                          Mk_MMIO f_mtime_6__ f_mtimecmp_7__ f_mtip_8__ f_msip_9__ uart' in
                        pair (Mem_Result_Ok v_u64) mmio'
                   else pair (Mem_Result_Err exc_code_load_access_fault) mmio.

Definition mmio_read_mtime : MMIO -> N :=
  fun mmio => f_mtime mmio.

Definition mmio_tick_mtime : MMIO -> MMIO :=
  fun mmio =>
    let mtip := f_mtip mmio in
    let mtimecmp := f_mtimecmp mmio in
    let mtime := f_mtime mmio in
    let mtime' := mtime + fromInteger 1 in
    let mtip' := (orb mtip (mtime' >= mtimecmp)) in
    let mmio' :=
      let 'Mk_MMIO f_mtime_5__ f_mtimecmp_6__ f_mtip_7__ f_msip_8__ f_uart_9__ :=
        mmio in
      Mk_MMIO mtime' f_mtimecmp_6__ mtip' f_msip_8__ f_uart_9__ in
    mmio'.

Definition mmio_write
   : MMIO -> InstrField -> N -> N -> (Mem_Result * MMIO)%type :=
  GHC.DeferredFix.deferredFix4 (fun mmio_write mmio funct3 addr val =>
                                  if (addr == addr_mtimecmp) : bool
                                  then let mmio' :=
                                         let 'Mk_MMIO f_mtime_0__ f_mtimecmp_1__ f_mtip_2__ f_msip_3__ f_uart_4__ :=
                                           mmio in
                                         Mk_MMIO f_mtime_0__ val false f_msip_3__ f_uart_4__ in
                                       pair (Mem_Result_Ok (fromInteger 0)) mmio'
                                  else if (addr == addr_msip) : bool
                                       then let mmio' :=
                                              let 'Mk_MMIO f_mtime_8__ f_mtimecmp_9__ f_mtip_10__ f_msip_11__
                                                 f_uart_12__ := mmio in
                                              Mk_MMIO f_mtime_8__ f_mtimecmp_9__ f_mtip_10__ val f_uart_12__ in
                                            pair (Mem_Result_Ok (fromInteger 0)) mmio'
                                       else if (addr == addr_htif_console_out) : bool
                                            then mmio_write mmio funct3 (addr_base_UART + addr_UART_thr) val
                                            else if (andb (addr_base_UART <= addr) (addr <
                                                           (addr_base_UART + addr_size_UART))) : bool
                                                 then let uart := f_uart mmio in
                                                      let uart' := uart_write uart (addr - addr_base_UART) val in
                                                      let mmio' :=
                                                        let 'Mk_MMIO f_mtime_18__ f_mtimecmp_19__ f_mtip_20__
                                                           f_msip_21__ f_uart_22__ := mmio in
                                                        Mk_MMIO f_mtime_18__ f_mtimecmp_19__ f_mtip_20__ f_msip_21__
                                                                uart' in
                                                      pair (Mem_Result_Ok (fromInteger 0)) mmio'
                                                 else pair (Mem_Result_Err exc_code_store_AMO_access_fault) mmio).

(* External variables:
     InstrField Mem_Result Mem_Result_Err Mem_Result_Ok N String UART_NS16550A
     addr_UART_thr addr_base_UART addr_htif_console_out addr_msip addr_mtime
     addr_mtimecmp addr_size_UART andb bitconcat_u32_u32_to_u64 bool cvt_s32_to_u32
     cvt_s64_to_u64 cvt_u32_to_s32 cvt_u64_to_s64 exc_code_load_access_fault
     exc_code_store_AMO_access_fault exc_code_store_AMO_addr_misaligned false
     fromInteger funct3_AMO_D funct3_AMO_W is_AMO_aligned mkUART msbs5_AMO_ADD
     msbs5_AMO_AND msbs5_AMO_LR msbs5_AMO_MAX msbs5_AMO_MAXU msbs5_AMO_MIN
     msbs5_AMO_MINU msbs5_AMO_OR msbs5_AMO_SC msbs5_AMO_SWAP msbs5_AMO_XOR negb
     op_zeze__ op_zg__ op_zgze__ op_zl__ op_zlze__ op_zm__ op_zp__ op_zsze__ op_zt__
     orb pair patternFailure trunc_u64_to_u32 uart_all_input uart_all_output
     uart_deq_output uart_enq_input uart_has_interrupt uart_read uart_write
     zeroExtend_u8_to_u64 Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__
     Data.Bits.shiftR Data.Bits.xor GHC.DeferredFix.deferredFix4
*)

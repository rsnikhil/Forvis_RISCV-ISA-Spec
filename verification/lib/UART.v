(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Converted imports: *)

Require Import Coq.Init.Datatypes.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require Import GHC.Char.
Require Import GHC.Num.
Require Import GHC.Real.
Import Data.Bits.Notations.

(* Converted type declarations: *)

Inductive UART_NS16550A : Type
  := Mk_UART_NS16550A
   : String ->
     String ->
     String -> String -> N -> N -> N -> N -> N -> N -> N -> N -> N -> UART_NS16550A.

Definition f_dll (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ _ _ _ _ _ f_dll _ _ := arg_0__ in
  f_dll.

Definition f_dlm (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ _ _ _ _ _ _ f_dlm _ := arg_0__ in
  f_dlm.

Definition f_fcr (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ _ _ _ _ _ _ _ f_fcr := arg_0__ in
  f_fcr.

Definition f_ier (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ f_ier _ _ _ _ _ _ _ _ := arg_0__ in
  f_ier.

Definition f_lcr (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ f_lcr _ _ _ _ _ _ _ := arg_0__ in
  f_lcr.

Definition f_lsr (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ _ _ f_lsr _ _ _ _ _ := arg_0__ in
  f_lsr.

Definition f_mcr (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ _ f_mcr _ _ _ _ _ _ := arg_0__ in
  f_mcr.

Definition f_msr (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ _ _ _ f_msr _ _ _ _ := arg_0__ in
  f_msr.

Definition f_rbr_a (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A f_rbr_a _ _ _ _ _ _ _ _ _ _ _ _ := arg_0__ in
  f_rbr_a.

Definition f_rbr_b (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ f_rbr_b _ _ _ _ _ _ _ _ _ _ _ := arg_0__ in
  f_rbr_b.

Definition f_scr (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ _ _ _ _ _ _ f_scr _ _ _ := arg_0__ in
  f_scr.

Definition f_thr_a (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ f_thr_a _ _ _ _ _ _ _ _ _ _ := arg_0__ in
  f_thr_a.

Definition f_thr_b (arg_0__ : UART_NS16550A) :=
  let 'Mk_UART_NS16550A _ _ _ f_thr_b _ _ _ _ _ _ _ _ _ := arg_0__ in
  f_thr_b.
(* Midamble *)

Require Import GHC.Err.

Instance UART_NS16550A_Default : Default UART_NS16550A :=
  Build_Default _ (Mk_UART_NS16550A default default default default default default default default default default default default default).


(* Converted value declarations: *)

Definition addr_UART_dll :=
  fromInteger 0 : N.

Definition addr_UART_dlm :=
  fromInteger 8 : N.

Definition addr_UART_fcr :=
  fromInteger 16 : N.

Definition addr_UART_ier :=
  fromInteger 8 : N.

Definition addr_UART_iir :=
  fromInteger 16 : N.

Definition addr_UART_lcr :=
  fromInteger 24 : N.

Definition addr_UART_lsr :=
  fromInteger 40 : N.

Definition addr_UART_mcr :=
  fromInteger 32 : N.

Definition addr_UART_msr :=
  fromInteger 48 : N.

Definition addr_UART_rbr :=
  fromInteger 0 : N.

Definition addr_UART_scr :=
  fromInteger 56 : N.

Definition addr_UART_thr :=
  fromInteger 0 : N.

Definition uart_all_input : UART_NS16550A -> (String * String)%type :=
  fun uart => pair (f_rbr_a uart) (f_rbr_b uart).

Definition uart_all_output : UART_NS16550A -> (String * String)%type :=
  fun uart => pair (f_thr_a uart) (f_thr_b uart).

Definition uart_deq_output : UART_NS16550A -> (String * UART_NS16550A)%type :=
  fun uart =>
    let thr_b := f_thr_b uart in
    match thr_b with
    | nil => pair (GHC.Base.hs_string__ "") uart
    | cs =>
        pair thr_b (let 'Mk_UART_NS16550A f_rbr_a_2__ f_rbr_b_3__ f_thr_a_4__
                       f_thr_b_5__ f_ier_6__ f_lcr_7__ f_mcr_8__ f_lsr_9__ f_msr_10__ f_scr_11__
                       f_dll_12__ f_dlm_13__ f_fcr_14__ := uart in
              Mk_UART_NS16550A f_rbr_a_2__ f_rbr_b_3__ (app (f_thr_a uart) thr_b)
                               (GHC.Base.hs_string__ "") f_ier_6__ f_lcr_7__ f_mcr_8__ f_lsr_9__ f_msr_10__
                               f_scr_11__ f_dll_12__ f_dlm_13__ f_fcr_14__)
    end.

Definition uart_ier_edssi :=
  fromInteger 8 : N.

Definition uart_ier_elsi :=
  fromInteger 4 : N.

Definition uart_ier_erbfi :=
  fromInteger 1 : N.

Definition uart_ier_etbei :=
  fromInteger 2 : N.

Definition uart_iir_cti :=
  fromInteger 12 : N.

Definition uart_iir_ms :=
  fromInteger 0 : N.

Definition uart_iir_none :=
  fromInteger 1 : N.

Definition uart_iir_rda :=
  fromInteger 4 : N.

Definition uart_iir_rls :=
  fromInteger 6 : N.

Definition uart_iir_thre :=
  fromInteger 2 : N.

Definition uart_lcr_bc :=
  fromInteger 64 : N.

Definition uart_lcr_dlab :=
  fromInteger 128 : N.

Definition uart_write : UART_NS16550A -> N -> N -> UART_NS16550A :=
  fun uart addr_offset val =>
    if (andb (addr_offset == addr_UART_thr) (((f_lcr uart) Data.Bits..&.(**)
               uart_lcr_dlab) ==
              fromInteger 0)) : bool
    then let thr_b := f_thr_b uart in
         let char := chr (fromIntegral val) in
         let uart' :=
           let 'Mk_UART_NS16550A f_rbr_a_2__ f_rbr_b_3__ f_thr_a_4__ f_thr_b_5__ f_ier_6__
              f_lcr_7__ f_mcr_8__ f_lsr_9__ f_msr_10__ f_scr_11__ f_dll_12__ f_dlm_13__
              f_fcr_14__ := uart in
           Mk_UART_NS16550A f_rbr_a_2__ f_rbr_b_3__ f_thr_a_4__ (app thr_b (cons char nil))
                            f_ier_6__ f_lcr_7__ f_mcr_8__ f_lsr_9__ f_msr_10__ f_scr_11__ f_dll_12__
                            f_dlm_13__ f_fcr_14__ in
         uart'
    else if (andb (addr_offset == addr_UART_ier) (((f_lcr uart) Data.Bits..&.(**)
                    uart_lcr_dlab) ==
                   fromInteger 0)) : bool
         then let 'Mk_UART_NS16550A f_rbr_a_18__ f_rbr_b_19__ f_thr_a_20__ f_thr_b_21__
                 f_ier_22__ f_lcr_23__ f_mcr_24__ f_lsr_25__ f_msr_26__ f_scr_27__ f_dll_28__
                 f_dlm_29__ f_fcr_30__ := uart in
              Mk_UART_NS16550A f_rbr_a_18__ f_rbr_b_19__ f_thr_a_20__ f_thr_b_21__
                               (fromIntegral (val Data.Bits..&.(**) fromInteger 255)) f_lcr_23__ f_mcr_24__
                               f_lsr_25__ f_msr_26__ f_scr_27__ f_dll_28__ f_dlm_29__ f_fcr_30__
         else if (addr_offset == addr_UART_fcr) : bool
              then let 'Mk_UART_NS16550A f_rbr_a_33__ f_rbr_b_34__ f_thr_a_35__ f_thr_b_36__
                      f_ier_37__ f_lcr_38__ f_mcr_39__ f_lsr_40__ f_msr_41__ f_scr_42__ f_dll_43__
                      f_dlm_44__ f_fcr_45__ := uart in
                   Mk_UART_NS16550A f_rbr_a_33__ f_rbr_b_34__ f_thr_a_35__ f_thr_b_36__ f_ier_37__
                                    f_lcr_38__ f_mcr_39__ f_lsr_40__ f_msr_41__ f_scr_42__ f_dll_43__ f_dlm_44__
                                    (fromIntegral (val Data.Bits..&.(**) fromInteger 255))
              else if (addr_offset == addr_UART_lcr) : bool
                   then let 'Mk_UART_NS16550A f_rbr_a_48__ f_rbr_b_49__ f_thr_a_50__ f_thr_b_51__
                           f_ier_52__ f_lcr_53__ f_mcr_54__ f_lsr_55__ f_msr_56__ f_scr_57__ f_dll_58__
                           f_dlm_59__ f_fcr_60__ := uart in
                        Mk_UART_NS16550A f_rbr_a_48__ f_rbr_b_49__ f_thr_a_50__ f_thr_b_51__ f_ier_52__
                                         (fromIntegral (val Data.Bits..&.(**) fromInteger 255)) f_mcr_54__ f_lsr_55__
                                         f_msr_56__ f_scr_57__ f_dll_58__ f_dlm_59__ f_fcr_60__
                   else if (addr_offset == addr_UART_mcr) : bool
                        then let 'Mk_UART_NS16550A f_rbr_a_63__ f_rbr_b_64__ f_thr_a_65__ f_thr_b_66__
                                f_ier_67__ f_lcr_68__ f_mcr_69__ f_lsr_70__ f_msr_71__ f_scr_72__ f_dll_73__
                                f_dlm_74__ f_fcr_75__ := uart in
                             Mk_UART_NS16550A f_rbr_a_63__ f_rbr_b_64__ f_thr_a_65__ f_thr_b_66__ f_ier_67__
                                              f_lcr_68__ (fromIntegral (val Data.Bits..&.(**) fromInteger 255))
                                              f_lsr_70__ f_msr_71__ f_scr_72__ f_dll_73__ f_dlm_74__ f_fcr_75__
                        else if (addr_offset == addr_UART_lsr) : bool
                             then uart
                             else if (addr_offset == addr_UART_msr) : bool
                                  then uart
                                  else if (addr_offset == addr_UART_scr) : bool
                                       then let 'Mk_UART_NS16550A f_rbr_a_78__ f_rbr_b_79__ f_thr_a_80__ f_thr_b_81__
                                               f_ier_82__ f_lcr_83__ f_mcr_84__ f_lsr_85__ f_msr_86__ f_scr_87__
                                               f_dll_88__ f_dlm_89__ f_fcr_90__ := uart in
                                            Mk_UART_NS16550A f_rbr_a_78__ f_rbr_b_79__ f_thr_a_80__ f_thr_b_81__
                                                             f_ier_82__ f_lcr_83__ f_mcr_84__ f_lsr_85__ f_msr_86__
                                                             (fromIntegral (val Data.Bits..&.(**) fromInteger 255))
                                                             f_dll_88__ f_dlm_89__ f_fcr_90__
                                       else if (andb (addr_offset == addr_UART_dll) (((f_lcr uart) Data.Bits..&.(**)
                                                       uart_lcr_dlab) /=
                                                      fromInteger 0)) : bool
                                            then let 'Mk_UART_NS16550A f_rbr_a_93__ f_rbr_b_94__ f_thr_a_95__
                                                    f_thr_b_96__ f_ier_97__ f_lcr_98__ f_mcr_99__ f_lsr_100__
                                                    f_msr_101__ f_scr_102__ f_dll_103__ f_dlm_104__ f_fcr_105__ :=
                                                   uart in
                                                 Mk_UART_NS16550A f_rbr_a_93__ f_rbr_b_94__ f_thr_a_95__ f_thr_b_96__
                                                                  f_ier_97__ f_lcr_98__ f_mcr_99__ f_lsr_100__
                                                                  f_msr_101__ f_scr_102__ (fromIntegral (val
                                                                                                         Data.Bits..&.(**)
                                                                                                         fromInteger
                                                                                                         255))
                                                                  f_dlm_104__ f_fcr_105__
                                            else if (andb (addr_offset == addr_UART_dlm) (((f_lcr uart)
                                                            Data.Bits..&.(**)
                                                            uart_lcr_dlab) /=
                                                           fromInteger 0)) : bool
                                                 then let 'Mk_UART_NS16550A f_rbr_a_108__ f_rbr_b_109__ f_thr_a_110__
                                                         f_thr_b_111__ f_ier_112__ f_lcr_113__ f_mcr_114__ f_lsr_115__
                                                         f_msr_116__ f_scr_117__ f_dll_118__ f_dlm_119__ f_fcr_120__ :=
                                                        uart in
                                                      Mk_UART_NS16550A f_rbr_a_108__ f_rbr_b_109__ f_thr_a_110__
                                                                       f_thr_b_111__ f_ier_112__ f_lcr_113__ f_mcr_114__
                                                                       f_lsr_115__ f_msr_116__ f_scr_117__ f_dll_118__
                                                                       (fromIntegral (val Data.Bits..&.(**)
                                                                                      fromInteger 255)) f_fcr_120__
                                                 else uart.

Definition uart_lcr_eps :=
  fromInteger 16 : N.

Definition uart_lcr_pen :=
  fromInteger 8 : N.

Definition uart_lcr_sp :=
  fromInteger 32 : N.

Definition uart_lcr_stb :=
  fromInteger 4 : N.

Definition uart_lcr_wls :=
  fromInteger 3 : N.

Definition uart_lsr_bi :=
  fromInteger 16 : N.

Definition uart_lsr_dr :=
  fromInteger 1 : N.

Definition uart_read_iir : UART_NS16550A -> N :=
  fun uart =>
    let lsr := f_lsr uart in
    let ier := f_ier uart in
    let iir :=
      if (andb ((ier Data.Bits..&.(**) uart_ier_erbfi) /= fromInteger 0) ((lsr
                 Data.Bits..&.(**)
                 uart_lsr_dr) /=
                fromInteger 0)) : bool
      then uart_iir_rda
      else if ((ier Data.Bits..&.(**) uart_ier_etbei) /= fromInteger 0) : bool
           then uart_iir_thre
           else fromInteger 0 in
    iir.

Definition uart_has_interrupt : UART_NS16550A -> bool :=
  fun uart =>
    let iir := uart_read_iir uart in
    let eip := ((iir Data.Bits..&.(**) uart_iir_none) == fromInteger 0) in eip.

Definition uart_enq_input : UART_NS16550A -> String -> UART_NS16550A :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | uart, nil => uart
    | uart, s =>
        let lsr := f_lsr uart in
        let lsr' := (lsr Data.Bits..|.(**) uart_lsr_dr) in
        let rbr_b := f_rbr_b uart in
        let rbr_b' := app rbr_b s in
        let uart' :=
          let 'Mk_UART_NS16550A f_rbr_a_6__ f_rbr_b_7__ f_thr_a_8__ f_thr_b_9__ f_ier_10__
             f_lcr_11__ f_mcr_12__ f_lsr_13__ f_msr_14__ f_scr_15__ f_dll_16__ f_dlm_17__
             f_fcr_18__ := uart in
          Mk_UART_NS16550A f_rbr_a_6__ rbr_b' f_thr_a_8__ f_thr_b_9__ f_ier_10__
                           f_lcr_11__ f_mcr_12__ lsr' f_msr_14__ f_scr_15__ f_dll_16__ f_dlm_17__
                           f_fcr_18__ in
        uart'
    end.

Definition uart_read : UART_NS16550A -> N -> (N * UART_NS16550A)%type :=
  fun uart addr_offset =>
    if (andb (addr_offset == addr_UART_rbr) (((f_lcr uart) Data.Bits..&.(**)
               uart_lcr_dlab) ==
              fromInteger 0)) : bool
    then let lsr := f_lsr uart in
         let rbr_b := f_rbr_b uart in
         let rbr_a := f_rbr_a uart in
         match (rbr_b) with
         | nil => pair (fromInteger 0) uart
         | cons c cs =>
             (let rbr_b' := cs in
              let lsr' :=
                if (rbr_b' == GHC.Base.hs_string__ "") : bool
                then (lsr Data.Bits..&.(**) Data.Bits.complement uart_lsr_dr)
                else (lsr Data.Bits..|.(**) uart_lsr_dr) in
              let rbr_a' := app rbr_a (cons c nil) in
              let uart' :=
                let 'Mk_UART_NS16550A f_rbr_a_7__ f_rbr_b_8__ f_thr_a_9__ f_thr_b_10__
                   f_ier_11__ f_lcr_12__ f_mcr_13__ f_lsr_14__ f_msr_15__ f_scr_16__ f_dll_17__
                   f_dlm_18__ f_fcr_19__ := uart in
                Mk_UART_NS16550A rbr_a' rbr_b' f_thr_a_9__ f_thr_b_10__ f_ier_11__ f_lcr_12__
                                 f_mcr_13__ lsr' f_msr_15__ f_scr_16__ f_dll_17__ f_dlm_18__ f_fcr_19__ in
              let v := fromIntegral (ord c) in pair v uart')
         end
    else if (andb (addr_offset == addr_UART_ier) (((f_lcr uart) Data.Bits..&.(**)
                    uart_lcr_dlab) ==
                   fromInteger 0)) : bool
         then pair (f_ier uart) uart
         else if (addr_offset == addr_UART_iir) : bool
              then pair (uart_read_iir uart) uart
              else if (addr_offset == addr_UART_lcr) : bool
                   then pair (f_lcr uart) uart
                   else if (addr_offset == addr_UART_mcr) : bool
                        then pair (f_mcr uart) uart
                        else if (addr_offset == addr_UART_lsr) : bool
                             then pair (f_lsr uart) uart
                             else if (addr_offset == addr_UART_msr) : bool
                                  then pair (f_msr uart) uart
                                  else if (addr_offset == addr_UART_scr) : bool
                                       then pair (f_scr uart) uart
                                       else if (andb (addr_offset == addr_UART_dll) (((f_lcr uart) Data.Bits..&.(**)
                                                       uart_lcr_dlab) /=
                                                      fromInteger 0)) : bool
                                            then pair (f_dll uart) uart
                                            else if (andb (addr_offset == addr_UART_dlm) (((f_lcr uart)
                                                            Data.Bits..&.(**)
                                                            uart_lcr_dlab) /=
                                                           fromInteger 0)) : bool
                                                 then pair (f_dlm uart) uart
                                                 else pair (fromInteger 255) uart.

Definition uart_lsr_fe :=
  fromInteger 8 : N.

Definition uart_lsr_oe :=
  fromInteger 2 : N.

Definition uart_lsr_pe :=
  fromInteger 4 : N.

Definition uart_lsr_rxfe :=
  fromInteger 128 : N.

Definition uart_lsr_temt :=
  fromInteger 64 : N.

Definition uart_lsr_thre :=
  fromInteger 32 : N.

Definition mkUART : UART_NS16550A :=
  Mk_UART_NS16550A (GHC.Base.hs_string__ "") (GHC.Base.hs_string__ "")
                   (GHC.Base.hs_string__ "") (GHC.Base.hs_string__ "") (fromInteger 0) (fromInteger
                    0) (fromInteger 0) (uart_lsr_temt Data.Bits..|.(**) uart_lsr_thre) (fromInteger
                    0) (fromInteger 0) (fromInteger 0) (fromInteger 0) (fromInteger 0).

(* External variables:
     N String andb app bool chr cons fromInteger fromIntegral nil op_zeze__ op_zsze__
     op_zt__ ord pair Data.Bits.complement Data.Bits.op_zizazi__
     Data.Bits.op_zizbzi__
*)

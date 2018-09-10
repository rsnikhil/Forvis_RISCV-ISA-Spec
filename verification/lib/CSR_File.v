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
Require Import Bit_Manipulation.
Require Import Coq.Init.Datatypes.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Data.Foldable.
Require Data.Map.Internal.
Require Data.Maybe.
Require Import GHC.Base.
Require GHC.DeferredFix.
Require Import GHC.Err.
Require GHC.List.
Require Import GHC.Num.
Require IO.
Require Numeric.
Import Data.Bits.Notations.

(* Converted type declarations: *)

Inductive CSR_Permission : Type
  := CSR_Permission_None : CSR_Permission
  |  CSR_Permission_RO : CSR_Permission
  |  CSR_Permission_RW : CSR_Permission.

Inductive CSR_File : Type
  := Mk_CSR_File : (Data.Map.Internal.Map CSR_Addr N) -> CSR_File.

Instance Default__CSR_Permission : Default CSR_Permission :=
  Build_Default _ CSR_Permission_None.
(* Converted value declarations: *)

(* Skipping instance Show__CSR_Permission of class Show *)

Local Definition Eq___CSR_Permission_op_zeze__
   : CSR_Permission -> CSR_Permission -> bool :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | CSR_Permission_None, CSR_Permission_None => true
    | CSR_Permission_RO, CSR_Permission_RO => true
    | CSR_Permission_RW, CSR_Permission_RW => true
    | _, _ => false
    end.

Local Definition Eq___CSR_Permission_op_zsze__
   : CSR_Permission -> CSR_Permission -> bool :=
  fun x y => negb (Eq___CSR_Permission_op_zeze__ x y).

Program Instance Eq___CSR_Permission : Eq_ CSR_Permission :=
  fun _ k =>
    k {| op_zeze____ := Eq___CSR_Permission_op_zeze__ ;
         op_zsze____ := Eq___CSR_Permission_op_zsze__ |}.

Definition csr_permission
   : CSR_File -> Priv_Level -> CSR_Addr -> CSR_Permission :=
  fun arg_0__ arg_1__ arg_2__ =>
    match arg_0__, arg_1__, arg_2__ with
    | Mk_CSR_File dm, priv, csr_addr =>
        let addr_11_10 := bitSlice csr_addr (fromInteger 11) (fromInteger 10) in
        let mstatus :=
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr_mstatus
                                                dm) in
        let tvm_fault :=
          (andb (csr_addr == csr_addr_satp) (Data.Bits.testBit mstatus
                 mstatus_tvm_bitpos)) in
        let addr_9_8 := bitSlice csr_addr (fromInteger 9) (fromInteger 8) in
        let priv_ok := priv >= addr_9_8 in
        let exists_ :=
          (orb (csr_addr == csr_addr_sstatus) (orb (csr_addr == csr_addr_ustatus) (orb
                                                    (csr_addr == csr_addr_sie) (orb (csr_addr == csr_addr_uie) (orb
                                                                                     (csr_addr == csr_addr_sip) (orb
                                                                                      (csr_addr == csr_addr_uip)
                                                                                      (Data.Map.Internal.member csr_addr
                                                                                       dm))))))) in
        if orb (negb exists_) (orb (negb priv_ok) tvm_fault) : bool
        then CSR_Permission_None
        else if (addr_11_10 == fromInteger 3) : bool
             then CSR_Permission_RO
             else CSR_Permission_RW
    end.

Definition csr_read : RV -> CSR_File -> CSR_Addr -> N :=
  fun arg_0__ arg_1__ arg_2__ =>
    match arg_0__, arg_1__, arg_2__ with
    | rv, Mk_CSR_File dm, csr_addr =>
        let sstatus_mask :=
          if (rv == RV32) : bool
          then sstatus_mask_RV32
          else sstatus_mask_RV64 in
        let ustatus_mask :=
          if (rv == RV32) : bool
          then ustatus_mask_RV32
          else ustatus_mask_RV64 in
        let mie :=
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr_mie
                                                dm) in
        let mip :=
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr_mip
                                                dm) in
        let mstatus :=
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr_mstatus
                                                dm) in
        let val :=
          if (csr_addr == csr_addr_ustatus) : bool
          then (mstatus Data.Bits..&.(**) ustatus_mask) else
          if (csr_addr == csr_addr_uip) : bool then (mip Data.Bits..&.(**) uip_mask) else
          if (csr_addr == csr_addr_uie) : bool then (mie Data.Bits..&.(**) uip_mask) else
          if (csr_addr == csr_addr_cycle) : bool
          then Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup
                                                     csr_addr_mcycle dm) else
          if (csr_addr == csr_addr_cycleh) : bool
          then (Data.Bits.shiftR (Data.Maybe.fromMaybe (fromInteger 0)
                                  (Data.Map.Internal.lookup csr_addr_mcycle dm)) (fromInteger 32)) else
          if (csr_addr == csr_addr_instret) : bool
          then Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup
                                                     csr_addr_minstret dm) else
          if (csr_addr == csr_addr_instreth) : bool
          then (Data.Bits.shiftR (Data.Maybe.fromMaybe (fromInteger 0)
                                  (Data.Map.Internal.lookup csr_addr_minstret dm)) (fromInteger 32)) else
          if (csr_addr == csr_addr_sstatus) : bool
          then (mstatus Data.Bits..&.(**) sstatus_mask) else
          if (csr_addr == csr_addr_sip) : bool then (mip Data.Bits..&.(**) sip_mask) else
          if (csr_addr == csr_addr_sie) : bool then (mie Data.Bits..&.(**) sip_mask) else
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr dm) in
        val
    end.

Definition print_CSR_File : String -> RV -> CSR_File -> IO.IO unit :=
  fun indent rv csr_file =>
    let unflatten {t} : Int -> list t -> list (list t) :=
      GHC.DeferredFix.deferredFix2 (fun unflatten n xs =>
                                      if Data.Foldable.length xs >= n : bool
                                      then cons (GHC.List.take n xs) (unflatten n (GHC.List.drop n xs)) else
                                      cons xs nil) in
    let print_csr :=
      fun '(pair csr_addr csr_name) =>
        (let csr_val := csr_read rv csr_file csr_addr in
         IO.putStr (app indent (app csr_name (app (GHC.Base.hs_string__ ":")
                                                  (Numeric.showHex csr_val (GHC.Base.hs_string__ "")))))) in
    let n := fromInteger 5 in
    let print_n_csrs :=
      fun arg_7__ =>
        match arg_7__ with
        | nil => return_ tt
        | xs =>
            Data.Foldable.mapM_ print_csr xs >> IO.putStrLn (GHC.Base.hs_string__ "")
        end in
    Data.Foldable.mapM_ print_n_csrs (unflatten n m_csr_addrs_and_names) >>
    (Data.Foldable.mapM_ print_n_csrs (unflatten n s_csr_addrs_and_names) >>
     Data.Foldable.mapM_ print_n_csrs (unflatten n u_csr_addrs_and_names)).

Definition csr_write : RV -> CSR_File -> CSR_Addr -> N -> CSR_File :=
  fun arg_0__ arg_1__ arg_2__ arg_3__ =>
    match arg_0__, arg_1__, arg_2__, arg_3__ with
    | rv, Mk_CSR_File dm, csr_addr, value =>
        let ustatus_mask :=
          if (rv == RV32) : bool
          then ustatus_mask_RV32
          else ustatus_mask_RV64 in
        let sstatus_mask :=
          if (rv == RV32) : bool
          then sstatus_mask_RV32
          else sstatus_mask_RV64 in
        let mstatus_mask :=
          if (rv == RV32) : bool
          then mstatus_mask_RV32
          else mstatus_mask_RV64 in
        let mie :=
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr_mie
                                                dm) in
        let mip :=
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr_mip
                                                dm) in
        let mstatus :=
          Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup csr_addr_mstatus
                                                dm) in
        let 'pair csr_addr' value' := (if (csr_addr == csr_addr_mstatus) : bool
                                       then pair csr_addr_mstatus ((mstatus Data.Bits..&.(**)
                                                   (Data.Bits.complement mstatus_mask)) Data.Bits..|.(**)
                                                  (value Data.Bits..&.(**) mstatus_mask)) else
                                       if (csr_addr == csr_addr_sstatus) : bool
                                       then pair csr_addr_mstatus ((mstatus Data.Bits..&.(**)
                                                   (Data.Bits.complement sstatus_mask)) Data.Bits..|.(**)
                                                  (value Data.Bits..&.(**) sstatus_mask)) else
                                       if (csr_addr == csr_addr_ustatus) : bool
                                       then pair csr_addr_mstatus ((mstatus Data.Bits..&.(**)
                                                   (Data.Bits.complement ustatus_mask)) Data.Bits..|.(**)
                                                  (value Data.Bits..&.(**) ustatus_mask)) else
                                       if (csr_addr == csr_addr_sip) : bool
                                       then pair csr_addr_mip ((mip Data.Bits..&.(**) (Data.Bits.complement sip_mask))
                                                  Data.Bits..|.(**)
                                                  (value Data.Bits..&.(**) sip_mask)) else
                                       if (csr_addr == csr_addr_uip) : bool
                                       then pair csr_addr_mip ((mip Data.Bits..&.(**) (Data.Bits.complement uip_mask))
                                                  Data.Bits..|.(**)
                                                  (value Data.Bits..&.(**) uip_mask)) else
                                       if (csr_addr == csr_addr_sie) : bool
                                       then pair csr_addr_mie ((mie Data.Bits..&.(**) (Data.Bits.complement sip_mask))
                                                  Data.Bits..|.(**)
                                                  (value Data.Bits..&.(**) sip_mask)) else
                                       if (csr_addr == csr_addr_uie) : bool
                                       then pair csr_addr_mie ((mie Data.Bits..&.(**) (Data.Bits.complement uip_mask))
                                                  Data.Bits..|.(**)
                                                  (value Data.Bits..&.(**) uip_mask)) else
                                       if (csr_addr == csr_addr_mcounteren) : bool
                                       then pair csr_addr (value Data.Bits..&.(**) fromInteger 7) else
                                       if (csr_addr == csr_addr_scounteren) : bool
                                       then pair csr_addr (value Data.Bits..&.(**) fromInteger 7) else
                                       pair csr_addr value) in
        let dm' :=
          if (Data.Map.Internal.member csr_addr' dm) : bool
          then Data.Map.Internal.insert csr_addr' value' dm
          else dm in
        Mk_CSR_File dm'
    end.

Definition m_csr_reset_values : RV -> list (CSR_Addr * N)%type :=
  fun rv =>
    cons (pair csr_addr_mvendorid (fromInteger 0)) (cons (pair csr_addr_marchid
                                                               (fromInteger 0)) (cons (pair csr_addr_mimpid (fromInteger
                                                                                             0)) (cons (pair
                                                                                                        csr_addr_mhartid
                                                                                                        (fromInteger 0))
                                                                                                       (cons (pair
                                                                                                              csr_addr_mstatus
                                                                                                              (if (rv ==
                                                                                                                   RV32) : bool
                                                                                                               then fromInteger
                                                                                                                    0
                                                                                                               else ((Data.Bits.shiftL
                                                                                                                      xl_rv64
                                                                                                                      mstatus_sxl_bitpos)
                                                                                                                     Data.Bits..|.(**)
                                                                                                                     (Data.Bits.shiftL
                                                                                                                      xl_rv64
                                                                                                                      mstatus_uxl_bitpos))))
                                                                                                             (cons (pair
                                                                                                                    csr_addr_misa
                                                                                                                    (let msbs :=
                                                                                                                       if (rv
                                                                                                                           ==
                                                                                                                           RV32) : bool
                                                                                                                       then (Data.Bits.shiftL
                                                                                                                             xl_rv32
                                                                                                                             misa_MXL_bitpos_RV32) else
                                                                                                                       if (rv
                                                                                                                           ==
                                                                                                                           RV64) : bool
                                                                                                                       then (Data.Bits.shiftL
                                                                                                                             xl_rv64
                                                                                                                             misa_MXL_bitpos_RV64) else
                                                                                                                       patternFailure in
                                                                                                                     let lsbs :=
                                                                                                                       ((((((Data.Bits.shiftL
                                                                                                                             (fromInteger
                                                                                                                              1)
                                                                                                                             misa_A_bitpos)
                                                                                                                            Data.Bits..|.(**)
                                                                                                                            (Data.Bits.shiftL
                                                                                                                             (fromInteger
                                                                                                                              1)
                                                                                                                             misa_I_bitpos))
                                                                                                                           Data.Bits..|.(**)
                                                                                                                           (Data.Bits.shiftL
                                                                                                                            (fromInteger
                                                                                                                             1)
                                                                                                                            misa_M_bitpos))
                                                                                                                          Data.Bits..|.(**)
                                                                                                                          (Data.Bits.shiftL
                                                                                                                           (fromInteger
                                                                                                                            1)
                                                                                                                           misa_N_bitpos))
                                                                                                                         Data.Bits..|.(**)
                                                                                                                         (Data.Bits.shiftL
                                                                                                                          (fromInteger
                                                                                                                           1)
                                                                                                                          misa_S_bitpos))
                                                                                                                        Data.Bits..|.(**)
                                                                                                                        (Data.Bits.shiftL
                                                                                                                         (fromInteger
                                                                                                                          1)
                                                                                                                         misa_U_bitpos)) in
                                                                                                                     (msbs
                                                                                                                      Data.Bits..|.(**)
                                                                                                                      lsbs)))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     csr_addr_medeleg
                                                                                                                     (fromInteger
                                                                                                                      0))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      csr_addr_mideleg
                                                                                                                      (fromInteger
                                                                                                                       0))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       csr_addr_mie
                                                                                                                       (fromInteger
                                                                                                                        0))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        csr_addr_mtvec
                                                                                                                        (fromInteger
                                                                                                                         0))
                                                                                                                       (cons
                                                                                                                        (pair
                                                                                                                         csr_addr_mcounteren
                                                                                                                         (fromInteger
                                                                                                                          0))
                                                                                                                        (cons
                                                                                                                         (pair
                                                                                                                          csr_addr_mscratch
                                                                                                                          (fromInteger
                                                                                                                           0))
                                                                                                                         (cons
                                                                                                                          (pair
                                                                                                                           csr_addr_mepc
                                                                                                                           (fromInteger
                                                                                                                            0))
                                                                                                                          (cons
                                                                                                                           (pair
                                                                                                                            csr_addr_mcause
                                                                                                                            (fromInteger
                                                                                                                             0))
                                                                                                                           (cons
                                                                                                                            (pair
                                                                                                                             csr_addr_mtval
                                                                                                                             (fromInteger
                                                                                                                              0))
                                                                                                                            (cons
                                                                                                                             (pair
                                                                                                                              csr_addr_mip
                                                                                                                              (fromInteger
                                                                                                                               0))
                                                                                                                             (cons
                                                                                                                              (pair
                                                                                                                               csr_addr_mcycle
                                                                                                                               (fromInteger
                                                                                                                                0))
                                                                                                                              (cons
                                                                                                                               (pair
                                                                                                                                csr_addr_minstret
                                                                                                                                (fromInteger
                                                                                                                                 0))
                                                                                                                               (cons
                                                                                                                                (pair
                                                                                                                                 csr_addr_mcycleh
                                                                                                                                 (fromInteger
                                                                                                                                  0))
                                                                                                                                (cons
                                                                                                                                 (pair
                                                                                                                                  csr_addr_minstreth
                                                                                                                                  (fromInteger
                                                                                                                                   0))
                                                                                                                                 (cons
                                                                                                                                  (pair
                                                                                                                                   csr_addr_tselect
                                                                                                                                   (fromInteger
                                                                                                                                    0))
                                                                                                                                  (cons
                                                                                                                                   (pair
                                                                                                                                    csr_addr_data1
                                                                                                                                    (fromInteger
                                                                                                                                     0))
                                                                                                                                   (cons
                                                                                                                                    (pair
                                                                                                                                     csr_addr_data2
                                                                                                                                     (fromInteger
                                                                                                                                      0))
                                                                                                                                    (cons
                                                                                                                                     (pair
                                                                                                                                      csr_addr_data3
                                                                                                                                      (fromInteger
                                                                                                                                       0))
                                                                                                                                     (cons
                                                                                                                                      (pair
                                                                                                                                       csr_addr_dcsr
                                                                                                                                       (fromInteger
                                                                                                                                        0))
                                                                                                                                      (cons
                                                                                                                                       (pair
                                                                                                                                        csr_addr_dpc
                                                                                                                                        (fromInteger
                                                                                                                                         0))
                                                                                                                                       (cons
                                                                                                                                        (pair
                                                                                                                                         csr_addr_dscratch
                                                                                                                                         (fromInteger
                                                                                                                                          0))
                                                                                                                                        nil)))))))))))))))))))))))))).

Definition s_csr_reset_values : RV -> list (CSR_Addr * N)%type :=
  fun rv =>
    cons (pair csr_addr_sedeleg (fromInteger 0)) (cons (pair csr_addr_sideleg
                                                             (fromInteger 0)) (cons (pair csr_addr_stvec (fromInteger
                                                                                           0)) (cons (pair
                                                                                                      csr_addr_scounteren
                                                                                                      (fromInteger 0))
                                                                                                     (cons (pair
                                                                                                            csr_addr_sscratch
                                                                                                            (fromInteger
                                                                                                             0)) (cons
                                                                                                            (pair
                                                                                                             csr_addr_sepc
                                                                                                             (fromInteger
                                                                                                              0)) (cons
                                                                                                             (pair
                                                                                                              csr_addr_scause
                                                                                                              (fromInteger
                                                                                                               0)) (cons
                                                                                                              (pair
                                                                                                               csr_addr_stval
                                                                                                               (fromInteger
                                                                                                                0))
                                                                                                              (cons
                                                                                                               (pair
                                                                                                                csr_addr_satp
                                                                                                                (fromInteger
                                                                                                                 0))
                                                                                                               nil)))))))).

Definition u_csr_reset_values : RV -> list (CSR_Addr * N)%type :=
  fun rv =>
    cons (pair csr_addr_utvec (fromInteger 0)) (cons (pair csr_addr_uscratch
                                                           (fromInteger 0)) (cons (pair csr_addr_uepc (fromInteger 0))
                                                                                  (cons (pair csr_addr_ucause
                                                                                              (fromInteger 0)) (cons
                                                                                         (pair csr_addr_utval
                                                                                               (fromInteger 0)) (cons
                                                                                          (pair csr_addr_fflags
                                                                                                (fromInteger 0)) (cons
                                                                                           (pair csr_addr_frm
                                                                                                 (fromInteger 0)) (cons
                                                                                            (pair csr_addr_fcsr
                                                                                                  (fromInteger 0)) (cons
                                                                                             (pair csr_addr_cycle
                                                                                                   (fromInteger 0))
                                                                                             (cons (pair csr_addr_time
                                                                                                         (fromInteger
                                                                                                          0)) (cons
                                                                                                    (pair
                                                                                                     csr_addr_instret
                                                                                                     (fromInteger 0))
                                                                                                    (cons (pair
                                                                                                           csr_addr_cycleh
                                                                                                           (fromInteger
                                                                                                            0)) (cons
                                                                                                           (pair
                                                                                                            csr_addr_timeh
                                                                                                            (fromInteger
                                                                                                             0)) (cons
                                                                                                            (pair
                                                                                                             csr_addr_instreth
                                                                                                             (fromInteger
                                                                                                              0))
                                                                                                            nil))))))))))))).

Definition mkCSR_File : RV -> CSR_File :=
  fun rv =>
    let dm :=
      (Data.Map.Internal.fromList (app (u_csr_reset_values rv) (app
                                        (s_csr_reset_values rv) (m_csr_reset_values rv)))) in
    Mk_CSR_File dm.

(* External variables:
     Build_Default CSR_Addr Default Eq_ Int N Priv_Level RV RV32 RV64 String andb app
     bitSlice bool cons csr_addr_cycle csr_addr_cycleh csr_addr_data1 csr_addr_data2
     csr_addr_data3 csr_addr_dcsr csr_addr_dpc csr_addr_dscratch csr_addr_fcsr
     csr_addr_fflags csr_addr_frm csr_addr_instret csr_addr_instreth csr_addr_marchid
     csr_addr_mcause csr_addr_mcounteren csr_addr_mcycle csr_addr_mcycleh
     csr_addr_medeleg csr_addr_mepc csr_addr_mhartid csr_addr_mideleg csr_addr_mie
     csr_addr_mimpid csr_addr_minstret csr_addr_minstreth csr_addr_mip csr_addr_misa
     csr_addr_mscratch csr_addr_mstatus csr_addr_mtval csr_addr_mtvec
     csr_addr_mvendorid csr_addr_satp csr_addr_scause csr_addr_scounteren
     csr_addr_sedeleg csr_addr_sepc csr_addr_sideleg csr_addr_sie csr_addr_sip
     csr_addr_sscratch csr_addr_sstatus csr_addr_stval csr_addr_stvec csr_addr_time
     csr_addr_timeh csr_addr_tselect csr_addr_ucause csr_addr_uepc csr_addr_uie
     csr_addr_uip csr_addr_uscratch csr_addr_ustatus csr_addr_utval csr_addr_utvec
     false fromInteger list m_csr_addrs_and_names misa_A_bitpos misa_I_bitpos
     misa_MXL_bitpos_RV32 misa_MXL_bitpos_RV64 misa_M_bitpos misa_N_bitpos
     misa_S_bitpos misa_U_bitpos mstatus_mask_RV32 mstatus_mask_RV64
     mstatus_sxl_bitpos mstatus_tvm_bitpos mstatus_uxl_bitpos negb nil op_zeze__
     op_zeze____ op_zgze__ op_zgzg__ op_zsze____ op_zt__ orb pair patternFailure
     return_ s_csr_addrs_and_names sip_mask sstatus_mask_RV32 sstatus_mask_RV64 true
     tt u_csr_addrs_and_names uip_mask unit ustatus_mask_RV32 ustatus_mask_RV64
     xl_rv32 xl_rv64 Data.Bits.complement Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__
     Data.Bits.shiftL Data.Bits.shiftR Data.Bits.testBit Data.Foldable.length
     Data.Foldable.mapM_ Data.Map.Internal.Map Data.Map.Internal.fromList
     Data.Map.Internal.insert Data.Map.Internal.lookup Data.Map.Internal.member
     Data.Maybe.fromMaybe GHC.DeferredFix.deferredFix2 GHC.List.drop GHC.List.take
     IO.IO IO.putStr IO.putStrLn Numeric.showHex
*)

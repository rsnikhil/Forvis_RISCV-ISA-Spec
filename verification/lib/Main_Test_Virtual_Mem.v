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
Require Import Coq.Init.Datatypes.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require GHC.DeferredFix.
Require Import GHC.Err.
Require Import GHC.Num.
Require IO.
Require Import Machine_State.
Require Import Mem_Ops.
Require Numeric.
Require Import Virtual_Mem.
Import Data.Bits.Notations.

(* No type declarations to convert. *)
(* Converted value declarations: *)

Definition do_tests
   : Machine_State ->
     N -> list (N * Priv_Level * bool * bool * N * String)%type -> IO.IO unit :=
  GHC.DeferredFix.deferredFix3 (fun do_tests arg_0__ arg_1__ arg_2__ =>
                                  match arg_0__, arg_1__, arg_2__ with
                                  | mstate, sv, nil => return_ tt
                                  | mstate, sv, cons test tests =>
                                      let 'pair (pair (pair (pair (pair mstatus priv) is_instr) is_read) va)
                                         comment := test in
                                      IO.putStrLn (GHC.Base.hs_string__
                                                   "----------------------------------------------------------------")
                                      >>
                                      (IO.putStr (app (GHC.Base.hs_string__ "Test: '") (app comment
                                                                                            (GHC.Base.hs_string__ "'")))
                                       >>
                                       (IO.putStr (app (GHC.Base.hs_string__ "  mstatus ") (Numeric.showHex mstatus
                                                        (GHC.Base.hs_string__ ""))) >>
                                        (IO.putStr (app (GHC.Base.hs_string__ " priv ") (hs_string__ "ELIDED_STRING"))
                                         >>
                                         (when (andb is_instr is_read) (IO.putStr (GHC.Base.hs_string__ " fetch")) >>
                                          (when (andb (negb is_instr) is_read) (IO.putStr (GHC.Base.hs_string__
                                                                                           " load")) >>
                                           (when (andb (negb is_instr) (negb is_read)) (IO.putStr (GHC.Base.hs_string__
                                                                                                   " store")) >>
                                            (IO.putStr (app (GHC.Base.hs_string__ " va ") (Numeric.showHex va
                                                             (GHC.Base.hs_string__ " = ("))) >>
                                             (when (sv /= sv32) (IO.putStr (Numeric.showHex (va_vpn_J sv va (fromInteger
                                                                                                             2))
                                                                            (GHC.Base.hs_string__ "."))) >>
                                              (IO.putStr (Numeric.showHex (va_vpn_J sv va (fromInteger 1))
                                                          (GHC.Base.hs_string__ ".")) >>
                                               (IO.putStr (Numeric.showHex (va_vpn_J sv va (fromInteger 0))
                                                           (GHC.Base.hs_string__ ".")) >>
                                                (IO.putStr (Numeric.showHex (va_offset va) (GHC.Base.hs_string__ ")"))
                                                 >>
                                                 (IO.putStrLn (GHC.Base.hs_string__ "") >>
                                                  (let ms_b := mstate_priv_write mstate priv in
                                                   let ms_c := mstate_csr_write ms_b csr_addr_mstatus mstatus in
                                                   let 'pair mem_result ms_d := vm_translate ms_c is_instr is_read va in
                                                   (match mem_result with
                                                    | Mem_Result_Err ec =>
                                                        IO.putStrLn (app (GHC.Base.hs_string__ "Trap ")
                                                                         (show_trap_exc_code ec))
                                                    | Mem_Result_Ok v =>
                                                        IO.putStrLn (app (GHC.Base.hs_string__ "Ok: 0x")
                                                                         (Numeric.showHex v (GHC.Base.hs_string__ "")))
                                                    end) >>
                                                   do_tests ms_d sv tests)))))))))))))
                                  end).

Definition load_sample_page_table
   : Machine_State -> InstrField -> list (N * N)%type -> Machine_State :=
  fix load_sample_page_table arg_0__ arg_1__ arg_2__
        := match arg_0__, arg_1__, arg_2__ with
           | mstate, funct3, nil => mstate
           | mstate, funct3, cons (pair a v) avs =>
               let 'pair mem_result mstate1 := mstate_mem_write mstate funct3 a v in
               load_sample_page_table mstate1 funct3 avs
           end.

Definition mk_sample_page_table : N -> list (N * N)%type :=
  fun sv =>
    let mk_pte :=
      fun addr uxwr =>
        (((Data.Bits.shiftL (Data.Bits.shiftR addr (fromInteger 12)) (fromInteger 10))
          Data.Bits..|.(**)
          (Data.Bits.shiftL uxwr (fromInteger 1))) Data.Bits..|.(**)
         fromInteger 193) in
    let pte_size_bytes :=
      if (sv == sv32) : bool then fromInteger 4 else
      if (sv == sv39) : bool then fromInteger 8 else
      if (sv == sv48) : bool then fromInteger 8 else
      patternFailure in
    let mem_contents :=
      cons (pair (fromInteger 69632) (fromInteger 69632)) (cons (pair (fromInteger
                                                                       73728) (fromInteger 73728)) (cons (pair
                                                                                                          (fromInteger
                                                                                                           77824)
                                                                                                          (fromInteger
                                                                                                           77824)) (cons
                                                                                                          (pair
                                                                                                           (fromInteger
                                                                                                            81920)
                                                                                                           (fromInteger
                                                                                                            81920))
                                                                                                          (cons (pair
                                                                                                                 (fromInteger
                                                                                                                  86016)
                                                                                                                 (fromInteger
                                                                                                                  86016))
                                                                                                                (cons
                                                                                                                 (pair
                                                                                                                  (fromInteger
                                                                                                                   90112)
                                                                                                                  (fromInteger
                                                                                                                   90112))
                                                                                                                 (cons
                                                                                                                  (pair
                                                                                                                   (fromInteger
                                                                                                                    94208)
                                                                                                                   (fromInteger
                                                                                                                    94208))
                                                                                                                  (cons
                                                                                                                   (pair
                                                                                                                    (fromInteger
                                                                                                                     98304)
                                                                                                                    (fromInteger
                                                                                                                     98304))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     (fromInteger
                                                                                                                      102400)
                                                                                                                     (fromInteger
                                                                                                                      102400))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      (fromInteger
                                                                                                                       106496)
                                                                                                                      (fromInteger
                                                                                                                       106496))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       (fromInteger
                                                                                                                        4194304)
                                                                                                                       (fromInteger
                                                                                                                        4194304))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        (fromInteger
                                                                                                                         1073741824)
                                                                                                                        (fromInteger
                                                                                                                         1073741824))
                                                                                                                       (cons
                                                                                                                        (pair
                                                                                                                         (fromInteger
                                                                                                                          2147495936
                                                                                                                          +
                                                                                                                          (fromInteger
                                                                                                                           0
                                                                                                                           *
                                                                                                                           pte_size_bytes))
                                                                                                                         (fromInteger
                                                                                                                          0))
                                                                                                                        (cons
                                                                                                                         (pair
                                                                                                                          (fromInteger
                                                                                                                           2147495936
                                                                                                                           +
                                                                                                                           (fromInteger
                                                                                                                            1
                                                                                                                            *
                                                                                                                            pte_size_bytes))
                                                                                                                          (mk_pte
                                                                                                                           (fromInteger
                                                                                                                            69632)
                                                                                                                           (fromInteger
                                                                                                                            1)))
                                                                                                                         (cons
                                                                                                                          (pair
                                                                                                                           (fromInteger
                                                                                                                            2147495936
                                                                                                                            +
                                                                                                                            (fromInteger
                                                                                                                             2
                                                                                                                             *
                                                                                                                             pte_size_bytes))
                                                                                                                           (mk_pte
                                                                                                                            (fromInteger
                                                                                                                             73728)
                                                                                                                            (fromInteger
                                                                                                                             3)))
                                                                                                                          (cons
                                                                                                                           (pair
                                                                                                                            (fromInteger
                                                                                                                             2147495936
                                                                                                                             +
                                                                                                                             (fromInteger
                                                                                                                              3
                                                                                                                              *
                                                                                                                              pte_size_bytes))
                                                                                                                            (mk_pte
                                                                                                                             (fromInteger
                                                                                                                              77824)
                                                                                                                             (fromInteger
                                                                                                                              4)))
                                                                                                                           (cons
                                                                                                                            (pair
                                                                                                                             (fromInteger
                                                                                                                              2147495936
                                                                                                                              +
                                                                                                                              (fromInteger
                                                                                                                               4
                                                                                                                               *
                                                                                                                               pte_size_bytes))
                                                                                                                             (mk_pte
                                                                                                                              (fromInteger
                                                                                                                               81920)
                                                                                                                              (fromInteger
                                                                                                                               5)))
                                                                                                                            (cons
                                                                                                                             (pair
                                                                                                                              (fromInteger
                                                                                                                               2147495936
                                                                                                                               +
                                                                                                                               (fromInteger
                                                                                                                                5
                                                                                                                                *
                                                                                                                                pte_size_bytes))
                                                                                                                              (mk_pte
                                                                                                                               (fromInteger
                                                                                                                                86016)
                                                                                                                               (fromInteger
                                                                                                                                7)))
                                                                                                                             (cons
                                                                                                                              (pair
                                                                                                                               (fromInteger
                                                                                                                                2147495936
                                                                                                                                +
                                                                                                                                (fromInteger
                                                                                                                                 6
                                                                                                                                 *
                                                                                                                                 pte_size_bytes))
                                                                                                                               (mk_pte
                                                                                                                                (fromInteger
                                                                                                                                 90112)
                                                                                                                                (fromInteger
                                                                                                                                 9)))
                                                                                                                              (cons
                                                                                                                               (pair
                                                                                                                                (fromInteger
                                                                                                                                 2147495936
                                                                                                                                 +
                                                                                                                                 (fromInteger
                                                                                                                                  7
                                                                                                                                  *
                                                                                                                                  pte_size_bytes))
                                                                                                                                (mk_pte
                                                                                                                                 (fromInteger
                                                                                                                                  94208)
                                                                                                                                 (fromInteger
                                                                                                                                  11)))
                                                                                                                               (cons
                                                                                                                                (pair
                                                                                                                                 (fromInteger
                                                                                                                                  2147495936
                                                                                                                                  +
                                                                                                                                  (fromInteger
                                                                                                                                   8
                                                                                                                                   *
                                                                                                                                   pte_size_bytes))
                                                                                                                                 (mk_pte
                                                                                                                                  (fromInteger
                                                                                                                                   98304)
                                                                                                                                  (fromInteger
                                                                                                                                   12)))
                                                                                                                                (cons
                                                                                                                                 (pair
                                                                                                                                  (fromInteger
                                                                                                                                   2147495936
                                                                                                                                   +
                                                                                                                                   (fromInteger
                                                                                                                                    9
                                                                                                                                    *
                                                                                                                                    pte_size_bytes))
                                                                                                                                  (mk_pte
                                                                                                                                   (fromInteger
                                                                                                                                    102400)
                                                                                                                                   (fromInteger
                                                                                                                                    13)))
                                                                                                                                 (cons
                                                                                                                                  (pair
                                                                                                                                   (fromInteger
                                                                                                                                    2147495936
                                                                                                                                    +
                                                                                                                                    (fromInteger
                                                                                                                                     10
                                                                                                                                     *
                                                                                                                                     pte_size_bytes))
                                                                                                                                   (mk_pte
                                                                                                                                    (fromInteger
                                                                                                                                     106496)
                                                                                                                                    (fromInteger
                                                                                                                                     15)))
                                                                                                                                  (cons
                                                                                                                                   (pair
                                                                                                                                    (fromInteger
                                                                                                                                     2147495936
                                                                                                                                     +
                                                                                                                                     (fromInteger
                                                                                                                                      11
                                                                                                                                      *
                                                                                                                                      pte_size_bytes))
                                                                                                                                    (fromInteger
                                                                                                                                     0))
                                                                                                                                   (cons
                                                                                                                                    (pair
                                                                                                                                     (fromInteger
                                                                                                                                      2147491840
                                                                                                                                      +
                                                                                                                                      (fromInteger
                                                                                                                                       0
                                                                                                                                       *
                                                                                                                                       pte_size_bytes))
                                                                                                                                     (fromInteger
                                                                                                                                      0))
                                                                                                                                    (cons
                                                                                                                                     (pair
                                                                                                                                      (fromInteger
                                                                                                                                       2147491840
                                                                                                                                       +
                                                                                                                                       (fromInteger
                                                                                                                                        1
                                                                                                                                        *
                                                                                                                                        pte_size_bytes))
                                                                                                                                      (mk_pte
                                                                                                                                       (fromInteger
                                                                                                                                        2147495936)
                                                                                                                                       (fromInteger
                                                                                                                                        0)))
                                                                                                                                     (cons
                                                                                                                                      (pair
                                                                                                                                       (fromInteger
                                                                                                                                        2147491840
                                                                                                                                        +
                                                                                                                                        (fromInteger
                                                                                                                                         2
                                                                                                                                         *
                                                                                                                                         pte_size_bytes))
                                                                                                                                       (fromInteger
                                                                                                                                        0))
                                                                                                                                      (cons
                                                                                                                                       (pair
                                                                                                                                        (fromInteger
                                                                                                                                         2147491840
                                                                                                                                         +
                                                                                                                                         (fromInteger
                                                                                                                                          3
                                                                                                                                          *
                                                                                                                                          pte_size_bytes))
                                                                                                                                        (fromInteger
                                                                                                                                         0))
                                                                                                                                       (cons
                                                                                                                                        (pair
                                                                                                                                         (fromInteger
                                                                                                                                          2147491840
                                                                                                                                          +
                                                                                                                                          (fromInteger
                                                                                                                                           4
                                                                                                                                           *
                                                                                                                                           pte_size_bytes))
                                                                                                                                         (fromInteger
                                                                                                                                          0))
                                                                                                                                        (cons
                                                                                                                                         (pair
                                                                                                                                          (fromInteger
                                                                                                                                           2147491840
                                                                                                                                           +
                                                                                                                                           (fromInteger
                                                                                                                                            5
                                                                                                                                            *
                                                                                                                                            pte_size_bytes))
                                                                                                                                          (fromInteger
                                                                                                                                           0))
                                                                                                                                         (cons
                                                                                                                                          (pair
                                                                                                                                           (fromInteger
                                                                                                                                            2147491840
                                                                                                                                            +
                                                                                                                                            (fromInteger
                                                                                                                                             6
                                                                                                                                             *
                                                                                                                                             pte_size_bytes))
                                                                                                                                           (fromInteger
                                                                                                                                            0))
                                                                                                                                          (cons
                                                                                                                                           (pair
                                                                                                                                            (fromInteger
                                                                                                                                             2147491840
                                                                                                                                             +
                                                                                                                                             (fromInteger
                                                                                                                                              7
                                                                                                                                              *
                                                                                                                                              pte_size_bytes))
                                                                                                                                            (mk_pte
                                                                                                                                             (fromInteger
                                                                                                                                              4194304)
                                                                                                                                             (fromInteger
                                                                                                                                              7)))
                                                                                                                                           (cons
                                                                                                                                            (pair
                                                                                                                                             (fromInteger
                                                                                                                                              2147491840
                                                                                                                                              +
                                                                                                                                              (fromInteger
                                                                                                                                               8
                                                                                                                                               *
                                                                                                                                               pte_size_bytes))
                                                                                                                                             (mk_pte
                                                                                                                                              (fromInteger
                                                                                                                                               5242880)
                                                                                                                                              (fromInteger
                                                                                                                                               7)))
                                                                                                                                            (cons
                                                                                                                                             (pair
                                                                                                                                              (fromInteger
                                                                                                                                               2147491840
                                                                                                                                               +
                                                                                                                                               (fromInteger
                                                                                                                                                9
                                                                                                                                                *
                                                                                                                                                pte_size_bytes))
                                                                                                                                              (fromInteger
                                                                                                                                               0))
                                                                                                                                             (cons
                                                                                                                                              (pair
                                                                                                                                               (fromInteger
                                                                                                                                                2147487744
                                                                                                                                                +
                                                                                                                                                (fromInteger
                                                                                                                                                 0
                                                                                                                                                 *
                                                                                                                                                 pte_size_bytes))
                                                                                                                                               (fromInteger
                                                                                                                                                0))
                                                                                                                                              (cons
                                                                                                                                               (pair
                                                                                                                                                (fromInteger
                                                                                                                                                 2147487744
                                                                                                                                                 +
                                                                                                                                                 (fromInteger
                                                                                                                                                  1
                                                                                                                                                  *
                                                                                                                                                  pte_size_bytes))
                                                                                                                                                (mk_pte
                                                                                                                                                 (fromInteger
                                                                                                                                                  2147491840)
                                                                                                                                                 (fromInteger
                                                                                                                                                  0)))
                                                                                                                                               (cons
                                                                                                                                                (pair
                                                                                                                                                 (fromInteger
                                                                                                                                                  2147487744
                                                                                                                                                  +
                                                                                                                                                  (fromInteger
                                                                                                                                                   2
                                                                                                                                                   *
                                                                                                                                                   pte_size_bytes))
                                                                                                                                                 (fromInteger
                                                                                                                                                  0))
                                                                                                                                                (cons
                                                                                                                                                 (pair
                                                                                                                                                  (fromInteger
                                                                                                                                                   2147487744
                                                                                                                                                   +
                                                                                                                                                   (fromInteger
                                                                                                                                                    3
                                                                                                                                                    *
                                                                                                                                                    pte_size_bytes))
                                                                                                                                                  (fromInteger
                                                                                                                                                   0))
                                                                                                                                                 (cons
                                                                                                                                                  (pair
                                                                                                                                                   (fromInteger
                                                                                                                                                    2147487744
                                                                                                                                                    +
                                                                                                                                                    (fromInteger
                                                                                                                                                     4
                                                                                                                                                     *
                                                                                                                                                     pte_size_bytes))
                                                                                                                                                   (fromInteger
                                                                                                                                                    0))
                                                                                                                                                  (cons
                                                                                                                                                   (pair
                                                                                                                                                    (fromInteger
                                                                                                                                                     2147487744
                                                                                                                                                     +
                                                                                                                                                     (fromInteger
                                                                                                                                                      5
                                                                                                                                                      *
                                                                                                                                                      pte_size_bytes))
                                                                                                                                                    (mk_pte
                                                                                                                                                     (fromInteger
                                                                                                                                                      1073741824)
                                                                                                                                                     (fromInteger
                                                                                                                                                      15)))
                                                                                                                                                   (cons
                                                                                                                                                    (pair
                                                                                                                                                     (fromInteger
                                                                                                                                                      2147487744
                                                                                                                                                      +
                                                                                                                                                      (fromInteger
                                                                                                                                                       6
                                                                                                                                                       *
                                                                                                                                                       pte_size_bytes))
                                                                                                                                                     (mk_pte
                                                                                                                                                      (fromInteger
                                                                                                                                                       1342177280)
                                                                                                                                                      (fromInteger
                                                                                                                                                       15)))
                                                                                                                                                    (cons
                                                                                                                                                     (pair
                                                                                                                                                      (fromInteger
                                                                                                                                                       2147487744
                                                                                                                                                       +
                                                                                                                                                       (fromInteger
                                                                                                                                                        7
                                                                                                                                                        *
                                                                                                                                                        pte_size_bytes))
                                                                                                                                                      (fromInteger
                                                                                                                                                       0))
                                                                                                                                                     nil))))))))))))))))))))))))))))))))))))))))) in
    mem_contents.

Definition mk_satp_rv32 : N -> N -> N -> N :=
  fun mode asid pt_base_addr =>
    (((Data.Bits.shiftL (mode Data.Bits..&.(**) fromInteger 1) (fromInteger 31))
      Data.Bits..|.(**)
      (Data.Bits.shiftL (asid Data.Bits..&.(**) fromInteger 511) (fromInteger 22)))
     Data.Bits..|.(**)
     ((Data.Bits.shiftR pt_base_addr (fromInteger 12)) Data.Bits..&.(**)
      fromInteger 4194303)).

Definition mk_satp_rv64 : N -> N -> N -> N :=
  fun mode asid pt_base_addr =>
    (((Data.Bits.shiftL (mode Data.Bits..&.(**) fromInteger 15) (fromInteger 60))
      Data.Bits..|.(**)
      (Data.Bits.shiftL (asid Data.Bits..&.(**) fromInteger 65535) (fromInteger 44)))
     Data.Bits..|.(**)
     ((Data.Bits.shiftR pt_base_addr (fromInteger 12)) Data.Bits..&.(**)
      fromInteger 17592186044415)).

Definition mk_sv32_va : N -> N -> N -> N :=
  fun vpn1 vpn0 offset =>
    (((Data.Bits.shiftL vpn1 (fromInteger 22)) Data.Bits..|.(**)
      (Data.Bits.shiftL vpn0 (fromInteger 12))) Data.Bits..|.(**)
     offset).

Definition tests_sv32 : list (N * Priv_Level * bool * bool * N * String)%type :=
  cons (pair (pair (pair (pair (pair (fromInteger 0) m_Priv_Level) true) true)
                   (mk_sv32_va (fromInteger 0) (fromInteger 0) (fromInteger 0)))
             (GHC.Base.hs_string__ "Invalid lev 1 PTE")) (cons (pair (pair (pair (pair (pair
                                                                                        (fromInteger 0) m_Priv_Level)
                                                                                       true) true) (mk_sv32_va
                                                                            (fromInteger 1) (fromInteger 0) (fromInteger
                                                                                                             0)))
                                                                     (GHC.Base.hs_string__ "Invalid lev 0 PTE")) (cons
                                                                (pair (pair (pair (pair (pair (fromInteger 0)
                                                                                              m_Priv_Level) true) true)
                                                                            (mk_sv32_va (fromInteger 1) (fromInteger 1)
                                                                             (fromInteger 0))) (GHC.Base.hs_string__
                                                                       "Fetch: X is 0")) (cons (pair (pair (pair (pair
                                                                                                                  (pair
                                                                                                                   (fromInteger
                                                                                                                    0)
                                                                                                                   m_Priv_Level)
                                                                                                                  false)
                                                                                                                 false)
                                                                                                           (mk_sv32_va
                                                                                                            (fromInteger
                                                                                                             1)
                                                                                                            (fromInteger
                                                                                                             1)
                                                                                                            (fromInteger
                                                                                                             0)))
                                                                                                     (GHC.Base.hs_string__
                                                                                                      "Store: W is 0"))
                                                                                               (cons (pair (pair (pair
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (fromInteger
                                                                                                                     0)
                                                                                                                    m_Priv_Level)
                                                                                                                   false)
                                                                                                                  true)
                                                                                                                 (mk_sv32_va
                                                                                                                  (fromInteger
                                                                                                                   1)
                                                                                                                  (fromInteger
                                                                                                                   1)
                                                                                                                  (fromInteger
                                                                                                                   0)))
                                                                                                           (GHC.Base.hs_string__
                                                                                                            "Load: R is 1"))
                                                                                                     (cons (pair (pair
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (fromInteger
                                                                                                                      0)
                                                                                                                     m_Priv_Level)
                                                                                                                    true)
                                                                                                                   true)
                                                                                                                  (mk_sv32_va
                                                                                                                   (fromInteger
                                                                                                                    1)
                                                                                                                   (fromInteger
                                                                                                                    2)
                                                                                                                   (fromInteger
                                                                                                                    0)))
                                                                                                                 (GHC.Base.hs_string__
                                                                                                                  "Fetch: X is 0"))
                                                                                                           (cons (pair
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (fromInteger
                                                                                                                       0)
                                                                                                                      m_Priv_Level)
                                                                                                                     false)
                                                                                                                    false)
                                                                                                                   (mk_sv32_va
                                                                                                                    (fromInteger
                                                                                                                     1)
                                                                                                                    (fromInteger
                                                                                                                     2)
                                                                                                                    (fromInteger
                                                                                                                     0)))
                                                                                                                  (GHC.Base.hs_string__
                                                                                                                   "Store: W is 1"))
                                                                                                                 (cons
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (fromInteger
                                                                                                                        0)
                                                                                                                       m_Priv_Level)
                                                                                                                      false)
                                                                                                                     true)
                                                                                                                    (mk_sv32_va
                                                                                                                     (fromInteger
                                                                                                                      1)
                                                                                                                     (fromInteger
                                                                                                                      2)
                                                                                                                     (fromInteger
                                                                                                                      0)))
                                                                                                                   (GHC.Base.hs_string__
                                                                                                                    "Load: R is 1"))
                                                                                                                  (cons
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (fromInteger
                                                                                                                         0)
                                                                                                                        m_Priv_Level)
                                                                                                                       true)
                                                                                                                      true)
                                                                                                                     (mk_sv32_va
                                                                                                                      (fromInteger
                                                                                                                       1)
                                                                                                                      (fromInteger
                                                                                                                       3)
                                                                                                                      (fromInteger
                                                                                                                       0)))
                                                                                                                    (GHC.Base.hs_string__
                                                                                                                     "Fetch: X is 1"))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (fromInteger
                                                                                                                          0)
                                                                                                                         m_Priv_Level)
                                                                                                                        false)
                                                                                                                       false)
                                                                                                                      (mk_sv32_va
                                                                                                                       (fromInteger
                                                                                                                        1)
                                                                                                                       (fromInteger
                                                                                                                        3)
                                                                                                                       (fromInteger
                                                                                                                        0)))
                                                                                                                     (GHC.Base.hs_string__
                                                                                                                      "Store: W is 0"))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (fromInteger
                                                                                                                           0)
                                                                                                                          m_Priv_Level)
                                                                                                                         false)
                                                                                                                        true)
                                                                                                                       (mk_sv32_va
                                                                                                                        (fromInteger
                                                                                                                         1)
                                                                                                                        (fromInteger
                                                                                                                         3)
                                                                                                                        (fromInteger
                                                                                                                         0)))
                                                                                                                      (GHC.Base.hs_string__
                                                                                                                       "Load: R is 0"))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (pair
                                                                                                                           (fromInteger
                                                                                                                            0)
                                                                                                                           m_Priv_Level)
                                                                                                                          true)
                                                                                                                         true)
                                                                                                                        (mk_sv32_va
                                                                                                                         (fromInteger
                                                                                                                          7)
                                                                                                                         (fromInteger
                                                                                                                          0)
                                                                                                                         (fromInteger
                                                                                                                          0)))
                                                                                                                       (GHC.Base.hs_string__
                                                                                                                        "Megapage: Fetch: X is 1"))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (pair
                                                                                                                           (pair
                                                                                                                            (fromInteger
                                                                                                                             0)
                                                                                                                            m_Priv_Level)
                                                                                                                           true)
                                                                                                                          true)
                                                                                                                         (mk_sv32_va
                                                                                                                          (fromInteger
                                                                                                                           7)
                                                                                                                          (fromInteger
                                                                                                                           0)
                                                                                                                          (fromInteger
                                                                                                                           4)))
                                                                                                                        (GHC.Base.hs_string__
                                                                                                                         "Megapage: Fetch: X is 1"))
                                                                                                                       (cons
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (pair
                                                                                                                           (pair
                                                                                                                            (pair
                                                                                                                             (fromInteger
                                                                                                                              0)
                                                                                                                             m_Priv_Level)
                                                                                                                            true)
                                                                                                                           true)
                                                                                                                          (mk_sv32_va
                                                                                                                           (fromInteger
                                                                                                                            8)
                                                                                                                           (fromInteger
                                                                                                                            0)
                                                                                                                           (fromInteger
                                                                                                                            0)))
                                                                                                                         (GHC.Base.hs_string__
                                                                                                                          "Megapage: misaligned"))
                                                                                                                        nil))))))))))))).

Definition main_test_sv32 : IO.IO unit :=
  let sample_pt := (mk_sample_page_table sv32) in
  let addr_byte_list := nil in
  let mem_size := fromInteger 18446744073709551615 in
  let mem_base := fromInteger 0 in
  let addr_ranges := cons (pair mem_base (mem_base + mem_size)) nil in
  let initial_PC := fromInteger 0 in
  let ms1 := mkMachine_State RV32 initial_PC addr_ranges addr_byte_list in
  let ms2 :=
    mstate_csr_write ms1 csr_addr_satp (mk_satp_rv32 sv32 (fromInteger 0)
                                        (fromInteger 2147491840)) in
  let ms3 := load_sample_page_table ms2 funct3_SW sample_pt in
  do_tests ms3 sv32 tests_sv32.

Definition mk_sv39_va : N -> N -> N -> N -> N :=
  fun vpn2 vpn1 vpn0 offset =>
    ((((Data.Bits.shiftL vpn2 (fromInteger 30)) Data.Bits..|.(**)
       (Data.Bits.shiftL vpn1 (fromInteger 21))) Data.Bits..|.(**)
      (Data.Bits.shiftL vpn0 (fromInteger 12))) Data.Bits..|.(**)
     offset).

Definition tests_sv39 : list (N * Priv_Level * bool * bool * N * String)%type :=
  cons (pair (pair (pair (pair (pair (fromInteger 0) m_Priv_Level) true) true)
                   (mk_sv39_va (fromInteger 0) (fromInteger 0) (fromInteger 0) (fromInteger 0)))
             (GHC.Base.hs_string__ "Invalid lev 0 PTE")) (cons (pair (pair (pair (pair (pair
                                                                                        (fromInteger 0) m_Priv_Level)
                                                                                       true) true) (mk_sv39_va
                                                                            (fromInteger 1) (fromInteger 0) (fromInteger
                                                                                                             0)
                                                                            (fromInteger 0))) (GHC.Base.hs_string__
                                                                      "Invalid lev 1 PTE")) (cons (pair (pair (pair
                                                                                                               (pair
                                                                                                                (pair
                                                                                                                 (fromInteger
                                                                                                                  0)
                                                                                                                 m_Priv_Level)
                                                                                                                true)
                                                                                                               true)
                                                                                                              (mk_sv39_va
                                                                                                               (fromInteger
                                                                                                                1)
                                                                                                               (fromInteger
                                                                                                                1)
                                                                                                               (fromInteger
                                                                                                                0)
                                                                                                               (fromInteger
                                                                                                                0)))
                                                                                                        (GHC.Base.hs_string__
                                                                                                         "Invalid lev 0 PTE"))
                                                                                                  (cons (pair (pair
                                                                                                               (pair
                                                                                                                (pair
                                                                                                                 (pair
                                                                                                                  (fromInteger
                                                                                                                   0)
                                                                                                                  m_Priv_Level)
                                                                                                                 true)
                                                                                                                true)
                                                                                                               (mk_sv39_va
                                                                                                                (fromInteger
                                                                                                                 1)
                                                                                                                (fromInteger
                                                                                                                 1)
                                                                                                                (fromInteger
                                                                                                                 1)
                                                                                                                (fromInteger
                                                                                                                 0)))
                                                                                                              (GHC.Base.hs_string__
                                                                                                               "Fetch: X is 0"))
                                                                                                        (cons (pair
                                                                                                               (pair
                                                                                                                (pair
                                                                                                                 (pair
                                                                                                                  (pair
                                                                                                                   (fromInteger
                                                                                                                    0)
                                                                                                                   m_Priv_Level)
                                                                                                                  false)
                                                                                                                 false)
                                                                                                                (mk_sv39_va
                                                                                                                 (fromInteger
                                                                                                                  1)
                                                                                                                 (fromInteger
                                                                                                                  1)
                                                                                                                 (fromInteger
                                                                                                                  1)
                                                                                                                 (fromInteger
                                                                                                                  0)))
                                                                                                               (GHC.Base.hs_string__
                                                                                                                "Store: W is 0"))
                                                                                                              (cons
                                                                                                               (pair
                                                                                                                (pair
                                                                                                                 (pair
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (fromInteger
                                                                                                                     0)
                                                                                                                    m_Priv_Level)
                                                                                                                   false)
                                                                                                                  true)
                                                                                                                 (mk_sv39_va
                                                                                                                  (fromInteger
                                                                                                                   1)
                                                                                                                  (fromInteger
                                                                                                                   1)
                                                                                                                  (fromInteger
                                                                                                                   1)
                                                                                                                  (fromInteger
                                                                                                                   0)))
                                                                                                                (GHC.Base.hs_string__
                                                                                                                 "Load: R is 1"))
                                                                                                               (cons
                                                                                                                (pair
                                                                                                                 (pair
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (fromInteger
                                                                                                                      0)
                                                                                                                     m_Priv_Level)
                                                                                                                    true)
                                                                                                                   true)
                                                                                                                  (mk_sv39_va
                                                                                                                   (fromInteger
                                                                                                                    1)
                                                                                                                   (fromInteger
                                                                                                                    1)
                                                                                                                   (fromInteger
                                                                                                                    2)
                                                                                                                   (fromInteger
                                                                                                                    0)))
                                                                                                                 (GHC.Base.hs_string__
                                                                                                                  "Fetch: X is 0"))
                                                                                                                (cons
                                                                                                                 (pair
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (fromInteger
                                                                                                                       0)
                                                                                                                      m_Priv_Level)
                                                                                                                     false)
                                                                                                                    false)
                                                                                                                   (mk_sv39_va
                                                                                                                    (fromInteger
                                                                                                                     1)
                                                                                                                    (fromInteger
                                                                                                                     1)
                                                                                                                    (fromInteger
                                                                                                                     2)
                                                                                                                    (fromInteger
                                                                                                                     0)))
                                                                                                                  (GHC.Base.hs_string__
                                                                                                                   "Store: W is 1"))
                                                                                                                 (cons
                                                                                                                  (pair
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (fromInteger
                                                                                                                        0)
                                                                                                                       m_Priv_Level)
                                                                                                                      false)
                                                                                                                     true)
                                                                                                                    (mk_sv39_va
                                                                                                                     (fromInteger
                                                                                                                      1)
                                                                                                                     (fromInteger
                                                                                                                      1)
                                                                                                                     (fromInteger
                                                                                                                      2)
                                                                                                                     (fromInteger
                                                                                                                      0)))
                                                                                                                   (GHC.Base.hs_string__
                                                                                                                    "Load: R is 1"))
                                                                                                                  (cons
                                                                                                                   (pair
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (fromInteger
                                                                                                                         0)
                                                                                                                        m_Priv_Level)
                                                                                                                       true)
                                                                                                                      true)
                                                                                                                     (mk_sv39_va
                                                                                                                      (fromInteger
                                                                                                                       1)
                                                                                                                      (fromInteger
                                                                                                                       1)
                                                                                                                      (fromInteger
                                                                                                                       3)
                                                                                                                      (fromInteger
                                                                                                                       0)))
                                                                                                                    (GHC.Base.hs_string__
                                                                                                                     "Fetch: X is 1"))
                                                                                                                   (cons
                                                                                                                    (pair
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (fromInteger
                                                                                                                          0)
                                                                                                                         m_Priv_Level)
                                                                                                                        false)
                                                                                                                       false)
                                                                                                                      (mk_sv39_va
                                                                                                                       (fromInteger
                                                                                                                        1)
                                                                                                                       (fromInteger
                                                                                                                        1)
                                                                                                                       (fromInteger
                                                                                                                        3)
                                                                                                                       (fromInteger
                                                                                                                        0)))
                                                                                                                     (GHC.Base.hs_string__
                                                                                                                      "Store: W is 0"))
                                                                                                                    (cons
                                                                                                                     (pair
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (fromInteger
                                                                                                                           0)
                                                                                                                          m_Priv_Level)
                                                                                                                         false)
                                                                                                                        true)
                                                                                                                       (mk_sv39_va
                                                                                                                        (fromInteger
                                                                                                                         1)
                                                                                                                        (fromInteger
                                                                                                                         1)
                                                                                                                        (fromInteger
                                                                                                                         3)
                                                                                                                        (fromInteger
                                                                                                                         0)))
                                                                                                                      (GHC.Base.hs_string__
                                                                                                                       "Load: R is 0"))
                                                                                                                     (cons
                                                                                                                      (pair
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (pair
                                                                                                                           (fromInteger
                                                                                                                            0)
                                                                                                                           m_Priv_Level)
                                                                                                                          true)
                                                                                                                         true)
                                                                                                                        (mk_sv39_va
                                                                                                                         (fromInteger
                                                                                                                          1)
                                                                                                                         (fromInteger
                                                                                                                          7)
                                                                                                                         (fromInteger
                                                                                                                          0)
                                                                                                                         (fromInteger
                                                                                                                          0)))
                                                                                                                       (GHC.Base.hs_string__
                                                                                                                        "Megapage: Fetch: X is 1"))
                                                                                                                      (cons
                                                                                                                       (pair
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (pair
                                                                                                                           (pair
                                                                                                                            (fromInteger
                                                                                                                             0)
                                                                                                                            m_Priv_Level)
                                                                                                                           true)
                                                                                                                          true)
                                                                                                                         (mk_sv39_va
                                                                                                                          (fromInteger
                                                                                                                           1)
                                                                                                                          (fromInteger
                                                                                                                           7)
                                                                                                                          (fromInteger
                                                                                                                           0)
                                                                                                                          (fromInteger
                                                                                                                           4)))
                                                                                                                        (GHC.Base.hs_string__
                                                                                                                         "Megapage: Fetch: X is 1"))
                                                                                                                       (cons
                                                                                                                        (pair
                                                                                                                         (pair
                                                                                                                          (pair
                                                                                                                           (pair
                                                                                                                            (pair
                                                                                                                             (fromInteger
                                                                                                                              0)
                                                                                                                             m_Priv_Level)
                                                                                                                            true)
                                                                                                                           true)
                                                                                                                          (mk_sv39_va
                                                                                                                           (fromInteger
                                                                                                                            1)
                                                                                                                           (fromInteger
                                                                                                                            8)
                                                                                                                           (fromInteger
                                                                                                                            0)
                                                                                                                           (fromInteger
                                                                                                                            0)))
                                                                                                                         (GHC.Base.hs_string__
                                                                                                                          "Megapage: misaligned"))
                                                                                                                        (cons
                                                                                                                         (pair
                                                                                                                          (pair
                                                                                                                           (pair
                                                                                                                            (pair
                                                                                                                             (pair
                                                                                                                              (fromInteger
                                                                                                                               0)
                                                                                                                              m_Priv_Level)
                                                                                                                             true)
                                                                                                                            true)
                                                                                                                           (mk_sv39_va
                                                                                                                            (fromInteger
                                                                                                                             5)
                                                                                                                            (fromInteger
                                                                                                                             0)
                                                                                                                            (fromInteger
                                                                                                                             0)
                                                                                                                            (fromInteger
                                                                                                                             0)))
                                                                                                                          (GHC.Base.hs_string__
                                                                                                                           "Gigapage: Fetch: X is 1"))
                                                                                                                         (cons
                                                                                                                          (pair
                                                                                                                           (pair
                                                                                                                            (pair
                                                                                                                             (pair
                                                                                                                              (pair
                                                                                                                               (fromInteger
                                                                                                                                0)
                                                                                                                               m_Priv_Level)
                                                                                                                              true)
                                                                                                                             true)
                                                                                                                            (mk_sv39_va
                                                                                                                             (fromInteger
                                                                                                                              5)
                                                                                                                             (fromInteger
                                                                                                                              0)
                                                                                                                             (fromInteger
                                                                                                                              0)
                                                                                                                             (fromInteger
                                                                                                                              4)))
                                                                                                                           (GHC.Base.hs_string__
                                                                                                                            "Gigapage: Fetch: X is 1"))
                                                                                                                          (cons
                                                                                                                           (pair
                                                                                                                            (pair
                                                                                                                             (pair
                                                                                                                              (pair
                                                                                                                               (pair
                                                                                                                                (fromInteger
                                                                                                                                 0)
                                                                                                                                m_Priv_Level)
                                                                                                                               true)
                                                                                                                              true)
                                                                                                                             (mk_sv39_va
                                                                                                                              (fromInteger
                                                                                                                               6)
                                                                                                                              (fromInteger
                                                                                                                               0)
                                                                                                                              (fromInteger
                                                                                                                               0)
                                                                                                                              (fromInteger
                                                                                                                               0)))
                                                                                                                            (GHC.Base.hs_string__
                                                                                                                             "Gigapage: misaligned"))
                                                                                                                           nil))))))))))))))))).

Definition main_test_sv39 : IO.IO unit :=
  let sample_pt := (mk_sample_page_table sv39) in
  let addr_byte_list := nil in
  let mem_size := fromInteger 18446744073709551615 in
  let mem_base := fromInteger 0 in
  let addr_ranges := cons (pair mem_base (mem_base + mem_size)) nil in
  let initial_PC := fromInteger 0 in
  let ms1 := mkMachine_State RV64 initial_PC addr_ranges addr_byte_list in
  let ms2 :=
    mstate_csr_write ms1 csr_addr_satp (mk_satp_rv64 sv39 (fromInteger 0)
                                        (fromInteger 2147487744)) in
  let ms3 := load_sample_page_table ms2 funct3_SD sample_pt in
  do_tests ms3 sv39 tests_sv39.

Definition main_test_virtual_mem : IO.IO unit :=
  main_test_sv39.

(* External variables:
     InstrField Machine_State Mem_Result_Err Mem_Result_Ok N Priv_Level RV32 RV64
     String andb app bool cons csr_addr_mstatus csr_addr_satp false fromInteger
     funct3_SD funct3_SW hs_string__ list m_Priv_Level mkMachine_State
     mstate_csr_write mstate_mem_write mstate_priv_write negb nil op_zeze__ op_zgzg__
     op_zp__ op_zsze__ op_zt__ pair patternFailure return_ show_trap_exc_code sv32
     sv39 sv48 true tt unit va_offset va_vpn_J vm_translate when
     Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__ Data.Bits.shiftL Data.Bits.shiftR
     GHC.DeferredFix.deferredFix3 IO.IO IO.putStr IO.putStrLn Numeric.showHex
*)

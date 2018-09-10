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
Require Import GHC.Base.
Require Import GHC.Num.

(* No type declarations to convert. *)
(* Converted value declarations: *)

Definition addr_base_UART :=
  fromInteger 3221225472 : N.

Definition addr_base_boot :=
  fromInteger 4096 : N.

Definition addr_base_htif :=
  fromInteger 65408 : N.

Definition addr_base_mem :=
  fromInteger 2147483648 : N.

Definition addr_htif_console_out : N :=
  fromInteger 65524.

Definition addr_msip :=
  fromInteger 33554432 : N.

Definition addr_mtime :=
  fromInteger 33603576 : N.

Definition addr_mtimecmp :=
  fromInteger 33570816 : N.

Definition addr_size_UART :=
  fromInteger 128 : N.

Definition mmio_addr_ranges : list (N * N)%type :=
  cons (pair addr_htif_console_out (addr_htif_console_out + fromInteger 8)) (cons
        (pair addr_base_UART (addr_base_UART + addr_size_UART)) (cons (pair addr_mtime
                                                                            (addr_mtime + fromInteger 8)) (cons (pair
                                                                                                                 addr_mtimecmp
                                                                                                                 (addr_mtimecmp
                                                                                                                  +
                                                                                                                  fromInteger
                                                                                                                  8))
                                                                                                                (cons
                                                                                                                 (pair
                                                                                                                  addr_msip
                                                                                                                  (addr_msip
                                                                                                                   +
                                                                                                                   fromInteger
                                                                                                                   8))
                                                                                                                 nil)))).

Definition is_IO_addr : N -> bool :=
  fun addr =>
    let check : list (N * N)%type -> bool :=
      fix check arg_0__
            := match arg_0__ with
               | nil => false
               | cons (pair base lim) ranges =>
                   if (andb (base <= addr) (addr < lim)) : bool
                   then true
                   else check ranges
               end in
    check mmio_addr_ranges.

Definition addr_size_boot :=
  fromInteger 4096 : N.

Definition addr_size_htif :=
  fromInteger 128 : N.

Definition addr_size_mem :=
  fromInteger 268435456 : N.

Definition memory_addr_ranges : list (N * N)%type :=
  cons (pair addr_base_boot (addr_base_boot + addr_size_boot)) (cons (pair
                                                                      addr_base_mem (addr_base_mem + addr_size_mem))
                                                                     (cons (pair addr_base_htif (addr_base_htif +
                                                                                  addr_size_htif)) nil)).

Definition addr_ranges : list (N * N)%type :=
  app memory_addr_ranges mmio_addr_ranges.

Definition pc_reset_value :=
  fromInteger 4096 : N.

(* External variables:
     N andb app bool cons false fromInteger list nil op_zl__ op_zlze__ op_zp__
     op_zt__ pair true
*)

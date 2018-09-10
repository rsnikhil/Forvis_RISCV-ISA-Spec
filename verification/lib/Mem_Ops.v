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
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require Import GHC.Num.
Import Data.Bits.Notations.

(* No type declarations to convert. *)
(* Converted value declarations: *)

Definition funct3_AMO_D :=
  fromInteger 3 : InstrField.

Definition funct3_AMO_W :=
  fromInteger 2 : InstrField.

Definition is_AMO_aligned : InstrField -> N -> bool :=
  fun funct3 addr =>
    (orb (andb (funct3 == funct3_AMO_W) ((addr Data.Bits..&.(**) fromInteger 3) ==
                fromInteger 0)) (andb (funct3 == funct3_AMO_D) ((addr Data.Bits..&.(**)
                                        fromInteger 7) ==
                                       fromInteger 0))).

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

Definition funct3_LWU :=
  fromInteger 6 : InstrField.

Definition is_LOAD_aligned : InstrField -> N -> bool :=
  fun funct3 addr =>
    (orb (orb (funct3 == funct3_LB) (funct3 == funct3_LBU)) (orb (andb (orb (funct3
                                                                             ==
                                                                             funct3_LH) (funct3 == funct3_LHU)) ((addr
                                                                         Data.Bits..&.(**)
                                                                         fromInteger 1) ==
                                                                        fromInteger 0)) (orb (andb (orb (funct3 ==
                                                                                                         funct3_LW)
                                                                                                        (funct3 ==
                                                                                                         funct3_LWU))
                                                                                                   ((addr
                                                                                                     Data.Bits..&.(**)
                                                                                                     fromInteger 3) ==
                                                                                                    fromInteger 0))
                                                                                             (andb (funct3 == funct3_LD)
                                                                                                   ((addr
                                                                                                     Data.Bits..&.(**)
                                                                                                     fromInteger 7) ==
                                                                                                    fromInteger 0))))).

Definition funct3_SB :=
  fromInteger 0 : InstrField.

Definition funct3_SD :=
  fromInteger 3 : InstrField.

Definition funct3_SH :=
  fromInteger 1 : InstrField.

Definition funct3_SW :=
  fromInteger 2 : InstrField.

Definition is_STORE_aligned : InstrField -> N -> bool :=
  fun funct3 addr =>
    (orb (funct3 == funct3_SB) (orb (andb (funct3 == funct3_SH) ((addr
                                            Data.Bits..&.(**)
                                            fromInteger 1) ==
                                           fromInteger 0)) (orb (andb (funct3 == funct3_SW) ((addr Data.Bits..&.(**)
                                                                        fromInteger 3) ==
                                                                       fromInteger 0)) (andb (funct3 == funct3_SD)
                                                                                             ((addr Data.Bits..&.(**)
                                                                                               fromInteger 7) ==
                                                                                              fromInteger 0))))).

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

(* External variables:
     InstrField N andb bool fromInteger op_zeze__ orb Data.Bits.op_zizazi__
*)

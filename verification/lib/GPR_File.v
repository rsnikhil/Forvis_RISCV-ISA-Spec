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
Require Data.Foldable.
Require Data.Map.Internal.
Require Data.Maybe.
Require Import GHC.Base.
Require GHC.Enum.
Require Import GHC.Num.
Require Import GHC.Real.
Require IO.
Require Numeric.

(* Converted type declarations: *)

Inductive GPR_File : Type
  := Mk_GPR_File : (Data.Map.Internal.Map InstrField N) -> GPR_File.
(* Converted value declarations: *)

(* Skipping instance Show__GPR_File of class Show *)

Definition gpr_read : GPR_File -> GPR_Addr -> N :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | Mk_GPR_File dm, reg =>
        Data.Maybe.fromMaybe (fromInteger 0) (Data.Map.Internal.lookup reg dm)
    end.

Definition print_GPR_File : String -> GPR_File -> IO.IO unit :=
  fun indent gpr_file =>
    let print_n : GPR_Addr -> GPR_Addr -> IO.IO unit :=
      fun r1 r2 =>
        (IO.putStr (app indent (app (hs_string__ "ELIDED_STRING") (GHC.Base.hs_string__
                                     ":"))) >>
         (Data.Foldable.mapM_ (fun rg =>
                                 IO.putStr (app (GHC.Base.hs_string__ "  ") (Numeric.showHex (gpr_read gpr_file
                                                                                              rg) (GHC.Base.hs_string__
                                                                                                   ""))))
          (GHC.Enum.enumFromTo r1 r2) >>
          IO.putStrLn (GHC.Base.hs_string__ ""))) in
    print_n (fromInteger 0) (fromInteger 3) >>
    (print_n (fromInteger 4) (fromInteger 7) >>
     (print_n (fromInteger 8) (fromInteger 11) >>
      (print_n (fromInteger 12) (fromInteger 15) >>
       (print_n (fromInteger 16) (fromInteger 19) >>
        (print_n (fromInteger 20) (fromInteger 23) >>
         (print_n (fromInteger 24) (fromInteger 27) >>
          print_n (fromInteger 28) (fromInteger 31))))))).

Definition gpr_write : GPR_File -> GPR_Addr -> N -> GPR_File :=
  fun arg_0__ arg_1__ arg_2__ =>
    match arg_0__, arg_1__, arg_2__ with
    | Mk_GPR_File dm, reg, val =>
        let val1 := if (reg == fromInteger 0) : bool then fromInteger 0 else val in
        Mk_GPR_File (Data.Map.Internal.insert reg val1 dm)
    end.

Definition mkGPR_File : GPR_File :=
  Mk_GPR_File (Data.Map.Internal.fromList (map (fun y =>
                                                  pair y (fromIntegral (fromInteger 0))) (GHC.Enum.enumFromTo
                                                (fromInteger 0) (fromInteger 31)))).

(* External variables:
     GPR_Addr InstrField N String app bool fromInteger fromIntegral hs_string__ map
     op_zeze__ op_zgzg__ pair unit Data.Foldable.mapM_ Data.Map.Internal.Map
     Data.Map.Internal.fromList Data.Map.Internal.insert Data.Map.Internal.lookup
     Data.Maybe.fromMaybe GHC.Enum.enumFromTo IO.IO IO.putStr IO.putStrLn
     Numeric.showHex
*)

(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

Require GHC.Base.
Require GHC.Char.
Require GHC.Num.

(* Should this be a Coinductive definition ? *)
Parameter IO : Type -> Type.

Instance IO_Functor     : GHC.Base.Functor IO. Admitted.
Instance IO_Applicative : GHC.Base.Applicative IO. Admitted.
Instance IO_Monad       : GHC.Base.Monad IO. Admitted.
Parameter putStrLn : GHC.Base.String -> IO unit.
Parameter putStr   : GHC.Base.String -> IO unit.

(* Exit *)

Parameter exit : forall a, nat -> IO a.

(* File handles *)

Parameter Handle : Type. 
Parameter stdin  : Handle.
Parameter stdout : Handle.

Parameter hWaitForInput : Handle -> GHC.Num.Int -> IO bool.
Parameter hGetChar : Handle -> IO GHC.Char.Char.
Parameter hFlush : Handle -> IO unit.

(* This is defined in Run_Program.v. But it may not be terminating *)
Parameter hGetLine_polled : Handle -> IO (GHC.Base.String).



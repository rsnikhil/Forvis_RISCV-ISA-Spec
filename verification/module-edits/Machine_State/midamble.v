(* Of course, we should fill in this definition with a suitable value. 
   But that will require defining some additional instances of the 
   Default type class. So, we admit for now. *)

Require Import GHC.Err.

Instance Default_Machine_State : GHC.Err.Default Machine_State.
Admitted.

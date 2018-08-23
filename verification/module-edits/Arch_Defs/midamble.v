Require Import GHC.Err.

Instance Mem_Result_Default : Default Mem_Result :=
  Build_Default _ (Mem_Result_Err default).


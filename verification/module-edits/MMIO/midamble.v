Require Import GHC.Err.

Instance MMIO_Default : Default MMIO :=
  Build_Default _ (Mk_MMIO default default default default default).


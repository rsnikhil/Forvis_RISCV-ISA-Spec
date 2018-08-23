Require Import GHC.Err.

Instance UART_NS16550A_Default : Default UART_NS16550A :=
  Build_Default _ (Mk_UART_NS16550A default default default default default default default default default default default default default).


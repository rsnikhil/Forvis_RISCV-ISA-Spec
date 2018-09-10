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
Require Import Forvis_Spec.
Require Import GHC.Base.
Require GHC.DeferredFix.
Require Import GHC.Num.
Require Import GHC.Real.
Require IO.
Require Import Machine_State.
Require Numeric.
Import Data.Bits.Notations.

(* No type declarations to convert. *)
(* Converted value declarations: *)

Definition fetch_and_execute : Machine_State -> IO.IO Machine_State :=
  fun mstate =>
    let 'pair intr_pending mstate2 := take_interrupt_if_any mstate in
    let verbosity := mstate_verbosity_read mstate in
    (match intr_pending with
     | None => return_ tt
     | Some exc_code =>
         let instret := mstate_csr_read mstate2 csr_addr_minstret in
         when (verbosity >= fromInteger 1) (IO.putStrLn (app (GHC.Base.hs_string__
                                                              "Taking interrupt; instret = ") (app (hs_string__
                                                                                                    "ELIDED_STRING")
                                                                                                   (app
                                                                                                    (GHC.Base.hs_string__
                                                                                                     "; exc_code = ")
                                                                                                    (app
                                                                                                     (Numeric.showHex
                                                                                                      exc_code
                                                                                                      (GHC.Base.hs_string__
                                                                                                       ""))
                                                                                                     (GHC.Base.hs_string__
                                                                                                      "")))))) >>
         when (verbosity >= fromInteger 2) (IO.putStrLn (GHC.Base.hs_string__
                                                         "---------------- State before interrupt trap setup") >>
                                            ((mstate_print (GHC.Base.hs_string__ "  ") mstate) >>
                                             (IO.putStrLn (GHC.Base.hs_string__
                                                           "---------------- State after interrupt trap setup") >>
                                              (mstate_print (GHC.Base.hs_string__ "  ") mstate2))))
     end) >>
    (let 'pair fetch_result mstate3 := instr_fetch mstate2 in
     let priv := mstate_priv_read mstate3 in
     let instret := mstate_csr_read mstate2 csr_addr_minstret in
     let pc := mstate_pc_read mstate2 in
     match fetch_result with
     | Fetch_Trap ec =>
         (when (verbosity >= fromInteger 1) (IO.putStrLn (app (GHC.Base.hs_string__
                                                               "Fetch Trap:") (show_trap_exc_code ec))) >>
          return_ mstate3)
     | Fetch_C u16 =>
         (let 'pair mstate4 spec_name := (exec_instr_C mstate3 u16) in
          when (verbosity >= fromInteger 1) (IO.putStr (app (GHC.Base.hs_string__ "inum:")
                                                            (hs_string__ "ELIDED_STRING")) >>
                                             (IO.putStr (app (GHC.Base.hs_string__ "  pc 0x") (Numeric.showHex pc
                                                              (GHC.Base.hs_string__ ""))) >>
                                              (IO.putStr (app (GHC.Base.hs_string__ "  instr.C 0x") (Numeric.showHex u16
                                                               (GHC.Base.hs_string__ ""))) >>
                                               (IO.putStr (app (GHC.Base.hs_string__ "  priv ") (hs_string__
                                                                "ELIDED_STRING")) >>
                                                IO.putStrLn (app (GHC.Base.hs_string__ "  ") spec_name))))) >>
          (when (verbosity > fromInteger 1) (mstate_print (GHC.Base.hs_string__ "  ")
                                             mstate4) >>
           return_ mstate4))
     | Fetch u32 =>
         (let 'pair mstate4 spec_name := (exec_instr mstate3 u32) in
          when (verbosity >= fromInteger 1) (IO.putStr (app (GHC.Base.hs_string__ "inum:")
                                                            (hs_string__ "ELIDED_STRING")) >>
                                             (IO.putStr (app (GHC.Base.hs_string__ "  pc 0x") (Numeric.showHex pc
                                                              (GHC.Base.hs_string__ ""))) >>
                                              (IO.putStr (app (GHC.Base.hs_string__ "  instr 0x") (Numeric.showHex u32
                                                               (GHC.Base.hs_string__ ""))) >>
                                               (IO.putStr (app (GHC.Base.hs_string__ "  priv ") (hs_string__
                                                                "ELIDED_STRING")) >>
                                                IO.putStrLn (app (GHC.Base.hs_string__ "  ") spec_name))))) >>
          (when (verbosity > fromInteger 1) (mstate_print (GHC.Base.hs_string__ "  ")
                                             mstate4) >>
           return_ mstate4))
     end).

Definition hGetLine_polled : IO.Handle -> IO.IO (String) :=
  GHC.DeferredFix.deferredFix1 (fun hGetLine_polled h =>
                                  IO.hWaitForInput h (fromInteger 0) >>=
                                  (fun input_available =>
                                     if negb input_available : bool
                                     then return_ (GHC.Base.hs_string__ "")
                                     else (IO.hGetChar h >>=
                                           (fun ch => hGetLine_polled h >>= (fun chs => return_ (cons ch chs)))))).

Definition get_tty_input : Machine_State -> IO.IO (Machine_State) :=
  fun mstate =>
    let mtime := mstate_mem_read_mtime mstate in
    (if ((mtime Data.Bits..&.(**) fromInteger 1023) == fromInteger 0) : bool
     then hGetLine_polled IO.stdin
     else return_ (GHC.Base.hs_string__ "")) >>=
    (fun console_input =>
       let mstate' :=
         if (console_input == GHC.Base.hs_string__ "") : bool
         then mstate
         else mstate_mem_enq_console_input mstate console_input in
       return_ mstate').

Definition mstate_mem_read_tohost
   : Machine_State -> option N -> (N * Machine_State)%type :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | mstate, None => pair (fromInteger 0) mstate
    | mstate, Some tohost_addr =>
        let 'pair load_result mstate' := mstate_mem_read mstate
                                           exc_code_load_access_fault funct3_LW tohost_addr in
        match load_result with
        | Mem_Result_Err exc_code => pair (fromInteger 0) mstate'
        | Mem_Result_Ok u64 => pair u64 mstate'
        end
    end.

Definition put_tty_output : Machine_State -> IO.IO (Machine_State) :=
  fun mstate =>
    let 'pair console_output mstate' := mstate_mem_deq_console_output mstate in
    when (console_output /= GHC.Base.hs_string__ "") (IO.putStr console_output >>
                                                      IO.hFlush IO.stdout) >>
    return_ mstate'.

Definition run_loop
   : Int -> (option N) -> Machine_State -> IO.IO (Int * Machine_State)%type :=
  GHC.DeferredFix.deferredFix3 (fun run_loop maxinstrs m_tohost_addr mstate =>
                                  let mstate1 := mstate_mem_tick mstate in
                                  let 'pair tohost_u64 mstate2 := mstate_mem_read_tohost mstate1 m_tohost_addr in
                                  let run_state := mstate_run_state_read mstate in
                                  let instret := mstate_csr_read mstate csr_addr_minstret in
                                  if (tohost_u64 /= fromInteger 0) : bool
                                  then (IO.putStrLn (app (GHC.Base.hs_string__
                                                          "Stopping due to write to <tohost> = ") (app (hs_string__
                                                                                                        "ELIDED_STRING")
                                                                                                       (app
                                                                                                        (GHC.Base.hs_string__
                                                                                                         "; instret = ")
                                                                                                        (hs_string__
                                                                                                         "ELIDED_STRING"))))
                                        >>
                                        (let exit_value :=
                                           (fromIntegral (Data.Bits.shiftR tohost_u64 (fromInteger 1))) : Int in
                                         return_ (pair exit_value mstate2)))
                                  else if instret >= fromIntegral maxinstrs : bool
                                       then (IO.putStrLn (app (GHC.Base.hs_string__ "Stopping due to instret limit (")
                                                              (app (hs_string__ "ELIDED_STRING") (GHC.Base.hs_string__
                                                                    ")"))) >>
                                             return_ (pair (fromInteger 0) mstate2))
                                       else if (andb (run_state /= Run_State_Running) (run_state /=
                                                      Run_State_WFI)) : bool
                                            then (IO.putStrLn (app (GHC.Base.hs_string__ "Stopping due to runstate ")
                                                                   (app (hs_string__ "ELIDED_STRING") (app
                                                                         (GHC.Base.hs_string__ "; instret = ")
                                                                         (hs_string__ "ELIDED_STRING")))) >>
                                                  return_ (pair (fromInteger 0) mstate2))
                                            else (when ((mod_ instret (fromInteger 10000000)) == fromInteger 0)
                                                  (let mtime := mstate_mem_read_mtime mstate2 in
                                                   IO.putStrLn (app (GHC.Base.hs_string__ "[Forvis: instret = ") (app
                                                                     (hs_string__ "ELIDED_STRING") (app
                                                                      (GHC.Base.hs_string__ "; MTIME = ") (app
                                                                       (hs_string__ "ELIDED_STRING")
                                                                       (GHC.Base.hs_string__ "]"))))) >>
                                                   IO.hFlush IO.stdout) >>
                                                  (get_tty_input mstate2 >>=
                                                   (fun mstate3 =>
                                                      (if (run_state == Run_State_Running) : bool
                                                       then fetch_and_execute mstate3
                                                       else let resume := mstate_wfi_resume mstate3 in
                                                            let mstate3_a :=
                                                              if (resume) : bool
                                                              then mstate_run_state_write mstate3 Run_State_Running
                                                              else mstate3 in
                                                            return_ mstate3_a) >>=
                                                      (fun mstate4 =>
                                                         put_tty_output mstate4 >>=
                                                         (fun mstate5 =>
                                                            let run_state5 := mstate_run_state_read mstate5 in
                                                            let pc5 := mstate_pc_read mstate5 in
                                                            let run_state3 := mstate_run_state_read mstate3 in
                                                            let pc3 := mstate_pc_read mstate3 in
                                                            if (andb (pc3 == pc5) (andb (run_state3 ==
                                                                                         Run_State_Running) (run_state5
                                                                                         ==
                                                                                         Run_State_Running))) : bool
                                                            then (IO.putStrLn (app (GHC.Base.hs_string__
                                                                                    "Stopping due to self-loop at PC ")
                                                                                   (app (Numeric.showHex pc5
                                                                                         (GHC.Base.hs_string__ "")) (app
                                                                                         (GHC.Base.hs_string__
                                                                                          "; instret = ") (hs_string__
                                                                                          "ELIDED_STRING")))) >>
                                                                  return_ (pair (fromInteger 0) mstate5))
                                                            else (run_loop maxinstrs m_tohost_addr mstate5))))))).

(* External variables:
     Fetch Fetch_C Fetch_Trap Int Machine_State Mem_Result_Err Mem_Result_Ok N None
     Run_State_Running Run_State_WFI Some String andb app bool cons csr_addr_minstret
     exc_code_load_access_fault exec_instr exec_instr_C fromInteger fromIntegral
     funct3_LW hs_string__ instr_fetch mod_ mstate_csr_read
     mstate_mem_deq_console_output mstate_mem_enq_console_input mstate_mem_read
     mstate_mem_read_mtime mstate_mem_tick mstate_pc_read mstate_print
     mstate_priv_read mstate_run_state_read mstate_run_state_write
     mstate_verbosity_read mstate_wfi_resume negb op_zeze__ op_zg__ op_zgze__
     op_zgzg__ op_zgzgze__ op_zsze__ op_zt__ option pair return_ show_trap_exc_code
     take_interrupt_if_any tt when Data.Bits.op_zizazi__ Data.Bits.shiftR
     GHC.DeferredFix.deferredFix1 GHC.DeferredFix.deferredFix3 IO.Handle IO.IO
     IO.hFlush IO.hGetChar IO.hWaitForInput IO.putStr IO.putStrLn IO.stdin IO.stdout
     Numeric.showHex
*)

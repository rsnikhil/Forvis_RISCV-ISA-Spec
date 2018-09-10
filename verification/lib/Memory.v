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
Require Bit_Manipulation.
Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Data.Map.Internal.
Require Import GHC.Base.
Require Import GHC.Err.
Require Import GHC.Num.
Require Import GHC.Real.
Require Import Mem_Ops.
Import Data.Bits.Notations.

(* Converted type declarations: *)

Inductive Mem : Type
  := Mk_Mem : Data.Map.Internal.Map N N -> option (N * N)%type -> Mem.

Definition f_dm (arg_0__ : Mem) :=
  let 'Mk_Mem f_dm _ := arg_0__ in
  f_dm.

Definition f_reserved_addr (arg_0__ : Mem) :=
  let 'Mk_Mem _ f_reserved_addr := arg_0__ in
  f_reserved_addr.
(* Converted value declarations: *)

Definition addr_byte_list_to_addr_word_list
   : list (Int * N)%type -> list (N * N)%type :=
  fix addr_byte_list_to_addr_word_list arg_0__
        := let j_1__ := let 'a_b_s := arg_0__ in undefined in
           let j_5__ :=
             match arg_0__ with
             | cons (pair a0 b0) rest =>
                 if (((a0 Data.Bits..&.(**) fromInteger 3) == fromInteger 0)) : bool
                 then (let a := (fromIntegral a0) : N in
                       let w0 := Bit_Manipulation.zeroExtend_u8_to_u32 b0 in
                       cons (pair a w0) (addr_byte_list_to_addr_word_list rest)) else
                 j_1__
             | _ => j_1__
             end in
           let j_11__ :=
             match arg_0__ with
             | cons (pair a0 b0) (cons (pair a1 b1) rest) =>
                 if (andb ((a0 Data.Bits..&.(**) fromInteger 3) == fromInteger 0) ((a0 +
                            fromInteger 1) ==
                           a1)) : bool
                 then (let a := (fromIntegral a0) : N in
                       let w1 := Bit_Manipulation.zeroExtend_u8_to_u32 b1 in
                       let w0 := Bit_Manipulation.zeroExtend_u8_to_u32 b0 in
                       let w :=
                         ((Data.Bits.shiftL w1 (fromInteger 8)) Data.Bits..|.(**)
                          (Data.Bits.shiftL w0 (fromInteger 0))) in
                       cons (pair a w) (addr_byte_list_to_addr_word_list rest)) else
                 j_5__
             | _ => j_5__
             end in
           let j_18__ :=
             match arg_0__ with
             | cons (pair a0 b0) (cons (pair a1 b1) (cons (pair a2 b2) rest)) =>
                 if (andb ((a0 Data.Bits..&.(**) fromInteger 3) == fromInteger 0) (andb ((a0 +
                                                                                          fromInteger 1) ==
                                                                                         a1) ((a0 + fromInteger 2) ==
                                                                                         a2))) : bool
                 then (let a := (fromIntegral a0) : N in
                       let w2 := Bit_Manipulation.zeroExtend_u8_to_u32 b2 in
                       let w1 := Bit_Manipulation.zeroExtend_u8_to_u32 b1 in
                       let w0 := Bit_Manipulation.zeroExtend_u8_to_u32 b0 in
                       let w :=
                         (((Data.Bits.shiftL w2 (fromInteger 16)) Data.Bits..|.(**)
                           (Data.Bits.shiftL w1 (fromInteger 8))) Data.Bits..|.(**)
                          (Data.Bits.shiftL w0 (fromInteger 0))) in
                       cons (pair a w) (addr_byte_list_to_addr_word_list rest)) else
                 j_11__
             | _ => j_11__
             end in
           match arg_0__ with
           | nil => nil
           | cons (pair a0 b0) (cons (pair a1 b1) (cons (pair a2 b2) (cons (pair a3 b3)
              rest))) =>
               if (andb ((a0 Data.Bits..&.(**) fromInteger 3) == fromInteger 0) (andb ((a0 +
                                                                                        fromInteger 1) ==
                                                                                       a1) (andb ((a0 + fromInteger 2)
                                                                                                  ==
                                                                                                  a2) ((a0 +
                                                                                                   fromInteger 3) ==
                                                                                                  a3)))) : bool
               then (let a := (fromIntegral a0) : N in
                     let w3 := Bit_Manipulation.zeroExtend_u8_to_u32 b3 in
                     let w2 := Bit_Manipulation.zeroExtend_u8_to_u32 b2 in
                     let w1 := Bit_Manipulation.zeroExtend_u8_to_u32 b1 in
                     let w0 := Bit_Manipulation.zeroExtend_u8_to_u32 b0 in
                     let w :=
                       ((((Data.Bits.shiftL w3 (fromInteger 24)) Data.Bits..|.(**)
                          (Data.Bits.shiftL w2 (fromInteger 16))) Data.Bits..|.(**)
                         (Data.Bits.shiftL w1 (fromInteger 8))) Data.Bits..|.(**)
                        (Data.Bits.shiftL w0 (fromInteger 0))) in
                     cons (pair a w) (addr_byte_list_to_addr_word_list rest)) else
               j_18__
           | _ => j_18__
           end.

Definition mkMem : list (Int * N)%type -> Mem :=
  fun addr_byte_list =>
    let addr_word_list := addr_byte_list_to_addr_word_list addr_byte_list in
    Mk_Mem (Data.Map.Internal.fromList addr_word_list) None.

Definition addrs_overlap : N -> N -> N -> N -> bool :=
  fun a1 a2 r1 r2 =>
    (orb (andb (a1 <= r1) (r1 <= a2)) (andb (a1 <= r2) (r2 <= a2))).

Definition mem_num_entries : Mem -> Int :=
  fun mem => let dm := f_dm mem in Data.Map.Internal.size dm.

Definition uninitialized_word :=
  fromInteger 0 : N.

Definition mem_read : Mem -> InstrField -> N -> (Mem_Result * Mem)%type :=
  fun mem funct3 addr =>
    let addr_w := (addr Data.Bits..&.(**) (Data.Bits.complement (fromInteger 3))) in
    let dm := f_dm mem in
    let fn_read_word :=
      fun a =>
        match (Data.Map.Internal.lookup a dm) with
        | Some w => w
        | None => uninitialized_word
        end in
    let omv_w0 := fn_read_word addr_w in
    let omv_w1 := fn_read_word (addr_w + fromInteger 4) in
    let 'pair ldv_w1 ldv_w0 := (if (orb (funct3 == funct3_LB) (funct3 ==
                                         funct3_LBU)) : bool
                                then let scrut_17__ := (addr Data.Bits..&.(**) fromInteger 3) in
                                     let 'num_18__ := scrut_17__ in
                                     if num_18__ == fromInteger 0 : bool
                                     then pair (fromInteger 0) ((Data.Bits.shiftR omv_w0 (fromInteger 0))
                                                Data.Bits..&.(**)
                                                fromInteger 255) else
                                     let 'num_19__ := scrut_17__ in
                                     if num_19__ == fromInteger 1 : bool
                                     then pair (fromInteger 0) ((Data.Bits.shiftR omv_w0 (fromInteger 8))
                                                Data.Bits..&.(**)
                                                fromInteger 255) else
                                     let 'num_20__ := scrut_17__ in
                                     if num_20__ == fromInteger 2 : bool
                                     then pair (fromInteger 0) ((Data.Bits.shiftR omv_w0 (fromInteger 16))
                                                Data.Bits..&.(**)
                                                fromInteger 255) else
                                     let 'num_21__ := scrut_17__ in
                                     if num_21__ == fromInteger 3 : bool
                                     then pair (fromInteger 0) ((Data.Bits.shiftR omv_w0 (fromInteger 24))
                                                Data.Bits..&.(**)
                                                fromInteger 255) else
                                     patternFailure else
                                if (orb (funct3 == funct3_LH) (funct3 == funct3_LHU)) : bool
                                then let scrut_9__ := (addr Data.Bits..&.(**) fromInteger 3) in
                                     let 'num_10__ := scrut_9__ in
                                     if num_10__ == fromInteger 0 : bool
                                     then pair (fromInteger 0) ((Data.Bits.shiftR omv_w0 (fromInteger 0))
                                                Data.Bits..&.(**)
                                                fromInteger 65535) else
                                     let 'num_11__ := scrut_9__ in
                                     if num_11__ == fromInteger 2 : bool
                                     then pair (fromInteger 0) ((Data.Bits.shiftR omv_w0 (fromInteger 16))
                                                Data.Bits..&.(**)
                                                fromInteger 65535) else
                                     patternFailure else
                                if (orb (funct3 == funct3_LW) (funct3 == funct3_LWU)) : bool
                                then pair (fromInteger 0) omv_w0 else
                                if (funct3 == funct3_LD) : bool then pair omv_w1 omv_w0 else
                                patternFailure) in
    let u64 := Bit_Manipulation.bitconcat_u32_u32_to_u64 ldv_w1 ldv_w0 in
    if (is_LOAD_aligned funct3 addr) : bool
    then pair (Mem_Result_Ok u64) mem
    else pair (Mem_Result_Err exc_code_load_addr_misaligned) mem.

Definition mem_amo
   : Mem ->
     N ->
     InstrField ->
     InstrField -> InstrField -> InstrField -> N -> (Mem_Result * Mem)%type :=
  fun mem addr funct3 msbs5 aq rl stv_d =>
    let 'pair a1 a2 := pair addr (if (funct3 == funct3_AMO_D) : bool
                             then (addr + fromInteger 7)
                             else (addr + fromInteger 3)) in
    let addr_w := (addr Data.Bits..&.(**) (Data.Bits.complement (fromInteger 3))) in
    let m_reserved_addr := f_reserved_addr mem in
    let reserved_addr_hit :=
      match m_reserved_addr with
      | None => false
      | Some (pair r1 r2) => addrs_overlap a1 a2 r1 r2
      end in
    let m_reserved_addr' :=
      if (msbs5 == msbs5_AMO_LR) : bool then Some (pair a1 a2) else
      if (msbs5 == msbs5_AMO_SC) : bool then None else
      if reserved_addr_hit : bool then None else
      m_reserved_addr in
    let dm := f_dm mem in
    let fn_read_word :=
      fun a =>
        match (Data.Map.Internal.lookup a dm) with
        | Some w => w
        | None => uninitialized_word
        end in
    let omv_w0 := fn_read_word addr_w in
    let omv_w1 := fn_read_word (addr_w + fromInteger 4) in
    let omv_d := Bit_Manipulation.bitconcat_u32_u32_to_u64 omv_w1 omv_w0 in
    let ldv :=
      if (msbs5 == msbs5_AMO_SC) : bool
      then if reserved_addr_hit : bool
           then fromInteger 0
           else fromInteger 1 else
      if (funct3 == funct3_AMO_W) : bool
      then Bit_Manipulation.bitconcat_u32_u32_to_u64 (fromInteger 0) omv_w0 else
      if (funct3 == funct3_AMO_D) : bool then omv_d else
      patternFailure in
    let stv_w1 :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR stv_d (fromInteger 32)) in
    let stv_w0 := Bit_Manipulation.trunc_u64_to_u32 stv_d in
    let 'pair nmv_w1 nmv_w0 := (if (msbs5 == msbs5_AMO_SC) : bool
                                then pair stv_w1 stv_w0 else
                                if (msbs5 == msbs5_AMO_SWAP) : bool then pair stv_w1 stv_w0 else
                                if (msbs5 == msbs5_AMO_ADD) : bool
                                then (if (funct3 == funct3_AMO_W) : bool
                                      then let z_w :=
                                             Bit_Manipulation.cvt_s32_to_u32 ((Bit_Manipulation.cvt_u32_to_s32 omv_w0) +
                                                                              (Bit_Manipulation.cvt_u32_to_s32
                                                                               stv_w0)) in
                                           pair (fromInteger 0) z_w
                                      else let z_d :=
                                             Bit_Manipulation.cvt_s64_to_u64 ((Bit_Manipulation.cvt_u64_to_s64 omv_d) +
                                                                              (Bit_Manipulation.cvt_u64_to_s64
                                                                               stv_d)) in
                                           pair (Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR z_d (fromInteger
                                                                                                          32)))
                                                (Bit_Manipulation.trunc_u64_to_u32 z_d)) else
                                if (msbs5 == msbs5_AMO_AND) : bool
                                then pair (omv_w1 Data.Bits..&.(**) stv_w1) (omv_w0 Data.Bits..&.(**)
                                           stv_w0) else
                                if (msbs5 == msbs5_AMO_OR) : bool
                                then pair (omv_w1 Data.Bits..|.(**) stv_w1) (omv_w0 Data.Bits..|.(**)
                                           stv_w0) else
                                if (msbs5 == msbs5_AMO_XOR) : bool
                                then pair (Data.Bits.xor omv_w1 stv_w1) (Data.Bits.xor omv_w0 stv_w0) else
                                if (msbs5 == msbs5_AMO_MAX) : bool
                                then (if (funct3 == funct3_AMO_W) : bool
                                      then let z_w :=
                                             if ((Bit_Manipulation.cvt_u32_to_s32 omv_w0) >
                                                 (Bit_Manipulation.cvt_u32_to_s32 stv_w0)) : bool
                                             then omv_w0
                                             else stv_w0 in
                                           pair (fromInteger 0) z_w
                                      else if ((Bit_Manipulation.cvt_u64_to_s64 omv_d) >
                                               (Bit_Manipulation.cvt_u64_to_s64 stv_d)) : bool
                                           then pair omv_w1 omv_w0
                                           else pair stv_w1 stv_w0) else
                                if (msbs5 == msbs5_AMO_MIN) : bool
                                then (if (funct3 == funct3_AMO_W) : bool
                                      then let z_w :=
                                             if ((Bit_Manipulation.cvt_u32_to_s32 omv_w0) <
                                                 (Bit_Manipulation.cvt_u32_to_s32 stv_w0)) : bool
                                             then omv_w0
                                             else stv_w0 in
                                           pair (fromInteger 0) z_w
                                      else if ((Bit_Manipulation.cvt_u64_to_s64 omv_d) <
                                               (Bit_Manipulation.cvt_u64_to_s64 stv_d)) : bool
                                           then pair omv_w1 omv_w0
                                           else pair stv_w1 stv_w0) else
                                if (msbs5 == msbs5_AMO_MAXU) : bool
                                then (if (funct3 == funct3_AMO_W) : bool
                                      then let z_w := if (omv_w0 > stv_w0) : bool then omv_w0 else stv_w0 in
                                           pair (fromInteger 0) z_w
                                      else if (omv_d > stv_d) : bool
                                           then pair omv_w1 omv_w0
                                           else pair stv_w1 stv_w0) else
                                if (msbs5 == msbs5_AMO_MINU) : bool
                                then (if (funct3 == funct3_AMO_W) : bool
                                      then let z_w := if (omv_w0 < stv_w0) : bool then omv_w0 else stv_w0 in
                                           pair (fromInteger 0) z_w
                                      else if (omv_d < stv_d) : bool
                                           then pair omv_w1 omv_w0
                                           else pair stv_w1 stv_w0) else
                                patternFailure) in
    let dm' :=
      if (msbs5 == msbs5_AMO_LR) : bool then dm else
      if (andb (msbs5 == msbs5_AMO_SC) (negb reserved_addr_hit)) : bool then dm else
      if (funct3 == funct3_AMO_W) : bool
      then Data.Map.Internal.insert addr_w nmv_w0 dm else
      if (funct3 == funct3_AMO_D) : bool
      then (let dm1 := Data.Map.Internal.insert addr_w nmv_w0 dm in
            let dm2 := Data.Map.Internal.insert (addr_w + fromInteger 4) nmv_w1 dm1 in
            dm2) else
      patternFailure in
    if (is_AMO_aligned funct3 addr) : bool
    then pair (Mem_Result_Ok ldv) (Mk_Mem dm' m_reserved_addr')
    else pair (Mem_Result_Err exc_code_store_AMO_addr_misaligned) mem.

Definition mem_write : Mem -> InstrField -> N -> N -> (Mem_Result * Mem)%type :=
  fun mem funct3 addr stv =>
    let 'pair a1 a2 := (if (funct3 == funct3_SD) : bool
                          then pair addr (addr + fromInteger 7)
                          else pair addr (addr + fromInteger 3)) in
    let addr_w := (addr Data.Bits..&.(**) (Data.Bits.complement (fromInteger 3))) in
    let m_reserved_addr := f_reserved_addr mem in
    let m_reserved_addr' :=
      match m_reserved_addr with
      | None => None
      | _ =>
          match m_reserved_addr with
          | Some (pair r1 r2) =>
              if addrs_overlap a1 a2 r1 r2 : bool
              then None
              else m_reserved_addr
          | _ => patternFailure
          end
      end in
    let dm := f_dm mem in
    let fn_read_word :=
      fun a =>
        match (Data.Map.Internal.lookup a dm) with
        | Some w => w
        | None => uninitialized_word
        end in
    let omv_w0 := fn_read_word addr_w in
    let omv_w1 := fn_read_word (addr_w + fromInteger 4) in
    let stv_w1 :=
      Bit_Manipulation.trunc_u64_to_u32 (Data.Bits.shiftR stv (fromInteger 32)) in
    let stv_w0 := Bit_Manipulation.trunc_u64_to_u32 stv in
    let dm' :=
      if (funct3 == funct3_SB) : bool
      then let scrut_25__ := (addr Data.Bits..&.(**) fromInteger 3) in
           let 'num_26__ := scrut_25__ in
           if num_26__ == fromInteger 0 : bool
           then Data.Map.Internal.insert addr_w ((omv_w0 Data.Bits..&.(**)
                                                  fromInteger 4294967040) Data.Bits..|.(**)
                                                 (Data.Bits.shiftL (stv_w0 Data.Bits..&.(**) fromInteger 255)
                                                  (fromInteger 0))) dm else
           let 'num_27__ := scrut_25__ in
           if num_27__ == fromInteger 1 : bool
           then Data.Map.Internal.insert addr_w ((omv_w0 Data.Bits..&.(**)
                                                  fromInteger 4294902015) Data.Bits..|.(**)
                                                 (Data.Bits.shiftL (stv_w0 Data.Bits..&.(**) fromInteger 255)
                                                  (fromInteger 8))) dm else
           let 'num_28__ := scrut_25__ in
           if num_28__ == fromInteger 2 : bool
           then Data.Map.Internal.insert addr_w ((omv_w0 Data.Bits..&.(**)
                                                  fromInteger 4278255615) Data.Bits..|.(**)
                                                 (Data.Bits.shiftL (stv_w0 Data.Bits..&.(**) fromInteger 255)
                                                  (fromInteger 16))) dm else
           let 'num_29__ := scrut_25__ in
           if num_29__ == fromInteger 3 : bool
           then Data.Map.Internal.insert addr_w ((omv_w0 Data.Bits..&.(**)
                                                  fromInteger 16777215) Data.Bits..|.(**)
                                                 (Data.Bits.shiftL (stv_w0 Data.Bits..&.(**) fromInteger 255)
                                                  (fromInteger 24))) dm else
           patternFailure else
      if (funct3 == funct3_SH) : bool
      then let scrut_17__ := (addr Data.Bits..&.(**) fromInteger 3) in
           let 'num_18__ := scrut_17__ in
           if num_18__ == fromInteger 0 : bool
           then Data.Map.Internal.insert addr_w ((omv_w0 Data.Bits..&.(**)
                                                  fromInteger 4294901760) Data.Bits..|.(**)
                                                 (Data.Bits.shiftL (stv_w0 Data.Bits..&.(**) fromInteger 65535)
                                                  (fromInteger 0))) dm else
           let 'num_19__ := scrut_17__ in
           if num_19__ == fromInteger 2 : bool
           then Data.Map.Internal.insert addr_w ((omv_w0 Data.Bits..&.(**)
                                                  fromInteger 65535) Data.Bits..|.(**)
                                                 (Data.Bits.shiftL (stv_w0 Data.Bits..&.(**) fromInteger 65535)
                                                  (fromInteger 16))) dm else
           patternFailure else
      if (funct3 == funct3_SW) : bool
      then Data.Map.Internal.insert addr_w stv_w0 dm else
      if (funct3 == funct3_SD) : bool
      then (let dm1 := Data.Map.Internal.insert addr_w stv_w0 dm in
            let dm2 := Data.Map.Internal.insert (addr_w + fromInteger 4) stv_w1 dm1 in
            dm2) else
      patternFailure in
    if (is_STORE_aligned funct3 addr) : bool
    then pair (Mem_Result_Ok (fromInteger 0)) (Mk_Mem dm' m_reserved_addr')
    else pair (Mem_Result_Err exc_code_store_AMO_addr_misaligned) mem.

(* External variables:
     InstrField Int Mem_Result Mem_Result_Err Mem_Result_Ok N None Some andb bool
     cons exc_code_load_addr_misaligned exc_code_store_AMO_addr_misaligned false
     fromInteger fromIntegral funct3_AMO_D funct3_AMO_W funct3_LB funct3_LBU
     funct3_LD funct3_LH funct3_LHU funct3_LW funct3_LWU funct3_SB funct3_SD
     funct3_SH funct3_SW is_AMO_aligned is_LOAD_aligned is_STORE_aligned list
     msbs5_AMO_ADD msbs5_AMO_AND msbs5_AMO_LR msbs5_AMO_MAX msbs5_AMO_MAXU
     msbs5_AMO_MIN msbs5_AMO_MINU msbs5_AMO_OR msbs5_AMO_SC msbs5_AMO_SWAP
     msbs5_AMO_XOR negb nil op_zeze__ op_zg__ op_zl__ op_zlze__ op_zp__ op_zt__
     option orb pair patternFailure undefined
     Bit_Manipulation.bitconcat_u32_u32_to_u64 Bit_Manipulation.cvt_s32_to_u32
     Bit_Manipulation.cvt_s64_to_u64 Bit_Manipulation.cvt_u32_to_s32
     Bit_Manipulation.cvt_u64_to_s64 Bit_Manipulation.trunc_u64_to_u32
     Bit_Manipulation.zeroExtend_u8_to_u32 Data.Bits.complement Data.Bits.op_zizazi__
     Data.Bits.op_zizbzi__ Data.Bits.shiftL Data.Bits.shiftR Data.Bits.xor
     Data.Map.Internal.Map Data.Map.Internal.fromList Data.Map.Internal.insert
     Data.Map.Internal.lookup Data.Map.Internal.size
*)

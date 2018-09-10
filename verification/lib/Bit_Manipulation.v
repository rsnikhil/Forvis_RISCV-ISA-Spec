(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Converted imports: *)

Require Import Coq.Numbers.BinNums.
Require Data.Bits.
Require Import GHC.Base.
Require Import GHC.Num.
Require Import GHC.Real.
Import Data.Bits.Notations.

(* No type declarations to convert. *)
(* Converted value declarations: *)

Definition bitSlice {a} `{Data.Bits.Bits a} `{Num a} : a -> Int -> Int -> a :=
  fun x hi lo =>
    (Data.Bits.shiftR x lo) Data.Bits..&.(**)
    (Data.Bits.complement (Data.Bits.shiftL (negate (fromInteger 1)) (hi +
                                             fromInteger 1 -
                                             lo))).

Definition bitconcat_u16_u16_to_u32 : N -> N -> N :=
  fun u16_lo u16_hi =>
    let u32_hi := fromIntegral u16_hi in
    let u32_lo := fromIntegral u16_lo in
    let u32 :=
      (Data.Bits.shiftL u32_hi (fromInteger 16)) Data.Bits..|.(**) u32_lo in
    u32.

Definition clear_bit : N -> Int -> N :=
  fun x position =>
    let mask := fromInteger 1 : N in
    let mask' := Data.Bits.complement (Data.Bits.shiftL mask position) in
    let x' := (x Data.Bits..&.(**) mask') in x'.

Definition cvt_Integer_to_u32 : Z -> N :=
  fun i => fromIntegral i.

Definition cvt_s32_to_u32 : Z -> N :=
  fun s => fromIntegral s.

Definition cvt_s64_to_u64 : Z -> N :=
  fun s => fromIntegral s.

Definition cvt_u32_to_Int : N -> Int :=
  fun u => fromIntegral u.

Definition cvt_u32_to_s32 : N -> Z :=
  fun u => fromIntegral u.

Definition cvt_u64_to_Int : N -> Int :=
  fun u => fromIntegral u.

Definition cvt_u64_to_s64 : N -> Z :=
  fun u => fromIntegral u.

Definition cvt_u64_to_u8 : N -> N :=
  fun u64 => fromIntegral u64.

Definition set_bit : N -> Int -> N :=
  fun x position =>
    let mask := fromInteger 1 : N in
    let mask' := Data.Bits.shiftL mask position in
    let x' := (x Data.Bits..|.(**) mask') in x'.

Definition signExtend : N -> Int -> N :=
  fun word n =>
    let fill_bits :=
      (Data.Bits.shiftL (fromInteger 1) (fromInteger 64 - n)) - fromInteger 1 in
    let fill_mask := Data.Bits.shiftL fill_bits n in
    let word' :=
      if Data.Bits.testBit word (n - fromInteger 1) : bool
      then word Data.Bits..|.(**) fill_mask
      else word Data.Bits..&.(**) (Data.Bits.complement fill_mask) in
    let u64 := if n >= fromInteger 64 : bool then word else word' in u64.

Definition signExtend_bit_in_u32 : N -> Int -> N :=
  fun word n =>
    let fill_bits :=
      (Data.Bits.shiftL (fromInteger 1) (fromInteger 32 - n)) - fromInteger 1 in
    let fill_mask := Data.Bits.shiftL fill_bits n in
    let word' :=
      if Data.Bits.testBit word (n - fromInteger 1) : bool
      then word Data.Bits..|.(**) fill_mask
      else word Data.Bits..&.(**) (Data.Bits.complement fill_mask) in
    let u32 := if n >= fromInteger 32 : bool then word else word' in u32.

Definition signExtend_s32_to_u64 : Z -> N :=
  fun s32 =>
    let s64 := (fromIntegral s32) : Z in let u64 := (fromIntegral s64) : N in u64.

Definition signExtend_u16_to_u64 : N -> N :=
  fun u16 =>
    let s16 := (fromIntegral u16) : Z in
    let s64 := (fromIntegral s16) : Z in let u64 := (fromIntegral s64) : N in u64.

Definition signExtend_u32_to_u64 : N -> N :=
  fun u32 =>
    let s32 := (fromIntegral u32) : Z in
    let s64 := (fromIntegral s32) : Z in let u64 := (fromIntegral s64) : N in u64.

Definition signExtend_u8_to_u64 : N -> N :=
  fun u8 =>
    let s8 := (fromIntegral u8) : Z in
    let s64 := (fromIntegral s8) : Z in let u64 := (fromIntegral s64) : N in u64.

Definition trunc_s64_to_s32 : Z -> Z :=
  fun s64 => fromIntegral s64.

Definition trunc_u64_to_s32 : N -> Z :=
  fun u64 =>
    let u32 := (fromIntegral u64) : N in let s32 := (fromIntegral u32) : Z in s32.

Definition trunc_u64_to_u16 : N -> N :=
  fun u64 => fromIntegral u64.

Definition trunc_u64_to_u32 : N -> N :=
  fun u64 => fromIntegral u64.

Definition zeroExtend_u16_to_u32 : N -> N :=
  fun u16 => fromIntegral u16.

Definition zeroExtend_u16_to_u64 : N -> N :=
  fun u16 => fromIntegral u16.

Definition zeroExtend_u32_to_u64 : N -> N :=
  fun u32 => fromIntegral u32.

Definition bitconcat_u32_u32_to_u64 : N -> N -> N :=
  fun w1 w0 =>
    let d1 := zeroExtend_u32_to_u64 w1 in
    let d0 := zeroExtend_u32_to_u64 w0 in
    ((Data.Bits.shiftL d1 (fromInteger 32)) Data.Bits..|.(**) d0).

Definition zeroExtend_u8_to_u32 : N -> N :=
  fun u8 => fromIntegral u8.

Definition zeroExtend_u8_to_u64 : N -> N :=
  fun u8 => fromIntegral u8.

(* External variables:
     Int N Num Z bool fromInteger fromIntegral negate op_zgze__ op_zm__ op_zp__
     Data.Bits.Bits Data.Bits.complement Data.Bits.op_zizazi__ Data.Bits.op_zizbzi__
     Data.Bits.shiftL Data.Bits.shiftR Data.Bits.testBit
*)

-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module ALU where

-- ================================================================
-- This module defines various RISC-V ALU functions.

-- ================================================================
-- Standard Haskell imports

import Data.Bits

-- Project imports

import Bit_Utils

-- ================================================================
-- EQ: test for equality

{-# INLINE alu_eq #-}
alu_eq :: Int -> Integer -> Integer -> Bool
alu_eq    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 == u2)

-- ================================================================
-- NEQ: test for inequality

{-# INLINE alu_ne #-}
alu_ne :: Int -> Integer -> Integer -> Bool
alu_ne    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 /= u2)

-- ================================================================
-- LT: test for less-than (signed)

{-# INLINE alu_lt #-}
alu_lt :: Int -> Integer -> Integer -> Bool
alu_lt    xlen   rs1_val    rs2_val =
  let
    s1 = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2 = cvt_2s_comp_to_Integer  xlen  rs2_val
  in
    (s1 < s2)

-- ================================================================
-- GE: test for greater-than-or-equal (signed)

{-# INLINE alu_ge #-}
alu_ge :: Int -> Integer -> Integer -> Bool
alu_ge    xlen   rs1_val    rs2_val =
  let
    s1 = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2 = cvt_2s_comp_to_Integer  xlen  rs2_val
  in
    (s1 >= s2)

-- ================================================================
-- LTU: test for less-than (unsigned)

{-# INLINE alu_ltu #-}
alu_ltu :: Int -> Integer -> Integer -> Bool
alu_ltu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 < u2)

-- ================================================================
-- GEU: test for greater-than-or-equal (unsigned)

{-# INLINE alu_geu #-}
alu_geu :: Int -> Integer -> Integer -> Bool
alu_geu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 >= u2)

-- ================================================================
-- ADD: signed addition

{-# INLINE alu_add #-}
alu_add :: Int -> Integer -> Integer -> Integer
alu_add    xlen   rs1_val    rs2_val =
  let
    s1     = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2     = cvt_2s_comp_to_Integer  xlen  rs2_val
    result = s1 + s2
  in
    cvt_Integer_to_2s_comp  xlen  result

-- ================
-- ADDW: signed addition of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_addw #-}
alu_addw :: Integer -> Integer -> Integer
alu_addw    rs1_val    rs2_val =
  let
    result_32 = alu_add  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================
-- SUB: signed subtraction

{-# INLINE alu_sub #-}
alu_sub :: Int -> Integer -> Integer -> Integer
alu_sub    xlen   rs1_val    rs2_val =
  let
    s1     = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2     = cvt_2s_comp_to_Integer  xlen  rs2_val
    result = s1 - s2
  in
    cvt_Integer_to_2s_comp  xlen  result

-- ================
-- SUBW: signed subtraction of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_subw #-}
alu_subw :: Integer -> Integer -> Integer
alu_subw    rs1_val    rs2_val =
  let
    result_32 = alu_sub  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================
-- SLT: set if less-than, signed

{-# INLINE alu_slt #-}
alu_slt :: Int -> Integer -> Integer -> Integer
alu_slt    xlen   rs1_val    rs2_val =
  let
    s1     = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2     = cvt_2s_comp_to_Integer  xlen  rs2_val
    result = if (s1 < s2) then 1 else 0
  in
    cvt_Integer_to_2s_comp  xlen  result

-- ================================================================
-- SLTU: set if less-than, unsigned

{-# INLINE alu_sltu #-}
alu_sltu :: Int -> Integer -> Integer -> Integer
alu_sltu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
    result = if (u1 < u2) then 1 else 0
  in
    cvt_Integer_to_2s_comp  xlen  result

-- ================================================================
-- AND: bitwise AND

{-# INLINE alu_and #-}
alu_and :: Int -> Integer -> Integer -> Integer
alu_and    xlen   rs1_val    rs2_val = rs1_val  .&.  rs2_val

-- ================================================================
-- OR: bitwise OR

{-# INLINE alu_or #-}
alu_or :: Int -> Integer -> Integer -> Integer
alu_or    xlen   rs1_val    rs2_val = rs1_val  .|.  rs2_val

-- ================================================================
-- XOR: bitwise XOR

{-# INLINE alu_xor #-}
alu_xor :: Int -> Integer -> Integer -> Integer
alu_xor    xlen   rs1_val    rs2_val = xor  rs1_val  rs2_val

-- ================================================================
-- SLL: shift-left logical

{-# INLINE alu_sll #-}
alu_sll :: Int -> Integer -> Integer -> Integer
alu_sll    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1        = rs1_val .&. mask_xlen
    shamt     = cvt_Integer_to_Int (rs2_val .&. (if xlen == 32 then 0x1F else 0x3F))
    result    = (shiftL  u1  shamt) .&. mask_xlen
  in
    result

-- ================
-- SLLW: shift-left logical of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_sllw #-}
alu_sllw :: Integer -> Integer -> Integer
alu_sllw    rs1_val    rs2_val =
  let
    result_32 = alu_sll  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================
-- SRL: shift-right logical

{-# INLINE alu_srl #-}
alu_srl :: Int -> Integer -> Integer -> Integer
alu_srl    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1
    u1        = rs1_val .&. mask_xlen
    shamt     = cvt_Integer_to_Int (rs2_val .&. (if xlen == 32 then 0x1F else 0x3F))
    result    = (shiftR  u1  shamt) .&. mask_xlen
  in
    result

-- ================
-- SRLW: shift-right logical of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_srlw #-}
alu_srlw :: Integer -> Integer -> Integer
alu_srlw    rs1_val    rs2_val =
  let
    result_32 = alu_srl  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================
-- SRA: shift-right arithmetic

{-# INLINE alu_sra #-}
alu_sra :: Int -> Integer -> Integer -> Integer
alu_sra    xlen   rs1_val    rs2_val =
  let
    s1        = cvt_2s_comp_to_Integer  xlen  rs1_val
    shamt     = cvt_Integer_to_Int (rs2_val .&. (if xlen == 32 then 0x1F else 0x3F))
    result_i  = shiftR  s1  shamt
    result    = cvt_Integer_to_2s_comp  xlen  result_i
  in
    result

-- ================
-- SRAW: shift-right arithmetic of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_sraw #-}
alu_sraw :: Integer -> Integer -> Integer
alu_sraw    rs1_val    rs2_val =
  let
    result_32 = alu_sra  32  rs1_val  rs2_val
    result    = sign_extend  32  64  result_32
  in
    result

-- ================================================================
-- MUL: signed multiplication

{-# INLINE alu_mul #-}
alu_mul :: Int -> Integer -> Integer -> Integer
alu_mul    xlen   rs1_val    rs2_val =
  let
    s1       = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2       = cvt_2s_comp_to_Integer  xlen  rs2_val
    result_i = s1 * s2
  in
    cvt_Integer_to_2s_comp  xlen  result_i

-- ================
-- MULW: signed multiplication of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_mulw #-}
alu_mulw :: Integer -> Integer -> Integer
alu_mulw    rs1_val    rs2_val =
  let
    result = alu_mul  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result

-- ================================================================
-- MULH: signed multiplication, high bits

{-# INLINE alu_mulh #-}
alu_mulh :: Int -> Integer -> Integer -> Integer
alu_mulh    xlen   rs1_val    rs2_val =
  let
    s1        = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2        = cvt_2s_comp_to_Integer  xlen  rs2_val
    result_i  = s1 * s2

    result_2_xlen = cvt_Integer_to_2s_comp  (2 * xlen)  result_i
    result        = bitSlice  result_2_xlen  (xlen + xlen - 1)  xlen
  in
    result

-- ================================================================
-- MULHU: unsigned multiplication, high bits

{-# INLINE alu_mulhu #-}
alu_mulhu :: Int -> Integer -> Integer -> Integer
alu_mulhu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1
    u1        = rs1_val  .&.  mask_xlen
    u2        = rs2_val  .&.  mask_xlen
    result_i  = u1 * u2

    result    = bitSlice  result_i  (xlen + xlen - 1)  xlen
  in
    result

-- ================================================================
-- MULHSU: signed x unsigned multiplication, high bits

{-# INLINE alu_mulhsu #-}
alu_mulhsu :: Int -> Integer -> Integer -> Integer
alu_mulhsu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1
    s1        = cvt_2s_comp_to_Integer  xlen  rs1_val
    u2        = rs2_val  .&.  mask_xlen
    result_i  = s1 * u2

    result_2_xlen = cvt_Integer_to_2s_comp  (2 * xlen)  result_i
    result        = bitSlice  result_2_xlen  (xlen + xlen - 1)  xlen
  in
    result

-- ================================================================
-- DIV: signed division
-- Special case: signed division by zero => quotient all bits set.
-- Special case: signed division overflow, when most-negative integer (xlen-bit) is divided by -1 (xlen-bit).
--     => quotient is equal to the dividend.

{-# INLINE alu_div #-}
alu_div :: Int -> Integer -> Integer -> Integer
alu_div    xlen   rs1_val    rs2_val =
  let
    -- Take lower xlen bits of args
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1        = rs1_val  .&.  mask_xlen
    u2        = rs2_val  .&.  mask_xlen

    -- Interpret as signed integers
    s1  = cvt_2s_comp_to_Integer  xlen  u1
    s2  = cvt_2s_comp_to_Integer  xlen  u2

    most_neg = bit (xlen - 1)    -- 0x8000_0000 or 0x8000_0000_0000_0000
    neg_1    = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF

    result_i = if (u2 == 0) then
                 (-1)
               else if (u1 == most_neg) && (u2 == neg_1) then
                      s1
                    else
                      quot  s1  s2
    result   = cvt_Integer_to_2s_comp  xlen  result_i
  in
    result

-- ================
-- DIVW: signed division of lower 32 bits of args; only in RV64.
-- Divides lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_divw #-}
alu_divw :: Integer -> Integer -> Integer
alu_divw    rs1_val    rs2_val =
  let
    result_32 = alu_div  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================
-- DIVU: unsigned division
-- Special case: unsigned division by zero => quotient all bits set.
-- Unsigned division overflow cannot occur.

{-# INLINE alu_divu #-}
alu_divu :: Int -> Integer -> Integer -> Integer
alu_divu    xlen   rs1_val    rs2_val =
  let
    -- Take lower xlen bits of args
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFFF or 0xFFFF_FFFFF_FFFFF_FFFFF
    u1        = rs1_val  .&.  mask_xlen
    u2        = rs2_val  .&.  mask_xlen
    max_xlen  = (bit xlen) - 1    -- 0xFFFF_FFFFF or 0xFFFF_FFFFF_FFFFF_FFFFF
  in
    if (u2 == 0) then
      max_xlen
    else
      let
        result_i = quot  u1  u2
        result   = cvt_Integer_to_2s_comp  xlen  result_i
      in
        result

-- ================
-- DIVUW: unsigned division of lower 32 bits of args; only in RV64.
-- Divides lower 32 bits of the args, treating them as 32-bit unsigned integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_divuw #-}
alu_divuw :: Integer -> Integer -> Integer
alu_divuw    rs1_val    rs2_val =
  let
    result_32 = alu_divu  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================
-- REM: signed remainder

-- Special case: signed remainder by zero => dividend (numerator)

-- Special case: signed remainder overflow, when most-negative integer (xlen-bit) is divided by -1 (xlen-bit).
-- => remainder is 0

{-# INLINE alu_rem #-}
alu_rem :: Int -> Integer -> Integer -> Integer
alu_rem    xlen   rs1_val    rs2_val =
  let
    -- Take lower xlen bits of args
    mask_xlen = (bit xlen) - 1
    u1        = rs1_val  .&.  mask_xlen
    u2        = rs2_val  .&.  mask_xlen

    -- Interpret as signed integers
    s1 = cvt_2s_comp_to_Integer  xlen  u1
    s2 = cvt_2s_comp_to_Integer  xlen  u2

    most_neg    = shiftL  1  (xlen - 1)    -- 0x8000_0000 or 0x8000_0000_0000_0000
    neg_1       = (bit xlen) - 1

    result_i = if (u2 == 0) then
                 s1
               else if (u1 == most_neg) && (u2 == neg_1) then
                      0
                    else
                      rem  s1  s2
    result = cvt_Integer_to_2s_comp  xlen  result_i
  in
    result

-- ================
-- REMW: signed remainder of lower 32 bits of args; only in RV64.
-- Remainders lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_remw #-}
alu_remw :: Integer -> Integer -> Integer
alu_remw    rs1_val    rs2_val =
  let
    result_32 = alu_rem  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================
-- REMU: unsigned remainder

-- Special case: unsigned remainder by zero => dividend (numerator)

-- Unsigned remainder overflow cannot occur.

{-# INLINE alu_remu #-}
alu_remu :: Int -> Integer -> Integer -> Integer
alu_remu    xlen   rs1_val    rs2_val =
  let
    -- Take lower xlen bits of args
    mask_xlen = (bit xlen) - 1
    u1        = rs1_val  .&.  mask_xlen
    u2        = rs2_val  .&.  mask_xlen
  in
    if (u2 == 0) then
      u1
    else
      let
        result_i = rem  u1  u2
        result   = cvt_Integer_to_2s_comp  xlen  result_i
      in
        result

-- ================
-- REMUW: unsigned remainder of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit unsigned integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

{-# INLINE alu_remuw #-}
alu_remuw :: Integer -> Integer -> Integer
alu_remuw    rs1_val    rs2_val =
  let
    result_32 = alu_remu  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

-- ================================================================

-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
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

alu_eq :: Int -> Integer -> Integer -> Bool
alu_eq    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 == u2)

{-# INLINE alu_eq #-}

-- ================================================================
-- NEQ: test for inequality

alu_ne :: Int -> Integer -> Integer -> Bool
alu_ne    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 /= u2)

{-# INLINE alu_ne #-}

-- ================================================================
-- LT: test for less-than (signed)

alu_lt :: Int -> Integer -> Integer -> Bool
alu_lt    xlen   rs1_val    rs2_val =
  let
    s1 = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2 = cvt_2s_comp_to_Integer  xlen  rs2_val
  in
    (s1 < s2)

{-# INLINE alu_lt #-}

-- ================================================================
-- GE: test for greater-than-or-equal (signed)

alu_ge :: Int -> Integer -> Integer -> Bool
alu_ge    xlen   rs1_val    rs2_val =
  let
    s1 = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2 = cvt_2s_comp_to_Integer  xlen  rs2_val
  in
    (s1 >= s2)

{-# INLINE alu_ge #-}

-- ================================================================
-- LTU: test for less-than (unsigned)

alu_ltu :: Int -> Integer -> Integer -> Bool
alu_ltu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 < u2)

{-# INLINE alu_ltu #-}

-- ================================================================
-- GEU: test for greater-than-or-equal (unsigned)

alu_geu :: Int -> Integer -> Integer -> Bool
alu_geu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
  in
    (u1 >= u2)

{-# INLINE alu_geu #-}

-- ================================================================
-- ADD: signed addition

alu_add :: Int -> Integer -> Integer -> Integer
alu_add    xlen   rs1_val    rs2_val =
  let
    s1     = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2     = cvt_2s_comp_to_Integer  xlen  rs2_val
    result = s1 + s2
  in
    cvt_Integer_to_2s_comp  xlen  result

{-# INLINE alu_add #-}

-- ================
-- ADDW: signed addition of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_addw :: Integer -> Integer -> Integer
alu_addw    rs1_val    rs2_val =
  let
    result_32 = alu_add  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_addw #-}

-- ================================================================
-- SUB: signed subtraction

alu_sub :: Int -> Integer -> Integer -> Integer
alu_sub    xlen   rs1_val    rs2_val =
  let
    s1     = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2     = cvt_2s_comp_to_Integer  xlen  rs2_val
    result = s1 - s2
  in
    cvt_Integer_to_2s_comp  xlen  result

{-# INLINE alu_sub #-}

-- ================
-- SUBW: signed subtraction of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_subw :: Integer -> Integer -> Integer
alu_subw    rs1_val    rs2_val =
  let
    result_32 = alu_sub  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_subw #-}

-- ================================================================
-- SLT: set if less-than, signed

alu_slt :: Int -> Integer -> Integer -> Integer
alu_slt    xlen   rs1_val    rs2_val =
  let
    s1     = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2     = cvt_2s_comp_to_Integer  xlen  rs2_val
    result = if (s1 < s2) then 1 else 0
  in
    cvt_Integer_to_2s_comp  xlen  result

{-# INLINE alu_slt #-}

-- ================================================================
-- SLTU: set if less-than, unsigned

alu_sltu :: Int -> Integer -> Integer -> Integer
alu_sltu    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1     = rs1_val  .&.  mask_xlen
    u2     = rs2_val  .&.  mask_xlen
    result = if (u1 < u2) then 1 else 0
  in
    cvt_Integer_to_2s_comp  xlen  result

{-# INLINE alu_sltu #-}

-- ================================================================
-- AND: bitwise AND

alu_and :: Int -> Integer -> Integer -> Integer
alu_and    xlen   rs1_val    rs2_val = rs1_val  .&.  rs2_val

{-# INLINE alu_and #-}

-- ================================================================
-- OR: bitwise OR

alu_or :: Int -> Integer -> Integer -> Integer
alu_or    xlen   rs1_val    rs2_val = rs1_val  .|.  rs2_val

{-# INLINE alu_or #-}

-- ================================================================
-- XOR: bitwise XOR

alu_xor :: Int -> Integer -> Integer -> Integer
alu_xor    xlen   rs1_val    rs2_val = xor  rs1_val  rs2_val

{-# INLINE alu_xor #-}

-- ================================================================
-- SLL: shift-left logical

alu_sll :: Int -> Integer -> Integer -> Integer
alu_sll    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1    -- 0xFFFF_FFFF or 0xFFFF_FFFF_FFFF_FFFF
    u1        = rs1_val .&. mask_xlen
    shamt     = cvt_Integer_to_Int (rs2_val .&. (if xlen == 32 then 0x1F else 0x3F))
    result    = (shiftL  u1  shamt) .&. mask_xlen
  in
    result

{-# INLINE alu_sll #-}

-- ================
-- SLLW: shift-left logical of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_sllw :: Integer -> Integer -> Integer
alu_sllw    rs1_val    rs2_val =
  let
    result_32 = alu_sll  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_sllw #-}

-- ================================================================
-- SRL: shift-right logical

alu_srl :: Int -> Integer -> Integer -> Integer
alu_srl    xlen   rs1_val    rs2_val =
  let
    mask_xlen = (bit xlen) - 1
    u1        = rs1_val .&. mask_xlen
    shamt     = cvt_Integer_to_Int (rs2_val .&. (if xlen == 32 then 0x1F else 0x3F))
    result    = (shiftR  u1  shamt) .&. mask_xlen
  in
    result

{-# INLINE alu_srl #-}

-- ================
-- SRLW: shift-right logical of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_srlw :: Integer -> Integer -> Integer
alu_srlw    rs1_val    rs2_val =
  let
    result_32 = alu_srl  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_srlw #-}

-- ================================================================
-- SRA: shift-right arithmetic

alu_sra :: Int -> Integer -> Integer -> Integer
alu_sra    xlen   rs1_val    rs2_val =
  let
    s1        = cvt_2s_comp_to_Integer  xlen  rs1_val
    shamt     = cvt_Integer_to_Int (rs2_val .&. (if xlen == 32 then 0x1F else 0x3F))
    result_i  = shiftR  s1  shamt
    result    = cvt_Integer_to_2s_comp  xlen  result_i
  in
    result

{-# INLINE alu_sra #-}

-- ================
-- SRAW: shift-right arithmetic of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_sraw :: Integer -> Integer -> Integer
alu_sraw    rs1_val    rs2_val =
  let
    result_32 = alu_sra  32  rs1_val  rs2_val
    result    = sign_extend  32  64  result_32
  in
    result

{-# INLINE alu_sraw #-}

-- ================================================================
-- MUL: signed multiplication

alu_mul :: Int -> Integer -> Integer -> Integer
alu_mul    xlen   rs1_val    rs2_val =
  let
    s1       = cvt_2s_comp_to_Integer  xlen  rs1_val
    s2       = cvt_2s_comp_to_Integer  xlen  rs2_val
    result_i = s1 * s2
  in
    cvt_Integer_to_2s_comp  xlen  result_i

{-# INLINE alu_mul #-}

-- ================
-- MULW: signed multiplication of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_mulw :: Integer -> Integer -> Integer
alu_mulw    rs1_val    rs2_val =
  let
    result = alu_mul  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result

{-# INLINE alu_mulw #-}

-- ================================================================
-- MULH: signed multiplication, high bits

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

{-# INLINE alu_mulh #-}

-- ================================================================
-- MULHU: unsigned multiplication, high bits

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

{-# INLINE alu_mulhu #-}

-- ================================================================
-- MULHSU: signed x unsigned multiplication, high bits

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

{-# INLINE alu_mulhsu #-}

-- ================================================================
-- DIV: signed division
-- Special case: signed division by zero => quotient all bits set.
-- Special case: signed division overflow, when most-negative integer (xlen-bit) is divided by -1 (xlen-bit).
--     => quotient is equal to the dividend.

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

{-# INLINE alu_div #-}

-- ================
-- DIVW: signed division of lower 32 bits of args; only in RV64.
-- Divides lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_divw :: Integer -> Integer -> Integer
alu_divw    rs1_val    rs2_val =
  let
    result_32 = alu_div  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_divw #-}

-- ================================================================
-- DIVU: unsigned division
-- Special case: unsigned division by zero => quotient all bits set.
-- Unsigned division overflow cannot occur.

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

{-# INLINE alu_divu #-}

-- ================
-- DIVUW: unsigned division of lower 32 bits of args; only in RV64.
-- Divides lower 32 bits of the args, treating them as 32-bit unsigned integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_divuw :: Integer -> Integer -> Integer
alu_divuw    rs1_val    rs2_val =
  let
    result_32 = alu_divu  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_divuw #-}

-- ================================================================
-- REM: signed remainder

-- Special case: signed remainder by zero => dividend (numerator)

-- Special case: signed remainder overflow, when most-negative integer (xlen-bit) is divided by -1 (xlen-bit).
-- => remainder is 0

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

{-# INLINE alu_rem #-}

-- ================
-- REMW: signed remainder of lower 32 bits of args; only in RV64.
-- Remainders lower 32 bits of the args, treating them as 32-bit signed integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_remw :: Integer -> Integer -> Integer
alu_remw    rs1_val    rs2_val =
  let
    result_32 = alu_rem  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_remw #-}

-- ================================================================
-- REMU: unsigned remainder

-- Special case: unsigned remainder by zero => dividend (numerator)

-- Unsigned remainder overflow cannot occur.

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

{-# INLINE alu_remu #-}

-- ================
-- REMUW: unsigned remainder of lower 32 bits of args; only in RV64.
-- Operates on lower 32 bits of the args, treating them as 32-bit unsigned integers.
-- Returns sign-extension to 64 bits of lower 32 bits of result.

alu_remuw :: Integer -> Integer -> Integer
alu_remuw    rs1_val    rs2_val =
  let
    result_32 = alu_remu  32  rs1_val  rs2_val
  in
    sign_extend  32  64  result_32

{-# INLINE alu_remuw #-}

-- ================================================================

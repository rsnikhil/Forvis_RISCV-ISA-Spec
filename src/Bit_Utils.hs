-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Bit_Utils where

-- ================================================================
-- This module defines some utilities on bit vectors represented as Integers

-- ================================================================
-- Standard Haskell imports

import Data.Bits
import Data.Int
import Data.Word    -- For Wordxx type (unsigned fixed-width ints)
import Numeric (showHex, readHex)

-- Project imports

-- None

-- ================================================================
-- This function corresponds to Verilog's  [hi:lo]  operator
-- Result has the bits in the lsbs, i.e., result [hi-lo: 0]

bitSlice :: Integer -> Int -> Int -> Integer
bitSlice  x  hi  lo = (shiftR  x  lo) .&. (complement $ shiftL (-1) (hi + 1 - lo))

{-# INLINE bitSlice #-}

-- ================================================================
-- Interpret n LSBs (least-significant bits) of Integer x (which
-- should always be non-negative) as a 2's-complement representation,
-- and return the corresponding actual Integer.

{-# INLINE cvt_2s_comp_to_Integer #-}
cvt_2s_comp_to_Integer :: Int -> Integer -> Integer
cvt_2s_comp_to_Integer  nbits  x =
  let
    mask  = (bit  nbits) - 1 :: Integer
    x1    = x .&. mask
  in
    if testBit  x1  (nbits - 1) then
      -- Negative
      - (xor (x1 - 1) mask)
    else
      -- Positive
      x1

{-# INLINE cvt_Integer_to_2s_comp #-}
cvt_Integer_to_2s_comp :: Int -> Integer -> Integer
cvt_Integer_to_2s_comp  nbits  x =
  let
    mask = ((bit  nbits) - 1) :: Integer
  in
    if x < 0 then
      -- Negative
      x + mask + 1
    else
      -- Positive
      x .&. mask

{-# INLINE sign_extend #-}
-- sign-extends w1 lsb bits to w2 bits
sign_extend :: Int -> Int -> Integer -> Integer
sign_extend  w1  w2  x =
  let
    mask_w1  = (bit  w1) - 1
    mask_w2  = (bit  w2) - 1
    neg_fill = mask_w2 - mask_w1
    x1       = x  .&.  mask_w1
    is_neg   = testBit  x  (w1 - 1)
  in
    if is_neg then
      neg_fill .|. x1
    else
      x1

-- This conversion is only needed because Haskell's 'Data.Bits.shiftL'
-- and 'Data.Bits.shiftR' require Ints for the shift-amount argument.

{-# INLINE cvt_Integer_to_Int #-}
cvt_Integer_to_Int :: Integer -> Int
cvt_Integer_to_Int  j = fromIntegral j

-- This conversion is required as the softfloat library functions expect
-- arguments in Word64 for DP and Word32 for SP, and return them in Word64
-- and Word32. Certain softfloat functions also expect and return Int32/64
{-# INLINE cvt_Integer_to_Word32 #-}
cvt_Integer_to_Word32 :: Integer -> Word32
cvt_Integer_to_Word32  j = fromIntegral j

{-# INLINE cvt_Integer_to_Word64 #-}
cvt_Integer_to_Word64 :: Integer -> Word64
cvt_Integer_to_Word64  j = fromIntegral j

{-# INLINE cvt_Word32_to_Integer  #-}
cvt_Word32_to_Integer :: Word32 -> Integer
cvt_Word32_to_Integer  j = toInteger j

{-# INLINE cvt_Word64_to_Integer  #-}
cvt_Word64_to_Integer :: Word64 -> Integer
cvt_Word64_to_Integer  j = toInteger j

{-# INLINE cvt_Integer_to_Int32 #-}
cvt_Integer_to_Int32 :: Integer -> Int32
cvt_Integer_to_Int32  j = fromIntegral j

{-# INLINE cvt_Integer_to_Int64 #-}
cvt_Integer_to_Int64 :: Integer -> Int64
cvt_Integer_to_Int64  j = fromIntegral j

{-# INLINE cvt_Int32_to_Integer #-}
cvt_Int32_to_Integer :: Int32 -> Integer
cvt_Int32_to_Integer  j = toInteger j

{-# INLINE cvt_Int64_to_Integer #-}
cvt_Int64_to_Integer :: Int64 -> Integer
cvt_Int64_to_Integer  j = toInteger j

-- ================================================================
-- Bit concatenations
-- Note: args are presented in 'little-endian' order (hi lo)

{-# INLINE bitconcat_u16_u16_to_u32 #-}
bitconcat_u16_u16_to_u32 :: Integer -> Integer -> Integer
bitconcat_u16_u16_to_u32  u16_hi  u16_lo =
  let
    u32   = (shiftL u16_hi  16) .|. u16_lo
  in
    u32

{-# INLINE bitconcat_u32_u32_to_u64 #-}
bitconcat_u32_u32_to_u64  :: Integer -> Integer -> Integer
bitconcat_u32_u32_to_u64  u32_hi  u32_lo =
  let
    u64   = (shiftL u32_hi  32) .|. u32_lo
  in
    u64

-- ================================================================
-- show_wordXL returns a string, the hex print-out of the arg, filling in
-- prefix spacers as necessary to get an xlen-based width
-- Filled leading zeros use 'zero_char' (typically '0' or '.')

show_wordXL :: Int -> Char -> Integer -> String
show_wordXL    xlen   zero_char  x =
  if (x < 0) then
    "[show_wordXL " ++ show x ++ " (NEGATIVE!)]"
  else if ((shiftR  x  xlen) /= 0) then
    "[show_wordXL " ++ showHex x "" ++ " (TOO WIDE: xlen = " ++ show xlen ++ ")]"
  else
    let
      s1    = showHex  x  ""
      n     = length  s1
      width = quot  xlen  4
      pad j = (let
                  sa = if (j /= 0) && (rem j 4 == 0) then "_" else ""
                  sb = if j < (width - n) then [zero_char] else [s1 !! (j - (width - n))]
               in
                 sa ++ sb)

      s2    = [ ch | j <- [0..(width-1)], ch <- pad j ]
    in
      s2

-- ================================================================

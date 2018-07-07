-- See LICENSE for license details

module Bit_Manipulation where

-- ================================================================
-- This module defines various bit-manipulation functions used by the
-- RISC-V ISA semantics.

-- Some of these are just renamings or minor variants of standard
-- Haskell functions, but are defined here to improve clarity by using
-- more suggestive names, more concrete (specialized) types, etc.

-- ================================================================
-- Standard Haskell imports

import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Data.List
import Numeric (showHex, readHex)

-- Project imports

-- None

-- ================================================================
-- Note: below, to improve readability we use identifiers
--     'u8', 'u16', 'u32' and 'u64' for unsigned ints 
--     's8', 's16', 's32' and 's64' for signed ints 
--     'u' for unsigned ints of XLEN width (32 for RV32, 64 for RV64)
--     's' for signed ints of XLEN width (32 for RV32, 64 for RV64)

-- ================================================================
-- This function is used in 'decode' to extract bit fields of an instruction
-- Corresponds to Verilog's  [hi:lo]
-- Result has the bits in the lsbs, i.e., result [hi-lo: 0]

bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice  x  hi  lo = (shiftR  x  lo) .&. (complement $ shiftL (-1) (hi + 1 - lo))

{-# SPECIALIZE bitSlice :: Word32 -> Int -> Int -> Word32 #-}
{-# SPECIALIZE bitSlice :: Word64 -> Int -> Int -> Word64 #-}

{-# INLINE bitSlice #-}

-- ================================================================
-- Sign-extend bit [n-1] to full width

signExtend :: Word64 -> Int -> Word64
signExtend  word  n =
  let
    fill_bits = (shiftL  1  (64 - n)) - 1    -- all ones 
    fill_mask = shiftL  fill_bits  n
    word'     = if testBit  word  (n-1)
                then word .|. fill_mask
                else word .&. (complement fill_mask)

    u64 | n >= 64   = word
        | otherwise = word'
  in
    u64


signExtend_bit_in_u32 :: Word32 -> Int -> Word32
signExtend_bit_in_u32  word  n =
  let
    fill_bits = (shiftL  1  (32 - n)) - 1    -- all ones 
    fill_mask = shiftL  fill_bits  n
    word'     = if testBit  word  (n-1)
                then word .|. fill_mask
                else word .&. (complement fill_mask)
    u32 | n >= 32   = word
        | otherwise = word'
  in
    u32

-- ================================================================
-- The following sign- or zero-extend smaller unsigned byte/word types
-- to an unsigned Word64

signExtend_u8_to_u64 :: Word8 -> Word64
signExtend_u8_to_u64  u8 =
  let
    s8  = (fromIntegral  u8) :: Int8
    s64 = (fromIntegral  s8) :: Int64
    u64 = (fromIntegral s64) :: Word64
  in
    u64

signExtend_u16_to_u64 :: Word16 -> Word64
signExtend_u16_to_u64  u16 =
  let
    s16 = (fromIntegral u16) :: Int16
    s64 = (fromIntegral s16) :: Int64
    u64 = (fromIntegral s64) :: Word64
  in
    u64

signExtend_u32_to_u64 :: Word32 -> Word64
signExtend_u32_to_u64  u32 =
  let
    s32 = (fromIntegral u32) :: Int32
    s64 = (fromIntegral s32) :: Int64
    u64 = (fromIntegral s64) :: Word64
  in
    u64

signExtend_s32_to_u64 :: Int32 -> Word64
signExtend_s32_to_u64  s32 =
  let
    s64 = (fromIntegral s32) :: Int64
    u64 = (fromIntegral s64) :: Word64
  in
    u64


zeroExtend_u8_to_u32 :: Word8 -> Word32
zeroExtend_u8_to_u32  u8 = fromIntegral u8

zeroExtend_u16_to_u32 :: Word16 -> Word32
zeroExtend_u16_to_u32  u16 = fromIntegral u16


zeroExtend_u8_to_u64 :: Word8 -> Word64
zeroExtend_u8_to_u64  u8 = fromIntegral u8

zeroExtend_u16_to_u64 :: Word16 -> Word64
zeroExtend_u16_to_u64  u16 = fromIntegral u16

zeroExtend_u32_to_u64 :: Word32 -> Word64
zeroExtend_u32_to_u64  u32 = fromIntegral u32

-- ================================================================
-- Truncates

trunc_u64_to_u16 :: Word64 -> Word16
trunc_u64_to_u16  u64 = fromIntegral u64

trunc_u64_to_u32 :: Word64 -> Word32
trunc_u64_to_u32  u64 = fromIntegral u64

cvt_u64_to_u8 :: Word64 -> Word8
cvt_u64_to_u8  u64 = fromIntegral u64

trunc_u64_to_s32 :: Word64 -> Int32
trunc_u64_to_s32  u64 =
  let
    u32 = (fromIntegral u64) :: Word32
    s32 = (fromIntegral u32) :: Int32
  in
    s32

trunc_s64_to_s32 :: Int64 -> Int32
trunc_s64_to_s32  s64 = fromIntegral s64

cvt_Integer_to_u32 :: Integer ->  Word32
cvt_Integer_to_u32  i = fromIntegral i

-- ================================================================
-- Same-size conversions

cvt_u64_to_s64 :: Word64 -> Int64
cvt_u64_to_s64  u = fromIntegral u

cvt_s64_to_u64 :: Int64 -> Word64
cvt_s64_to_u64  s = fromIntegral s

cvt_u32_to_s32 :: Word32 -> Int32
cvt_u32_to_s32  u = fromIntegral u

cvt_s32_to_u32 :: Int32 -> Word32
cvt_s32_to_u32  s = fromIntegral s

-- ================================================================
-- Haskell's 'shiftL' and 'shiftR' functions require a Haskell 'Int'
-- arg for the shift amount (shamt).  The following conversion
-- produces that.

cvt_u32_to_Int :: Word32 -> Int
cvt_u32_to_Int  u = fromIntegral u

cvt_u64_to_Int :: Word64 -> Int
cvt_u64_to_Int  u = fromIntegral u

-- ================================================================
-- Bit concatenations

concat_u16_u16_to_u32 :: Word16 -> Word16 -> Word32
concat_u16_u16_to_u32  u16_lo  u16_hi =
  let
    u32_lo = fromIntegral  u16_lo
    u32_hi = fromIntegral  u16_hi
    u32   = (shiftL u32_hi  16) .|. u32_lo
  in
    u32

-- ================================================================
-- Set and clear specific bits

set_bit :: Word64 -> Int -> Word64
set_bit  x  position =
  let
    mask  = 1 :: Word64
    mask' = shiftL mask position

    x'    = (x .|. mask')
  in
    x'

clear_bit :: Word64 -> Int -> Word64
clear_bit  x  position =
  let
    mask  = 1 :: Word64
    mask' = complement (shiftL mask position)

    x'   = (x .&. mask')
  in
    x'

-- ================================================================
-- read_hex parses a hex number from a string, ensuring it fits in 'width' bits

read_hex :: Int -> String -> Integer
read_hex  width  s =
  let
    check :: Int -> [(Integer, String)] -> Integer
    check  width  []          = 0
    check  width  ((x,s):xss) = x
  in
    check width (readHex s)

-- ================================================================
-- read_vhex and read_vbin reads a hex or binary number from a string
-- which may optionally being with "0x" or "0b"
-- and which may use the Verilog convention of allowing '_' spacers
-- between digits.
-- In fact it's more lenient than Verilog: any non-digit is ignored

read_vhex :: String -> Word64
read_vhex  s =
  let
    f n digit | isHexDigit digit = 16 * n + fromIntegral (digitToInt  digit)
              | otherwise        = n
    s' = if "0x" `isPrefixOf` s then drop 2 s else s
  in
    foldl  f  0  s'

read_vbin :: String -> Word64
read_vbin  s =
  let
    f n '0' = 2 * n
    f n '1' = 2 * n + 1
    f n  _  = n

    s' = if "0b" `isPrefixOf` s then drop 2 s else s
  in
    foldl  f  0  s'

-- ================================================================

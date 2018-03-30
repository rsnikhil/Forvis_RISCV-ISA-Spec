module BitManipulation where

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

import ArchDefs

-- ================================================================
-- Note: below, to improve readability we use identifiers
--     'u8', 'u16', 'u32' and 'u64' for unsigned ints 
--     's8', 's16', 's32' and 's64' for signed ints 
--     'u' for unsigned ints of XLEN width (32 for RV32, 64 for RV64)
--     's' for signed ints of XLEN width (32 for RV32, 64 for RV64)

-- ================================================================
-- This function is used in 'decode' to extract bit fields of an instruction
-- and in certain 'execute' operations on 32b values in RV64
-- Specifically, extracts  word [end-1:start]
-- Result has the bits are in the lsbs, i.e., result [end-start-1: 0]

bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (shiftR x start) .&. (complement $ shiftL (-1) (end - start))

{-# SPECIALIZE bitSlice :: Word32 -> Int -> Int -> Word32 #-}

{-# INLINE bitSlice #-}

-- ================================================================
-- Sign-extend bit [n-1] to full width

signExtend :: UInt -> Int -> UInt
signExtend  word  n | n >= 64   = word
                    | otherwise = word'
  where  fill_bits = (shiftL  1  (64 - n)) - 1    -- all ones 
         fill_mask = shiftL  fill_bits  n
         word'     = if testBit  word  (n-1)
                     then word .|. fill_mask
                     else word .&. (complement fill_mask)

-- ================================================================
-- The following sign- or zero-extend smaller unsigned byte/word types
-- to an unsigned UInt

signExtend_u8_to_u :: Word8 -> UInt
signExtend_u8_to_u  u8 = u
  where s8 :: Int8
        s8 = fromIntegral u8
        s  :: SInt
        s  = fromIntegral s8
        u  :: UInt
        u  = fromIntegral s

signExtend_u16_to_u :: Word16 -> UInt
signExtend_u16_to_u  u16 = u
  where s16 :: Int16
        s16 = fromIntegral u16
        s  :: SInt
        s  = fromIntegral s16
        u  :: UInt
        u  = fromIntegral s

signExtend_u32_to_u :: Word32 -> UInt
signExtend_u32_to_u  u32 = u
  where s32 :: Int32
        s32 = fromIntegral u32
        s  :: SInt
        s  = fromIntegral s32
        u  :: UInt
        u  = fromIntegral s

zeroExtend_u8_to_u :: Word8 -> UInt
zeroExtend_u8_to_u  u8 = u
  where u :: UInt
        u = fromIntegral u8

zeroExtend_u16_to_u :: Word16 -> UInt
zeroExtend_u16_to_u  u16 = u
  where u :: UInt
        u = fromIntegral u16

zeroExtend_u32_to_u :: Word32 -> UInt
zeroExtend_u32_to_u  u32 = u
  where u :: UInt
        u = fromIntegral u32

-- ================================================================
-- The following truncate a UInt to shorter byte/word types

trunc_u_to_u8 :: UInt -> Word8
trunc_u_to_u8  u = u8
  where u8 = fromIntegral u

trunc_u_to_u16 :: UInt -> Word16
trunc_u_to_u16  u = u16
  where u16 = fromIntegral u

trunc_u_to_u32 :: UInt -> Word32
trunc_u_to_u32  u = u32
  where u32 = fromIntegral u

-- ================================================================
-- Conversions between specific sizes

signExtend_u32_to_u64 :: Word32 -> Word64
signExtend_u32_to_u64  u32 = u64
  where s32 :: Int32
        s32 = fromIntegral u32
        s64 :: Int64
        s64 = fromIntegral s32
        u64 :: Word64
        u64 = fromIntegral s64

zeroExtend_u32_to_u64 :: Word32 -> Word64
zeroExtend_u32_to_u64  u32 = u64
  where u64 :: Word64
        u64 = fromIntegral u32

signExtend_s32_to_u64 :: Int32 -> Word64
signExtend_s32_to_u64  s32 = u64
  where s64 :: Int64
        s64 = fromIntegral s32
        u64 :: Word64
        u64 = fromIntegral s64

trunc_u64_to_u32 :: Word64 -> Word32
trunc_u64_to_u32  u64 = u32
  where u32 :: Word32
        u32 = fromIntegral u64

trunc_u64_to_s32 :: Word64 -> Int32
trunc_u64_to_s32  u64 = s32
  where u32 :: Word32
        u32 = fromIntegral u64
        s32 :: Int32
        s32 = fromIntegral u32

trunc_s64_to_s32 :: Int64 -> Int32
trunc_s64_to_s32  s64 = s32
  where s32 :: Int32
        s32 = fromIntegral s64

-- ================================================================
-- Same-size conversions

cvt_u_to_s :: UInt -> SInt
cvt_u_to_s  u = fromIntegral u

cvt_s_to_u :: SInt -> UInt
cvt_s_to_u  s = fromIntegral s

-- ================================================================
-- Haskell's 'shiftL' and 'shiftR' functions require a Haskell 'Int'
-- arg for the shift amount (shamt).  The following conversion
-- produces that.

cvt_u_to_Int :: UInt -> Int
cvt_u_to_Int  u = i
  where i = fromIntegral u

-- ================================================================
-- read_hex parses a hex number from a string, ensuring it fits in 'width' bits

read_hex :: Int -> String -> Integer
read_hex  width  s = check width (readHex s)
  where check :: Int -> [(Integer, String)] -> Integer
        check  width  []          = 0
        check  width  ((x,s):xss) = x

-- ================================================================
-- read_vhex and read_vbin reads a hex or binary number from a string
-- which may optionally being with "0x" or "0b"
-- and which may use the Verilog convention of allowing '_' spacers
-- between digits.
-- In fact it's more lenient than Verilog: any non-digit is ignored

read_vhex :: String -> UInt
read_vhex  s = foldl  f  0  s'
  where s' = if "0x" `isPrefixOf` s then drop 2 s else s
        f n digit | isHexDigit digit = 16 * n + fromIntegral (digitToInt  digit)
                  | otherwise        = n

read_vbin :: String -> UInt
read_vbin  s = foldl  f  0  s'
  where s' = if "0b" `isPrefixOf` s then drop 2 s else s
        f n '0' = 2 * n
        f n '1' = 2 * n + 1
        f n  _  = n

-- ================================================================

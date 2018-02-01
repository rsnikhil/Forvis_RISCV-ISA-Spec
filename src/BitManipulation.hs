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
import Numeric (showHex, readHex)

-- Project imports

import ArchDefs64

-- ================================================================
-- Note: below, to improve readability we use identifiers
--     'u8', 'u16', 'u32' and 'u64' for unsigned ints 
--     's8', 's16', 's32' and 's64' for signed ints 
--     'u' for unsigned ints of XLEN width (32 for RV32, 64 for RV64)
--     's' for signed ints of XLEN width (32 for RV32, 64 for RV64)

-- ================================================================
-- This function is used in 'decode' to extract bit fields of an instruction
-- Specifically, extracts  word32 [end-1:start]
-- Result is also a word32, but the bits are in the lsbs, i.e., result [end-start-1: 0]

bitSlice :: Word32 -> Int -> Int -> Word32
bitSlice  u32  start  end =  (shiftR u32 start) .&. (complement (shiftL (0xFFFFFFFF) (end - start)))

-- ================================================================
-- This function is used in 'decode' to sign-extend instruction
-- bit fields of various lengths to a full signed machine word
-- (MachineWord_S).  The relevant bit field is in word [nbits-1:0]
-- (words [31:nbits] are not relevant).

signExtend_bits_to_s :: Int -> Word32 -> MachineWord_S
signExtend_bits_to_s  nbits  word32 = s
  where u32 = if testBit word32 (nbits - 1)
              then word32 - (2 ^ nbits)
              else word32
        s32 :: Int32
        s32 = fromIntegral u32
        s   :: MachineWord_S
        s   = fromIntegral s32

-- ================================================================
-- The following sign- or zero-extend smaller unsigned byte/word types
-- to an unsigned MachineWord

signExtend_u8_to_u :: Word8 -> MachineWord
signExtend_u8_to_u  u8 = u
  where s8 :: Int8
        s8 = fromIntegral u8
        s  :: MachineWord_S
        s  = fromIntegral s8
        u  :: MachineWord
        u  = fromIntegral s

signExtend_u16_to_u :: Word16 -> MachineWord
signExtend_u16_to_u  u16 = u
  where s16 :: Int16
        s16 = fromIntegral u16
        s  :: MachineWord_S
        s  = fromIntegral s16
        u  :: MachineWord
        u  = fromIntegral s

-- Note: the following is the identity function if a MachineWord is 32b
signExtend_u32_to_u :: Word32 -> MachineWord
signExtend_u32_to_u  u32 = u
  where s32 :: Int32
        s32 = fromIntegral u32
        s  :: MachineWord_S
        s  = fromIntegral s32
        u  :: MachineWord
        u  = fromIntegral s

zeroExtend_u8_to_u :: Word8 -> MachineWord
zeroExtend_u8_to_u  u8 = u
  where u :: MachineWord
        u = fromIntegral u8

zeroExtend_u16_to_u :: Word16 -> MachineWord
zeroExtend_u16_to_u  u16 = u
  where u :: MachineWord
        u = fromIntegral u16

-- Note: the following is the identity function if a MachineWord is 32b
zeroExtend_u32_to_u :: Word32 -> MachineWord
zeroExtend_u32_to_u  u32 = u
  where u :: MachineWord
        u = fromIntegral u32

-- ================================================================
-- The following truncate a MachineWord to shorter byte/word types

trunc_u_to_u8 :: MachineWord -> Word8
trunc_u_to_u8  u = u8
  where u8 = fromIntegral u

trunc_u_to_u16 :: MachineWord -> Word16
trunc_u_to_u16  u = u16
  where u16 = fromIntegral u

-- Note: the following is the identity function if a MachineWord is 32b
trunc_u_to_u32 :: MachineWord -> Word32
trunc_u_to_u32  u = u32
  where u32 = fromIntegral u

-- ================================================================
-- Conversions between specific sizes (not dependent on XLEN)

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

cvt_u_to_s :: MachineWord -> MachineWord_S
cvt_u_to_s  u = fromIntegral u

cvt_s_to_u :: MachineWord_S -> MachineWord
cvt_s_to_u  s = fromIntegral s

-- ================================================================
-- Haskell's 'shiftL' and 'shiftR' functions require a Haskell 'Int'
-- arg for the shift amount (shamt).  The following conversion
-- produces that.

cvt_u_to_Int :: MachineWord -> Int
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

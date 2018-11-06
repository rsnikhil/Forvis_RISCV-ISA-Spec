-- Copyright (c) 2018 Rishiyur S. Nikhil and Niraj N. Sharma
-- See LICENSE for license details

module FP_Bit_Utils where

-- ================================================================
-- This module has definitions specific to manipulating single and
-- double-precision IEEE floating point values. 
-- Also contains functions to make it more convenient to interface
-- to the softfloat library

-- ================================================================
-- Standard Haskell imports

import Data.Bits
import Data.Char

-- Project imports

import Bit_Utils
import Arch_Defs
import SoftFloat

-- Definitions of Q-NaNs for single and double precision
canonicalNaN32 = 0x7fc00000 :: Integer
canonicalNaN64 = 0x7ff8000000000000 :: Integer

-- IEEE format for DP values
dp_sgn_bitpos           = 63 :: Int
dp_exp_bitpos           = 52 :: Int

-- IEEE format for DP values
sp_sgn_bitpos           = 31 :: Int
sp_exp_bitpos           = 23 :: Int

-- Extract the individual components (sign, exponent, mantissa) from a DP value
extractFromDP :: Integer -> (Integer, Integer, Integer)
extractFromDP    val = 
  let
    sign       = bitSlice  val   dp_sgn_bitpos      dp_sgn_bitpos
    exponent   = bitSlice  val  (dp_sgn_bitpos-1)  (dp_exp_bitpos)
    mantissa   = bitSlice  val  (dp_exp_bitpos-1)   0
  in
    (sign, exponent, mantissa)

-- Compose a DP value from its components
composeDP :: Integer -> Integer -> Integer -> Integer
composeDP    sgn        exp        man = 
  let
    res =     (shiftL  sgn  dp_sgn_bitpos)
          .|. (shiftL  exp  dp_exp_bitpos)
          .|.  man
  in
    res

-- Extract the individual components (sign, exponent, mantissa) from a DP value
extractFromSP :: Integer -> (Integer, Integer, Integer)
extractFromSP    val = 
  let
    sign       = bitSlice  val   sp_sgn_bitpos      sp_sgn_bitpos
    exponent   = bitSlice  val  (sp_sgn_bitpos-1)  (sp_exp_bitpos)
    mantissa   = bitSlice  val  (sp_exp_bitpos-1)   0
  in
    (sign, exponent, mantissa)

-- Compose a SP value from its components. Note that the upper 32-bits
-- are unused and the value is not NaN-Boxed
composeSP :: Integer -> Integer -> Integer -> Integer
composeSP    sgn        exp        man  = 
  let
    res =     (shiftL  sgn  sp_sgn_bitpos)
          .|. (shiftL  exp  sp_exp_bitpos)
          .|.  man
  in
    res


-- Convert a frm bit pattern to RoundingMode used by softFloat
frm_to_RoundingMode :: InstrField -> RoundingMode
frm_to_RoundingMode  0x0 = RoundNearEven
frm_to_RoundingMode  0x1 = RoundMinMag
frm_to_RoundingMode  0x2 = RoundMin
frm_to_RoundingMode  0x3 = RoundMax
frm_to_RoundingMode  0x4 = RoundNearMaxMag
frm_to_RoundingMode  x   = RoundNearEven  -- a catch all

-- Separates out the Single Precision result (F32Result) from the SoftFloat
-- function call to a value for the FPR register file and the FCSR.FFLAGS
extractRdSPResult :: F32Result          -> Integer
extractRdSPResult    (Result res flags) = cvt_Word32_to_Integer  res

extractFFlagsSPResult :: F32Result          -> Integer
extractFFlagsSPResult    (Result res flags) = 
  let
    nxf = inexact    flags
    uff = underflow  flags
    off = overflow   flags
    dzf = infinite   flags
    nvf = invalid    flags
    fflags = form_fflags_word  nxf uff off dzf nvf
  in
    fflags


-- Separates out the Double Precision result (F64Result) from the SoftFloat
-- function call to a value for the FPR register file and the FCSR.FFLAGS
extractRdDPResult :: F64Result          -> Integer
extractRdDPResult    (Result res flags) = cvt_Word64_to_Integer  res

extractFFlagsDPResult :: F64Result          -> Integer
extractFFlagsDPResult    (Result res flags) = 
  let
    nxf = inexact    flags
    uff = underflow  flags
    off = overflow   flags
    dzf = infinite   flags
    nvf = invalid    flags
    fflags = form_fflags_word  nxf uff off dzf nvf
  in
    fflags


-- Separates out the Signed-64 result (I64Result) from the SoftFloat
-- function call to a value for the GPR register file
extractRdLResult  :: I64Result          -> Integer
extractRdLResult     (Result res flags) = cvt_Integer_to_2s_comp  64  (cvt_Int64_to_Integer  res)

extractFFlagsLResult :: I64Result           -> Integer
extractFFlagsLResult    (Result res flags)  = 
  let
    nxf = inexact    flags
    uff = underflow  flags
    off = overflow   flags
    dzf = infinite   flags
    nvf = invalid    flags
    fflags = form_fflags_word  nxf uff off dzf nvf
  in
    fflags

-- Separates out the Unsigned-64 result (I64Result) from the SoftFloat
-- function call to a value for the GPR register file
extractRdLUResult :: Ui64Result         -> Integer
extractRdLUResult    (Result res flags) = cvt_Word64_to_Integer  res

extractFFlagsLUResult :: Ui64Result         -> Integer
extractFFlagsLUResult    (Result res flags) = 
  let
    nxf = inexact    flags
    uff = underflow  flags
    off = overflow   flags
    dzf = infinite   flags
    nvf = invalid    flags
    fflags = form_fflags_word  nxf uff off dzf nvf
  in
    fflags

-- Separates out the Signed-64 result (I64Result) from the SoftFloat
-- function call to a value for the GPR register file
-- the sign-extension is necessary as in RISC-V sub-xlen values are in
-- sign-extended form
extractRdWResult  :: I32Result          -> Integer
extractRdWResult     (Result res flags) = sign_extend  32  64  (cvt_Integer_to_2s_comp  32  (cvt_Int32_to_Integer  res))

extractFFlagsWResult :: I32Result          -> Integer
extractFFlagsWResult    (Result res flags) = 
  let
    nxf = inexact    flags
    uff = underflow  flags
    off = overflow   flags
    dzf = infinite   flags
    nvf = invalid    flags
    fflags = form_fflags_word  nxf uff off dzf nvf
  in
    fflags

-- Separates out the Unsigned-64 result (I64Result) from the SoftFloat
-- function call to a value for the GPR register file
extractRdWUResult :: Ui32Result         -> Integer
extractRdWUResult    (Result res flags) = sign_extend  32  64  (cvt_Word32_to_Integer  res)

extractFFlagsWUResult :: Ui32Result -> Integer
extractFFlagsWUResult (Result res flags) = 
  let
    nxf = inexact    flags
    uff = underflow  flags
    off = overflow   flags
    dzf = infinite   flags
    nvf = invalid    flags
    fflags = form_fflags_word  nxf uff off dzf nvf
  in
    fflags

-- ================================================================

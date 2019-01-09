-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module FPU where

#ifdef FLOAT

-- ================================================================
-- This module defines various RISC-V FPU functions.

-- ================================================================
-- Standard Haskell imports

import Data.Bits

-- Project imports

import Data.Word    -- For Wordxx type (unsigned fixed-width ints)
import Bit_Utils
import Arch_Defs

-- Other library imports

import SoftFloat    -- from https://github.com/GaloisInc/softfloat-hs.git


-- ================================================================
-- IEEE Format based definitions for SP and DP values
--
-- Definitions of Q-NaNs for single and double precision
canonicalNaN32 = 0x7fc00000 :: Integer
canonicalNaN64 = 0x7ff8000000000000 :: Integer

-- IEEE format for DP values
dp_sgn_bitpos           = 63 :: Int
dp_exp_bitpos           = 52 :: Int

-- IEEE format for DP values
sp_sgn_bitpos           = 31 :: Int
sp_exp_bitpos           = 23 :: Int


-- ================================================================
-- Helper functions to assemble and disassemble SP and DP values
-- Extract the individual components (sign, exponent, mantissa) from a DP value
disassembleDP :: Integer -> (Integer, Integer, Integer)
disassembleDP    val = 
  let
    sign       = bitSlice  val   dp_sgn_bitpos      dp_sgn_bitpos
    exponent   = bitSlice  val  (dp_sgn_bitpos-1)  (dp_exp_bitpos)
    mantissa   = bitSlice  val  (dp_exp_bitpos-1)   0
  in
    (sign, exponent, mantissa)

-- Compose a DP value from its components
assembleDP :: Integer -> Integer -> Integer -> Integer
assembleDP    sgn        exp        man = 
  let
    res =     (shiftL  sgn  dp_sgn_bitpos)
          .|. (shiftL  exp  dp_exp_bitpos)
          .|.  man
  in
    res

-- Extract the individual components (sign, exponent, mantissa) from a DP value
disassembleSP :: Integer -> (Integer, Integer, Integer)
disassembleSP    val = 
  let
    sign       = bitSlice  val   sp_sgn_bitpos      sp_sgn_bitpos
    exponent   = bitSlice  val  (sp_sgn_bitpos-1)  (sp_exp_bitpos)
    mantissa   = bitSlice  val  (sp_exp_bitpos-1)   0
  in
    (sign, exponent, mantissa)

-- Compose a SP value from its components. Note that the upper 32-bits
-- are unused and the value is not NaN-Boxed
assembleSP :: Integer -> Integer -> Integer -> Integer
assembleSP    sgn        exp        man  = 
  let
    res =     (shiftL  sgn  sp_sgn_bitpos)
          .|. (shiftL  exp  sp_exp_bitpos)
          .|.  man
  in
    res


-- ================================================================
-- Helper functions to use with the softfloat foreign function lib
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
-- Miscellanous helper functions to manipulate SP and DP values
-- Unbox a NaN-boxed value
unboxSP :: Integer -> Integer
unboxSP    rawVal =
  let
    upperBits = bitSlice rawVal 63 32
    unboxedVal = (
      if (upperBits == 0xffffffff) then (bitSlice rawVal 31 0)
      else canonicalNaN32)
  in
    unboxedVal


-- NaN-box bit [32] to full width
nanBox :: Integer -> Integer
nanBox    word =
  let
    fill_bits = (shiftL  1  32) - 1    -- all ones 
    fill_mask = shiftL  fill_bits  32
    word'     = word .|. fill_mask
  in
    word'


-- Flip the sign of a double precision value
negateD :: Integer -> Integer
negateD    word = 
  let
    (s, e, m) = disassembleDP  word
    s' = (if (s == 0) then 1 else 0)
    word' = assembleDP  s'  e  m
  in
    word'


-- Flip the sign of a single precision value
negateS :: Integer -> Integer
negateS    word = 
  let
    (s, e, m) = disassembleSP  word
    s' = (if (s == 0) then 1 else 0)
    word' = assembleSP  s'  e  m
  in
    word'


-- ================================================================
-- FPU Function: Checks types of SP and FP values
-- Checks if a DP value is a signalling NaN
fpu_f64IsSNaN :: Integer -> Bool
fpu_f64IsSNaN    val =
  let
    (_, e, m) = disassembleDP  val
    qBitIsSet = testBit  m  51
    res = (e == 0x7ff) && (qBitIsSet == False) && (m /= 0)
  in 
    res


-- Checks if a DP value is a quiet NaN
fpu_f64IsQNaN :: Integer -> Bool
fpu_f64IsQNaN    val =
  let
    (_, e, m) = disassembleDP  val
    qBitIsSet = testBit  m  51
    res = (e == 0x7ff) && qBitIsSet
  in 
    res


-- Checks if a DP value is positive inifinity
fpu_f64IsPosInf :: Integer -> Bool
fpu_f64IsPosInf    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 0) && (e == 0x7ff) && (m == 0)
  in 
    res


-- Checks if a DP value is a positive normal value
fpu_f64IsPosNorm :: Integer -> Bool
fpu_f64IsPosNorm    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 0) && (e /= 0) && (e /= 0x7ff) 
  in 
    res


-- Checks if a DP value is a positive subnormal value
fpu_f64IsPosSubNorm :: Integer -> Bool
fpu_f64IsPosSubNorm    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 0) && (e == 0x0) && (m /= 0)
  in 
    res


-- Checks if a DP value is a positive zero
fpu_f64IsPosZero :: Integer -> Bool
fpu_f64IsPosZero    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 0) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a DP value is a negative infinity
fpu_f64IsNegInf :: Integer -> Bool
fpu_f64IsNegInf    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 0x1) && (e == 0x7ff) && (m == 0)
  in 
    res


-- Checks if a DP value is a negative normal value
fpu_f64IsNegNorm :: Integer -> Bool
fpu_f64IsNegNorm    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 0x1) && (e /= 0) && (e /= 0x7ff) 
  in 
    res


-- Checks if a DP value is a negative subnormal value
fpu_f64IsNegSubNorm :: Integer -> Bool
fpu_f64IsNegSubNorm    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 0x1) && (e == 0x0) && (m /= 0)
  in 
    res


-- Checks if a DP value is a negative zero value
fpu_f64IsNegZero :: Integer -> Bool
fpu_f64IsNegZero    val =
  let
    (s, e, m) = disassembleDP  val
    res = (s == 1) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a SP value is a signalling NaN
fpu_f32IsSNaN :: Integer -> Bool
fpu_f32IsSNaN    val =
  let
    (s, e, m) = disassembleSP  val
    qBitIsSet = testBit  m  22
    res = (e == 0xff) && (qBitIsSet == False) && (m /= 0)
  in 
    res


-- Checks if a SP value is a quiet NaN
fpu_f32IsQNaN :: Integer -> Bool
fpu_f32IsQNaN    val =
  let
    (s, e, m) = disassembleSP  val
    qBitIsSet = testBit  m  22
    res = (e == 0xff) && qBitIsSet
  in 
    res


-- Checks if a SP value is positive inifinity
fpu_f32IsPosInf :: Integer -> Bool
fpu_f32IsPosInf    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0) && (e == 0xff) && (m == 0)
  in 
    res


-- Checks if a SP value is a positive normal value
fpu_f32IsPosNorm :: Integer -> Bool
fpu_f32IsPosNorm    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0) && (e /= 0) && (e /= 0xff) 
  in 
    res


-- Checks if a SP value is a positive subnormal value
fpu_f32IsPosSubNorm :: Integer -> Bool
fpu_f32IsPosSubNorm    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0) && (e == 0) && (m /= 0)
  in 
    res


-- Checks if a SP value is a positive zero
fpu_f32IsPosZero :: Integer -> Bool
fpu_f32IsPosZero    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a SP value is a negative infinity
fpu_f32IsNegInf :: Integer -> Bool
fpu_f32IsNegInf    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0x1) && (e == 0xff) && (m == 0)
  in 
    res


-- Checks if a SP value is a negative normal value
fpu_f32IsNegNorm :: Integer -> Bool
fpu_f32IsNegNorm    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0x1) && (e /= 0) && (e /= 0xff) 
  in 
    res


-- Checks if a SP value is a negative subnormal value
fpu_f32IsNegSubNorm :: Integer -> Bool
fpu_f32IsNegSubNorm    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0x1) && (e == 0) && (m /= 0)
  in 
    res


-- Checks if a SP value is a negative zero
fpu_f32IsNegZero :: Integer -> Bool
fpu_f32IsNegZero    val =
  let
    (s, e, m) = disassembleSP  val
    res = (s == 0x1) && (e == 0x0) && (m == 0)
  in 
    res


-- ================================================================
-- EQQ: test for equality (quiet)
-- DP1 == DP2
-- Checks if the first argument is equal to the second argument for two DP
-- numbers. Pos0 and Neg0 are considered equal. Sets INV only if either argument
-- is a SNaN (quiet)
fpu_f64EQQ :: Integer -> Integer -> (Bool, Integer)
fpu_f64EQQ    rs1        rs2 = 
  let
    (s1, e1, m1) = disassembleDP rs1
    (s2, e2, m2) = disassembleDP rs2

    rs1IsSNaN = fpu_f64IsSNaN     rs1
    rs2IsSNaN = fpu_f64IsSNaN     rs2
    rs1IsPos0 = fpu_f64IsPosZero  rs1
    rs2IsPos0 = fpu_f64IsPosZero  rs2
    rs1IsNeg0 = fpu_f64IsNegZero  rs1
    rs2IsNeg0 = fpu_f64IsNegZero  rs2
    rs1Is0    = rs1IsNeg0 || rs1IsPos0
    rs2Is0    = rs2IsNeg0 || rs2IsPos0

    res = (rs1 == rs2) || (rs1Is0 && rs2Is0)
    fflags = if (rs1IsSNaN || rs2IsSNaN) then nvFlag else 0
  in
    (res, fflags)


-- SP1 == SP2
-- Checks if the first argument is equal to the second argument for two SP
-- numbers. Pos0 and Neg0 are considered equal. Sets INV only if either argument
-- is a SNaN (quiet)
fpu_f32EQQ :: Integer -> Integer -> (Bool, Integer)
fpu_f32EQQ    rs1        rs2 = 
  let
    (s1, e1, m1) = disassembleSP  rs1
    (s2, e2, m2) = disassembleSP  rs2
    rs1IsSNaN = fpu_f32IsSNaN rs1
    rs2IsSNaN = fpu_f32IsSNaN rs2
    rs1IsPos0 = fpu_f32IsPosZero  rs1
    rs2IsPos0 = fpu_f32IsPosZero  rs2
    rs1IsNeg0 = fpu_f32IsNegZero  rs1
    rs2IsNeg0 = fpu_f32IsNegZero  rs2
    rs1Is0    = rs1IsNeg0 || rs1IsPos0
    rs2Is0    = rs2IsNeg0 || rs2IsPos0

    res = (rs1 == rs2) || (rs1Is0 && rs2Is0)
    fflags = if (rs1IsSNaN || rs2IsSNaN) then nvFlag else 0
  in
    (res, fflags)


-- ================================================================
-- LE: test for less-than-or-equal

-- DP1 <= DP2
-- Checks if the first argument is less than or equal to the second argument for
-- two DP numbers. The isQuiet flag controls when the INV flag is set
fpu_f64LE :: Integer -> Integer -> Bool -> (Bool, Integer)
fpu_f64LE    rs1        rs2        isQuiet = 
  let
    (s1, e1, m1) = disassembleDP rs1
    (s2, e2, m2) = disassembleDP rs2

    rs1IsQNaN = fpu_f64IsQNaN rs1
    rs2IsQNaN = fpu_f64IsQNaN rs2
    rs1IsSNaN = fpu_f64IsSNaN rs1
    rs2IsSNaN = fpu_f64IsSNaN rs2
    rs1IsNaN = rs1IsQNaN || rs1IsSNaN
    rs2IsNaN = rs2IsQNaN || rs2IsSNaN

    rs1IsPos0  = fpu_f64IsPosZero  rs1
    rs2IsPos0  = fpu_f64IsPosZero  rs2
    rs1IsNeg0  = fpu_f64IsNegZero  rs1
    rs2IsNeg0  = fpu_f64IsNegZero  rs2
    rs1Is0     = rs1IsNeg0 || rs1IsPos0
    rs2Is0     = rs2IsNeg0 || rs2IsPos0

    res = if (s1 == 0) && (s2 == 0) then
            if (e1 == e2) then
              m1 <= m2
            else
              e1 < e2
          else if (s1 == 0) && (s2 == 1) then
            (rs1Is0 && rs2Is0)      -- Equal in this case (+0=-0)
          else if (s1 == 1) && (s2 == 0) then
            True
          else
            if (e1 == e2) then
              m1 >= m2
            else
              e1 > e2
    fflags = if isQuiet then 
               if (rs1IsSNaN || rs2IsSNaN) then nvFlag else 0
             else
               if (rs1IsNaN || rs2IsNaN) then nvFlag else 0
  in
    (res, fflags)


-- SP1 <= SP2
-- Checks if the first argument is less than or equal to the second argument for
-- two SP numbers. The isQuiet flag controls when the INV flag is set
fpu_f32LE :: Integer -> Integer -> Bool -> (Bool, Integer)
fpu_f32LE    rs1        rs2        isQuiet = 
  let
    (s1, e1, m1) = disassembleSP  rs1
    (s2, e2, m2) = disassembleSP  rs2

    rs1IsQNaN = fpu_f32IsQNaN rs1
    rs2IsQNaN = fpu_f32IsQNaN rs2
    rs1IsSNaN = fpu_f32IsSNaN rs1
    rs2IsSNaN = fpu_f32IsSNaN rs2
    rs1IsNaN = rs1IsQNaN || rs1IsSNaN
    rs2IsNaN = rs2IsQNaN || rs2IsSNaN

    rs1IsPos0  = fpu_f32IsPosZero  rs1
    rs2IsPos0  = fpu_f32IsPosZero  rs2
    rs1IsNeg0  = fpu_f32IsNegZero  rs1
    rs2IsNeg0  = fpu_f32IsNegZero  rs2
    rs1Is0     = rs1IsNeg0 || rs1IsPos0
    rs2Is0     = rs2IsNeg0 || rs2IsPos0

    res = if (s1 == 0) && (s2 == 0) then
            if (e1 == e2) then
              m1 <= m2
            else
              e1 < e2
          else if (s1 == 0) && (s2 == 1) then
            (rs1Is0 && rs2Is0)      -- Equal in this case (+0=-0)
          else if (s1 == 1) && (s2 == 0) then
            True
          else
            if (e1 == e2) then
              m1 >= m2
            else
              e1 > e2
    fflags = if isQuiet then 
               if (rs1IsSNaN || rs2IsSNaN) then nvFlag else 0
             else
               if (rs1IsNaN || rs2IsNaN) then nvFlag else 0
  in
    (res, fflags)


-- ================================================================
-- LT: test for less-than

-- DP1 < DP2
-- Checks if the first argument is less than the second argument for
-- two DP numbers. The isQuiet flag controls when the INV flag is set
fpu_f64LT :: Integer -> Integer -> Bool -> (Bool, Integer)
fpu_f64LT    rs1        rs2        isQuiet = 
  let
    (s1, e1, m1) = disassembleDP rs1
    (s2, e2, m2) = disassembleDP rs2

    rs1IsQNaN  = fpu_f64IsQNaN     rs1
    rs2IsQNaN  = fpu_f64IsQNaN     rs2
    rs1IsSNaN  = fpu_f64IsSNaN     rs1
    rs2IsSNaN  = fpu_f64IsSNaN     rs2
    rs1IsNaN   = rs1IsQNaN || rs1IsSNaN
    rs2IsNaN   = rs2IsQNaN || rs2IsSNaN

    res = if (s1 == 0) && (s2 == 0) then
            if (e1 == e2) then
              m1 < m2
            else
              e1 < e2
          else if (s1 == 0) && (s2 == 1) then
            False
          else if (s1 == 1) && (s2 == 0) then
            True
          else
            if (e1 == e2) then
              m1 > m2
            else
              e1 > e2
    fflags = if isQuiet then 
               if (rs1IsSNaN || rs2IsSNaN) then nvFlag else 0
             else
               if (rs1IsNaN || rs2IsNaN) then nvFlag else 0
  in
    (res, fflags)


-- SP1 < SP2
-- Checks if the first argument is less than or equal to the second argument for
-- two SP numbers. The isQuiet flag controls when the INV flag is set
fpu_f32LT :: Integer -> Integer -> Bool -> (Bool, Integer)
fpu_f32LT    rs1        rs2        isQuiet = 
  let
    (s1, e1, m1) = disassembleSP  rs1
    (s2, e2, m2) = disassembleSP  rs2

    rs1IsQNaN  = fpu_f32IsQNaN    rs1
    rs2IsQNaN  = fpu_f32IsQNaN    rs2
    rs1IsSNaN  = fpu_f32IsSNaN    rs1
    rs2IsSNaN  = fpu_f32IsSNaN    rs2
    rs1IsNaN   = rs1IsQNaN || rs1IsSNaN
    rs2IsNaN   = rs2IsQNaN || rs2IsSNaN

    res = if (s1 == 0) && (s2 == 0) then
            if (e1 == e2) then
              m1 < m2
            else
              e1 < e2
          else if (s1 == 0) && (s2 == 1) then
            False
          else if (s1 == 1) && (s2 == 0) then
            True
          else
            if (e1 == e2) then
              m1 > m2
            else
              e1 > e2
    fflags = if isQuiet then 
               if (rs1IsSNaN || rs2IsSNaN) then nvFlag else 0
             else
               if (rs1IsNaN || rs2IsNaN) then nvFlag else 0
  in
    (res, fflags)

-- ================================================================
-- ADD: addition

fpu_f32Add ::   InstrField -> Word32  ->  Word32 -> (Integer, Integer)
fpu_f32Add      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut   = f32Add  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdSPResult sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


fpu_f64Add ::   InstrField -> Word64  ->  Word64 -> (Integer, Integer)
fpu_f64Add      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut   = f64Add  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdDPResult sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- SUB: subtraction

fpu_f32Sub ::   InstrField -> Word32  ->  Word32 -> (Integer, Integer)
fpu_f32Sub      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut   = f32Sub  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdSPResult sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


fpu_f64Sub ::   InstrField -> Word64  ->  Word64 -> (Integer, Integer)
fpu_f64Sub      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut   = f64Sub  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdDPResult sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- MUL: multiplication

fpu_f32Mul ::   InstrField -> Word32  ->  Word32 -> (Integer, Integer)
fpu_f32Mul      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32Mul  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdSPResult sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


fpu_f64Mul ::   InstrField -> Word64  ->  Word64 -> (Integer, Integer)
fpu_f64Mul      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64Mul  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdDPResult sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- DIV: division

fpu_f32Div ::   InstrField -> Word32  ->  Word32 -> (Integer, Integer)
fpu_f32Div      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32Div  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdSPResult sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


fpu_f64Div ::   InstrField -> Word64  ->  Word64 -> (Integer, Integer)
fpu_f64Div      frmVal        rs1Val      rs2Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64Div  rmVal  rs1Val  rs2Val

    -- Extract the results and the flags
    rdVal   = extractRdDPResult sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- SQRT: square root

fpu_f32Sqrt ::   InstrField -> Word32  ->  (Integer, Integer)
fpu_f32Sqrt      frmVal        rs1Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32Sqrt  rmVal  rs1Val

    -- Extract the results and the flags
    rdVal   = extractRdSPResult sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


fpu_f64Sqrt ::   InstrField -> Word64  ->  (Integer, Integer)
fpu_f64Sqrt      frmVal        rs1Val  =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64Sqrt  rmVal  rs1Val

    -- Extract the results and the flags
    rdVal   = extractRdDPResult     sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- Multiply-Add and Multiply-Sub Operations
-- Multiply-Add

fpu_f32MulAdd   ::  InstrField  -> Integer  -> Integer  -> Integer  -> (Integer, Integer)
fpu_f32MulAdd       frmVal         rs1Val      rs2Val      rs3Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal       = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    rs1Val32    = cvt_Integer_to_Word32  rs1Val
    rs2Val32    = cvt_Integer_to_Word32  rs2Val
    rs3Val32    = cvt_Integer_to_Word32  rs3Val
    sfOut       = f32MulAdd rmVal  rs1Val32  rs2Val32  rs3Val32

    -- Extract the results and the flags
    rdVal       = extractRdSPResult     sfOut
    flags       = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


fpu_f64MulAdd   ::  InstrField  -> Integer  -> Integer  -> Integer  -> (Integer, Integer)
fpu_f64MulAdd       frmVal         rs1Val      rs2Val      rs3Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal       = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    rs1Val64    = cvt_Integer_to_Word64  rs1Val
    rs2Val64    = cvt_Integer_to_Word64  rs2Val
    rs3Val64    = cvt_Integer_to_Word64  rs3Val
    sfOut       = f64MulAdd rmVal  rs1Val64  rs2Val64  rs3Val64

    -- Extract the results and the flags
    rdVal       = extractRdDPResult     sfOut
    flags       = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- FPU conversion operations
-- 64-bit signed integer to SP
--
fpu_i64ToF32    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_i64ToF32       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = i64ToF32   rmVal   (cvt_Integer_to_Int64   rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdSPResult     sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


-- 64-bit signed integer to DP
--
fpu_i64ToF64    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_i64ToF64       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = i64ToF64   rmVal   (cvt_Integer_to_Int64   rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdDPResult     sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- 64-bit unsigned integer to SP
--                              
fpu_ui64ToF32   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_ui64ToF32      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = ui64ToF32   rmVal   (cvt_Integer_to_Word64 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdSPResult     sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


-- 64-bit unsigned integer to DP
--                              
fpu_ui64ToF64   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_ui64ToF64      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = ui64ToF64   rmVal   (cvt_Integer_to_Word64 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdDPResult     sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- 32-bit signed integer to SP
--
fpu_i32ToF32    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_i32ToF32       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = i32ToF32   rmVal   (cvt_Integer_to_Int32   rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdSPResult     sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


-- 32-bit signed integer to DP
--
fpu_i32ToF64    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_i32ToF64       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = i32ToF64   rmVal   (cvt_Integer_to_Int32   rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdDPResult     sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- 32-bit unsigned integer to SP
--                              
fpu_ui32ToF32   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_ui32ToF32      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = ui32ToF32   rmVal   (cvt_Integer_to_Word32 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdSPResult     sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


-- 32-bit unsigned integer to DP
--                              
fpu_ui32ToF64   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_ui32ToF64      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = ui32ToF64   rmVal   (cvt_Integer_to_Word32 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdDPResult     sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- 32-bit SP to 64-bit DP
--
fpu_f32ToF64    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f32ToF64       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32ToF64   rmVal   (cvt_Integer_to_Word32  rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdDPResult     sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- 64-bit DP to 32-bit SP
--
fpu_f64ToF32    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f64ToF32       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64ToF32   rmVal   (cvt_Integer_to_Word64  rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdSPResult     sfOut
    flags   = extractFFlagsSPResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- SP to 32-bit unsigned integer
--                              
fpu_f32ToUi32   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f32ToUi32      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32ToUi32   rmVal   (cvt_Integer_to_Word32 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdWUResult     sfOut
    flags   = extractFFlagsWUResult sfOut
  in
    (flags, rdVal)


-- DP to 32-bit unsigned integer
--                              
fpu_f64ToUi32   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f64ToUi32      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64ToUi32   rmVal   (cvt_Integer_to_Word64 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdWUResult     sfOut
    flags   = extractFFlagsWUResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- SP to 32-bit signed integer
--                              
fpu_f32ToI32    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f32ToI32       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32ToI32    rmVal   (cvt_Integer_to_Word32 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdWResult      sfOut
    flags   = extractFFlagsWResult  sfOut
  in
    (flags, rdVal)


-- DP to 32-bit signed integer
--                              
fpu_f64ToI32    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f64ToI32       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64ToI32    rmVal   (cvt_Integer_to_Word64 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdWResult      sfOut
    flags   = extractFFlagsWResult  sfOut
  in
    (flags, rdVal)


-- ================================================================
-- SP to 64-bit unsigned integer
--                              
fpu_f32ToUi64   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f32ToUi64      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32ToUi64   rmVal   (cvt_Integer_to_Word32 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdLUResult     sfOut
    flags   = extractFFlagsLUResult sfOut
  in
    (flags, rdVal)


-- DP to 64-bit unsigned integer
--                              
fpu_f64ToUi64   :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f64ToUi64      frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64ToUi64   rmVal   (cvt_Integer_to_Word64 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdLUResult     sfOut
    flags   = extractFFlagsLUResult sfOut
  in
    (flags, rdVal)


-- ================================================================
-- SP to 64-bit signed integer
--                              
fpu_f32ToI64    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f32ToI64       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f32ToI64    rmVal   (cvt_Integer_to_Word32 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdLResult      sfOut
    flags   = extractFFlagsLResult  sfOut
  in
    (flags, rdVal)


-- DP to 64-bit signed integer
--                              
fpu_f64ToI64    :: InstrField   -> Integer  -> (Integer, Integer)
fpu_f64ToI64       frmVal          rs1Val   =
  let
    -- Convert the RISC-V rounding mode to one understood by SoftFloat
    rmVal   = frm_to_RoundingMode frmVal

    -- Soft-float call to carry out the operation
    sfOut  = f64ToI64    rmVal   (cvt_Integer_to_Word64 rs1Val)

    -- Extract the results and the flags
    rdVal   = extractRdLResult      sfOut
    flags   = extractFFlagsLResult  sfOut
  in
    (flags, rdVal)


-- ================================================================
#endif

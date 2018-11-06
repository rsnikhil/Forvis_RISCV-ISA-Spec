-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module FPU where

-- ================================================================
-- This module defines various RISC-V FPU functions.

-- ================================================================
-- Standard Haskell imports

import Data.Bits

-- Project imports

import Bit_Utils
import FP_Bit_Utils

-- Other library imports

import SoftFloat    -- from https://github.com/GaloisInc/softfloat-hs.git

-- Checks if a DP value is a signalling NaN
fpu_f64IsSNaN :: Integer -> Bool
fpu_f64IsSNaN    val =
  let
    (_, e, m) = extractFromDP  val
    qBitIsSet = testBit  m  51
    res = (e == 0x7ff) && (qBitIsSet == False) && (m /= 0)
  in 
    res


-- Checks if a DP value is a quiet NaN
fpu_f64IsQNaN :: Integer -> Bool
fpu_f64IsQNaN    val =
  let
    (_, e, m) = extractFromDP  val
    qBitIsSet = testBit  m  51
    res = (e == 0x7ff) && qBitIsSet
  in 
    res


-- Checks if a DP value is positive inifinity
fpu_f64IsPosInf :: Integer -> Bool
fpu_f64IsPosInf    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e == 0x7ff) && (m == 0)
  in 
    res


-- Checks if a DP value is a positive normal value
fpu_f64IsPosNorm :: Integer -> Bool
fpu_f64IsPosNorm    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e /= 0) && (e /= 0x7ff) 
  in 
    res


-- Checks if a DP value is a positive subnormal value
fpu_f64IsPosSubNorm :: Integer -> Bool
fpu_f64IsPosSubNorm    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e == 0x0) && (m /= 0)
  in 
    res


-- Checks if a DP value is a positive zero
fpu_f64IsPosZero :: Integer -> Bool
fpu_f64IsPosZero    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a DP value is a negative infinity
fpu_f64IsNegInf :: Integer -> Bool
fpu_f64IsNegInf    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0x1) && (e == 0x7ff) && (m == 0)
  in 
    res


-- Checks if a DP value is a negative normal value
fpu_f64IsNegNorm :: Integer -> Bool
fpu_f64IsNegNorm    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0x1) && (e /= 0) && (e /= 0x7ff) 
  in 
    res


-- Checks if a DP value is a negative subnormal value
fpu_f64IsNegSubNorm :: Integer -> Bool
fpu_f64IsNegSubNorm    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0x1) && (e == 0x0) && (m /= 0)
  in 
    res


-- Checks if a DP value is a negative zero value
fpu_f64IsNegZero :: Integer -> Bool
fpu_f64IsNegZero    val =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 1) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a SP value is a signalling NaN
fpu_f32IsSNaN :: Integer -> Bool
fpu_f32IsSNaN    val =
  let
    (s, e, m) = extractFromSP  val
    qBitIsSet = testBit  m  22
    res = (e == 0xff) && (qBitIsSet == False) && (m /= 0)
  in 
    res


-- Checks if a SP value is a quiet NaN
fpu_f32IsQNaN :: Integer -> Bool
fpu_f32IsQNaN    val =
  let
    (s, e, m) = extractFromSP  val
    qBitIsSet = testBit  m  22
    res = (e == 0xff) && qBitIsSet
  in 
    res


-- Checks if a SP value is positive inifinity
fpu_f32IsPosInf :: Integer -> Bool
fpu_f32IsPosInf    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0) && (e == 0xff) && (m == 0)
  in 
    res


-- Checks if a SP value is a positive normal value
fpu_f32IsPosNorm :: Integer -> Bool
fpu_f32IsPosNorm    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0) && (e /= 0) && (e /= 0xff) 
  in 
    res


-- Checks if a SP value is a positive subnormal value
fpu_f32IsPosSubNorm :: Integer -> Bool
fpu_f32IsPosSubNorm    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0) && (e == 0) && (m /= 0)
  in 
    res


-- Checks if a SP value is a positive zero
fpu_f32IsPosZero :: Integer -> Bool
fpu_f32IsPosZero    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a SP value is a negative infinity
fpu_f32IsNegInf :: Integer -> Bool
fpu_f32IsNegInf    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0x1) && (e == 0xff) && (m == 0)
  in 
    res


-- Checks if a SP value is a negative normal value
fpu_f32IsNegNorm :: Integer -> Bool
fpu_f32IsNegNorm    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0x1) && (e /= 0) && (e /= 0xff) 
  in 
    res


-- Checks if a SP value is a negative subnormal value
fpu_f32IsNegSubNorm :: Integer -> Bool
fpu_f32IsNegSubNorm    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0x1) && (e == 0) && (m /= 0)
  in 
    res


-- Checks if a SP value is a negative zero
fpu_f32IsNegZero :: Integer -> Bool
fpu_f32IsNegZero    val =
  let
    (s, e, m) = extractFromSP  val
    res = (s == 0x1) && (e == 0x0) && (m == 0)
  in 
    res


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
    (s, e, m) = extractFromDP  word
    s' = (if (s == 0) then 1 else 0)
    word' = composeDP  s'  e  m
  in
    word'


-- Flip the sign of a single precision value
negateS :: Integer -> Integer
negateS    word = 
  let
    (s, e, m) = extractFromSP  word
    s' = (if (s == 0) then 1 else 0)
    word' = composeSP  s'  e  m
  in
    word'


-- ================================================================
-- EQQ: test for equality (quiet)
-- DP1 == DP2
-- Checks if the first argument is equal to the second argument for two DP
-- numbers. Pos0 and Neg0 are considered equal. Sets INV only if either argument
-- is a SNaN (quiet)
fpu_f64EQQ :: Integer -> Integer -> (Bool, Integer)
fpu_f64EQQ    rs1        rs2 = 
  let
    (s1, e1, m1) = extractFromDP rs1
    (s2, e2, m2) = extractFromDP rs2

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
    (s1, e1, m1) = extractFromSP  rs1
    (s2, e2, m2) = extractFromSP  rs2
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
    (s1, e1, m1) = extractFromDP rs1
    (s2, e2, m2) = extractFromDP rs2

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
    (s1, e1, m1) = extractFromSP  rs1
    (s2, e2, m2) = extractFromSP  rs2

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
    (s1, e1, m1) = extractFromDP rs1
    (s2, e2, m2) = extractFromDP rs2

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
    (s1, e1, m1) = extractFromSP  rs1
    (s2, e2, m2) = extractFromSP  rs2

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
    rdVal   = extractRdDPResult sfOut
    flags   = extractFFlagsDPResult sfOut
  in
    (flags, rdVal)



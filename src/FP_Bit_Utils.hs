module FP_Bit_Utils where

-- ================================================================
-- This module has definitions specific to manipulating single and
-- double-precision IEEE floating point values. 
-- Also contains functions to make it more convenient to interface
-- to the softfloat library

-- ================================================================
-- Standard Haskell imports

import Data.Word    -- for Word8/16/32/64 (unsigned)
import Data.Int     -- for Int8/16/32/64 (signed)
import Data.Bits
import Data.Char
import Foreign.C.Types

-- Project imports

import Bit_Manipulation
import Arch_Defs
import SoftFloat


-- Definitions of Q-NaNs for single and double precision
canonicalNaN32 = 0x7fc00000 :: Word32
canonicalNaN64 = 0x7ff8000000000000 :: Word64

-- IEEE format for DP values
dp_sgn_bitpos           = 63 :: Int
dp_exp_bitpos           = 52 :: Int

-- IEEE format for DP values
sp_sgn_bitpos           = 31 :: Int
sp_exp_bitpos           = 23 :: Int

-- Extract the individual components (sign, exponent, mantissa) from a DP value
extractFromDP :: Integer -> (Integer, Integer, Integer)
extractFromDP (val) = 
  let
    sign       = bitSlice  val   dp_sgn_bitpos      dp_sgn_bitpos
    exponent   = bitSlice  val  (dp_sgn_bitpos-1)  (dp_exp_bitpos)
    mantissa   = bitSlice  val  (dp_exp_bitpos-1)   0
  in
    (sign, exponent, mantissa)

-- Compose a DP value from its components
composeDP :: Integer -> Integer -> Integer -> Integer
composeDP sgn  exp  man = 
  let
    res =     (shiftL  sgn  dp_sgn_bitpos)
          .|. (shiftL  exp  dp_exp_bitpos)
          .|.  man
  in
    res

-- Extract the individual components (sign, exponent, mantissa) from a DP value
extractFromSP :: Integer -> (Integer, Integer, Integer)
extractFromSP (val) = 
  let
    sign       = bitSlice  val   sp_sgn_bitpos      sp_sgn_bitpos
    exponent   = bitSlice  val  (sp_sgn_bitpos-1)  (sp_exp_bitpos)
    mantissa   = bitSlice  val  (sp_exp_bitpos-1)   0
  in
    (sign, exponent, mantissa)

-- Compose a SP value from its components. Note that the upper 32-bits
-- are unused and the value is not NaN-Boxed
composeSP :: Integer -> Integer -> Integer -> Integer
composeSP sgn  exp  man  = 
  let
    res =     (shiftL  sgn  sp_sgn_bitpos)
          .|. (shiftL  exp  sp_exp_bitpos)
          .|.  man
  in
    res

-- Checks if a DP value is a signalling NaN
f64IsSNaN :: Integer -> Bool
f64IsSNaN (val) =
  let
    (_, e, m) = extractFromDP  val
    qBitIsSet = testBit  m  51
    res = (e == 0x7ff) && (qBitIsSet == False) && (m /= 0)
  in 
    res


-- Checks if a DP value is a quiet NaN
f64IsQNaN :: Integer -> Bool
f64IsQNaN (val) =
  let
    (_, e, m) = extractFromDP  val
    qBitIsSet = testBit  m  51
    res = (e == 0x7ff) && qBitIsSet
  in 
    res


-- Checks if a DP value is positive inifinity
f64IsPosInf :: Integer -> Bool
f64IsPosInf (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e == 0x7ff) && (m == 0)
  in 
    res


-- Checks if a DP value is a positive normal value
f64IsPosNorm :: Integer -> Bool
f64IsPosNorm (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e /= 0) && (e /= 0x7ff) 
  in 
    res


-- Checks if a DP value is a positive subnormal value
f64IsPosSubNorm :: Integer -> Bool
f64IsPosSubNorm (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e == 0x0) && (m /= 0)
  in 
    res


-- Checks if a DP value is a positive zero
f64IsPosZero :: Integer -> Bool
f64IsPosZero (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a DP value is a negative infinity
f64IsNegInf :: Integer -> Bool
f64IsNegInf (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0x1) && (e == 0x7ff) && (m == 0)
  in 
    res


-- Checks if a DP value is a negative normal value
f64IsNegNorm :: Integer -> Bool
f64IsNegNorm (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0x1) && (e /= 0) && (e /= 0x7ff) 
  in 
    res


-- Checks if a DP value is a negative subnormal value
f64IsNegSubNorm :: Integer -> Bool
f64IsNegSubNorm (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 0x1) && (e == 0x0) && (m /= 0)
  in 
    res


-- Checks if a DP value is a negative zero value
f64IsNegZero :: Integer -> Bool
f64IsNegZero (val) =
  let
    (s, e, m) = extractFromDP  val
    res = (s == 1) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a SP value is a signalling NaN
f32IsSNaN :: Integer -> Bool
f32IsSNaN (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    qBitIsSet = testBit  m  22
    res = (e == 0xff) && (qBitIsSet == False) && (m /= 0)
  in 
    res


-- Checks if a SP value is a quiet NaN
f32IsQNaN :: Integer -> Bool
f32IsQNaN (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    qBitIsSet = testBit  m  22
    res = (e == 0xff) && qBitIsSet
  in 
    res


-- Checks if a SP value is positive inifinity
f32IsPosInf :: Integer -> Bool
f32IsPosInf (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0) && (e == 0xff) && (m == 0)
  in 
    res


-- Checks if a SP value is a positive normal value
f32IsPosNorm :: Integer -> Bool
f32IsPosNorm (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0) && (e /= 0) && (e /= 0xff) 
  in 
    res


-- Checks if a SP value is a positive subnormal value
f32IsPosSubNorm :: Integer -> Bool
f32IsPosSubNorm (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0) && (e == 0) && (m /= 0)
  in 
    res


-- Checks if a SP value is a positive zero
f32IsPosZero :: Integer -> Bool
f32IsPosZero (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0) && (e == 0x0) && (m == 0)
  in 
    res


-- Checks if a SP value is a negative infinity
f32IsNegInf :: Integer -> Bool
f32IsNegInf (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0x1) && (e == 0xff) && (m == 0)
  in 
    res


-- Checks if a SP value is a negative normal value
f32IsNegNorm :: Integer -> Bool
f32IsNegNorm (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0x1) && (e /= 0) && (e /= 0xff) 
  in 
    res


-- Checks if a SP value is a negative subnormal value
f32IsNegSubNorm :: Integer -> Bool
f32IsNegSubNorm (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0x1) && (e == 0) && (m /= 0)
  in 
    res


-- Checks if a SP value is a negative zero
f32IsNegZero :: Integer -> Bool
f32IsNegZero (val) =
  let
    (s, e, m) = extractFromSP  (zeroExtend_u32_to_u64  val)
    res = (s == 0x1) && (e == 0x0) && (m == 0)
  in 
    res


-- Unbox a NaN-boxed value
unboxSP :: Integer -> Integer
unboxSP  rawVal =
  let
    upperBits = bitSlice rawVal 63 32
    unboxedVal = (
      if (upperBits == 0xffffffff) then (bitSlice rawVal 31 0)
      else canonicalNaN32)
  in
    unboxedVal


-- NaN-box bit [32] to full width
nanBox :: Integer -> Integer
nanBox  word =
  let
    fill_bits = (shiftL  1  32) - 1    -- all ones 
    fill_mask = shiftL  fill_bits  32
    word'     = word .|. fill_mask
  in
    word'


-- Flip the sign of a double precision value
negateD :: Integer -> Integer
negateD  word = 
  let
    (s, e, m) = extractFromDP  word
    s' = (if (s == 0) then 1 else 0)
    word' = composeDP  s'  e  m
  in
    word'


-- Flip the sign of a single precision value
negateS :: Integer -> Integer
negateS  word = 
  let
    (s, e, m) = extractFromSP  word
    s' = (if (s == 0) then 1 else 0)
    word' = composeSP  s'  e  m
  in
    word'

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
extractRdSPResult :: F32Result -> Integer
extractRdSPResult (Result res flags) = cvt_Word32_to_Integer  res

extractFFlagsSPResult :: F32Result -> Integer
extractFFlagsSPResult (Result res flags) = 
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
extractRdDPResult :: F64Result -> Integer
extractRdDPResult (Result res flags) = cvt_Word64_to_Integer  res

extractFFlagsDPResult :: F64Result -> Integer
extractFFlagsDPResult (Result res flags) = 
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
extractRdLResult  :: I64Result -> Integer
extractRdLResult  (Result res flags) = cvt_Integer_to_2s_comp  64  (cvt_Int64_to_Integer  res)

extractFFlagsLResult :: I64Result -> Integer
extractFFlagsLResult (Result res flags) = 
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
extractRdLUResult :: Ui64Result -> Integer
extractRdLUResult  (Result res flags) = cvt_Word64_to_Integer  res

extractFFlagsLUResult :: Ui64Result -> Integer
extractFFlagsLUResult (Result res flags) = 
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
extractRdWResult  :: I32Result -> Integer
extractRdWResult  (Result res flags) = cvt_Integer_to_2s_comp  32  (cvt_Int32_to_Integer  res)

extractFFlagsWResult :: I32Result -> Integer
extractFFlagsWResult (Result res flags) = 
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
extractRdWUResult :: Ui32Result -> Integer
extractRdWUResult  (Result res flags) = cvt_Word32_to_Integer  res

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


-- DP1 == DP2
-- Checks if the first argument is equal to the second argument for two DP
-- numbers. Pos0 and Neg0 are considered equal. Sets INV only if either argument
-- is a SNaN (quiet)
f64IsEQQ :: Integer -> Integer -> (Bool, Integer)
f64IsEQQ rs1 rs2 = 
  let
    (s1, e1, m1) = extractFromDP rs1
    (s2, e2, m2) = extractFromDP rs2

    rs1IsSNaN = f64IsSNaN     rs1
    rs2IsSNaN = f64IsSNaN     rs2
    rs1IsPos0 = f64IsPosZero  rs1
    rs2IsPos0 = f64IsPosZero  rs2
    rs1IsNeg0 = f64IsNegZero  rs1
    rs2IsNeg0 = f64IsNegZero  rs2
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
f32IsEQQ :: Integer -> Integer -> (Bool, Integer)
f32IsEQQ rs1 rs2 = 
  let
    (s1, e1, m1) = extractFromSP (zeroExtend_u32_to_u64 rs1)
    (s2, e2, m2) = extractFromSP (zeroExtend_u32_to_u64 rs2)
    rs1IsSNaN = f32IsSNaN rs1
    rs2IsSNaN = f32IsSNaN rs2
    rs1IsPos0 = f32IsPosZero  rs1
    rs2IsPos0 = f32IsPosZero  rs2
    rs1IsNeg0 = f32IsNegZero  rs1
    rs2IsNeg0 = f32IsNegZero  rs2
    rs1Is0    = rs1IsNeg0 || rs1IsPos0
    rs2Is0    = rs2IsNeg0 || rs2IsPos0

    res = (rs1 == rs2) || (rs1Is0 && rs2Is0)
    fflags = if (rs1IsSNaN || rs2IsSNaN) then nvFlag else 0
  in
    (res, fflags)


-- DP1 <= DP2
-- Checks if the first argument is less than or equal to the second argument for
-- two DP numbers. The isQuiet flag controls when the INV flag is set
f64IsLE :: Integer -> Integer -> Bool -> (Bool, Integer)
f64IsLE rs1 rs2 isQuiet = 
  let
    (s1, e1, m1) = extractFromDP rs1
    (s2, e2, m2) = extractFromDP rs2

    rs1IsQNaN = f64IsQNaN rs1
    rs2IsQNaN = f64IsQNaN rs2
    rs1IsSNaN = f64IsSNaN rs1
    rs2IsSNaN = f64IsSNaN rs2
    rs1IsNaN = rs1IsQNaN || rs1IsSNaN
    rs2IsNaN = rs2IsQNaN || rs2IsSNaN

    rs1IsPos0  = f64IsPosZero  rs1
    rs2IsPos0  = f64IsPosZero  rs2
    rs1IsNeg0  = f64IsNegZero  rs1
    rs2IsNeg0  = f64IsNegZero  rs2
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
f32IsLE :: Integer -> Integer -> Bool -> (Bool, Integer)
f32IsLE rs1 rs2 isQuiet = 
  let
    (s1, e1, m1) = extractFromSP (zeroExtend_u32_to_u64 rs1)
    (s2, e2, m2) = extractFromSP (zeroExtend_u32_to_u64 rs2)

    rs1IsQNaN = f32IsQNaN rs1
    rs2IsQNaN = f32IsQNaN rs2
    rs1IsSNaN = f32IsSNaN rs1
    rs2IsSNaN = f32IsSNaN rs2
    rs1IsNaN = rs1IsQNaN || rs1IsSNaN
    rs2IsNaN = rs2IsQNaN || rs2IsSNaN

    rs1IsPos0  = f32IsPosZero  rs1
    rs2IsPos0  = f32IsPosZero  rs2
    rs1IsNeg0  = f32IsNegZero  rs1
    rs2IsNeg0  = f32IsNegZero  rs2
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


-- DP1 < DP2
-- Checks if the first argument is less than the second argument for
-- two DP numbers. The isQuiet flag controls when the INV flag is set
f64IsLT :: Integer -> Integer -> Bool -> (Bool, Integer)
f64IsLT rs1 rs2 isQuiet = 
  let
    (s1, e1, m1) = extractFromDP rs1
    (s2, e2, m2) = extractFromDP rs2

    rs1IsQNaN  = f64IsQNaN     rs1
    rs2IsQNaN  = f64IsQNaN     rs2
    rs1IsSNaN  = f64IsSNaN     rs1
    rs2IsSNaN  = f64IsSNaN     rs2
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


-- SP1 <= SP2
-- Checks if the first argument is less than or equal to the second argument for
-- two SP numbers. The isQuiet flag controls when the INV flag is set
f32IsLT :: Integer -> Integer -> Bool -> (Bool, Integer)
f32IsLT rs1 rs2 isQuiet = 
  let
    (s1, e1, m1) = extractFromSP (zeroExtend_u32_to_u64 rs1)
    (s2, e2, m2) = extractFromSP (zeroExtend_u32_to_u64 rs2)

    rs1IsQNaN  = f32IsQNaN    rs1
    rs2IsQNaN  = f32IsQNaN    rs2
    rs1IsSNaN  = f32IsSNaN    rs1
    rs2IsSNaN  = f32IsSNaN    rs2
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

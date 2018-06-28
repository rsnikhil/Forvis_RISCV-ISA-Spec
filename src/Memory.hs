-- See LICENSE for license details

module Memory where

-- ================================================================
-- This module defines an abstraction ('Mem') for
-- a RISC-V Memory (byte-addressed),
-- accessed via the exported get/set API.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.Map.Strict as Data_Map

-- Project imports

import Bit_Manipulation
import Arch_Defs
import Mem_Ops

-- ================================================================
-- All memory locations have this value until written
-- This is just for debugging convenience, not part of the spec.

uninitialized_byte :: Word8
uninitialized_byte = 0x00
-- uninitialized_byte = 0xaa

-- ================================================================
-- Memory representation: Data.Map.Map from Word64 (address) to bytes
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data Mem = Mem { f_dm            :: Data_Map.Map  Word64  Word8,
                 f_reserved_addr :: Maybe (Word64, Word64)
               }

mkMem :: [(Int, Word8)] -> Mem
mkMem  addr_byte_list =
  let
    addr_byte_list' = map  (\(addr,byte) -> (fromIntegral addr, byte))
                           addr_byte_list
  in
    Mem  (Data_Map.fromList addr_byte_list')  Nothing

-- ================================================================
-- Read data from memory
-- TODO: Currently we only return Mem_Result_Ok and never return Mem_Result_Err
--     We could return Mem_Result_Err on uninitialized locations.
--     We could return Mem_Result_Err on misaligned accesses.
--     We could return Mem_Result_Err if there are address bounds.

mem_read :: Mem -> InstrField -> Word64 -> (Mem_Result, Mem)
mem_read  mem  funct3  addr =
  let
    dm = f_dm  mem
    read_byte  offset = case (Data_Map.lookup  (addr + offset)  dm) of
                          Just b  -> b
                          Nothing -> uninitialized_byte

    (b0, b1, b2, b3, b4, b5, b6, b7) = (read_byte  0, read_byte  1, read_byte  2, read_byte  3,
                                        read_byte  4, read_byte  5, read_byte  6, read_byte  7)

    u64 | ((funct3 == funct3_LB) || (funct3 == funct3_LBU)) = (mk_u64 b0  0  0  0  0  0  0  0)
        | ((funct3 == funct3_LH) || (funct3 == funct3_LHU)) = (mk_u64 b0 b1  0  0  0  0  0  0)
        | ((funct3 == funct3_LW) || (funct3 == funct3_LWU)) = (mk_u64 b0 b1 b2 b3  0  0  0  0)
        |  (funct3 == funct3_LD)                            = (mk_u64 b0 b1 b2 b3 b4 b5 b6 b7)
  in
    (Mem_Result_Ok u64, mem)

-- ================================================================
-- Write data into memory
-- TODO: Currently we only return Mem_Result_Ok and never return Mem_Result_Err
-- We could return Mem_Result_Err on uninitialized locations.
-- We could return Mem_Result_Err on misaligned accesses.
-- We could return Mem_Result_Err if there are address bounds.

mem_write :: Mem -> InstrField -> Word64 -> Word64 -> (Mem_Result, Mem)
mem_write  mem  funct3  addr  stv =
  let
    dm              = f_dm             mem
    m_reserved_addr = f_reserved_addr  mem
    (a1,a2) = if (funct3 == funct3_SD) then (addr, addr + 7)
              else (addr, addr + 3)

    stv0 = cvt_u64_to_u8  stv
    stv1 = cvt_u64_to_u8  (shiftR stv  8)
    stv2 = cvt_u64_to_u8  (shiftR stv 16)
    stv3 = cvt_u64_to_u8  (shiftR stv 24)
    stv4 = cvt_u64_to_u8  (shiftR stv 32)
    stv5 = cvt_u64_to_u8  (shiftR stv 40)
    stv6 = cvt_u64_to_u8  (shiftR stv 48)
    stv7 = cvt_u64_to_u8  (shiftR stv 56)

    dm0 = Data_Map.insert  (addr + 0)  stv0  dm
    dm1 = Data_Map.insert  (addr + 1)  stv1  dm0
    dm2 = Data_Map.insert  (addr + 2)  stv2  dm1
    dm3 = Data_Map.insert  (addr + 3)  stv3  dm2
    dm4 = Data_Map.insert  (addr + 4)  stv4  dm3
    dm5 = Data_Map.insert  (addr + 5)  stv5  dm4
    dm6 = Data_Map.insert  (addr + 6)  stv6  dm5
    dm7 = Data_Map.insert  (addr + 7)  stv7  dm6

    dm' | (funct3 == funct3_SB) = dm0
        | (funct3 == funct3_SH) = dm1
        | (funct3 == funct3_SW) = dm3
        | (funct3 == funct3_SD) = dm7

    m_reserved_addr' | Nothing      <- m_reserved_addr = Nothing
                     | Just (r1,r2) <- m_reserved_addr = if addrs_overlap  a1  a2  r1  r2 then
                                                           Nothing
                                                         else
                                                           m_reserved_addr
  in
     (Mem_Result_Ok  0, Mem  dm'  m_reserved_addr')

addrs_overlap :: Word64 -> Word64 -> Word64 -> Word64 -> Bool
addrs_overlap  a1  a2  r1  r2 = ((   (a1 <= r1) && (r1 <= a2))
                                 || ((a1 <= r2) && (r2 <= a2)))

-- ================================================================
-- AMO op

mem_amo :: Mem -> Word64 -> InstrField -> InstrField -> InstrField -> InstrField -> Word64 ->
           (Mem_Result, Mem)

mem_amo  mem  addr  funct3  msbs5  aq  rl  stv_in =
  let
    dm              = f_dm  mem
    m_reserved_addr = f_reserved_addr  mem
    (a1,a2) = if (funct3 == funct3_AMO_D) then (addr, addr + 7)
              else (addr, addr + 3)

    -- Incoming store-value bytes
    stv  = if (funct3 == funct3_AMO_W) then (stv_in .&. 0xffffFFFF) else stv_in
    stv0 = fromIntegral (stv             .&. 0xFF)
    stv1 = fromIntegral ((shiftR stv  8) .&. 0xFF)
    stv2 = fromIntegral ((shiftR stv 16) .&. 0xFF)
    stv3 = fromIntegral ((shiftR stv 24) .&. 0xFF)
    stv4 = fromIntegral ((shiftR stv 32) .&. 0xFF)
    stv5 = fromIntegral ((shiftR stv 40) .&. 0xFF)
    stv6 = fromIntegral ((shiftR stv 48) .&. 0xFF)
    stv7 = fromIntegral ((shiftR stv 56) .&. 0xFF)

    fn_read_byte  offset = case (Data_Map.lookup  (addr + offset)  dm) of
                             Just b  -> b
                             Nothing -> uninitialized_byte

    -- Old memory value
    (omv0, omv1, omv2, omv3,
     omv4, omv5, omv6, omv7) = (fn_read_byte  0, fn_read_byte  1, fn_read_byte  2, fn_read_byte  3,
                                fn_read_byte  4, fn_read_byte  5, fn_read_byte  6, fn_read_byte  7)
    omv = if (funct3 == funct3_AMO_W) then
            mk_u64  omv0  omv1  omv2  omv3     0     0     0     0
          else
            mk_u64  omv0  omv1  omv2  omv3  omv4  omv5  omv6  omv7

    reserved_addr_hit = case m_reserved_addr of
                          Nothing -> False
                          Just (r1,r2) -> addrs_overlap  a1  a2  r1  r2

    -- Load-value (to be returned to CPU)
    ldv | (msbs5  == msbs5_AMO_SC) = if reserved_addr_hit then 0 else 1
        | (funct3 == funct3_AMO_W) = mk_u64  omv0  omv1  omv2  omv3  0     0     0     0
        | (funct3 == funct3_AMO_D) = omv

        -- New memory value (to be stored back)
    nmv | (msbs5 == msbs5_AMO_SC)   = stv
        | (msbs5 == msbs5_AMO_SWAP) = stv
        | (msbs5 == msbs5_AMO_ADD)  = (if (funct3 == funct3_AMO_W) then
                                         let
                                           omv_32 = mk_u32  omv0  omv1  omv2  omv3
                                           stv_32 = mk_u32  stv0  stv1  stv2  stv3
                                           z_32  = cvt_s32_to_u32 ((cvt_u32_to_s32  omv_32) + (cvt_u32_to_s32  stv_32))
                                         in
                                           zeroExtend_u32_to_u64  z_32
                                       else
                                         cvt_s64_to_u64 ((cvt_u64_to_s64  omv) + (cvt_u64_to_s64  stv)))
        | (msbs5 == msbs5_AMO_ADD)  = cvt_s64_to_u64 ((cvt_u64_to_s64  omv) + (cvt_u64_to_s64  stv))
        | (msbs5 == msbs5_AMO_AND)  = (omv .&. stv)
        | (msbs5 == msbs5_AMO_OR)   = (omv .|. stv)
        | (msbs5 == msbs5_AMO_XOR)  = xor  omv  stv
        | (msbs5 == msbs5_AMO_MAX)  = (if (funct3 == funct3_AMO_W) then
                                         let
                                           omv_32 = mk_u32  omv0  omv1  omv2  omv3
                                           stv_32 = mk_u32  stv0  stv1  stv2  stv3
                                           z_32   = if ((cvt_u32_to_s32  omv_32) > (cvt_u32_to_s32  stv_32)) then omv_32
                                                    else stv_32
                                         in
                                           zeroExtend_u32_to_u64  z_32
                                       else
                                         if (cvt_u64_to_s64  omv > cvt_u64_to_s64  stv) then
                                           omv
                                         else
                                           stv)
        | (msbs5 == msbs5_AMO_MIN)  = (if (funct3 == funct3_AMO_W) then
                                         let
                                           omv_32 = mk_u32  omv0  omv1  omv2  omv3
                                           stv_32 = mk_u32  stv0  stv1  stv2  stv3
                                           z_32   = if ((cvt_u32_to_s32  omv_32) < (cvt_u32_to_s32  stv_32)) then omv_32
                                                    else stv_32
                                         in
                                           zeroExtend_u32_to_u64  z_32
                                       else
                                         if ((cvt_u64_to_s64  omv) < (cvt_u64_to_s64  stv)) then omv
                                         else stv)

        | (msbs5 == msbs5_AMO_MAXU) = if (omv > stv) then omv else stv
        | (msbs5 == msbs5_AMO_MINU) = if (omv < stv) then omv else stv

    nmv0 = fromIntegral (nmv             .&. 0xFF)
    nmv1 = fromIntegral ((shiftR nmv  8) .&. 0xFF)
    nmv2 = fromIntegral ((shiftR nmv 16) .&. 0xFF)
    nmv3 = fromIntegral ((shiftR nmv 24) .&. 0xFF)
    nmv4 = fromIntegral ((shiftR nmv 32) .&. 0xFF)
    nmv5 = fromIntegral ((shiftR nmv 40) .&. 0xFF)
    nmv6 = fromIntegral ((shiftR nmv 48) .&. 0xFF)
    nmv7 = fromIntegral ((shiftR nmv 56) .&. 0xFF)

    m_reserved_addr' | (msbs5 == msbs5_AMO_LR) = Just (a1, a2)    -- replace old reservation
                     | (msbs5 == msbs5_AMO_SC) = Nothing          -- always cancel old reservation
                     | reserved_addr_hit       = Nothing          -- cancel old reservation for other AMOs
                     | True                    = m_reserved_addr  -- preserve reservation

    dm' | (msbs5 == msbs5_AMO_LR)                              = dm
        | ((msbs5 == msbs5_AMO_SC) && (not reserved_addr_hit)) = dm
        | True = (let
                     dm1 = Data_Map.insert  addr        nmv0  dm
                     dm2 = Data_Map.insert  (addr + 1)  nmv1  dm1
                     dm3 = Data_Map.insert  (addr + 2)  nmv2  dm2
                     dm4 = Data_Map.insert  (addr + 3)  nmv3  dm3
                     dm5 = Data_Map.insert  (addr + 4)  nmv4  dm4
                     dm6 = Data_Map.insert  (addr + 5)  nmv5  dm5
                     dm7 = Data_Map.insert  (addr + 6)  nmv6  dm6
                     dm8 = Data_Map.insert  (addr + 7)  nmv7  dm7
                  in
                     if (funct3 == funct3_AMO_W) then dm4 else dm8)
  in
    (Mem_Result_Ok  ldv, Mem  dm'  m_reserved_addr')

-- ================================================================
-- Utility to combine bytes into words (u32) and doublewords (u64)

mk_u32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mk_u32  byte0  byte1  byte2  byte3 =
  let
    w0 :: Word32; w0 = fromIntegral byte0
    w1 :: Word32; w1 = fromIntegral byte1
    w2 :: Word32; w2 = fromIntegral byte2
    w3 :: Word32; w3 = fromIntegral byte3
    w  = ((shiftL w3 24) .|. (shiftL w2 16) .|. (shiftL w1  8) .|. w0)
  in
    w

mk_u64 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
mk_u64  byte0  byte1  byte2  byte3  byte4  byte5  byte6  byte7 =
  let
    d0 :: Word64; d0 = fromIntegral byte0
    d1 :: Word64; d1 = fromIntegral byte1
    d2 :: Word64; d2 = fromIntegral byte2
    d3 :: Word64; d3 = fromIntegral byte3
    d4 :: Word64; d4 = fromIntegral byte4
    d5 :: Word64; d5 = fromIntegral byte5
    d6 :: Word64; d6 = fromIntegral byte6
    d7 :: Word64; d7 = fromIntegral byte7
    d  = ((shiftL d7 56) .|. (shiftL d6 48) .|. (shiftL d5 40) .|. (shiftL d4 32) .|.
          (shiftL d3 24) .|. (shiftL d2 16) .|. (shiftL d1  8) .|.         d0)
  in
    d

-- ================================================================

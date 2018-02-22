module Memory (Mem, mkMem,
               getMem8, getMem16, getMem32, getMem64,
               setMem8, setMem16, setMem32, setMem64
              ) where

-- ================================================================
-- This module defines an abstraction ('Mem') for
-- a RISC-V Memory (byte-addressed),
-- accessed via the exported get/set API.

-- ================================================================
-- Standard Haskell imports

import Control.Monad
import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.Map.Strict as Data_Map
import Numeric (showHex, readHex)

-- Project imports

import ArchDefs64

-- ================================================================
-- Memory representation: Data.Map.Map from MachineWords (addresses) to bytes
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

newtype Mem = Mem_Con (Data_Map.Map  MachineWord  Word8)

mkMem :: [(Int, Word8)] -> Mem
mkMem  addr_byte_list = mem
  where
    addr_byte_list' = map (\(addr,byte) -> (fromIntegral addr, byte))
                          addr_byte_list
    mem             = Mem_Con (Data_Map.fromList addr_byte_list')

uninitialized_data :: Integer
uninitialized_data = 0xaaaaAAAAaaaaAAAA

-- ----------------
-- get (read) data from memory
-- Currently we only return LoadResult_Ok and never return LoadResult_Err
-- We could return LoadResult_Err on uninitialized locations.
-- We could return LoadResult_Err if there are address bounds.

getMem8 :: Mem -> MachineWord -> LoadResult Word8
getMem8  (Mem_Con dm)  addr = result
  where m_b0 = Data_Map.lookup  addr  dm
        result = case (m_b0) of
                   (Just b0) -> LoadResult_Ok  b0
                   _         -> LoadResult_Ok  (fromIntegral  uninitialized_data)

getMem16 :: Mem -> MachineWord -> LoadResult Word16
getMem16  (Mem_Con dm)  addr = result
  where m_b0 = Data_Map.lookup  addr        dm
        m_b1 = Data_Map.lookup  (addr + 1)  dm
        result = case (m_b0, m_b1) of
                   (Just b0, Just b1) -> LoadResult_Ok  (mk_u16  b0  b1)
                   _                  -> LoadResult_Ok  (fromIntegral  uninitialized_data)

getMem32 :: Mem -> MachineWord -> LoadResult Word32
getMem32  (Mem_Con dm)  addr = result
  where m_b0 = Data_Map.lookup  addr        dm
        m_b1 = Data_Map.lookup  (addr + 1)  dm
        m_b2 = Data_Map.lookup  (addr + 2)  dm
        m_b3 = Data_Map.lookup  (addr + 3)  dm
        result = case (m_b0, m_b1, m_b2, m_b3) of
                   (Just b0, Just b1, Just b2, Just b3) -> LoadResult_Ok  (mk_u32  b0  b1  b2  b3)
                   _                                    -> LoadResult_Ok  (fromIntegral  uninitialized_data)

getMem64 :: Mem -> MachineWord -> LoadResult Word64
getMem64  (Mem_Con dm)  addr = result
  where m_b0 = Data_Map.lookup  addr        dm
        m_b1 = Data_Map.lookup  (addr + 1)  dm
        m_b2 = Data_Map.lookup  (addr + 2)  dm
        m_b3 = Data_Map.lookup  (addr + 3)  dm
        m_b4 = Data_Map.lookup  (addr + 4)  dm
        m_b5 = Data_Map.lookup  (addr + 5)  dm
        m_b6 = Data_Map.lookup  (addr + 6)  dm
        m_b7 = Data_Map.lookup  (addr + 7)  dm
        result = case (m_b0, m_b1, m_b2, m_b3, m_b4, m_b5, m_b6, m_b7) of
                   (Just b0, Just b1, Just b2, Just b3,
                    Just b4, Just b5, Just b6, Just b7) -> LoadResult_Ok  (mk_u64  b0  b1  b2  b3  b4  b5  b6  b7)
                   _                                    -> LoadResult_Ok  (fromIntegral  uninitialized_data)

-- ----------------
-- set (write) data into memory
-- Currently we don't return any StoreResult.
-- We could return StoreResult_Err if there are address bounds.

setMem8 :: Mem -> MachineWord -> Word8 -> Mem
setMem8  (Mem_Con dm)  addr  val = Mem_Con dm1
  where byte0 = val
        dm1 = Data_Map.insert  addr  byte0  dm

setMem16 :: Mem -> MachineWord -> Word16 -> Mem
setMem16  (Mem_Con dm)  addr  val = Mem_Con dm2
  where byte0 = fromIntegral (val            .&. 0xFF)
        byte1 = fromIntegral ((shiftR val 8) .&. 0xFF)

        dm1 = Data_Map.insert  addr        byte0  dm
        dm2 = Data_Map.insert  (addr + 1)  byte1  dm1

setMem32 :: Mem -> MachineWord -> Word32 -> Mem
setMem32  (Mem_Con dm)  addr  val = Mem_Con dm4
  where byte0 = fromIntegral (val             .&. 0xFF)
        byte1 = fromIntegral ((shiftR val 8)  .&. 0xFF)
        byte2 = fromIntegral ((shiftR val 16) .&. 0xFF)
        byte3 = fromIntegral ((shiftR val 24) .&. 0xFF)

        dm1 = Data_Map.insert  addr        byte0  dm
        dm2 = Data_Map.insert  (addr + 1)  byte1  dm1
        dm3 = Data_Map.insert  (addr + 2)  byte2  dm2
        dm4 = Data_Map.insert  (addr + 3)  byte3  dm3

setMem64 :: Mem -> MachineWord -> Word64 -> Mem
setMem64  (Mem_Con dm)  addr  val = Mem_Con dm8
  where byte0 = fromIntegral (val             .&. 0xFF)
        byte1 = fromIntegral ((shiftR val  8) .&. 0xFF)
        byte2 = fromIntegral ((shiftR val 16) .&. 0xFF)
        byte3 = fromIntegral ((shiftR val 24) .&. 0xFF)
        byte4 = fromIntegral ((shiftR val 32) .&. 0xFF)
        byte5 = fromIntegral ((shiftR val 40) .&. 0xFF)
        byte6 = fromIntegral ((shiftR val 48) .&. 0xFF)
        byte7 = fromIntegral ((shiftR val 56) .&. 0xFF)

        dm1 = Data_Map.insert  addr        byte0  dm
        dm2 = Data_Map.insert  (addr + 1)  byte1  dm1
        dm3 = Data_Map.insert  (addr + 2)  byte2  dm2
        dm4 = Data_Map.insert  (addr + 3)  byte3  dm3
        dm5 = Data_Map.insert  (addr + 4)  byte4  dm4
        dm6 = Data_Map.insert  (addr + 5)  byte5  dm5
        dm7 = Data_Map.insert  (addr + 6)  byte6  dm6
        dm8 = Data_Map.insert  (addr + 7)  byte7  dm7

-- ================================================================
-- Utilities to combine bytes into halfwords (u16) , words (u32) and
-- doublewords (u64)

mk_u16 :: Word8 -> Word8 -> Word16
mk_u16  byte0  byte1 = hw
  where hw0 :: Word16; hw0 = fromIntegral byte0
        hw1 :: Word16; hw1 = fromIntegral byte1
        hw  = (shiftL hw1 8) .|. hw0

mk_u32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mk_u32  byte0  byte1  byte2  byte3 = w
  where w0 :: Word32; w0 = fromIntegral byte0
        w1 :: Word32; w1 = fromIntegral byte1
        w2 :: Word32; w2 = fromIntegral byte2
        w3 :: Word32; w3 = fromIntegral byte3
        w  = (shiftL w3 24) .|. (shiftL w2 16) .|. (shiftL w1 8) .|. w0

mk_u64 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
mk_u64  byte0  byte1  byte2  byte3  byte4  byte5  byte6  byte7 = d
  where d0 :: Word64; d0 = fromIntegral byte0
        d1 :: Word64; d1 = fromIntegral byte1
        d2 :: Word64; d2 = fromIntegral byte2
        d3 :: Word64; d3 = fromIntegral byte3
        d4 :: Word64; d4 = fromIntegral byte4
        d5 :: Word64; d5 = fromIntegral byte5
        d6 :: Word64; d6 = fromIntegral byte6
        d7 :: Word64; d7 = fromIntegral byte7
        d  = (shiftL d7 56) .|. (shiftL d6 48) .|. (shiftL d5 40) .|. (shiftL d4 32) .|.
             (shiftL d3 24) .|. (shiftL d2 16) .|. (shiftL d1  8) .|.         d0

-- ================================================================

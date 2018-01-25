module ReadHexFile (readHexFile) where

-- ================================================================
-- This code is taken from MIT's riscv-semantics repo

-- This module implements a function that reads a hex-memory file
-- and returns a memory (i.e., list of (addr, byte)).

-- ================================================================
-- Standard Haskell imports

import System.IO
import Data.Word
import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

import Elf

-- ================================================================
-- Read the RISC-V Hex file and return a memory and possibly address of "tohost"' symbol

readHexFile :: FilePath -> IO [(Int, Word8)]
readHexFile f = do
  h <- openFile f ReadMode
  helper h (0, [])
  where helper h l = do
          s <- hGetLine h
          done <- hIsEOF h
          if (null s)
            then return (snd l)
            else if done
                 then return  (snd  (processLine s l))
                 else helper  h  (processLine s l)

processLine :: String -> (Int, [(Int, Word8)]) -> (Int, [(Int, Word8)])
processLine ('@':xs) (p, l) = ((fst $ head $ readHex xs) * 4, l)
processLine s (p, l) = (p + 4, l ++ (zip [p..] $ splitWord (fst $ head $ readHex s :: Word32)))

splitWord :: Word32 -> [Word8]
splitWord  u32 = map  byte_at  [0, 8, 16, 24]
  where byte_at :: Int -> Word8
        byte_at  lsb = fromIntegral ((shiftR  u32  lsb) .&. 0xFF)

-- ================================================================

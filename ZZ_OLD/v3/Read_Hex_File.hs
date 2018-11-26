-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Read_Hex_File where

-- ================================================================
-- This code is adapted from MIT's riscv-semantics repo

-- This module implements a function that reads a hex-memory file
-- and returns a memory (i.e., list of (addr, byte)).

-- ================================================================
-- Standard Haskell imports

import System.IO
import Data.Char
import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

-- None

-- ================================================================
-- Read a Mem-Hex file (each datum should represent one byte)
-- and return a memory (list of (addr,byte))

read_hex8_file :: FilePath -> IO [(Integer, Integer)]
read_hex8_file f = do
  let
    helper h  line_num  next_addr  mem = do
      s    <- hGetLine h
      if (null s)
        then (do
                 return (reverse mem))
        else (do
                 let (next_addr', mem') = process_hex8_line  s  next_addr
                 done <- hIsEOF h
                 if done
                   then return  (reverse (mem' ++ mem))
                   else helper  h  (line_num + 1)  next_addr'  (mem' ++ mem))

  h <- openFile f ReadMode
  helper h 0 0 []

-- Process a line from an 8b Mem-Hex file, which is
-- either an address line ('@hex-address')
-- or a data line (a hex byte in memory)

process_hex8_line :: String -> Integer -> (Integer, [(Integer, Integer)])
process_hex8_line  ('@':xs)  next_addr = (fst $ head $ readHex (dropWhile  isSpace  xs), [])
process_hex8_line  line      next_addr =
  let
    parses = (readHex (dropWhile isSpace line) :: [(Integer, String)])
  in
    case parses of
      []            -> (next_addr, [])
      (word8, _):_  -> (next_addr + 1, [ (next_addr, word8) ])

-- ================================================================
-- Read a Mem-Hex file (each datum should represent 32 bits (4 bytes))
-- and return a memory (list of (addr,byte))

read_hex32_file :: FilePath -> IO [(Integer, Integer)]
read_hex32_file f = do
  let
    helper h  line_num  next_addr  mem = do
      s <- hGetLine h
      if (null s)
        then (do
                 return (reverse mem))
        else (do
                 let (next_addr', mem') = process_hex32_line  s  next_addr
                 done <- hIsEOF h
                 if done
                   then return  (reverse (mem' ++ mem))
                   else helper  h  (line_num + 1)  next_addr'  (mem' ++ mem))

  h <- openFile f ReadMode
  helper  h  0  0  []

-- Process a line from a 32b Mem-Hex file, which is
-- either an address line ('@hex-address')
-- or a data line (a hex 32-bit value in memory)

process_hex32_line :: String -> Integer -> (Integer, [(Integer, Integer)])
process_hex32_line  ('@':xs)  next_addr = (fst $ head $ readHex (dropWhile isSpace xs), [])
process_hex32_line  line      next_addr =
  let
    parses = (readHex (dropWhile isSpace line) :: [(Integer, String)])
  in
    case parses of
      []             -> (next_addr, [])
      (word32, _):_  -> (let
                            b3, b2, b1, b0 :: Integer
                            b3     = fromIntegral ((shiftR  word32  24) .&. 0xFF)
                            b2     = fromIntegral ((shiftR  word32  16) .&. 0xFF)
                            b1     = fromIntegral ((shiftR  word32   8) .&. 0xFF)
                            b0     = fromIntegral ((shiftR  word32   0) .&. 0xFF)
                          in
                            (next_addr + 4, [ (next_addr + 3, b3),
                                              (next_addr + 2, b2),
                                              (next_addr + 1, b1),
                                              (next_addr + 0, b0) ]))

-- ================================================================

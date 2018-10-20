-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module FPR_File where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V FPR (Floating Point Register) register file.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Bits
import qualified Data.Map.Strict as Data_Map

-- Project imports

import Bit_Utils
import Arch_Defs

-- ================================================================
-- The FPR file is represented as Data.Map.Map from FPR addresses (0..31) to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

-- In particular: Data_Map.lookup is a general facility returning a
-- 'Maybe Word64' which may be 'Just v' if the index is in the map,
-- and 'Nothing' otherwise.  Here, we only use indexes that are
-- present, so we use 'fromMaybe' to extract 'v' from 'Just v'.

data FPR_File = FPR_File  (Data_Map.Map  InstrField  Integer)
  deriving (Show)

mkFPR_File :: FPR_File
mkFPR_File = FPR_File (Data_Map.fromList (zip
                                           [0..31]
                                           (repeat (fromIntegral 0))))

fpr_read :: FPR_File -> FPR_Addr -> Integer
fpr_read  (FPR_File dm)  reg = fromMaybe 0 (Data_Map.lookup  reg  dm)

fpr_write :: FPR_File -> FPR_Addr -> Integer -> FPR_File
fpr_write  (FPR_File dm)  reg  val = FPR_File (Data_Map.insert  reg  val  dm)

-- ================================================================
-- print_FPR_File prints four regs per line, in hex, with given indent
-- For debugging only

print_FPR_File :: String -> Int -> FPR_File -> IO ()
print_FPR_File    indent    xlen   fpr_file = do
  let regval_strings = map  (\j -> show_wordXL  xlen  '.'  (fpr_read  fpr_file  j))  [0..31]

  putStr (indent)
  putStr ("f0/ft0   " ++ (regval_strings !!  0))
  putStr (" ft1  "  ++ (regval_strings !!  1))
  putStr (" ft2  " ++ (regval_strings !!  2))
  putStr (" ft3  " ++ (regval_strings !!  3))
  putStrLn ("")

  putStr (indent)
  putStr ("f4/ft4   " ++ (regval_strings !!  4))
  putStr (" ft5  "  ++ (regval_strings !!  5))
  putStr (" ft6  " ++ (regval_strings !!  6))
  putStr (" ft7  " ++ (regval_strings !!  7))
  putStrLn ("")

  putStr (indent)
  putStr ("f8/fs0   " ++ (regval_strings !!  8))
  putStr (" fs1  "  ++ (regval_strings !!  9))
  putStr ("f10/fa0  " ++ (regval_strings !!  10))
  putStr (" fa1  " ++ (regval_strings !!  11))
  putStrLn ("")

  putStr (indent)
  putStr ("f12/fa2   " ++ (regval_strings !!  12))
  putStr (" fa3  "  ++ (regval_strings !!  13))
  putStr (" fa4  " ++ (regval_strings !!  14))
  putStr (" fa5  " ++ (regval_strings !!  15))
  putStrLn ("")

  putStr (indent)
  putStr ("f16/fa6  " ++ (regval_strings !!  16))
  putStr (" fa7  "  ++ (regval_strings !!  17))
  putStr ("f18/fs2  " ++ (regval_strings !!  18))
  putStr (" fs3  " ++ (regval_strings !!  19))
  putStrLn ("")

  putStr (indent)
  putStr ("f20/fs4  " ++ (regval_strings !!  20))
  putStr (" fs5  "  ++ (regval_strings !!  21))
  putStr (" fs6  " ++ (regval_strings !!  22))
  putStr (" fs7  " ++ (regval_strings !!  23))
  putStrLn ("")

  putStr (indent)
  putStr ("f24/fs8   " ++ (regval_strings !!  24))
  putStr (" fs9  "  ++ (regval_strings !!  25))
  putStr (" fs10  " ++ (regval_strings !!  26))
  putStr (" fs11  " ++ (regval_strings !!  27))
  putStrLn ("")

  putStr (indent)
  putStr ("f28/ft8  " ++ (regval_strings !!  28))
  putStr (" ft9  "  ++ (regval_strings !!  29))
  putStr (" ft10  " ++ (regval_strings !!  30))
  putStr (" ft11  " ++ (regval_strings !!  31))
  putStrLn ("")

-- ================================================================

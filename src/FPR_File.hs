-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module FPR_File where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V FPR (Floating Point Register) register file.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Word
-- import Numeric (showHex, readHex)
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

print_FPR_File :: String -> FPR_File -> IO ()
print_FPR_File    indent    fpr_file = do
  let
    print_n :: FPR_Addr -> FPR_Addr -> IO ()
    print_n  r1 r2 = (do
                         putStr (indent ++ show r1 ++ ":")
                         mapM_
                           (\rg -> putStr ("  " ++ showHex  (fpr_read  fpr_file  rg)  ""))
                           [r1..r2]
                         putStrLn "")
  print_n  0   3
  print_n  4   7
  print_n  8   11
  print_n  12  15
  print_n  16  19
  print_n  20  23
  print_n  24  27
  print_n  28  31

-- ================================================================

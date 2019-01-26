-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module GPR_File where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V GPR (General Purpose Register) register file.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Bits
import qualified Data.Map.Strict as Data_Map

-- Project imports

import Bit_Utils
import Arch_Defs

-- ================================================================
-- The GPR file is represented as Data.Map.Map from GPR addresses (0..31) to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

-- In particular: Data_Map.lookup is a general facility returning a
-- 'Maybe GPR_Val' which may be 'Just v' if the index is in the map,
-- and 'Nothing' otherwise.  Here, we only use indexes that are
-- present, so we use 'fromMaybe' to extract 'v' from 'Just v'.
                                                                    -- \begin_latex{GPR_File}
newtype GPR_File = GPR_File  (Data_Map.Map  GPR_Addr  GPR_Val)

mkGPR_File :: GPR_File
mkGPR_File = GPR_File (Data_Map.fromList (zip
                                           [0..31]
                                           (repeat (fromIntegral 0))))
                                                                    -- \end_latex{GPR_File}

-- Read a GPR

gpr_read :: GPR_File ->    GPR_Addr -> GPR_Val
gpr_read    (GPR_File dm)  reg = fromMaybe 0 (Data_Map.lookup  reg  dm)

{-# INLINE gpr_read #-}

-- Write a value to a GPR

gpr_write :: GPR_File ->    GPR_Addr -> GPR_Val -> GPR_File
gpr_write    (GPR_File dm)  reg         val =
  let
    -- Register 0 should always be 0
    val1 = if (reg == 0) then 0
           else val
  in
    -- We use 'seq' to force evaluation of val1
    seq  val1  (GPR_File (Data_Map.insert  reg  val1  dm))

{-# INLINE gpr_write #-}

-- ================================================================
-- print_GPR_File prints four regs per line, in hex, with given indent
-- For debugging only

print_GPR_File :: String -> Int -> GPR_File -> IO ()
print_GPR_File    indent    xlen   gpr_file = do
  let regval_strings = map  (\j -> show_wordXL  xlen  '.'  (gpr_read  gpr_file  j))  [0..31]

  putStr (indent)
  putStr ("x0       " ++ (regval_strings !!  0))
  putStr (" ra "  ++ (regval_strings !!  1))
  putStr (" sp  " ++ (regval_strings !!  2))
  putStr (" gp  " ++ (regval_strings !!  3))
  putStrLn ("")

  putStr (indent)
  putStr ("x4/tp    " ++ (regval_strings !!  4))
  putStr (" t0 "  ++ (regval_strings !!  5))
  putStr (" t1  " ++ (regval_strings !!  6))
  putStr (" t2  " ++ (regval_strings !!  7))
  putStrLn ("")

  putStr (indent)
  putStr ("x8/s0/fp " ++ (regval_strings !!  8))
  putStr (" s1 "  ++ (regval_strings !!  9))
  putStr (" a0  " ++ (regval_strings !!  10))
  putStr (" a1  " ++ (regval_strings !!  11))
  putStrLn ("")

  putStr (indent)
  putStr ("x12/a2   " ++ (regval_strings !!  12))
  putStr (" a3 "  ++ (regval_strings !!  13))
  putStr (" a4  " ++ (regval_strings !!  14))
  putStr (" a5  " ++ (regval_strings !!  15))
  putStrLn ("")

  putStr (indent)
  putStr ("x16/a6   " ++ (regval_strings !!  16))
  putStr (" a7 "  ++ (regval_strings !!  17))
  putStr (" s2  " ++ (regval_strings !!  18))
  putStr (" s3  " ++ (regval_strings !!  19))
  putStrLn ("")

  putStr (indent)
  putStr ("x20/s4   " ++ (regval_strings !!  20))
  putStr (" s5 "  ++ (regval_strings !!  21))
  putStr (" s6  " ++ (regval_strings !!  22))
  putStr (" s7  " ++ (regval_strings !!  23))
  putStrLn ("")

  putStr (indent)
  putStr ("x24/s8   " ++ (regval_strings !!  24))
  putStr (" s9 "  ++ (regval_strings !!  25))
  putStr (" s10 " ++ (regval_strings !!  26))
  putStr (" s11 " ++ (regval_strings !!  27))
  putStrLn ("")

  putStr (indent)
  putStr ("x28/t3   " ++ (regval_strings !!  28))
  putStr (" t4 "  ++ (regval_strings !!  29))
  putStr (" t5  " ++ (regval_strings !!  30))
  putStr (" t6  " ++ (regval_strings !!  31))
  putStrLn ("")

-- ================================================================

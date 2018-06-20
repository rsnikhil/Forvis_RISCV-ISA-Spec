-- See LICENSE for license details

module GPR_File where

-- ================================================================
-- This module defines an abstraction for
-- a RISC-V GPR (General Purpose Register) register file.

-- ================================================================
-- Standard Haskell imports

import Data.Maybe
import Data.Word
import Numeric (showHex, readHex)
import qualified Data.Map.Strict as Data_Map

-- Project imports

import Arch_Defs

-- ================================================================
-- The GPR file is represented as Data.Map.Map from GPR addresses (0..31) to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

-- In particular: Data_Map.lookup is a general facility returning a
-- 'Maybe Word64' which may be 'Just v' if the index is in the map,
-- and 'Nothing' otherwise.  Here, we only use indexes that are
-- present, so we use 'fromMaybe' to extract 'v' from 'Just v'.

data GPR_File = GPR_File  (Data_Map.Map  InstrField  Word64)
  deriving (Show)

mkGPR_File :: GPR_File
mkGPR_File = GPR_File (Data_Map.fromList (zip
                                           [0..31]
                                           (repeat (fromIntegral 0))))

gpr_read :: GPR_File -> GPR_Addr -> Word64
gpr_read  (GPR_File dm)  reg = fromMaybe 0 (Data_Map.lookup  reg  dm)

gpr_write :: GPR_File -> GPR_Addr -> Word64 -> GPR_File
gpr_write  (GPR_File dm)  reg  val =
  let
    -- Register 0 should always be 0
    val1 = if (reg == 0) then 0 else val
  in
    GPR_File (Data_Map.insert  reg  val1  dm)

-- ================================================================
-- print_GPR_File prints four regs per line, in hex, with given indent
-- For debugging only

print_GPR_File :: String -> GPR_File -> IO ()
print_GPR_File  indent  gpr_file = do
  let
    print_n :: GPR_Addr -> GPR_Addr -> IO ()
    print_n  r1 r2 = (do
                         putStr (indent ++ show r1 ++ ":")
                         mapM_
                           (\rg -> putStr ("  " ++ showHex  (gpr_read  gpr_file  rg)  ""))
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

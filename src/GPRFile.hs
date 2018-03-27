module GPRFile (GPRFile, print_GPRFile, mkGPRFile, get_gpr, set_gpr) where

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

import ArchDefs64

-- ================================================================
-- The GPR file is represented as Data.Map.Map from GPR addresses (0..31) to values
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

newtype GPRFile = GPRFile (Data_Map.Map  Register  WordXLEN)
  deriving (Show)

-- print_GPRFile prints four regs per line, in hex, with given indent
print_GPRFile :: String -> GPRFile -> IO ()
print_GPRFile  indent  gprfile = do
  print_n  Rg_x0   Rg_x3
  print_n  Rg_x4   Rg_x7
  print_n  Rg_x8   Rg_x11
  print_n  Rg_x12  Rg_x15
  print_n  Rg_x16  Rg_x19
  print_n  Rg_x20  Rg_x23
  print_n  Rg_x24  Rg_x27
  print_n  Rg_x28  Rg_x31
  where
    print_n :: Register -> Register -> IO ()
    print_n  r1 r2 = do
      putStr (indent ++ show r1 ++ ":")
      mapM_  (\rg -> putStr ("  " ++ showHex (get_gpr gprfile rg) ""))
             [r1..r2]
      putStrLn ""

mkGPRFile :: GPRFile
mkGPRFile = GPRFile (Data_Map.fromList (zip  (enumFromTo  Rg_x0  Rg_x31)
                                             (repeat (fromIntegral 0))))

get_gpr :: GPRFile -> Register -> WordXLEN
get_gpr  (GPRFile gprfile)  reg = fromMaybe 0 (Data_Map.lookup  reg  gprfile)

set_gpr :: GPRFile -> Register -> WordXLEN -> GPRFile
set_gpr  (GPRFile gprfile)  reg  val = GPRFile (Data_Map.insert  reg  val'  gprfile)
  where
    val' = if (reg == Rg_x0) then 0 else val

-- ================================================================

{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}
module MachineLenses where

-- From Haskell libraries
import Control.Lens
import Control.Lens.Fold

-- From /src
import Arch_Defs
import Bit_Utils
import CSR_File
import Encoder
import Forvis_Spec_I
import Forvis_Spec_Instr_Fetch
import GPR_File
import Machine_State
import Memory

import Data.Map (Map)

fpc :: Lens' Machine_State Integer
fpc f m = (\pc' -> m{f_pc = pc'}) <$> f (f_pc m)

fgpr_map :: Lens' GPR_File (Map GPR_Addr GPR_Val)
fgpr_map f (GPR_File m) = GPR_File <$> f m

fgpr_gpr :: Lens' Machine_State GPR_File
fgpr_gpr f m = (\gprs' -> m{f_gprs = gprs'}) <$> f (f_gprs m)

fgprs :: Lens' Machine_State (Map GPR_Addr GPR_Val)
fgprs = fgpr_gpr . fgpr_map

fmem_map :: Lens' Mem (Map Integer Integer)
fmem_map f (Mem dm ra) = (\dm' -> Mem dm' ra) <$> f dm

fmem_mem :: Lens' Machine_State Mem
fmem_mem f m = (\mem' -> m{f_mem = mem'}) <$> f (f_mem m)

fmem :: Lens' Machine_State (Map Integer Integer)
fmem = fmem_mem . fmem_map

{-

data Machine_State =
  Machine_State { -- Architectural state
                  f_pc   :: Integer,
                  f_gprs :: GPR_File,
                  f_fprs :: FPR_File,
                  f_csrs :: CSR_File,
                  f_priv :: Priv_Level,

                  -- Memory and mory mapped IO
                  f_mem  :: Mem,
                  f_mmio :: MMIO,

                  -- For Tandem Verification
                  f_eaddr :: Integer,
                  f_wdata :: Integer,

                  -- Implementation options

                  -- Legal memory addresses: list of (addr_start, addr_lim)
                  f_mem_addr_ranges :: [(Integer, Integer)],

                  -- For convenience and debugging only; no semantic relevance
                  f_rv                 :: RV,   -- redundant copy of info in CSR MISA
                  f_run_state          :: Run_State,
                  f_last_instr_trapped :: Bool,
                  f_verbosity          :: Int
                }
-}

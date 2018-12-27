module PIPE where

import qualified Data.Map.Strict as Data_Map
import Data.Maybe
import qualified Data.Set as Data_Set
import Data.Set (Set)

import Bit_Utils
import Arch_Defs

-- Maybe?
import Machine_State
import Forvis_Spec_I
import GPR_File
import Memory

-- This might belong elsewhere
import Test.QuickCheck

--------------------------------------------------------

newtype Color = C Int
                deriving (Eq, Show, Ord)

dfltcolor = C 0
othercolor = C 1

-- LATER: This should really be passed to the policy as a parameter
-- that is chosen by the test harness. 
initialColors = 5

data AllocOrNot = Alloc | NoAlloc deriving (Eq, Show)

data Tag = MTagP 
         | MTagI AllocOrNot 
         | MTagR Color
         | MTagM Color   -- contents (stored pointer)
                 Color   -- location
         deriving (Eq, Show)

initPC = MTagP
initStackTag = MTagM dfltcolor dfltcolor
initHeapTag = initStackTag
initR = MTagR dfltcolor
initR_SP = initR
plainInst = MTagI NoAlloc

---------------------------------

newtype GPR_FileT = GPR_FileT  { unGPR :: Data_Map.Map  InstrField  Tag }
  deriving (Eq, Show)

mkGPR_FileT :: GPR_FileT
mkGPR_FileT = GPR_FileT (Data_Map.fromList (zip [0..31] (repeat initR)))

gpr_readT :: GPR_FileT ->    GPR_Addr -> Tag
gpr_readT    (GPR_FileT dm)  reg = maybe (error (show reg)) id (Data_Map.lookup  reg  dm)

gpr_writeT :: GPR_FileT ->    GPR_Addr -> Tag -> GPR_FileT
gpr_writeT    (GPR_FileT dm)  reg         val =
    seq  val  (GPR_FileT (Data_Map.insert  reg  val  dm))

newtype MemT = MemT {unMemT :: Data_Map.Map Integer Tag}
  deriving (Eq, Show)

mkMemT = MemT (Data_Map.fromList [])

---------------------------------

data PIPE_Result = PIPE_Trap String
                 | PIPE_Success

data PIPE_State = PIPE_State {
  p_pc   :: Tag,
  p_gprs :: GPR_FileT,
  p_mem  :: MemT,
  p_nextcolor :: Int
  }
  deriving (Eq, Show)

init_pipe_state = PIPE_State {
  p_pc = initPC,
  p_gprs = mkGPR_FileT,
  p_mem = mkMemT,
  p_nextcolor = initialColors -- Should not be able to allocate an existing color...
  }

fresh_color :: PIPE_State -> (Color, PIPE_State)
#ifndef M_FRESH_COLOR
fresh_color p = (C $ p_nextcolor p, p {p_nextcolor = p_nextcolor p + 1})
#else
fresh_color p = (C 1, p)
#endif

-- Should be done with lenses...
get_rtag :: PIPE_State -> GPR_Addr -> Tag
get_rtag p = gpr_readT (p_gprs p)

set_rtag :: PIPE_State -> GPR_Addr -> Tag -> PIPE_State
set_rtag p a t = p {p_gprs = gpr_writeT (p_gprs p) a t}

get_mtag :: PIPE_State -> Integer -> Tag
get_mtag p a = maybe (MTagM dfltcolor dfltcolor) id $ Data_Map.lookup a (unMemT $ p_mem p) 

set_mtag :: PIPE_State -> Integer -> Tag -> PIPE_State
set_mtag p a t = p { p_mem = MemT (Data_Map.insert a t (unMemT $ p_mem p)) }

---------------------------------

-- Shorthand for (indistinguishable) pairs of m- and p-states 
data MStatePair =
  M (Machine_State, PIPE_State) (Machine_State, PIPE_State)

---------------------------------

ok p = (p, PIPE_Success)
notok p s = (p, PIPE_Trap s)

exec_pipe :: PIPE_State -> Machine_State -> Integer -> (PIPE_State, PIPE_Result)
exec_pipe p m u32 =
  let rv  = mstate_rv_read  m
      res = decode_I rv u32
      ic = get_mtag p (f_pc m)
  in case res of
      Just i -> 
        case i of
          ADDI rd rs imm 
#ifndef M_WRONG_ADDI
            | ic == MTagI NoAlloc -> ok $ set_rtag p rd $ get_rtag p rs
#else
            | ic == MTagI NoAlloc -> ok $ set_rtag p rd (MTagR (C 1))
#endif
            | otherwise -> 
                let (c, p') = fresh_color p in
                ok $ set_rtag p rd (MTagR c)
          ADD rd rs1 rs2 ->
#ifndef M_WRONG_ADDI
            ok $ set_rtag p rd $ get_rtag p rs1
#else
            ok $ set_rtag p rd (MTagR (C 1))
#endif
          LW rd rs imm   -> 
            let rsc = get_rtag p rs
--                rs1_val  = mstate_gpr_read  mstate  rs1    -- address base
--                s_imm12  = sign_extend  12  xlen  imm12
--                eaddr1   = alu_add  xlen  rs1_val  s_imm12
--                addr   = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)
                addr = mstate_gpr_read m rs
                memc = get_mtag p (addr+imm) in 
            case (rsc,memc) of
              (MTagR t1c, MTagM t2vc t2lc)
#ifndef M_LW_NOCHECK
                | t1c==t2lc -> ok $ set_rtag p rd (MTagR t2vc)
                | otherwise -> notok p $ "Different colors on Load: " ++ show t1c ++ " and " ++ show t2lc
#else
                | otherwise -> ok $ set_rtag p rd (MTagR t2vc)
#endif
              _ -> notok p $ "Mangled tags on Load: " ++ show rsc ++ " and " ++ show memc
          SW rs1 rs2 imm -> 
            let addr = mstate_gpr_read m rs1
                rs1c = get_rtag p rs1 
                rs2c = get_rtag p rs2 in 
            case (rs1c,rs2c) of
#ifndef M_MANGLED_STORE
              (MTagR t1c, MTagR t2c) -> ok $ set_mtag p (addr+imm) (MTagM t2c t1c)
#else
-- TODO: Can we find this one??
--              (MTagR t1c, MTagR t2c) -> ok $ set_mtag p (addr+imm) (MTagM t1c t2c)
              (MTagR t1c, MTagR t2c) -> ok $ set_mtag p (addr+imm) (MTagM (C 1) (C 1))
#endif
              _ -> notok p $ "Mangled tags on Store: " ++ show rs1c ++ " and " ++ show rs2c
          _ -> ok p 
      Nothing -> ok p


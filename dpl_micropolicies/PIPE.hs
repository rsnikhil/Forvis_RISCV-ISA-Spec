module PIPE(PIPE_Policy,
            load_pipe_policy,
            TagSet,
            mkTagSet,
            GPR_FileT(..),
            mkGPR_FileT, gpr_readT, gpr_writeT,
            MemT(..),
            mkMemT, mem_readT, mem_writeT,
            PIPE_State(..),
            init_pipe_state,
            PIPE_Result(..),
            exec_pipe) where

import qualified Data.Map.Strict as Data_Map
import qualified Data.Map as M -- should clean up to use just one kind of Data.Map
import Data.Maybe
import qualified Data.Set as Data_Set
import Data.Set (Set)
import Data.List (find)
import Numeric (showHex)
import Data.Char
                     
import Bit_Utils
import Arch_Defs

import AST
import Symbols
import qualified EvalCommon as EC
import qualified Eval as E

import Machine_State
import Forvis_Spec_I
import GPR_File
-- import Memory

type PIPE_Policy = E.QPolMod

type TagSet = EC.TagValue 

load_pipe_policy :: String {- policy file name -}
                 -> IO PIPE_Policy
load_pipe_policy fname =
     do polMod <- E.loadPolicyAndModules fname                           
        case polMod of
          Nothing -> error $ "load_pipe_policy failure on " ++ fname
          Just polMod -> return polMod

{- Use 'requires' section in module definition to translate from 
   a dotted string ([String]) to a TagSet -}
mkTagSet :: PIPE_Policy -> [String] -> TagSet
mkTagSet (_,_,symtabs) name =
  let Init _ _ (ISExact _ ts) =          
        maybe (error $ "mkTag cannot find " ++ (show name)) id $
            find (\ (Init _ name' _) -> name == name')
                 (concatMap requires (map snd symtabs))
  in M.fromList (map (\tag -> (qsym tag,Nothing)) ts)
    
{- We may need other magic ways to build tags indexed by integers... -}

---------------------------------

newtype GPR_FileT = GPR_FileT  { unGPR :: Data_Map.Map  InstrField  TagSet }
  deriving (Eq, Show)

mkGPR_FileT :: TagSet -> GPR_FileT
mkGPR_FileT t = GPR_FileT (Data_Map.fromList (zip [0..31] (repeat t)))

gpr_readT :: GPR_FileT ->    GPR_Addr -> TagSet
gpr_readT    (GPR_FileT dm)  reg =
    maybe (error $ "undefined gpr_readT" ++ (show reg)) id (Data_Map.lookup reg dm)

gpr_writeT :: GPR_FileT ->    GPR_Addr -> TagSet -> GPR_FileT
gpr_writeT    (GPR_FileT dm)  reg         val =
    seq  val  (GPR_FileT (Data_Map.insert  reg  val  dm))

---------------------------------

newtype MemT = MemT {unMemT :: Data_Map.Map Integer TagSet}
  deriving (Eq, Show)

mkMemT :: [(Integer,TagSet)] -> MemT 
mkMemT = foldr (\ (a,t) m -> mem_writeT m a t) (MemT Data_Map.empty)

mem_readT :: MemT -> Integer -> TagSet
mem_readT m a =
     maybe (error $ "undefined mem_readT" ++ (show a)) id (Data_Map.lookup a (unMemT m))
 
mem_writeT :: MemT -> Integer -> TagSet -> MemT
mem_writeT m a t = foldr (\a m -> mem_writeT' m a t) m [a0,a0+1,a0+2,a0+3]
         where a0 = (a `div` 4) * 4

{- private -}                
mem_writeT' :: MemT -> Integer -> TagSet -> MemT
mem_writeT' m a t = MemT (Data_Map.insert a t (unMemT m))

---------------------------------

data PIPE_State = PIPE_State {
  p_pc   :: TagSet,
  p_gprs :: GPR_FileT,
  p_mem  :: MemT,  {- Byte-indexed. Any store should set tag on all bytes of 
                     the aligned word that includes the address -}
  p_next :: Int    {- next generator value -}
  }
  deriving (Eq, Show)

{- Build an initial state with default tags. This can then be
   tweaked using the exported gpr and mem functions.  -}
init_pipe_state :: TagSet {- default PC tag -} ->
                   TagSet {- default GPR tag -} ->
                   Int    {- initial generator value -} -> 
                   PIPE_State
init_pipe_state initPC initGPR initGen = PIPE_State {
  p_pc = initPC,
  p_gprs = mkGPR_FileT initGPR,
  p_mem = mkMemT [],
  p_next = initGen
  }

{- These operators are private (for no very strong reason). -}
-- Should be done with lenses...
set_rtag :: PIPE_State -> GPR_Addr -> TagSet -> PIPE_State
set_rtag p a t = p {p_gprs = gpr_writeT (p_gprs p) a t}

get_rtag :: PIPE_State -> GPR_Addr -> TagSet
get_rtag p = gpr_readT (p_gprs p)

set_mtag :: PIPE_State -> Integer -> TagSet -> PIPE_State
set_mtag p a t = p {p_mem = mem_writeT (p_mem p) a t}

get_mtag :: PIPE_State -> Integer -> TagSet
get_mtag p = mem_readT (p_mem p) 

---------------------------------

data PIPE_Result = PIPE_Trap String
                 | PIPE_Success

exec_pipe :: E.QPolMod -> PIPE_State -> Machine_State -> Integer -> (PIPE_State, PIPE_Result)
exec_pipe polMod p m u32 =
  let rv  = mstate_rv_read m
      inst = case decode_I rv u32 of
               Just i -> i
               Nothing -> error $ "exec_pipe cannot decode instruction 0x" ++ (showHex u32 "")
      maddr = case inst of 
                LB _ rs1 imm -> mstate_gpr_read m rs1 + imm
                LH _ rs1 imm -> mstate_gpr_read m rs1 + imm
                LW _ rs1 imm -> mstate_gpr_read m rs1 + imm
                LBU _ rs1 imm -> mstate_gpr_read m rs1 + imm
                LHU _ rs1 imm -> mstate_gpr_read m rs1 + imm
                SB rs1 _ imm -> mstate_gpr_read m rs1 + imm
                SH rs1 _ imm -> mstate_gpr_read m rs1 + imm
                SW rs1 _ imm -> mstate_gpr_read m rs1 + imm
                _ -> error $ "maddr undefined for " ++ (show inst)
  in exec_pipe' polMod p (f_pc m) inst maddr

{- Proceed with only PIPE_State -}
exec_pipe' :: E.QPolMod -> PIPE_State -> Integer -> Instr_I -> Integer -> (PIPE_State, PIPE_Result)
exec_pipe' polMod p pc inst maddr =
  let inp0 :: EC.OperandTags
      inp0 = M.fromList [
              (Right EC.ESKEnv, p_pc p),
              (Right EC.ESKCode, get_mtag p pc)]
      {- generate opcode name in usual form for 'group' section -- a bit hacky -}
      name = map toLower $ takeWhile (not . isSpace) $ show inst  
      look k m = maybe (error $ "lookup failure " ++ (show k)) id (M.lookup k m)
      ex inp outf =          
            let (r,next') =
                  EC.runTagResult
                    (p_next p)
                    (E.evalPolMod polMod (name, inp0 `M.union` (EC.wrapESKMap inp))) 
            in case r of
                 Left EC.TFImplicit -> (p,PIPE_Trap "no applicable rule")
                 Left (EC.TFExplicit s) -> (p, PIPE_Trap s)
                 Right out -> 
                           ((outf out) 
                            {p_pc = look (Right EC.ESKEnv) out,
                             p_next = next'},
                            PIPE_Success)
      get = get_rtag p
      set = set_rtag p
      r0d0 =
        ex M.empty
           (\_ -> p)
      r0d1 rd =
        ex M.empty
           (\out -> set rd $ look (Left RD) out) 
      r1d1 rs1 rd = 
        ex (M.fromList [(RS1,get rs1)])
           (\out -> set rd $ look (Left RD) out)
      r2d0 rs1 rs2 = 
        ex (M.fromList [(RS1,get rs1),(RS2,get rs2)])
           (\_ -> p)
      r2d1 rs1 rs2 rd = 
        ex (M.fromList [(RS1,get rs1),(RS2,get rs2)])
              (\out -> set rd $ look (Left RD) out)
      r1md1 rs1 rd =
        ex (M.fromList [(RS1,get rs1),(Mem, get_mtag p maddr)])
           (\out -> set rd $ look (Left RD) out)              
      r2md0m rs1 rs2 =
        ex (M.fromList [(RS1,get rs1),(RS2,get rs2),(Mem, get_mtag p maddr)])
           (\out -> set_mtag p maddr $ look  (Left Mem) out)              
  in case inst of
       LUI rd _ -> r0d1 rd
       AUIPC rd _ -> r0d1 rd
       JAL rd _ -> r0d1 rd
       JALR rd rs1 _ -> r1d1 rs1 rd
       BEQ rs1 rs2 _ -> r2d0 rs1 rs2
       BNE rs1 rs2 _ -> r2d0 rs1 rs2
       BLT rs1 rs2 _ -> r2d0 rs1 rs2
       BGE rs1 rs2 _ -> r2d0 rs1 rs2
       BLTU rs1 rs2 _ -> r2d0 rs1 rs2
       BGEU rs1 rs2 _ -> r2d0 rs1 rs2
       LB rd rs1 _ -> r1md1 rs1 rd  
       LH rd rs1 _ -> r1md1 rs1 rd  
       LW rd rs1 _ -> r1md1 rs1 rd  
       LBU rd rs1 _ -> r1md1 rs1 rd  
       LHU rd rs1 _ -> r1md1 rs1 rd  
       SB rs1 rs2 _ -> r2md0m rs1 rs2
       SH rs1 rs2 _ -> r2md0m rs1 rs2
       SW rs1 rs2 _ -> r2md0m rs1 rs2
       ADDI rd rs1 _ -> r1d1 rs1 rd
       SLTI rd rs1 _ -> r1d1 rs1 rd
       SLTIU rd rs1 _ -> r1d1 rs1 rd
       XORI rd rs1 _ -> r1d1 rs1 rd
       ORI rd rs1 _ -> r1d1 rs1 rd
       ANDI rd rs1 _ -> r1d1 rs1 rd
       SLLI rd rs1 _ -> r1d1 rs1 rd
       SRLI rd rs1 _ -> r1d1 rs1 rd
       SRAI rd rs1 _ -> r1d1 rs1 rd
       ADD rd rs1 rs2 -> r2d1 rs1 rs2 rd
       SUB rd rs1 rs2 -> r2d1 rs1 rs2 rd
       SLL rd rs1 rs2 -> r2d1 rs1 rs2 rd
       SLT rd rs1 rs2 -> r2d1 rs1 rs2 rd
       SLTU rd rs1 rs2 -> r2d1 rs1 rs2 rd
       XOR rd rs1 rs2 -> r2d1 rs1 rs2 rd
       SRL rd rs1 rs2 -> r2d1 rs1 rs2 rd
       SRA rd rs1 rs2 -> r2d1 rs1 rs2 rd
       OR rd rs1 rs2 -> r2d1 rs1 rs2 rd
       AND rd rs1 rs2 -> r2d1 rs1 rs2 rd
       FENCE _ _ _ -> r0d0
       ECALL -> r0d0
       EBREAK -> r0d0

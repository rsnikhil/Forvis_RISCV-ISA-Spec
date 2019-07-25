{-# Language ExistentialQuantification, TemplateHaskell #-}

module PIPE(PIPE_Policy,  -- TODO: Maybe this is not needed?
            PolicyPlus(..),
            askPolicy,
            load_pipe_policy,
            TagSet,
            P,
            Color,
            -- APT: These might be useful one day (even if not for autotesting)
            -- Should wait until integrating Chris's new parser, which supports int fields in tags. 
            -- Still need to fix to translate tags to fully qualified form.
            -- mkTagSet,
            -- rdTagSet,
            showTagSet,
            fromExt,
            toExt,
            GPR_FileT(..),
            mkGPR_FileT, unGPRT, gpr_readT, gpr_writeT,
            MemT(..),
            mkMemT, unMemT, mem_readT, mem_writeT,
            PIPE_State(..),
            ppc, pgpr, pmem, pgpr_gpr, pmem_mem,
            p_pc, p_gprs, p_mem,
            init_pipe_state,
            MStatePair(..),
            PIPE_Result(..),
            exec_pipe) where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find,sort,intercalate)
import Numeric (showHex)
import Data.Char
import Bit_Utils
import Arch_Defs

import AST
import Symbols
import CommonFn as CF
import qualified EvalCommon as EC
import qualified Eval as E

import Machine_State
import MachineLenses
import Forvis_Spec_I
import GPR_File
-- import Memory

import Test.QuickCheck
import Control.Monad.Reader

import Text.PrettyPrint (Doc, (<+>), ($$))

import Control.Lens

import Debug.Trace

-----------------------------------------------------------------
type PIPE_Policy = E.QPolMod

newtype TagSet = TagSet {unTagSet :: EC.TagValue}
  deriving (Eq, Show)

showTagSet t = 
    let f (s, Nothing) = s
        f (s, Just c) = s ++ " " ++ show c 
    in
    "{" ++ intercalate ", " (map f (toExt t)) ++ "}"

-- This type is defined here rather than within specific policies /
-- properties because that's how the DPL interpreter does it.  (And
-- because, similarly, the initNextColor field becomes an argument to
-- the policy interpreter.)
type Color = Int

-- TODO: Not sure whether more things should be deleted from this
-- TODO: tidy commented-out bits
data PolicyPlus =
  PolicyPlus {
    -- The policy itself
    policy :: PIPE_Policy
    -- Features for generation
-- , genMStatePair :: PolicyPlus -> Gen testState
  , initGPR :: TagSet 
  , initMem :: TagSet 
  , initPC :: TagSet 
  , initNextColor :: Color
  , emptyInstTag :: TagSet
      -- (The next three are arguably policy-local things and should be removed from here)
  , dataMemLow :: Integer
  , dataMemHigh :: Integer
  , instrLow :: Integer
  -- Features for shrinking
--  , shrinkMStatePair :: PolicyPlus -> testState -> [testState]
  -- Features for testing
--  , prop :: PolicyPlus -> testState -> Property
  }

type P a = Reader PolicyPlus a

askPolicy :: P PIPE_Policy
askPolicy =
  do ppol_plus <- ask
     return (policy ppol_plus)

load_pipe_policy :: String {- policy file name -}
                 -> IO PIPE_Policy
load_pipe_policy fname =
     do polMod <- E.loadPolicyAndModules fname                           
        case polMod of
          Nothing -> error $ "load_pipe_policy failure on " ++ fname
          Just polMod -> return polMod

{- Use 'requires' section in module definition to translate from 
   a dotted string ([String]) to a TagSet.
   Each tag in the tag set is applied to the corresponding (Maybe Int) param,
   which represents an (optional) parameter (e.g. color) to attach to the tag.  -}
-- mkTagSet :: PIPE_Policy -> [String] -> [Maybe Int] -> TagSet
-- mkTagSet (_,_,symtabs) name params =
--   let Init _ _ (ISExact _ ts) =          
--         maybe (error $ "mkTag cannot find " ++ (show name)) id $
--             find (\ (Init _ name' _) -> name == name')
--                  (concatMap requires (map snd symtabs))
--   in Map.fromList (zipWith (\tag param -> (qsym tag,param)) ts params)
    
{- Inverse of mkTagSet. Returns a list, since zero or more 'requires'
   might match a given TagSet. 
   (The implementation is fiddly (and inefficient!) because maps are 
   sorted by key, but we must remember the order in which tag bits 
   are listed in the 'requires' clause so as to match up the parameters.) -}
-- rdTagSet :: PIPE_Policy -> TagSet -> [([String],[Maybe Int])]
-- rdTagSet (_,_,symtabs) ts =  
--   map (\ (name,tbs) -> (name, map ((Map.!) ts) tbs)) matching_tss
--    where
--     matching_tss = filter (\ (_,tbs) -> (Map.keys ts == sort tbs)) all_tss
--     all_tss =
--           map
--            (\ (Init _ name (ISExact _ ts))  -> (name, map qsym ts))
--            (concatMap requires (map snd symtabs))

{- A somewhat quick-and-dirty way to build and retrieve tag sets 
   using just the tag names. The external format is a list
   of (tagname, optional color) pairs.  The list should always
   be sorted by tagname. -}

fromExt :: [(String,Maybe Int)] -> TagSet
fromExt ext = 
   if sort ext /= ext then
     error "fromExt on unsorted list"
   else
     TagSet $ Map.fromList $ map (\ (s,a) -> (QTag $ CF.parseDotName s,a)) ext

toExt :: TagSet -> [(String,Maybe Int)]
toExt ts =       
  sort (Map.foldrWithKey (\ k a b -> (CF.qualSymStr k,a):b) [] (unTagSet ts))
  
---------------------------------

newtype GPR_FileT = GPR_FileT  { _pgpr_map :: Map InstrField  TagSet }
  deriving (Eq, Show)

makeLenses ''GPR_FileT

unGPRT = _pgpr_map

mkGPR_FileT :: TagSet -> GPR_FileT
mkGPR_FileT t = GPR_FileT (Map.fromList (zip [0..31] (repeat t)))

gpr_readT :: GPR_FileT ->    GPR_Addr -> TagSet
gpr_readT    (GPR_FileT dm)  reg =
    maybe (error $ "undefined gpr_readT" ++ (show reg)) id (Map.lookup reg dm)

gpr_writeT :: GPR_FileT ->    GPR_Addr -> TagSet -> GPR_FileT
gpr_writeT    (GPR_FileT dm)  reg         val =
    seq  val  (GPR_FileT (Map.insert  reg  val  dm))

---------------------------------

newtype MemT = MemT { _pmem_map :: Map Integer TagSet}
  deriving (Eq, Show)

makeLenses ''MemT

unMemT = _pmem_map

mkMemT :: [(Integer,TagSet)] -> MemT 
mkMemT = foldr (\ (a,t) m -> mem_writeT m a t) (MemT Map.empty)

mem_readT :: PolicyPlus -> MemT -> Integer -> TagSet
mem_readT pplus m a = maybe (initMem pplus) id (Map.lookup a (_pmem_map m))
 
-- LATER: We are not sure this will do the right thing (i.e., something
-- coherent with what the processor does) on unaligned writes.
-- Something seems fishy -- should check with the Dover folks to see
-- what the actual PIPE does about this.
-- 
-- (Later: Yes, things are fishy.  We're temporarily going to "fix" this
-- function to just error out if someone tries a misaligned access,
-- but we guess the behavior we really want is to generate a pipe
-- trap?)
mem_writeT :: MemT -> Integer -> TagSet -> MemT
-- mem_writeT m a t = foldr (\a m -> mem_writeT' m a t) m [a0,a0+1,a0+2,a0+3]
--          where a0 = (a `div` 4) * 4
mem_writeT m a t =
  if (a `mod` 4) == 0 then mem_writeT' m a t
  else error "Unaligned memory accesses not supported"

{- private -}                
mem_writeT' :: MemT -> Integer -> TagSet -> MemT
mem_writeT' m a t = MemT (Map.insert a t (_pmem_map m))

---------------------------------

data PIPE_State = PIPE_State {
  _ppc       :: TagSet,
  _pgpr_gpr  :: GPR_FileT,
  _pmem_mem  :: MemT,  {- Byte-indexed. Any store should set tag on all bytes of 
                     the aligned word that includes the address -}
  _pnext :: Int    {- next generator value -}
  }
  deriving (Eq, Show)

makeLenses ''PIPE_State

p_gprs = _pgpr_gpr
p_mem  = _pmem_mem
p_pc   = _ppc

pgpr :: Lens' PIPE_State (Map InstrField TagSet)
pgpr = pgpr_gpr . pgpr_map

pmem :: Lens' PIPE_State (Map Integer TagSet)
pmem = pmem_mem . pmem_map

{- Build an initial state with default tags. This can then be
   tweaked using the exported gpr and mem functions.  -}
init_pipe_state :: PolicyPlus -> 
                   PIPE_State
init_pipe_state pplus = PIPE_State (initPC pplus) -- pc
                                   (mkGPR_FileT (initGPR pplus)) -- GPR_FileT
                                   (mkMemT []) -- mem
                                   (initNextColor pplus) -- color

{- These operators are private (for no very strong reason). -}
-- Should be done with lenses...
set_rtag :: PIPE_State -> GPR_Addr -> TagSet -> PIPE_State
set_rtag p a t = p & pgpr . at a ?~ t 
   
get_rtag :: PIPE_State -> GPR_Addr -> TagSet
get_rtag p a = maybe (error $ "get_rtag: " ++ show a ++  "\n" ++ show p) id $ p ^. pgpr . at a

set_mtag :: PIPE_State -> Integer -> TagSet -> PIPE_State
set_mtag p a t = p & pmem . at a ?~ t 

-- We may want to generalize the default case when memory layouts
-- become more interesting...
get_mtag :: PolicyPlus -> PIPE_State -> Integer -> Instr_I -> TagSet
get_mtag pplus p a i =
  maybe (if a == 0 || a >= instrLow pplus
           then emptyInstTag pplus
           else initMem pplus)
        id $ p ^. pmem . at a 

data MStatePair =
  M (Machine_State, PIPE_State) (Machine_State, PIPE_State)

---------------------------------

data PIPE_Result = PIPE_Trap String
                 | PIPE_Success

exec_pipe :: PolicyPlus -> Machine_State -> PIPE_State -> Integer -> (PIPE_State, PIPE_Result)
exec_pipe pplus m p u32 =
  let rv  = mstate_rv_read m in
  case decode_I rv u32 of
    Nothing ->
      -- LEO+BCP: e.g. when the instruction memory runs out -> interrupt. More careful thought required
      (p, PIPE_Success)
      -- error $ "exec_pipe cannot decode instruction 0x" ++ (showHex u32 "") ++ " at pc: " ++ (show $ mstate_pc_read m)
    Just inst ->
      let maddr = case inst of 
                    LB _ rs1 imm -> mstate_gpr_read rs1 m + imm
                    LH _ rs1 imm -> mstate_gpr_read rs1 m + imm
                    LW _ rs1 imm -> mstate_gpr_read rs1 m + imm
                    LBU _ rs1 imm -> mstate_gpr_read rs1 m + imm
                    LHU _ rs1 imm -> mstate_gpr_read rs1 m + imm
                    SB rs1 _ imm -> mstate_gpr_read rs1 m + imm
                    SH rs1 _ imm -> mstate_gpr_read rs1 m + imm
                    SW rs1 _ imm -> mstate_gpr_read rs1 m + imm
                    _ -> error $ "maddr undefined for " ++ (show inst)
      in exec_pipe' pplus p (f_pc m) inst maddr

{- Proceed with only PIPE_State -}
exec_pipe' :: PolicyPlus -> PIPE_State -> Integer -> Instr_I -> Integer -> (PIPE_State, PIPE_Result)
exec_pipe' pplus p pc inst maddr =
  let inp0 :: EC.OperandTags
      inp0 = Map.fromList [
              (Right EC.ESKEnv , unTagSet $ _ppc p),
              (Right EC.ESKCode, unTagSet $ get_mtag pplus p pc inst)]
      {- generate opcode name in usual form for 'group' section -- a bit hacky -}
      name = map toLower $ takeWhile (not . isSpace) $ show inst  
      look k m = -- trace ("Calling lookup with " ++ show k ++ " in m: " ++ show m)
                 maybe (error $ "lookup failure " ++ (show k) ++ " in " ++ show m) id
                         (Map.lookup k m)   
      ex inp outf =          
            let (r,next') =
                  EC.runTagResult
                    (_pnext p)
                    (E.evalPolMod (policy pplus) (name, inp0 `Map.union` (EC.wrapESKMap inp))) 
            in case r of
                 -- LATER: The trap message on the next line should be
                 -- displayed using the external representation of
                 -- tags, not just `show`.  Also, it would be more
                 -- helpful if it included the PC tag and the opcode
                 -- (or opgroup?) that we're currently trying to
                 -- execute (I've tried to add this but it doesn't
                 -- look good yet)
                 Left EC.TFImplicit ->
                   let tags = map (\(k,t) -> show k ++ "=" ++ showTagSet (TagSet t)) (Map.assocs inp) in
                   let i = "[" ++ intercalate ", " tags ++ "]" in
                   (p, PIPE_Trap $ "no applicable rule for " ++ i
                                    ++ " and instr group " ++ show name)
                 Left (EC.TFExplicit s) -> (p, PIPE_Trap s)
                 Right out -> 
                           ((outf out) 
                            {_ppc = TagSet $ look (Right EC.ESKEnv) out,
                             _pnext = next'},
                            PIPE_Success)
      get x = get_rtag p x
      set x = set_rtag p x
      r0d0 =
        ex Map.empty
           (\_ -> p)
      r0d1 rd =
        ex Map.empty
           (\out -> set rd $ TagSet $ look (Left RD) out) 
      r1d1 rs1 rd = 
        ex (Map.fromList [(RS1, unTagSet $ get rs1)])
           (\out -> set rd $ TagSet $ look (Left RD) out)
      r2d0 rs1 rs2 = 
        ex (Map.fromList [(RS1, unTagSet $ get rs1),(RS2, unTagSet $ get rs2)])
           (\_ -> p)
      r2d1 rs1 rs2 rd = 
        ex (Map.fromList [(RS1, unTagSet $ get rs1),(RS2, unTagSet $ get rs2)])
              (\out -> set rd $ TagSet $ look (Left RD) out)
      r1md1 rs1 rd =
        ex (Map.fromList [(RS1, unTagSet $ get rs1),(Mem, unTagSet $ get_mtag pplus p maddr inst)])
           (\out -> set rd $ TagSet $ look (Left RD) out)              
      r2md0m rs1 rs2 =
        ex (Map.fromList [(RS1, unTagSet $ get rs1),(RS2, unTagSet $ get rs2),(Mem, unTagSet $ get_mtag pplus p maddr inst)])
           (\out -> set_mtag p maddr $ TagSet $ look  (Left Mem) out)              
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

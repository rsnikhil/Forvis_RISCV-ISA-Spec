{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, FlexibleContexts #-}
module TestState where

import Debug.Trace

import Data.Functor
import Control.Monad

import Control.Arrow (first, second)

import Control.Lens hiding (elements)
import Control.Lens.Fold

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Maybe

import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>), ($$), fcat)
import qualified Text.PrettyPrint as P

import Machine_State
import GPR_File
import FPR_File
import CSR_File
import Forvis_Spec_Instr_Fetch
import Arch_Defs
import Forvis_Spec_I
import Memory

import MachineLenses
import PIPE
import Run_Program_PIPE
import Encoder
import Gen (initMachine) -- Move?

-- | Basic Definition and Lenses
--------------------------------
data RichState = Rich { _ms :: Machine_State
                      , _ps :: PIPE_State
                      }

data StackElem a = SE { _mp_state :: RichState
                      , _info :: a 
                      }

data TestState a = TS { _mp :: RichState
                      , _variants :: [StackElem a]
                      }

instance Show (TestState a) where
  show _ = ""

makeLenses ''RichState
makeLenses ''StackElem
makeLenses ''TestState

richStates :: Traversal' (TestState a) RichState
richStates f (TS mp vars) = TS <$> f mp <*> traverse (mp_state %%~ f) vars

-- | TODO: Organization. Should different functionalities be broken in different files?

-- | Diffing. Comparing one rich state to another and producing a small delta
data Diff = Diff { _d_pc    :: Maybe (Integer, TagSet)        -- value and tag of the current PC
                 , _d_instr :: Maybe Instr_I                  -- current instruction
                 , _d_reg   :: [(GPR_Addr, Integer, TagSet)]  -- change in registers
                 , _d_mem   :: [(Integer, Integer, TagSet)]   -- Change in memory
                 }

-- Generic "find diffs" function: Takes two association lists l1 and
-- l2, both assumed sorted by their keys and both representing
-- *infinite* maps with some default value d (passed as third
-- parameter as a function of the index), and returns a list of differences
--
-- N.b. In the cases where we are returning something, we first have
-- to check whether the thing we are returning is equal to d!  (And
-- not return it in this case.)
diff :: (Ord a, Eq b) => [(a, b)] -> [(a, b)] -> (a -> b) -> [(a, (b, b))]
diff [] [] f = []
diff ((x1,y1):l1) [] f = let d = f x1 in (if y1==d then [] else [(x1,(y1,d))]) ++ diff l1 [] f
diff [] ((x2,y2):l2) f = let d = f x2 in (if y2==d then [] else [(x2,(d,y2))]) ++ diff [] l2 f
diff ((x1,y1):l1) ((x2,y2):l2) f
         | x1 < x2   = let d = f x1 in (if y1==d then [] else [(x1,(y1,d))]) ++ diff l1 ((x2,y2):l2) f
         | x1 > x2   = let d = f x2 in (if y2==d then [] else [(x2,(d,y2))]) ++ diff ((x1,y1):l1) l2 f
         | otherwise = (if y1==y2 then [] else [(x1,(y1,y2))]) ++ diff l1 l2 f

eqOn l x y = x ^. l == y ^. l

mergeDiffs :: RichState -> RichState -> 
              [(Integer, (Integer, Integer))] -> [(Integer, (TagSet , TagSet ))] ->
              [Maybe (Integer, Integer, TagSet)]
mergeDiffs st1 st2 [] [] = []
mergeDiffs st1 st2 rd@((i,(d1,d2)):rs) td@((j,(l1,l2)):ts)
  | i == j = (Just (i,d2,l2)) : mergeDiffs st1 st2 rs ts
  | i <= j = ((i,d2,) <$> (st2 ^. ps . pgpr . at i)) : mergeDiffs st1 st2 rs td
  | i >= j = ((j,,l2) <$> (st2 ^. ms . fgpr . at j)) : mergeDiffs st1 st2 rd ts
mergeDiffs st1 st2 ((i,(d1,d2)):rs) [] = ((i,d2,) <$> (st2 ^. ps . pgpr . at i)) : mergeDiffs st1 st2 rs []
mergeDiffs st1 st2 [] ((j,(l1,l2)):ts) = ((j,,l2) <$> (st2 ^. ms . fgpr . at j)) : mergeDiffs st1 st2 [] ts

calcDiff :: PolicyPlus -> RichState -> RichState -> Diff
calcDiff pplus st1 st2 =
  Diff {
    _d_pc = guard (not $ eqOn (ms . fpc) st1 st2 && eqOn (ps . ppc) st1 st2)
            $> (st2 ^. ms . fpc, st2 ^. ps . ppc)
              
  , _d_instr =
      case (instr_fetch $ _ms st1, instr_fetch $ _ms st2) of
        ((Fetch u32,_), (Fetch u32',_))
          | u32 == u32' -> Nothing
          | otherwise   -> decode_I RV32 u32
        _ -> error "Bad instr fetch in calc diff"
  , _d_reg =
      let regDiff = diff (Map.assocs $ st1 ^. ms . fgpr)
                         (Map.assocs $ st2 ^. ms . fgpr) (const uninitialized_word)
          tagDiff = diff (Map.assocs $ st1 ^. ps . pgpr)
                         (Map.assocs $ st2 ^. ps . pgpr) (const $ initGPR pplus)

      in catMaybes $ mergeDiffs st1 st2 regDiff tagDiff

  , _d_mem =
      let dataDiff = diff (Map.assocs $ st1 ^. ms . fmem)
                          (Map.assocs $ st2 ^. ms . fmem) (const uninitialized_word)
          tagDiff  = diff (Map.assocs $ st1 ^. ps . pmem)
                          (Map.assocs $ st2 ^. ps . pmem) (\i -> if i == 0 || i >= instrLow pplus then emptyInstTag pplus else initMem pplus)
      in catMaybes $ mergeDiffs st1 st2 dataDiff tagDiff
  } 

-- | Printing | --
------------------

-- Helper Functions
x <|> y  = x P.<> P.text "\t|\t" P.<> y
x <:> y  = x P.<> P.text ": "    P.<> y
x <@> y  = x P.<> P.text " "     P.<> y
x <||> y = x P.<> P.text " || "  P.<> y

pad :: Int -> Doc -> Doc
pad i p = let s = show p in
          P.text (s ++ take (i - (length s)) (repeat ' '))

class Pretty a where
  pretty :: a -> Doc

instance Pretty TagSet where
  pretty t = P.text (showTagSet t)

instance Pretty Integer where
  pretty n = P.sizedText 2 $ show n

instance Pretty (Integer, TagSet) where
  pretty (n,t) = pretty n <@> pretty t

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing  = P.text "-"
  pretty (Just a) = pretty a

docPCs :: (Integer, TagSet) -> [Maybe (Integer,TagSet)] -> Doc
docPCs pc [] = pretty pc
docPCs pc pcs
  | all isNothing pcs = pretty pc
  | otherwise = foldl1 (<||>) (pretty pc : map pretty pcs)

-- | Takes an address and a list of diffs
-- | Assume: diffs are ordered and >= addr
-- | Returns all relevant to the addr diffs and the "remainder"
destrAssocs :: Ord addr =>
  Int -> addr -> [[(addr, val, tag)]] -> ([Maybe (addr,val,tag)], [[(addr,val,tag)]])
destrAssocs _ addr [] = ([], [])
destrAssocs n addr (avt:avts) =
  let (front, back) = destrAssocs (n+1) addr avts in
  case avt of
    [] -> (Nothing:front, []:back)
    (a,v,t):rest
      | a == addr -> (Just (a,v,t) : front, rest : back)
      | a >= addr -> (Nothing        : front, avt  : back)
      | otherwise -> error "destrAssocs"

pr_register :: InstrField -> Doc
pr_register n = P.char 'r' P.<> P.integer n  

pr_instr_I_type :: String -> InstrField -> InstrField -> InstrField -> Doc
pr_instr_I_type label rd rs imm =
  P.text label <+> pr_register rd <+> pr_register rs <+> P.integer imm

pr_instr_B_type :: String -> InstrField -> InstrField -> InstrField -> Doc
pr_instr_B_type label rd rs imm =
  P.text label <+> pr_register rd <+> pr_register rs <+> P.integer imm

pr_instr_R_type :: String -> InstrField -> InstrField -> InstrField -> Doc
pr_instr_R_type label rd rs1 rs2  =
  P.text label <+> pr_register rd <+> pr_register rs1 <+> pr_register rs2

pr_instr_J_type :: String -> InstrField -> InstrField -> Doc
pr_instr_J_type label rs imm =
  P.text label <+> pr_register rs <+> P.integer imm

instance Pretty Instr_I where
  pretty (ADD 0 0 0) = P.text "<NOP>"
  pretty (ADDI rd rs imm) = pr_instr_I_type "ADDI" rd rs imm
  pretty (LW rd rs imm) = pr_instr_I_type "LW" rd rs imm
  pretty (SW rd rs imm) = pr_instr_I_type "SW" rd rs imm
  pretty (ADD rd rs1 rs2) = pr_instr_R_type "ADD" rd rs1 rs2
  pretty (JAL rs imm) = pr_instr_J_type "JAL" rs imm
  pretty (BLT rs1 rs2 imm) = pr_instr_B_type "BLT" rs1 rs2 imm  
  pretty i = error $ show i

instance Pretty (Integer, Integer, TagSet) where
  pretty (a,v,t) =
    case decode_I RV32 v of
      -- HACK: if it parses as an instruction, it still might be data. However unlikely
      Just inst -> pretty inst <@> pretty t
      Nothing   -> pretty v <@> pretty t

docAssocs :: [(Integer, Integer, TagSet)] -> [[(Integer, Integer, TagSet)]] -> Doc
docAssocs [] _ = P.empty
docAssocs ((addr,val,tag):rest) diffs =
  let (top, rem) = destrAssocs 0 addr diffs in
--  trace ("Before destr:\n" ++ show diffs ++ "\nAfter destr:\n" ++ show top ++ "\n" ++ show rem) $
  if all isNothing top then
    pretty addr <:> pretty (addr,val, tag)
    $$ docAssocs rest rem
  else
    pretty addr <:> (foldl1 (<||>) (pretty (addr,val,tag) : map pretty top))
    $$ docAssocs rest rem
--docAssocs [] diffs = error $ "Diffs not exhausted: " ++ show diffs

docMaps :: (Map Integer Integer, Map Integer TagSet) -> [[(Integer, Integer, TagSet)]] -> Doc
docMaps (d, t) diffs =
--  trace ("Calling docMaps with:\n" ++ show d ++ "\n" ++ show t ++ "\n" ++ show diffs) $
  let assocs = zipWith (\(i,d) (j,t) -> (i,d,t)) (Map.assocs d) (Map.assocs t)
  in docAssocs assocs diffs

docRichStates :: RichState -> [Diff] -> Doc
docRichStates st diffs =
  P.vcat [ P.text "PC:" <+> docPCs (st ^. ms . fpc, st ^. ps . ppc)
                                   (map _d_pc diffs)
         , P.text "Registers:"
           $$ P.nest 2 (docMaps (st ^. ms . fgpr, st ^. ps . pgpr)
                                (map _d_reg diffs))
         , P.text "Memories:"
           $$ P.nest 2 (docMaps (st ^.  ms . fmem, st ^.  ps . pmem)
                                (map _d_mem diffs))
         ]

docTestState :: PolicyPlus -> TestState a -> Doc
docTestState pplus ts = docRichStates (ts ^. mp) (map (\x -> calcDiff pplus (ts ^. mp) (x ^. mp_state)) (ts ^. variants))

printTestState :: PolicyPlus -> TestState a -> String
printTestState pplus ts = P.render $ docTestState pplus ts
  

-- | Generation | --
--------------------

-- GPR's are hard coded to be [0..31], but we only use a couple of them
maxReg = 3

-- Generate a random register for source
genSourceReg :: Machine_State -> Gen GPR_Addr
genSourceReg ms =
  choose (0, maxReg)

-- Generate a target register GPR
-- For now, just avoid R0
genTargetReg :: Machine_State -> Gen GPR_Addr
genTargetReg ms =
  choose (1, maxReg)

-- Generate an immediate up to number
-- Multiple of 4
genImm :: Integer -> Gen InstrField
-- -- (Hmm - Why did we never generate 0 at some point?)
-- genImm n = (4*) <$> choose (1, n `div` 4)   
genImm n = (4*) <$> choose (0, n `div` 4)  

-- Data structure to capture the current status of the registers for generation
data RegInfo = RegInfo { _dataPtr :: [DataPtrInfo] 
                       , _codePtr :: [CodePtrInfo] 
                       , _arith   :: [ArithInfo  ] 
                       }

-- No information needed (for now) for Arith registers
data ArithInfo = AI { _aID :: GPR_Addr
                    -- Register ID
                    }

data CodePtrInfo = CPI { _cID  :: GPR_Addr
                       -- Register id
                       , _cMinImm :: Integer
                       -- Minimum immediate needed to make it a valid code pointer
                       }

data DataPtrInfo = DPI { _dID :: GPR_Addr
                       -- Register id
                       , _dVal :: Integer
                       -- Register contents
                       , _dMinImm :: Integer
                       -- Minimum immediate needed to make it a valid data pointer
                       , _dMaxImm :: Integer
                       -- Maximum immediate needed to make it a valid data pointer
                       , _dTag :: TagSet
                       -- Associated Register Tag
                       } 

makeLenses ''ArithInfo
makeLenses ''CodePtrInfo
makeLenses ''DataPtrInfo

-- dataP, codeP : Predicates over the tagset to establish potential invariants for code/data pointers.
-- Picks out valid (data registers + content + min immediate + max immediate + tag),
--                 (jump registers + min immediate),
--                 integer registers
groupRegisters :: PolicyPlus -> GPR_File -> GPR_FileT ->
                  (TagSet -> Bool) -> (TagSet -> Bool) ->
                  RegInfo
groupRegisters pplus (GPR_File rs) (GPR_FileT ts) dataP codeP =
  -- Assuming that the register files are same length and they have no holes
  let regs = Map.assocs rs
      tags = Map.assocs ts
      rts = zip regs tags

      validData ((reg_id,reg_content),(_reg_id, reg_tag)) 
        | reg_content >= dataMemLow pplus &&
          reg_content <= dataMemHigh pplus
          && dataP reg_tag =
           Just $ DPI reg_id reg_content 0 (dataMemHigh pplus - reg_content) reg_tag
        | reg_content == 0 && dataP reg_tag =
        -- We can allow a 0 register by adding at least 4
           Just $ DPI reg_id 0 (dataMemLow pplus) (dataMemHigh pplus) reg_tag
        | otherwise =
           Nothing
        
      validJump ((reg_id,reg_content),(_, reg_tag))
        | reg_content < instrLow pplus && codeP reg_tag =
          Just $ CPI reg_id (instrLow pplus - reg_content)
        | otherwise =
          Nothing

      dataRegs    = map (fromJust) $ filter (isJust) $ map validData rts
      controlRegs = map (fromJust) $ filter (isJust) $ map validJump rts
      arithRegs   = map (AI . fst) regs
  in RegInfo dataRegs controlRegs arithRegs

-- TODO: This might need to be further generalized in the future
genInstr :: PolicyPlus -> Machine_State -> PIPE_State ->
            (TagSet -> Bool) -> (TagSet -> Bool) ->
            (Instr_I -> Gen TagSet) -> Gen (Instr_I, TagSet)
genInstr pplus ms ps dataP codeP genInstrTag =
  let RegInfo dataInfo ctrlInfo arithInfo =
        groupRegisters pplus (f_gprs ms) (p_gprs ps) dataP codeP

      onNonEmpty [] _= 0
      onNonEmpty _ n = n

  in
  frequency [ (onNonEmpty arithInfo 1,
               do -- ADDI
                  AI rs <- elements arithInfo
                  rd <- genTargetReg ms
                  imm <- genImm $ dataMemHigh pplus
                  let instr = ADDI rd rs imm
                  tag <- genInstrTag instr
                  return (instr, tag)
              )
            , (onNonEmpty dataInfo 3,
               do -- LOAD
                  DPI rs content min_imm max_imm tag <- elements dataInfo
                  rd <- genTargetReg ms
                  imm <- (min_imm+) <$> genImm (max_imm - min_imm)
                  -- TODO: Think about generalizing this reachability thingy.
--                  let locs = --traceShow (content, min_imm, max_imm, tag) $
--                             reachableLocsBetween pplus (f_mem ms) (p_mem ps) (content+min_imm) (content+max_imm) tag
--                  rd <- genTargetReg ms
--                  imm <- frequency [ -- Generate a reachable location)
--                                     (--traceShow locs $
--                                       onNonEmpty locs 1,
--                                      do addr <- elements locs
--                                         return $ addr - content)
--                                   , (1, (min_imm+) <$> genImm (max_imm - min_imm))
--                                   ]
                  let instr = LW rd rs imm
                  tag <- genInstrTag instr
                  return (instr, tag)
              )
            , (onNonEmpty dataInfo 3 * onNonEmpty arithInfo 1,
               do -- STORE
                  DPI rd content min_imm max_imm tag <- elements dataInfo
                  rs <- genTargetReg ms
                  imm <- (min_imm+) <$> genImm (max_imm - min_imm)
                  let instr = SW rd rs imm
                  tag <- genInstrTag instr
                  return (instr, tag)
              )
            , (onNonEmpty arithInfo 1,
               do -- ADD
                  AI rs1 <- elements arithInfo
                  AI rs2 <- elements arithInfo
                  rd <- genTargetReg ms
                  let instr = ADD rd rs1 rs2 
                  tag <- genInstrTag instr
                  return (instr, tag)
              )
            , (onNonEmpty arithInfo 1,
               do -- BLT
                  AI rs1 <- elements arithInfo
                  AI rs2 <- elements arithInfo
                  imm <- (8+) <$> genImm 12 --TODO: More principled relative jumps
                  -- BLT does multiples of 2
                  let instr = BLT rs1 rs2 imm
                  tag <- genInstrTag instr
                  return (instr, tag)
              )
            ]

genDataMemory :: PolicyPlus -> (PolicyPlus -> Gen TagSet) -> Gen (Mem, MemT)
genDataMemory pplus genMTag = do
  let idx = [dataMemLow pplus, (dataMemLow pplus)+4..(dataMemHigh pplus)]
  combined <- mapM (\i -> do d <- genImm $ dataMemHigh pplus 
                             t <- genMTag pplus
                             return ((i, d),(i,t))) idx
  let (m,pm) = unzip combined
  return (Mem (Map.fromList m) Nothing, MemT $ Map.fromList pm)

setInstrI :: Machine_State -> Instr_I -> Machine_State
setInstrI ms i =
  ms & fmem . at (f_pc ms) ?~ (encode_I RV32 i) 

setInstrTagI :: Machine_State -> PIPE_State -> TagSet -> PIPE_State
setInstrTagI ms ps it =
  ps & pmem . at (f_pc ms) ?~ it 

-- | Generation by execution receives an initial machine X PIPE state and
-- | generates instructions until n steps have been executed.
-- | Returns the original machines with just the instruction memory locations
-- | updated.
genByExec :: PolicyPlus -> Int -> Machine_State -> PIPE_State ->
             (TagSet -> Bool) -> (TagSet -> Bool) -> (Instr_I -> Gen TagSet) ->
             Gen (Machine_State, PIPE_State)
genByExec pplus n init_ms init_ps dataP codeP genInstrTag =
  exec_aux n init_ms init_ps init_ms init_ps
  where exec_aux 0 ims ips ms ps = return (ims, ips)
        exec_aux n ims ips ms ps 
        -- Check if an instruction already exists
          | Map.member (f_pc ms) (f_dm $ f_mem ms) =
            case fetch_and_execute pplus ms ps of
              Right (ms'', ps'') ->
                exec_aux (n-1) ims ips ms'' ps'' 
              Left err ->
                -- trace ("Warning: Fetch and execute failed with " ++ show n
                --        ++ " steps remaining and error: " ++ show err) $
                return (ms, ps)
          | otherwise = do
              -- Generate an instruction for the current state
              (is, it) <- genInstr pplus ms ps dataP codeP genInstrTag
              -- Update the i-memory of both the machine we're stepping...
              let ms' = ms & fmem . at (f_pc ms) ?~ (encode_I RV32 is)
                  ps' = ps & pmem . at (f_pc ms) ?~ it 
              -- .. and the i-memory of the inital pair _at the f_pc ms location_
                  ims' = ims & fmem . at (f_pc ms) ?~ (encode_I RV32 is)
                  ips' = ips & pmem . at (f_pc ms) ?~ it
              -- Proceed with execution
              -- traceShow ("Instruction generated...", is) $
              case fetch_and_execute pplus ms' ps' of
                Right (ms'', ps'') ->
                  -- trace "Successful execution" $
                  exec_aux (n-1) ims' ips' ms'' ps'' 
                Left err ->
                  -- trace ("Warning: Fetch and execute failed with "
                  --       ++ show n ++ " steps remaining and error: " ++ show err) $
                  return (ims', ips')

genGPRs :: Machine_State -> Gen Machine_State
-- Map GPR_Addr GPR_Val -> Gen (Map GPR_Addr GPR_Val) 
genGPRs m = do
  ds <- replicateM 3 $ genImm 40
  return $ m & fgpr %~ Map.union (Map.fromList $ zip [1..] ds)

genGPRTs :: PolicyPlus -> PIPE_State -> Gen TagSet -> Gen PIPE_State
genGPRTs pplus p genGPRTag = do 
  cs <- replicateM 3 genGPRTag
  return $ p & pgpr %~ Map.union (Map.fromList $ zip [1..] cs)

genMachine :: PolicyPlus -> (PolicyPlus -> Gen TagSet) -> (PolicyPlus -> Gen TagSet) ->
             (TagSet -> Bool) -> (TagSet -> Bool) -> (Instr_I -> Gen TagSet) ->
             Gen RichState
genMachine pplus genMTag genGPRTag dataP codeP genITag = do

  -- | Initial memory
  (mm,pm) <- genDataMemory pplus genMTag
  let ms = initMachine
             & fmem_mem .~ mm 
             & fmem . at (f_pc initMachine) ?~ (encode_I RV32 $ JAL 0 1000) 
      ps = init_pipe_state pplus
             & pmem_mem .~ pm
             & pmem . at (f_pc ms) ?~ (emptyInstTag pplus) 

  -- | Update registers
  ms' <- genGPRs  ms
  ps' <- genGPRTs pplus ps (genGPRTag pplus)

  -- | Do generation by execution
  (ms_fin, ps_fin) <- genByExec pplus maxInstrsToGenerate ms' ps' dataP codeP genITag

  return $ Rich ms_fin ps_fin

maxInstrsToGenerate :: Int
maxInstrsToGenerate = 10

varySecretMap :: PolicyPlus -> (TagSet -> Bool) ->
  Map Integer Integer -> Map Integer TagSet ->
  Gen (Map Integer Integer, Map Integer TagSet)
varySecretMap pplus isSecret m pm = do 
  combined <- mapM (\((i,d),(j,t)) -> 
                       if isSecret t then do
                         d' <- genImm 12       -- TODO: This makes no sense
                         return ((i,d'),(j,t)) -- TODO: Here we could scramble v
                       else
                         return ((i,d),(j,t))
                   ) $ zip (Map.assocs m) (Map.assocs pm)
  let (m', pm') = unzip combined
  return (Map.fromList m', Map.fromList pm')

varySecretMem pplus isSecret (Mem m ra) (MemT pm) = do
  (m', pm') <- varySecretMap pplus isSecret m pm
  return (Mem m' ra, MemT pm')

varySecretGPR pplus isSecret (GPR_File m) (GPR_FileT pm) = do
  (m', pm') <- varySecretMap pplus isSecret m pm
  return (GPR_File m', GPR_FileT pm')

varySecretState :: PolicyPlus -> (Machine_State -> PIPE_State -> TagSet -> Bool) ->
                   RichState -> Gen RichState
varySecretState pplus isSecretMP rs@(Rich m p) = do
  let isSecret = isSecretMP m p
  (mem', pmem') <- varySecretMem pplus isSecret (f_mem m) (p_mem p)
  (gpr', pgpr') <- varySecretGPR pplus isSecret (f_gprs m) (p_gprs p)
  return $ Rich ( m & fmem_mem .~ mem'
                    & fgpr_gpr .~ gpr')
                ( p & pmem_mem .~ pmem'
                    & pgpr_gpr .~ pgpr')

genSingleTestState :: PolicyPlus
                   -> (PolicyPlus -> Gen TagSet) -> (PolicyPlus -> Gen TagSet)
                   -> (TagSet -> Bool) -> (TagSet -> Bool)
                   -> (Instr_I -> Gen TagSet)
                   -> Gen (TestState a)
genSingleTestState pplus genMTag genGPRTag dataP codeP genITag = do
  rs <- genMachine pplus genMTag genGPRTag dataP codeP genITag
  return $ TS rs []

genVariationTestState :: PolicyPlus
                      -> (PolicyPlus -> Gen TagSet) -> (PolicyPlus -> Gen TagSet)
                      -> (TagSet -> Bool) -> (TagSet -> Bool)
                      -> (Instr_I -> Gen TagSet)
                      -> (Machine_State -> PIPE_State -> TagSet -> Bool)
                      -> (Machine_State -> PIPE_State -> a)
                      -> Gen (TestState a)
genVariationTestState pplus genMTag genGPRTag dataP codeP genITag isSecretMP mkInfo = do
  rs  <- genMachine pplus genMTag genGPRTag dataP codeP genITag
  rs' <- varySecretState pplus isSecretMP rs
  let a = mkInfo (rs' ^. ms) (rs' ^. ps)
  return $ TS rs [SE rs' a]

--genTestStateVariation :: PolicyPlus ->
--                         (Machine_State -> PIPE_State -> TagSet) ->
--                         (Machine_State -> PIPE_State -> a)
--                         Gen (TestState a)
--genTestStateVariation pplus isSecretMP mkInfo = do
--  rs  <- genMachine pplus
--  rs' <- varySecretState pplus isSecretMP
--  return $ TS rs [SE rs' (mkInfo (rs' ^. ms) (rs' ^. ps))]

-- | Execution |--
------------------

running :: Machine_State -> Bool
running m = mstate_run_state_read m == Run_State_Running

exec :: PolicyPlus -> RichState -> Either String RichState
exec pplus (Rich m p) = uncurry Rich <$> fetch_and_execute pplus m p
  
--Either String TestState
step :: PolicyPlus -> TestState a -> Either String (TestState a)
step pplus ts
  -- If all machines are in running state
  | allOf (richStates . ms) running ts =
      ts & richStates . ms %~  mstate_io_tick
         & richStates      %%~ exec pplus
  | otherwise =
      Left "Not Running State" 

traceExec :: PolicyPlus -> TestState a -> Int -> ([TestState a], String)
traceExec pplus ts 0 = ([], "Out of fuel")
traceExec pplus ts n =
  case step pplus ts of
    Right ts' -> first (ts:) $ traceExec pplus ts' (n-1)
    Left  err -> ([], err)

-- | Indistinguishability |--
-----------------------------

-- TODO: Default values? Reuse calcDiff and filter that?
indist :: (TagSet -> Bool) -> RichState -> RichState -> Bool
indist isPublic rs rs' = 
  let filterAux [] _ = []
      filterAux _ [] = []
      filterAux ((i,d):ds) ((j,t):ts)
        | i == j = 
            if isPublic t then
              filterAux ds ts
            else d : filterAux ds ts
        | i < j = filterAux ds ((j,t):ts)
        | i > j = filterAux ((i,d):ds) ts
 
      m1 = filterAux (Map.assocs $ f_dm $ f_mem $ _ms rs)
                     (Map.assocs $ unMemT $ p_mem $ _ps rs')
      m2 = filterAux (Map.assocs $ f_dm $ f_mem $ _ms rs)
                     (Map.assocs $ unMemT $ p_mem $ _ps rs')
      r1 = filterAux (Map.assocs $ rs  ^. ms . fgpr)
                     (Map.assocs $ unGPRT $ p_gprs $ _ps rs)
      r2 = filterAux (Map.assocs $ rs' ^. ms . fgpr)
                     (Map.assocs $ unGPRT $ p_gprs $ _ps rs')
 
  in 
  (r1 == r2) && (m1 == m2)


indistinguishable :: (TagSet -> Bool) -> TestState a -> Bool
indistinguishable isPublic ts =
  allOf (variants . folded . mp_state) (indist isPublic (ts ^. mp)) ts
 
pcInSync :: TestState a -> Bool
pcInSync ts =
  allOf (variants . folded . mp_state . ms . fpc) (== (ts ^. mp . ms . fpc)) ts


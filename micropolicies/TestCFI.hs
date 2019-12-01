{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module TestCFI where

-- From Haskell libraries
import Control.Arrow (second, (***))
import Control.Exception.Base (assert)
import Control.Monad.Reader

import Data.Bits
import Data.List (zip4,unzip4)
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace

import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P

import Control.Lens hiding (elements)


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

-- From .
import Gen
import Run_Program_PIPE
import PIPE
import Printing
import Terminal 
import MachineLenses

------------------------------------------------------------------------------------
-- Printing

-- TODO: Instance p pretty
instance PP (Machine_State, PIPE_State) where
  pp pplus (m,p) =
    let ppol = policy pplus in
    P.vcat [ P.text "PC:" <+> pretty pplus (f_pc m, p_pc p) (f_pc m, p_pc p)
           , P.text "Registers:" $$ P.nest 2 (pretty pplus (f_gprs m, p_gprs p) (f_gprs m, p_gprs p))
           , P.text "Memories:" $$ P.nest 2 (pretty pplus (f_mem m, p_mem p) (f_mem m, p_mem p))
           ]

verboseTracing = False
--verboseTracing = True

printTrace pplus tr = putStrLn $ P.render $ prettyTrace pplus tr

prettyTrace :: PolicyPlus -> [(Machine_State, PIPE_State)] -> Doc 
prettyTrace pplus [] = P.empty
prettyTrace pplus [(m,p)] = pp pplus (m,p)
prettyTrace pplus (tr@((m,p):_)) =
    pp pplus (m,p) $$ P.text ""
      $$ P.text "Trace:" $$ prettyDiffs pplus tr

prettyDiffs :: PolicyPlus -> [(Machine_State, PIPE_State)] -> Doc
prettyDiffs pplus ((m1,p1):(m2,p2):tr) =
  (if verboseTracing then
       P.text "----------------------------------------------------------------"
    $$ P.nest 10 (P.text "Raw Machine memory:" $$ P.nest 3 (P.text (show $ f_dm $ f_mem m1)))
    $$ P.nest 10 (P.text "Raw Machine 1 tags:" $$ P.nest 3 (P.text (show $ p_mem p1)))
    $$ P.nest 10 (P.text "Machine:" $$ P.nest 3 (pretty pplus m2 p2))
  else P.empty)
  $$ pp pplus (calcDiff pplus (m1,p1) (m2,p2))
  $$ prettyDiffs pplus ((m2,p2):tr)
prettyDiffs pplus [(m,p)] =
  P.text "" $$ P.text "Final:" $$ pp pplus (m,p) 
prettyDiffs _ _ = P.empty

data Diff = Diff { d_pc :: (Integer, TagSet)               -- value and tag of the current PC
                 , d_instr :: Maybe Instr_I                -- current instruction
                 , d_reg :: [(GPR_Addr, Integer, TagSet)]  -- change in registers
                 , d_mem :: [(Integer, Integer, TagSet)]   -- Change in memory
                 }

-- Generic "find diffs" function: Takes two association lists l1 and
-- l2, both assumed sorted by their keys and both representing
-- *infinite* maps with some default value d (passed as third
-- parameter), and returns a list of changes
--
-- N.b. In the cases where we are returning something, we first have
-- to check whether the thing we are returning is equal to d!  (And
-- not return it in this case.)
diff :: (Ord a, Eq b) => [(a, b)] -> [(a, b)] -> b -> [(a, (b, b))]
diff [] [] d = []
diff ((x1,y1):l1) [] d = (if y1==d then [] else [(x1,(y1,d))]) ++ diff l1 [] d
diff [] ((x2,y2):l2) d = (if y2==d then [] else [(x2,(d,y2))]) ++ diff [] l2 d
diff ((x1,y1):l1) ((x2,y2):l2) d
         | x1 < x2   = (if y1==d then [] else [(x1,(y1,d))]) ++ diff l1 ((x2,y2):l2) d
         | x1 > x2   = (if y2==d then [] else [(x2,(d,y2))]) ++ diff ((x1,y1):l1) l2 d
         | otherwise = (if y1==y2 then [] else [(x1,(y1,y2))]) ++ diff l1 l2 d 

calcDiff :: PolicyPlus -> (Machine_State, PIPE_State) -> (Machine_State, PIPE_State) -> Diff
calcDiff pplus (m1,p1) (m2,p2) =
  Diff {
    d_pc = (f_pc m1, p_pc p1)
  , d_instr =
      case fst $ instr_fetch m1 of
        Fetch u32 -> decode_I RV32 u32
        _ -> error "Bad instr fetch in calcDiff"
  , d_reg =
      let GPR_File r1 = f_gprs m1
          GPR_File r2 = f_gprs m2
          GPR_FileT t1 = p_gprs p1
          GPR_FileT t2 = p_gprs p2
          reg_diff =
            filter (\((i1,d1),(i2,d2)) -> assert (i1 == i2) $ d1 /= d2)
                   (zip (Map.assocs r1) (Map.assocs r2))
          tag_diff =
            filter (\((i1,l1),(i2,l2)) -> assert (i1 == i2) $ l1 /= l2)
                   (zip (Map.assocs t1) (Map.assocs t2))
      in case (reg_diff, tag_diff) of
           ([], []) -> []
           ([((i,_),(_,d))],[((j,_),(_,l))]) | i == j -> [(i,d,l)]
           ([((i,_),(_,d))],[]) ->
             catMaybes [(i,d,) <$> Map.lookup i t2]
           ([],[((i,_),(_,l))]) ->
             catMaybes [(i,,l) <$> Map.lookup i r2]
           _ -> -- TODO (Leo!)
                error $ "More than one diff in register file:" ++
                        " registers = " ++ show reg_diff ++
                        " and tags = " ++ show tag_diff
  , d_mem =
      let Mem dm1 _ = f_mem m1
          Mem dm2 _ = f_mem m2
          MemT pm1 = p_mem p1
          MemT pm2 = p_mem p2
          both1 = map (\((i,d),(j,t)) -> assert (i==j) $ (i,(d,t))) $ zip (Map.assocs dm1) (Map.assocs pm1)
          both2 = map (\((i,d),(j,t)) -> assert (i==j) $ (i,(d,t))) $ zip (Map.assocs dm2) (Map.assocs pm2)
          diffs = diff both1 both2 (uninitialized_word, emptyInstTag pplus)
          extract (i,(_,(d,t))) = (i,d,t)
       in map extract diffs 
  }


ppRegDiff pplus ((i,d,l):r) =
  (P.char 'r' P.<> P.integer i <+> P.text "<-" <+> pretty pplus d l)
        $$ ppRegDiff pplus r
ppRegDiff _ [] = P.empty

ppMemDiff pplus ((i,d,l):m) =
  (P.char '[' P.<> P.integer i P.<> P.char ']' <+> P.text "<-" <+> pretty pplus d l)
  $$ ppMemDiff pplus m
ppMemDiff _ [] = P.empty

instance PP (Maybe Instr_I) where
  pp pplus (Just i) = pp pplus i
  pp _ Nothing = P.text "<Bad instr>"

instance PP Diff where
  pp pplus d =
    P.hcat [ pad 17 (pp pplus (d_pc d))
           , P.text " "
           , pad 17 (pp pplus (d_instr d))
           , P.text "     "
           , ppRegDiff pplus (d_reg d)
           , ppMemDiff pplus (d_mem d)
           ]

-- Null "show" functions, for things that we don't want QuickCheck trying to print
instance Show Machine_State where
  show _ = ""
instance Show MStatePair where
  show _ = ""

------------------------------------------------------------------------------------------
-- Generation

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

-- Picks out valid (data registers + content + min immediate + max immediate + tag),
--                 (jump registers + min immediate),
--                 integer registers
groupRegisters :: PolicyPlus -> GPR_File -> GPR_FileT ->
                  ([(GPR_Addr, Integer, Integer, Integer, TagSet)],
                   [(GPR_Addr, Integer)],
                   [GPR_Addr])
groupRegisters pplus (GPR_File rs) (GPR_FileT ts) =
  -- Assuming that the register files are same length and they have no holes
  let regs = Map.assocs rs
      tags = Map.assocs ts
      rts = zip regs tags

      validData ((reg_id,reg_content),(_reg_id, reg_tag)) 
        | reg_content >= dataMemLow pplus &&
          reg_content <= dataMemHigh pplus =
           Just (reg_id, reg_content, 0, dataMemHigh pplus - reg_content, reg_tag)
        | reg_content == 0 =
        -- We can allow a 0 register by adding at least 4
           Just (reg_id, 0, dataMemLow pplus, dataMemHigh pplus, reg_tag)
        | otherwise =
           Nothing
        
      validJump ((reg_id,reg_content),(_, reg_tag))
        | reg_content < instrLow pplus =
          Just (reg_id, instrLow pplus - reg_content)
        | otherwise =
          Nothing

      dataRegs    = map (fromJust) $ filter (isJust) $ map validData rts
      controlRegs = map (fromJust) $ filter (isJust) $ map validJump rts
      arithRegs   = map fst regs
  in (dataRegs, controlRegs, arithRegs)


genInstr :: PolicyPlus -> Machine_State -> PIPE_State -> Gen (Instr_I, TagSet)
genInstr pplus ms ps =
  let (dataRegs, ctrlRegs, arithRegs) = groupRegisters pplus (f_gprs ms) (p_gprs ps)
      onNonEmpty [] _= 0
      onNonEmpty _ n = n
  in 
  frequency [ (onNonEmpty arithRegs 1,
               do -- ADDI
                  rs <- elements arithRegs
                  rd <- genTargetReg ms
                  imm <- genImm (dataMemHigh pplus)
                  tag <- genITag
                  return (ADDI rd rs imm, tag))
            , (onNonEmpty dataRegs 3,
               do -- LOAD
                  (rs,content,min_imm,max_imm,tag) <- elements dataRegs
                  rd <- genTargetReg ms
                  imm <- (min_imm +) <$> genImm (max_imm - min_imm)
                  tag <- genITag
                  return (LW rd rs imm, tag)
              )
            , (onNonEmpty dataRegs 3 * onNonEmpty arithRegs 1,
               do -- STORE    -- TODO: Don't write to WriteNever locations too often!
                  (rd,content, min_imm,max_imm,tag) <- elements dataRegs
                  rs <- genTargetReg ms
                  imm <- (min_imm+) <$> genImm (max_imm - min_imm)
                  tag <- genITag
                  return (SW rd rs imm, tag))
            , (onNonEmpty arithRegs 1,
               do -- ADD
                  rs1 <- elements arithRegs
                  rs2 <- elements arithRegs
                  rd <- genTargetReg ms
                  tag <- genITag
                  return (ADD rd rs1 rs2, tag))
            , (1,
               do -- JMP := JAL r0
                  imm <- (4*) <$> choose (0, 10)
                  tag <- genITag
                  return (JAL 0 imm, tag))
            ]

genITag :: Gen TagSet
genITag = do
  frequency [ (1, pure defaultTag)
            , (1, pure targetTag) ]

genDataMemory :: PolicyPlus -> Gen (Mem, MemT)
genDataMemory pplus = do
  let idx = [dataMemLow pplus, (dataMemLow pplus)+4..(dataMemHigh pplus)]
  combined <- mapM (\i -> do d <- genImm $ dataMemHigh pplus    -- BCP: This always puts 4 in every location!
                             t <- pure defaultTag
                             return ((i, d),(i,t))) idx
  let (m,pm) = unzip combined
  return (Mem (Map.fromList m) Nothing, MemT $ Map.fromList pm)

-- | Generation by execution receives an initial machine X PIPE state and
-- | generates instructions until n steps have been executed.
-- | Returns the original machines with just the instruction memory locations
-- | updated.
genByExec :: PolicyPlus -> Int -> Machine_State -> PIPE_State ->
             Gen (Machine_State, PIPE_State)
genByExec pplus n init_ms init_ps = exec_aux n init_ms init_ps init_ms init_ps
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
              (is, it) <- genInstr pplus ms ps
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
--    Map.union (Map.fromList $ zip [1..] ds) rs
--  [d1, d2, d3] <- 
--  let rs' :: Map.Map Integer Integer = Map.insert 1 d1 $ Map.insert 2 d2 $ Map.insert 3 d3 rs
--  return $ GPR_File rs'

-- updRegs :: GPR_File -> Gen GPR_File
-- updRegs (GPR_File rs) = do
--   [d1, d2, d3] <- replicateM 3 $ genImm 40
--   let rs' :: Map.Map Integer Integer = Map.insert 1 d1 $ Map.insert 2 d2 $ Map.insert 3 d3 rs
--   return $ GPR_File rs'

genMachine :: PolicyPlus -> Gen (Machine_State, PIPE_State)
genMachine pplus = do
  -- registers
  (mm,pm) <- genDataMemory pplus
  let ms = initMachine
             & fmem_mem .~ mm --{f_mem = mem}
             & fmem . at (f_pc initMachine) ?~ (encode_I RV32 $ JAL 0 1000) 
      ps = init_pipe_state pplus
             & pmem_mem .~ pm
             & pmem . at (f_pc ms) ?~ (emptyInstTag pplus) 

  ms' <- genGPRs  ms
  let ps' = ps 

  (ms_fin, ps_fin) <- genByExec pplus maxInstrsToGenerate ms' ps' 

  return (ms_fin, ps_fin & pmem . at 1000 ?~ targetTag)

------------------------------------------------------------------------------------------
-- Shrinking

shrinkTag :: TagSet -> [TagSet]
shrinkTag t =
  case toExt t of
    [("writeonce.WriteOnce", Nothing)] -> [defaultTag]
    _ -> []

-- INV: If we're shrinking registers, everything should already be equal.
shrinkRegister :: PolicyPlus -> (Integer, TagSet) -> [(Integer, TagSet)]
shrinkRegister pplus (d,t) = [(d',t') | d' <- shrink d, t' <- shrinkTag t]

shrinkVector :: (a -> [a]) -> [a] -> [[a]]
shrinkVector f []    = []
shrinkVector f (h:t) = map (:t) (f h) ++ map (h:) (shrinkVector f t)

-- INV: The register files are also identical
shrinkGPR :: PolicyPlus -> (GPR_File, GPR_FileT) -> [(GPR_File, GPR_FileT)] 
shrinkGPR pplus (GPR_File d, GPR_FileT t) =
  -- assert (d1==d2 && t1 == t2) $
  let combined :: [( (GPR_Addr, GPR_Val), (InstrField, TagSet) )] 
      combined = zip (Map.assocs d) (Map.assocs t)
  in 
  [ (GPR_File $ Map.fromList d', GPR_FileT $ Map.fromList t') 
  | (d',t') <- map unzip $ shrinkVector shrinkR combined
  ]
  where shrinkR :: ((GPR_Addr, GPR_Val), (InstrField, TagSet))
               -> [((GPR_Addr, GPR_Val), (InstrField, TagSet))]
        shrinkR ((i1,v1),(j1,l1)) =
             [ ((i1,v'),(j1,l1)) | v' <- shrink v1 ]
          ++ [ ((i1,v1),(j1,l')) | l' <- shrinkTag l1 ]

-- To shrink an instruction, try converting it to a noop (ADD 0 0 0)
shrinkInstr :: Instr_I -> [Instr_I]
shrinkInstr (ADD 0 0 0)  = []
-- Do not shrink the initial JAL
shrinkInstr (JAL 0 1000) = []
shrinkInstr _ = [ADD 0 0 0]

type IndexedTaggedInt = ((Integer,Integer), (Integer,TagSet))

-- Have to perform the same thing to both memories at once
-- We also need the set of reachable things for data memories
-- INV: Original memories contain identical indices
shrinkMem :: PolicyPlus -> (Mem, MemT) -> [(Mem, MemT)]
shrinkMem pplus (Mem m i, MemT t) =
  let m' = Map.assocs m
      t' = Map.assocs t

      isData  i = i >= dataMemLow pplus && i <= dataMemHigh pplus
      isInstr i = i == 0 || i >= instrLow pplus
 
      shrinkMemLoc :: (Integer, Integer, TagSet) -> [ IndexedTaggedInt ]
      shrinkMemLoc (j,d,l)
        | isInstr j =
          case decode_I RV32 d of
            (Just i) ->
              [ ( (j, d'), (j, l) ) | d' <- encode_I RV32 <$> shrinkInstr i] ++
              [ ( (j, d), (j, l') ) | l' <- shrinkTag l ]
            _ -> error "Instruction can't be decoded while shrinking"
        | otherwise =
              [ ( (j, d'), (j, l) ) | d' <- shrink d] ++
              [ ( (j, d), (j, l') ) | l' <- shrinkTag l ]
 
      shrinkMemAux :: [ IndexedTaggedInt ] -> [ [IndexedTaggedInt] ]
      shrinkMemAux [] = []
      shrinkMemAux (((j,d),(_,l)):more) =
        -- Shrink Current memory location and rebuild mem
        [ (loc':more) | loc' <- shrinkMemLoc (j,d,l) ]
        ++
        -- Keep current memory location and shrink something later on
        [ ( ((j,d),(j,l)) : more' ) | more' <- shrinkMemAux more ]
 
      indexTagedIntsToMem :: Maybe (Integer,Integer) -> [IndexedTaggedInt] -> (Mem, MemT)
      indexTagedIntsToMem i itis = ((flip Mem i) . Map.fromList) *** (MemT . Map.fromList) $ unzip itis
        
  in map (indexTagedIntsToMem i) $ shrinkMemAux (zip m' t') 
        
shrinkMState :: PolicyPlus -> (Machine_State, PIPE_State) -> [(Machine_State, PIPE_State)]
shrinkMState pplus (m,p) =
     [ (m & fmem_mem .~ mem, p & pmem_mem .~ pmem)
     | (mem, pmem) <- shrinkMem pplus (f_mem m, p_mem p) ]
  ++ [ (m & fgpr_gpr .~ gpr, p & pgpr_gpr .~ pgpr)
     | ((gpr, pgpr)) <- shrinkGPR pplus (f_gprs m, p_gprs p) ]


------------------------------------------------------------------------------------------
-- Top-level non-interference policy
  
{- Noninterference:
     - for each program p and machine state s1
     - for each s2 that agrees with s on (the pure values stored in)
       memory cells colored with reachable colors
     - p coterminates on s1 and s2
     - moreover, if it terminates in s1' and s2', then s1' and s2'
       also agree on all reachable memory cells

   Note that this is quite an intensional property -- not so easy for
   programmers to reason about their implications!  Also, an
   interesting extension is to add a stack, either (initially) in
   hardware or (harder) protected with tags so that called procedures
   cannot access their callers' stack frames.  This gives a more
   interesting (though, pragmatically, still rather weak) property.
   To get a pragmatically more useful property, something like "sealed
   capabilities" (aka closures) or a protected stack is needed. -}

defaultTag   = fromExt [("cfi.None", Nothing)]
targetTag    = fromExt [("cfi.Target", Nothing)]

step pplus (m,p) = 
  let run_state = mstate_run_state_read m
      m' = mstate_io_tick m in
  if run_state /= Run_State_Running then 
    Left (show run_state) 
  else
    fetch_and_execute pplus m' p 

eval pplus count maxcount trace (m,p) =
  if count >= maxcount then 
    reverse trace
  else   
    case step pplus (m,p) of
      Right (m',p') -> eval pplus (count+1) maxcount ((m,p) : trace) (m',p')
      Left _ -> reverse trace

genGraph :: Machine_State -> [(Integer, Integer)]
genGraph' ms = 
  let ds = Map.assocs (ms ^. fmem)
      pairs = catMaybes $ map (\(i,c) -> case decode_I RV32 c of
                                           Just (JAL _ imm) -> Just (i, i + imm)
                                           Just _ -> Nothing
                                           Nothing -> Nothing
                              ) ds
  in pairs
--  let genEdge = (4*) <$> choose (960 `div` 4, 1040 `div` 4 )
--      graphSize = 42
--  sources <- replicateM graphSize genEdge
--  targets <- replicateM graphSize genEdge

genGraph ms = [(0,1000)]

-- graph :: [(Int, Int)]
prop_CFI pplus count maxcount (m,p) =
  let graph = genGraph m in
  let trace = eval pplus count maxcount [] (m,p)
      -- Allows you to jump to the next instruction!
      aux []  = property True
      aux [_] = property True
      aux ((m1,p1):(m2,p2):rest) =
        if f_pc m1 + 4 /= f_pc m2 && f_pc m2 /= 0 then
          if (elem (f_pc m1, f_pc m2) graph) then aux ((m2,p2):rest)
          else 
          whenFail (do putStrLn $ "Bad branch from " ++ show (f_pc m1) ++ " to " ++ show (f_pc m2)
                       printTrace pplus trace
                   ) False
        else aux ((m2,p2):rest) in
  aux trace

maxInstrsToGenerate :: Int
maxInstrsToGenerate = 20

newtype MP = MP (Machine_State, PIPE_State)

instance Show MP where
  show (MP (m, p)) = show m

prop :: PolicyPlus -> MP -> Property
prop pplus (MP (m,p)) = prop_CFI pplus 0 maxInstrsToGenerate (m,p)

------------------------------------------------------------------------------------------
-- The heap-safety policy
  
load_policy = do
  ppol <- load_pipe_policy "cfi.main"
  let pplus = PolicyPlus
        { policy = ppol
        , initGPR = defaultTag
        , initMem = defaultTag
        , initPC = defaultTag
        , initNextColor = 5
        , emptyInstTag = defaultTag
        , instrLow = 0
        , instrHigh   = 100
        , dataMemLow  = 1000
        , dataMemHigh = 1020
        }
  return pplus

-- The real one
main_test = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1000}
    $ forAllShrink (MP <$> genMachine pplus)
                   (\(MP m) -> [] --map MP $ shrinkMState pplus m ++ 
--                                        concatMap (shrinkMState pplus) (shrinkMState pplus m)
                   )
    $ \m -> prop pplus m

main = main_test

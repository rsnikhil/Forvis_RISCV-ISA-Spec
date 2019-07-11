{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module TestWriteOnce where

-- From Haskell libraries
import Control.Arrow (second, (***))
import Control.Exception.Base (assert)
import Control.Monad.Reader

import Data.Bits
import Data.List (zip4,unzip4)
import Data.Maybe
import qualified Data.List as Data_List
import qualified Data.Map.Strict as Data_Map
import qualified Data.Set as Data_Set
import Data.Set (Set)

import Debug.Trace

import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P

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
                   (zip (Data_Map.assocs r1) (Data_Map.assocs r2))
          tag_diff =
            filter (\((i1,l1),(i2,l2)) -> assert (i1 == i2) $ l1 /= l2)
                   (zip (Data_Map.assocs t1) (Data_Map.assocs t2))
      in case (reg_diff, tag_diff) of
           ([], []) -> []
           ([((i,_),(_,d))],[((j,_),(_,l))]) | i == j -> [(i,d,l)]
           ([((i,_),(_,d))],[]) ->
             catMaybes [(i,d,) <$> Data_Map.lookup i t2]
           ([],[((i,_),(_,l))]) ->
             catMaybes [(i,,l) <$> Data_Map.lookup i r2]
           _ -> -- TODO (Leo!)
                error $ "More than one diff in register file:" ++
                        " registers = " ++ show reg_diff ++
                        " and tags = " ++ show tag_diff
  , d_mem =
      let Mem dm1 _ = f_mem m1
          Mem dm2 _ = f_mem m2
          MemT pm1 = p_mem p1
          MemT pm2 = p_mem p2
          both1 = map (\((i,d),(j,t)) -> assert (i==j) $ (i,(d,t))) $ zip (Data_Map.assocs dm1) (Data_Map.assocs pm1)
          both2 = map (\((i,d),(j,t)) -> assert (i==j) $ (i,(d,t))) $ zip (Data_Map.assocs dm2) (Data_Map.assocs pm2)
          diffs = diff both1 both2 (uninitialized_word, emptyInstTag pplus)
          extract (i,(_,(d,t))) = (i,d,t)
       in map extract diffs 
  }

--          data_diff =
--            filter (\((i1,d1),(i2,d2)) ->
--                      if i1 == i2 then d1 /= d2 else error $ "DIFF: " ++ show ("i1", i1, "d1", d1, "i2", i2, "d2", d2, "dm1", dm1, "dm2", dm2))
----                             assert (i1 == i2) $ d1 /= d2)
--                   (zip (Data_Map.assocs dm1) (Data_Map.assocs dm2))
--          tag_diff =
--            filter (\((i1,l1),(i2,l2)) -> assert (i1 == i2) $ l1 /= l2) (zip (Data_Map.assocs pm1) (Data_Map.assocs pm2))
--      in case (data_diff, tag_diff) of
--           ([], []) -> Nothing
--           ([((i,_),(_,d))],[((j,_),(_,l))]) | i == j -> Just (i,d,l)
--           ([((i,_),(_,d))],[]) ->
--             (i,d,) <$> Data_Map.lookup i pm2
--           ([],[((i,_),(_,l))]) ->
--             (i,,l) <$> Data_Map.lookup i dm2
--           _ -> error $ "More than one diff in memory file:" ++
--                        " data = " ++ show data_diff ++
--                        " and tags = " ++ show tag_diff

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
  let regs = Data_Map.assocs rs
      tags = Data_Map.assocs ts
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
                  return (ADDI rd rs imm, defaultTag))
            , (onNonEmpty dataRegs 3,
               do -- LOAD
                  (rs,content,min_imm,max_imm,tag) <- elements dataRegs
                  rd <- genTargetReg ms
                  imm <- (min_imm +) <$> genImm (max_imm - min_imm)
                  return (LW rd rs imm, defaultTag)
              )
            , (onNonEmpty dataRegs 3 * onNonEmpty arithRegs 1,
               do -- STORE    -- TODO: Don't write to WriteNever locations too often!
                  (rd,content, min_imm,max_imm,tag) <- elements dataRegs
                  rs <- genTargetReg ms
                  imm <- (min_imm+) <$> genImm (max_imm - min_imm)
                  return (SW rd rs imm, defaultTag))
            , (onNonEmpty arithRegs 1,
               do -- ADD
                  rs1 <- elements arithRegs
                  rs2 <- elements arithRegs
                  rd <- genTargetReg ms
                  return (ADD rd rs1 rs2, defaultTag))
            ]

genMTag :: PolicyPlus -> Gen TagSet
genMTag pplus = do
  frequency [ (1, pure defaultTag)
            , (1, pure writeOnceTag) ]

genDataMemory :: PolicyPlus -> Gen (Mem, MemT)
genDataMemory pplus = do
  let idx = [dataMemLow pplus, (dataMemLow pplus)+4..(dataMemHigh pplus)]
  combined <- mapM (\i -> do d <- genImm $ dataMemHigh pplus    -- BCP: This always puts 4 in every location!
                             t <- genMTag pplus
                             return ((i, d),(i,t))) idx
  let (m,pm) = unzip combined
  return (Mem (Data_Map.fromList m) Nothing, MemT $ Data_Map.fromList pm)

setInstrI :: Machine_State -> Instr_I -> Machine_State
setInstrI ms i =
  ms {f_mem = (f_mem ms) { f_dm = Data_Map.insert (f_pc ms) (encode_I RV32 i) (f_dm $ f_mem ms) } }

setInstrTagI :: Machine_State -> PIPE_State -> TagSet -> PIPE_State
setInstrTagI ms ps it =
  ps {p_mem = ( MemT $ Data_Map.insert (f_pc ms) (it) (unMemT $ p_mem ps) ) }

genByExec :: PolicyPlus -> Int -> Machine_State -> PIPE_State -> Set GPR_Addr
             -> Gen (Machine_State, PIPE_State, Set GPR_Addr)
genByExec pplus 0 ms ps instrlocs = return (ms, ps, instrlocs)
genByExec pplus n ms ps instrlocs
  -- Check if an instruction already exists
  | Data_Map.member (f_pc ms) (f_dm $ f_mem ms) =
    case fetch_and_execute pplus ms ps of
      Right (ms'', ps'') ->
        genByExec pplus (n-1) ms'' ps'' instrlocs
      Left err ->
        -- trace ("Warning: Fetch and execute failed with " ++ show n
        --        ++ " steps remaining and error: " ++ show err) $
        return (ms, ps, instrlocs)
  | otherwise = do
    (is, it) <- genInstr pplus ms ps
    let ms' = setInstrI ms is
        ps' = setInstrTagI ms ps it
    case -- traceShow ("Instruction generated...", is) $
         fetch_and_execute pplus ms' ps' of
      Right (ms'', ps'') ->
        -- trace "Successful execution" $
        genByExec pplus (n-1) ms'' ps'' (Data_Set.insert (f_pc ms') instrlocs)
      Left err ->
        -- trace ("Warning: Fetch and execute failed with "
        --       ++ show n ++ " steps remaining and error: " ++ show err) $
        return (ms', ps', instrlocs)

updRegs :: GPR_File -> Gen GPR_File
updRegs (GPR_File rs) = do
  [d1, d2, d3] <- replicateM 3 $ genImm 40
  let rs' :: Data_Map.Map Integer Integer = Data_Map.insert 1 d1 $ Data_Map.insert 2 d2 $ Data_Map.insert 3 d3 rs
  return $ GPR_File rs'

genMachine :: PolicyPlus -> Gen (Machine_State, PIPE_State)
genMachine pplus = do
  -- registers
  (mem,pmem) <- genDataMemory pplus
  let ms = initMachine {f_mem = mem}
      ps = (init_pipe_state pplus){p_mem = pmem}
      ms2 = setInstrI ms (JAL 0 1000)
      ps2 = setInstrTagI ms ps (emptyInstTag pplus)  -- Needed??

  rs' <- updRegs $ f_gprs ms2
  let ms' = ms2 {f_gprs = rs'}
      ps' = ps2
  
  (ms_fin, ps_fin, instrlocs) <- genByExec pplus maxInstrsToGenerate ms' ps' Data_Set.empty

  let final_mem = f_dm $ f_mem ms_fin
      res_mem = foldr (\a mem -> Data_Map.insert a (fromJust $ Data_Map.lookup a final_mem) mem) (f_dm $ f_mem ms') instrlocs
      ms_fin' = 
        ms' {f_mem = (f_mem ms') { f_dm = res_mem } }  

      final_pmem = unMemT $ p_mem ps_fin
      res_pmem = foldr (\a pmem -> Data_Map.insert a (fromJust $ Data_Map.lookup a final_pmem) pmem) (unMemT $ p_mem ps') instrlocs
      ps_fin' =
        ps' {p_mem = MemT res_pmem}

  return (ms_fin', ps_fin')

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
      combined = zip (Data_Map.assocs d) (Data_Map.assocs t)
  in 
  [ (GPR_File $ Data_Map.fromList d', GPR_FileT $ Data_Map.fromList t') 
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
  let m' = Data_Map.assocs m
      t' = Data_Map.assocs t

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
      indexTagedIntsToMem i itis = ((flip Mem i) . Data_Map.fromList) *** (MemT . Data_Map.fromList) $ unzip itis
        
  in map (indexTagedIntsToMem i) $ shrinkMemAux (zip m' t') 
        
shrinkMState :: PolicyPlus -> (Machine_State, PIPE_State) -> [(Machine_State, PIPE_State)]
shrinkMState pplus (m,p) =
     [ (m{f_mem = mem},p{p_mem = pmem})
     | (mem, pmem) <- shrinkMem pplus (f_mem m, p_mem p) ]
  ++ [ (m{f_gprs = gpr},p{p_gprs = pgpr})
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

defaultTag = fromExt [("writeonce.Default", Nothing)]
writeOnceTag = fromExt [("writeonce.WriteOnce", Nothing)]
-- writeNeverTag = fromExt [("writeonce.WriteNever", Nothing)]


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

cellContents trace addr =
  map (\(m,_) -> (f_dm $ f_mem m) Data_Map.! addr) trace

prop_WO pplus count maxcount (m,p) =
  let trace = eval pplus count maxcount [] (m,p) 
      addrs = map fst $ filter (\(_,l) -> l == writeOnceTag) $
                         Data_Map.assocs $ unMemT $ p_mem p 
      contents = map (cellContents trace) addrs in
  whenFail (do putStrLn $ "Memory location tagged WriteOnce written twice!"
               let finalTrace = trace
               printTrace pplus finalTrace) $ 
           all (\l -> length (Data_List.group l) <= 2) contents

maxInstrsToGenerate :: Int
maxInstrsToGenerate = 10

newtype MP = MP (Machine_State, PIPE_State)

instance Show MP where
  show _ = ""

prop :: PolicyPlus -> MP -> Property
prop pplus (MP (m,p)) = prop_WO pplus 0 maxInstrsToGenerate (m,p)

------------------------------------------------------------------------------------------
-- The heap-safety policy
  
load_policy = do
  ppol <- load_pipe_policy "writeonce.main"
  let pplus = PolicyPlus
        { policy = ppol
        , initGPR = defaultTag
        , initMem = defaultTag
        , initPC = defaultTag
        , initNextColor = 5
        , emptyInstTag = defaultTag
        , dataMemLow = 4
        , dataMemHigh = 20  
        , instrLow = 1000
        }
  return pplus

-- The real one
main_test = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1000}
    $ forAllShrink (MP <$> genMachine pplus)
                   (\(MP m) -> map MP $ shrinkMState pplus m ++ 
                                        concatMap (shrinkMState pplus) (shrinkMState pplus m)
                   )
    $ \m -> prop pplus m

main = main_test

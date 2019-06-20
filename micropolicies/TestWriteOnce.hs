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

verboseTracing = False
--verboseTracing = True

{-
printTrace pplus tr1 tr2 = putStrLn $ P.render $ prettyTrace pplus tr1 tr2

prettyTrace :: PolicyPlus -> [(Machine_State, PIPE_State)] -> [(Machine_State, PIPE_State)] -> Doc
prettyTrace pplus [] [] = P.empty
prettyTrace pplus [(m1,p1)] [(m2,p2)] = prettyMStatePair pplus (M (m1,p1) (m2,p2))
prettyTrace pplus (tr1@((m1,p1):_)) (tr2@((m2,p2):_)) =
    prettyMStatePair pplus (M (m1,p1) (m2,p2)) $$ P.text ""
      $$ P.text "Trace:" $$ prettyDiffs pplus tr1 tr2

prettyDiffs :: PolicyPlus -> [(Machine_State, PIPE_State)] -> [(Machine_State, PIPE_State)] -> Doc
prettyDiffs pplus ((m11,p11):(m12,p12):tr1) ((m21,p21):(m22,p22):tr2) =
  (if verboseTracing then
       P.text "----------------------------------------------------------------"
    $$ P.nest 10 (P.text "Raw Machine 1 memory:" $$ P.nest 3 (P.text (show $ f_dm $ f_mem m12)))
    $$ P.nest 10 (P.text "Raw Machine 1 tags:" $$ P.nest 3 (P.text (show $ p_mem p12)))
    $$ P.nest 10 (P.text "Raw Machine 2 memory:" $$ P.nest 3 (P.text (show $ f_dm $ f_mem m22)))
    $$ P.nest 10 (P.text "Raw Machine 2 tags:" $$ P.nest 3 (P.text (show $ p_mem p22)))
    $$ P.nest 10 (P.text "Machine 1:" $$ P.nest 3 (pretty pplus m12 p12) $$
                  P.text "Machine 2" $$ P.nest 3 (pretty pplus m22 p22) )
  else
    P.empty)
  $$ pretty pplus (calcDiff pplus (m11,p11) (m12,p12))
                  (calcDiff pplus (m21,p21) (m22,p22))
  $$ prettyDiffs pplus ((m12,p12):tr1) ((m22,p22):tr2)
prettyDiffs pplus [(m1,p1)] [(m2,p2)] =
  P.text "" $$ P.text "Final:" $$ prettyMStatePair pplus (M (m1,p1) (m2,p2))
prettyDiffs _ _ _ = P.empty

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

prettyRegDiff pplus ((i,d,l):r1) ((i', d', l'):r2)
    | i == i', d == d', l == l' =
        (P.char 'r' P.<> P.integer i <+> P.text "<-" <+> pretty pplus d l)
        $$ prettyRegDiff pplus r1 r2
    | otherwise =
      (ppStrong (P.char 'r' P.<> P.integer i <+> P.text "<-" <+> pretty pplus d l <||>
                 P.char 'r' P.<> P.integer i' <+> P.text "<-" <+> pretty pplus d' l'))
      $$ prettyRegDiff pplus r1 r2
prettyRegDiff _ [] [] = P.empty
-- TODO (Leo): This can happen a lot now...
prettyRegDiff _ r1 r2 = P.text $ "<prettyRegDiff??> " ++ show (r1,r2)

prettyMemDiff pplus ((i,d,l):m1) ((i', d', l'):m2)
    | i == i', d == d', l == l' =
        (P.char '[' P.<> P.integer i P.<> P.char ']' <+> P.text "<-" <+> pretty pplus d l)
        $$ prettyMemDiff pplus m1 m2
    | otherwise =
      (ppStrong (P.char '[' P.<> P.integer i P.<> P.char ']' <+> P.text "<-" <+> pretty pplus d l
                 <||> P.char '[' P.<> P.integer i' P.<> P.char ']' <+> P.text "<-" <+> pretty pplus d' l'))
      $$ prettyMemDiff pplus m1 m2
prettyMemDiff _ [] [] = P.empty
prettyMemDiff _ _ _ = P.text "<prettyMemDiff??>"

instance CoupledPP (Maybe Instr_I) (Maybe Instr_I) where
  pretty pplus (Just i1) (Just i2)
    | i1 == i2  = pp pplus i1
    | otherwise = ppStrong (pp pplus i1 <||> pp pplus i2)
  pretty _ Nothing Nothing = P.text "<Bad instr>"

instance CoupledPP Diff Diff where
  pretty pplus d1 d2 =
    P.hcat [ pad 17 (pretty pplus (d_pc d1) (d_pc d2))
           , P.text " "
           , pad 17 (pretty pplus (d_instr d1) (d_instr d2))
           , P.text "     "
           , prettyRegDiff pplus (d_reg d1) (d_reg d2)
           , prettyMemDiff pplus (d_mem d1) (d_mem d2)
           ]

-- Null "show" functions, for things that we don't want QuickCheck trying to print
instance Show Machine_State where
  show _ = ""
instance Show MStatePair where
  show _ = ""

------------------------------------------------------------------------------------
-- Reachability

{- A stupid n^2 reachability algorithm for now.  If we find it is too
   slow as memories get larger, we could improve it like this:
      - As we go along, maintain a set of "reachable colors" plus a
        map from "unreachable colors" to the addresses tagged with
        each of them.  If an unreachable color ever becomes reachable,
        then add it to the reachable set and recursively traverse the
        things on its list.
-}

cellColorOf :: TagSet -> Maybe Color
cellColorOf t = 
  -- trace ("cellColorOf " ++ show t ++ " i.e. " ++ show (toExt t)) $
  join $ Data_List.lookup "heap.Cell" (toExt t)

-- pointerColorOf :: TagSet -> P (Maybe Color)
-- pointerColorOf t = do
--   ppol <- askPolicy
--   let l = rdTagSet ppol t
--   -- Ughly:
--   case (Data_List.lookup ["test","CP"] l, Data_List.lookup ["test","Pointer"] l) of
--     (Just [_,p], _) -> return p
--     (_, Just [p]) -> return p
--     _ -> return Nothing

pointerColorOf :: TagSet -> Maybe Color
pointerColorOf t = 
  join $ Data_List.lookup "heap.Pointer" (toExt t)

envColorOf :: TagSet -> Maybe Color
envColorOf t = do
  join $ Data_List.lookup "heap.Env" (toExt t)

reachableInOneStep :: MemT -> Set Color -> P (Set Color)
reachableInOneStep m s =
  foldM (\s t -> do  --  do notation not actually needed here
           let c = cellColorOf t
           let p = pointerColorOf t
           case (c,p) of
             (Just c', Just p') | Data_Set.member c' s -> return $ Data_Set.insert p' s
             _ -> return s)
   s (Data_Map.elems $ unMemT m)

reachableLoop :: MemT -> Set Color -> P (Set Color)
reachableLoop m s = do
  s' <- reachableInOneStep m s 
  if s == s' then return s else reachableLoop m s'

registerColors :: PIPE_State -> P (Set Color)
registerColors pstate = 
  foldM (\s t -> do -- Do notation not actually needed
            let c = pointerColorOf t
            case c of
              Just c' -> return $ Data_Set.insert c' s 
              Nothing -> return s)
    Data_Set.empty (unGPR $ p_gprs pstate) 

reachable :: PIPE_State -> P (Set Color)
reachable p = registerColors p >>= reachableLoop (p_mem p) 

sameReachablePart :: MStatePair -> P Bool
sameReachablePart (M (s1, p1) (s2, p2)) = do
  r1 <- reachable p1
  r2 <- reachable p2

  let filterAux [] _ = return []
      filterAux _ [] = return []
      filterAux ((i,d):ds) ((j,t):ts)
        | i == j = do
            case cellColorOf t of
              Just c' | Data_Set.member c' r1 -> (d :) <$> filterAux ds ts
              _ -> filterAux ds ts
        | i < j = filterAux ds ((j,t):ts)
        | i > j = filterAux ((i,d):ds) ts

  f1 <- filterAux (Data_Map.assocs $ f_dm $ f_mem s1) (Data_Map.assocs $ unMemT $ p_mem p1)
  f2 <- filterAux (Data_Map.assocs $ f_dm $ f_mem s2) (Data_Map.assocs $ unMemT $ p_mem p2)

  return $ r1 == r2 && (f_gprs s1 == f_gprs s2) && (f1 == f2)
-}

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

{-
------------------------------------------------------------------------------------------
-- Shrinking

-- Tag shrinking basically amounts to shrinking the colors
-- of things to C 0. Assuming that C 0 is always reachable.
-- We can't change the Tag type. We can't change the Color
-- arbitrarily.
shrinkColor :: Color -> [Color]
shrinkColor (0) = []
shrinkColor (1) = [0]
shrinkColor (n) = [0,n-1]

shrinkTag :: TagSet -> [TagSet]
shrinkTag t =
  case toExt t of
    [("heap.Alloc", Nothing), ("heap.Instr", Nothing)] ->
      [fromExt [("heap.Instr", Nothing)]]
    [("heap.Pointer", Just cp)] ->
      [fromExt [("heap.Pointer", Just cp')] | cp' <- shrinkColor cp]
    [("heap.Cell", Just cc), ("heap.Pointer", Just cp)] ->
         [fromExt [("heap.Cell", Just cc'), ("heap.Pointer", Just cp )] | cc' <- shrinkColor cc]
      ++ [fromExt [("heap.Cell", Just cc),  ("heap.Pointer", Just cp')] | cp' <- shrinkColor cp]
    _ -> []


-- INV: If we're shrinking registers, everything should already be equal.
shrinkRegister :: PolicyPlus -> (Integer, TagSet) -> [(Integer, TagSet)]
shrinkRegister pplus (d,t) = [(d',t') | d' <- shrink d, t' <- shrinkTag t]

shrinkVector :: (a -> [a]) -> [a] -> [[a]]
shrinkVector f []    = []
shrinkVector f (h:t) = map (:t) (f h) ++ map (h:) (shrinkVector f t)

-- INV: The register files are also identical
shrinkGPRs :: PolicyPlus -> (GPR_File, GPR_FileT) -> (GPR_File, GPR_FileT)
                -> [((GPR_File, GPR_FileT),(GPR_File, GPR_FileT))]
shrinkGPRs pplus (GPR_File d1, GPR_FileT t1) (GPR_File d2, GPR_FileT t2) =
  -- assert (d1==d2 && t1 == t2) $
  let combined :: [((GPR_Addr, GPR_Val), (InstrField, TagSet), (GPR_Addr, GPR_Val), (InstrField, TagSet))]
      combined = zip4 (Data_Map.assocs d1) (Data_Map.assocs t1) (Data_Map.assocs d2) (Data_Map.assocs t2) in
  [ ((GPR_File $ Data_Map.fromList d1', GPR_FileT $ Data_Map.fromList t1'), 
     (GPR_File $ Data_Map.fromList d2', GPR_FileT $ Data_Map.fromList t2'))
  | (d1',t1',d2',t2') <- map unzip4 $ shrinkVector shrinkR combined
  ]
  where shrinkR :: ((GPR_Addr, GPR_Val), (InstrField, TagSet), (GPR_Addr, GPR_Val), (InstrField, TagSet))
               -> [((GPR_Addr, GPR_Val), (InstrField, TagSet), (GPR_Addr, GPR_Val), (InstrField, TagSet))]
        shrinkR ((i1,v1),(j1,l1),(i2,v2),(j2,l2)) =
             [ ((i1,v'),(j1,l1),(i2,v'),(j2,l2)) | v' <- shrink v1 ]
          ++ [ ((i1,v1),(j1,l'),(i2,v2),(j2,l')) | l' <- shrinkTag l1 ]

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
shrinkMems :: PolicyPlus -> Set Color -> (Mem, MemT) -> (Mem, MemT) -> [((Mem, MemT), (Mem,MemT))]
shrinkMems pplus reachable (Mem m1 i1, MemT t1) (Mem m2 i2, MemT t2) = 
  let m1' = Data_Map.assocs m1
      t1' = Data_Map.assocs t1
      m2' = Data_Map.assocs m2
      t2' = Data_Map.assocs t2

      isData  i = i >= dataMemLow pplus && i <= dataMemHigh pplus
      isInstr i = i == 0 || i >= instrLow pplus
 
      shrinkMemLoc :: (Integer, Integer, TagSet) -> (Integer, Integer, TagSet) ->
                      [ (IndexedTaggedInt, IndexedTaggedInt) ]
      shrinkMemLoc (j,d1,l1) (_,d2,l2)
        | isInstr j =
          case (decode_I RV32 d1, decode_I RV32 d2) of
            -- Both (identical) instructions
            (Just i1, Just i2)
              | i1 == i2 && l1 == l2 ->
                -- Shrink instruction
                [ (((j, d'), (j, l1)), ((j, d'),(j, l1))) | d' <- encode_I RV32 <$> shrinkInstr i1] ++
                -- Or shrink tag (alloc)
                [ (((j, d1), (j, l')), ((j, d1),(j, l'))) | l' <- shrinkTag l1 ]
              | otherwise -> error $ "Distinguishable memory locations: " ++ show (j,d1,l1,d2,l2)
            _ -> error "Instructions can't be decoded while shrinking"
        | otherwise =
            case (cellColorOf l1, pointerColorOf l1, cellColorOf l2, pointerColorOf l2) of 
--            case (l1, l2) of
              (Just loc1, Just _, Just loc2, Just _) 
--              (MTagM v1 loc1, MTagM v2 loc2)
                -- Both reachable, everything should be identical
                | Data_Set.member loc1 reachable && Data_Set.member loc2 reachable && l1 == l2 && d1 == d2->
                  -- shrink the first and copy
                  -- Shrink data
                  [ (((j, d'), (j, l1)), ((j, d'),(j, l1))) | d' <- shrink d1 ]
                  ++ 
                  -- Or shrink tag 
                  [ (((j, d1), (j, l')), ((j, d2),(j, l'))) | l' <- shrinkTag l1 ]
                -- Both unreachable, shrink independently
                | not (Data_Set.member loc1 reachable) && not (Data_Set.member loc2 reachable) ->
                  -- Shrink first data value 
                  [ (((j, d1'), (j, l1)), ((j, d2),(j, l2))) | d1' <- shrink d1 ]
                  ++
                  -- Shrink first tag to something unreachable 
                  [ (((j, d1), (j, l1')), ((j, d2),(j, l2)))
                  | l1' <- shrinkTag l1,
                    not $ Data_Set.member (fromJust $ cellColorOf l1') reachable ]
                  ++
                  -- Shrink first tag to something reachable (and make sure first and second components are the same!)
                  [ (((j, d1), (j, l1')), ((j, d1),(j, l1')))
                  | l1' <- shrinkTag l1,
                    Data_Set.member (fromJust $ cellColorOf l1') reachable ]
                  ++
                  -- ... same for second register state
                  [ (((j, d1), (j, l1)), ((j, d2'),(j, l2))) | d2' <- shrink d2 ]
                  ++
                  [ (((j, d1), (j, l1)), ((j, d2),(j, l2')))
                  | l2' <- shrinkTag l2,
                    not $ Data_Set.member (fromJust $ cellColorOf l2') reachable ]
                  ++
                  [ (((j, d1), (j, l2')), ((j, d1),(j, l2')))
                  | l2' <- shrinkTag l2,
                    Data_Set.member (fromJust $ cellColorOf l2') reachable ]
                | otherwise -> error $ "Not both reachable or unreachable?" ++ show (d1,l1,d2,l2)
              otherwise -> error "Data memory without cell or pointer color?"
 
      shrinkMemAux :: [ IndexedTaggedInt ] -> [ IndexedTaggedInt] -> [ ([IndexedTaggedInt], [IndexedTaggedInt]) ]
      shrinkMemAux [] [] = []
      shrinkMemAux (((j1,d1),(_,l1)):more1) (((j2,d2),(_,l2)):more2) =
        -- Shrink Current memory location and rebuild mem
        [ ((loc1':more1), (loc2':more2)) | (loc1', loc2') <- shrinkMemLoc (j1,d1,l1) (j2,d2,l2) ]
        ++
        -- Keep current memory location and shrink something later on
        [ ( ((j1,d1),(j1,l1)) : more1', ((j2,d2),(j2,l2)) : more2' )
        | (more1', more2') <- shrinkMemAux more1 more2 ]
 
      indexTagedIntsToMem :: Maybe (Integer,Integer) -> [IndexedTaggedInt] -> (Mem, MemT)
      indexTagedIntsToMem i itis = ((flip Mem i) . Data_Map.fromList) *** (MemT . Data_Map.fromList) $ unzip itis
        
  in map (indexTagedIntsToMem i1 *** indexTagedIntsToMem i2) $ shrinkMemAux (zip m1' t1') (zip m2' t2')
        
shrinkMStatePair :: PolicyPlus -> MStatePair -> [MStatePair]
shrinkMStatePair pplus (M (m1,p1) (m2,p2)) =
  let r = runReader (reachable p1) pplus
      -- Shrink Memories
  in
     [ M (m1{f_mem = mem1},p1{p_mem = pmem1}) (m2{f_mem=mem2}, p2{p_mem=pmem2})
     | ((mem1, pmem1), (mem2, pmem2)) <- shrinkMems pplus r (f_mem m1, p_mem p1) (f_mem m2, p_mem p2) ]
  ++ [ M (m1{f_gprs = gpr1},p1{p_gprs = pgpr1}) (m2{f_gprs=gpr2}, p2{p_gprs=pgpr2})
     | ((gpr1, pgpr1), (gpr2, pgpr2)) <- shrinkGPRs pplus (f_gprs m1, p_gprs p1) (f_gprs m2, p_gprs p2) ]


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
-}

defaultTag = fromExt [("writeonce.Default", Nothing)]
writeOnceTag = fromExt [("writeonce.WriteOnce", Nothing)]
-- writeNeverTag = fromExt [("writeonce.WriteNever", Nothing)]

shrinkMState = undefined

step pplus (m,p) = 
  let run_state = mstate_run_state_read m
      m' = mstate_io_tick m in
  if run_state /= Run_State_Running then 
    Left (show run_state) 
  else
    fetch_and_execute pplus m' p 

eval pplus count maxcount trace (m,p) =
  if count >= maxcount then 
    trace
  else   
    case step pplus (m,p) of
      Right (m',p') -> eval pplus (count+1) maxcount ((m,p) : trace) (m',p')
      Left _ -> trace

cellContents trace addr =
  map (\(m,_) -> (f_dm $ f_mem m) Data_Map.! addr) trace

prop_WO pplus count maxcount (m,p) =
  let trace = eval pplus count maxcount [] (m,p) 
      addrs = map fst $ filter (\(_,l) -> l == writeOnceTag) $
                         Data_Map.assocs $ unMemT $ p_mem p 
      contents = map (cellContents trace) addrs in
  property $ all (\l -> length (Data_List.group l) <= 2) contents

maxInstrsToGenerate :: Int
maxInstrsToGenerate = 10

prop :: PolicyPlus -> (Machine_State, PIPE_State) -> Property
prop pplus (m,p) = prop_WO pplus 0 maxInstrsToGenerate (m,p)

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

instance Show Machine_State where
  show _ = ""

-- The real one
main_test = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1000}
    $ forAllShrink (genMachine pplus)
           (\m -> [] -- shrinkMState pplus m ++ 
                  -- concatMap (shrinkMState pplus) (shrinkMState pplus m)
           )
    $ \m -> prop pplus m

main = main_test

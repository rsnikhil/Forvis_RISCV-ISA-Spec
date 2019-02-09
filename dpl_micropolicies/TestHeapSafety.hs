{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses #-}
module TestHeapSafety where

import qualified Data.Map.Strict as Data_Map
import Data.Maybe
import qualified Data.Set as Data_Set
import Data.Set (Set)
import qualified Data.List as Data_List

import Bit_Utils
import Arch_Defs

-- Maybe?
import Machine_State
import Forvis_Spec_I
import Forvis_Spec_Instr_Fetch
import GPR_File
import Memory

-- This might belong elsewhere
import Test.QuickCheck

import Control.Monad.Reader

--------------------------------------------------------
-- This belongs in /src!

import Control.Exception.Base (assert)

import Data.Bits
import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P

import Printing
import PIPE
import Run_Program_PIPE

------------------------------------------------------------------------
-- Testing

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

----------------------------------------
-- Reachability. Where should this live?

{- A stupid n^2 reachability algorithm for now.  If we find it is too
   slow as memories get larger, we could improve it like this:
      - As we go along, maintain a set of "reachable colors" plus a
        map from "unreachable colors" to the addresses tagged with
        each of them.  If an unreachable color ever becomes reachable,
        then add it to the reachable set and recursively traverse the
        things on its list.
-}

cellColorOf :: TagSet -> P (Maybe Color)
cellColorOf t = do
  ppol <- ask
  let l = rdTagSet ppol t
  -- Ughly:
  case (Data_List.lookup ["test","CP"] l, Data_List.lookup ["test","Cell"] l) of
    (Just [c,_], _) -> return c
    (_, Just [c]) -> return c
    _ -> return Nothing

pointerColorOf :: TagSet -> P (Maybe Color)
pointerColorOf t = do
  ppol <- ask
  let l = rdTagSet ppol t
  -- Ughly:
  case (Data_List.lookup ["test","CP"] l, Data_List.lookup ["test","Pointer"] l) of
    (Just [_,p], _) -> return p
    (_, Just [p]) -> return p
    _ -> return Nothing

envColorOf :: TagSet -> P (Maybe Color)
envColorOf t = do
  ppol <- ask
  let l = rdTagSet ppol t
  -- Ughly:
  case (Data_List.lookup ["test","Env"] l) of
    (Just [p]) -> return p
    _ -> return Nothing


reachableInOneStep :: MemT -> Set Color -> P (Set Color)
reachableInOneStep m s =
  foldM (\s t -> do
           c <- cellColorOf t
           p <- pointerColorOf t
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
  foldM (\s t -> do
            c <- pointerColorOf t
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
            c <- cellColorOf t
            case c of
              Just c' | Data_Set.member c' r1 -> (d :) <$> filterAux ds ts
              _ -> filterAux ds ts
        | i < j = filterAux ds ((j,t):ts)
        | i > j = filterAux ((i,d):ds) ts

  f1 <- filterAux (Data_Map.assocs $ f_dm $ f_mem s1) (Data_Map.assocs $ unMemT $ p_mem p1)
  f2 <- filterAux (Data_Map.assocs $ f_dm $ f_mem s2) (Data_Map.assocs $ unMemT $ p_mem p2)

  return $ r1 == r2 && (f_gprs s1 == f_gprs s2) && (f1 == f2)

--- If you want reachability information, this needs to be before the prop_noninterference.
-- Shorthand for (indistinguishable) pairs of m- and p-states 
data MStatePair =
  M (Machine_State, PIPE_State) (Machine_State, PIPE_State)

emptyInstTag :: PIPE_Policy -> TagSet
emptyInstTag ppol = 
  mkTagSet ppol ["test","Inst"] [Nothing]

allocInstTag :: PIPE_Policy -> TagSet
allocInstTag ppol =
  mkTagSet ppol ["test","AllocInst"] [Nothing, Nothing]

prettyMStatePair :: PIPE_Policy -> MStatePair -> Doc
prettyMStatePair ppol (M (m1, p1) (m2, p2)) =
    P.vcat [ P.text "Reachable Colors:" <+> pretty ppol (runReader (reachable p1) ppol) (runReader (reachable p2) ppol)
           , P.text "PC:" <+> pretty ppol (f_pc m1, p_pc p1) (f_pc m2, p_pc p2)
           , P.text "Registers:" $$ P.nest 2 (pretty ppol (f_gprs m1, p_gprs p1) (f_gprs m2, p_gprs p2))
           , P.text "Memories:" $$ P.nest 2 (pretty ppol (f_mem m1, p_mem p1) (f_mem m2, p_mem p2))
           ]

print_mstatepair :: PIPE_Policy -> MStatePair -> IO ()
print_mstatepair ppol m = putStrLn $ P.render $ prettyMStatePair ppol m

prop_noninterference :: PIPE_Policy -> MStatePair -> Property
prop_noninterference ppol (M (m1,p1) (m2,p2)) =
  let (r1,ss1') = run_loop ppol 100 p1 m1
      (r2,ss2') = run_loop ppol 100 p2 m2
      ((p1',m1'),(p2', m2')) = head $ reverse $ zip (reverse ss1') (reverse ss2') in
  whenFail (do putStrLn $ "Reachable parts differ after execution!"
               putStrLn $ "Original machines:"
               print_mstatepair ppol (M (m1,p1) (m2,p2))
               putStrLn $ "After execution..."
               print_mstatepair ppol (M (m1', p1') (m2', p2'))
--               putStrLn "First One:"
--               print_coupled m1' p1'
--               putStrLn "Second One:"
--               print_coupled m2' p2'
           )
           (collect (case fst $ instr_fetch m1' of Fetch u32 -> decode_I RV32 u32)
             (runReader (sameReachablePart (M (m1', p1') (m2', p2'))) ppol))

verboseTracing = False
--verboseTracing = True

printTrace ppol tr1 tr2 = putStrLn $ P.render $ prettyTrace ppol tr1 tr2

prettyTrace :: PIPE_Policy -> [(PIPE_State, Machine_State)] -> [(PIPE_State, Machine_State)] -> Doc
prettyTrace ppol [] [] = P.text ""
prettyTrace ppol [(p1,m1)] [(p2,m2)] = prettyMStatePair ppol (M (m1,p1) (m2,p2))
prettyTrace ppol (tr1@((p1,m1):_)) (tr2@((p2,m2):_)) =
    prettyMStatePair ppol (M (m1,p1) (m2,p2)) $$ P.text "------------------------------" $$ prettyDiffs ppol tr1 tr2

prettyDiffs :: PIPE_Policy -> [(PIPE_State, Machine_State)] -> [(PIPE_State, Machine_State)] -> Doc
prettyDiffs ppol ((p11,m11):(p12,m12):tr1) ((p21,m21):(p22,m22):tr2) =
  (if verboseTracing then
       P.text "----------------------------------------------------------------"
    $$ P.nest 10 (P.text "Raw Machine 1 memory:" $$ P.nest 3 (P.text (show $ f_dm $ f_mem m12)))
    $$ P.nest 10 (P.text "Raw Machine 1 tags:" $$ P.nest 3 (P.text (show $ p_mem p12)))
    $$ P.nest 10 (P.text "Raw Machine 2 memory:" $$ P.nest 3 (P.text (show $ f_dm $ f_mem m22)))
    $$ P.nest 10 (P.text "Raw Machine 2 tags:" $$ P.nest 3 (P.text (show $ p_mem p22)))
    $$ P.nest 10 (P.text "Machine 1:" $$ P.nest 3 (pretty ppol m12 p12) $$ P.text "Machine 2" $$ P.nest 3 (pretty ppol m22 p22) )
  else
    P.empty)
  $$ pretty ppol (calcDiff ppol (p11,m11) (p12,m12))
                 (calcDiff ppol (p21,m21) (p22,m22))
  $$ prettyDiffs ppol ((p12,m12):tr1) ((p22,m22):tr2)
prettyDiffs ppol [(p1,m1)] [(p2,m2)] =
  P.text "------------------------------" $$
  P.text "FINAL MACHINE STATES:" $$ prettyMStatePair ppol (M (m1,p1) (m2,p2))
prettyDiffs _ _ _ = P.text ""

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

calcDiff :: PIPE_Policy -> (PIPE_State, Machine_State) -> (PIPE_State, Machine_State) -> Diff
calcDiff ppol (p1,m1) (p2,m2) =
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
           _ -> -- TODO!
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
          diffs = diff both1 both2 (uninitialized_word, emptyInstTag ppol)
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

prettyRegDiff ppol ((i,d,l):r1) ((i', d', l'):r2)
    | i == i', d == d', l == l' =
        (P.char 'r' P.<> P.integer i <+> P.text "<-" <+> pretty ppol d l)
        $$ prettyRegDiff ppol r1 r2
    | otherwise =
      (P.char 'r' P.<> P.integer i <+> P.text "<-" <+> pretty ppol d l <||>
      P.char 'r' P.<> P.integer i' <+> P.text "<-" <+> pretty ppol d' l')
      $$ prettyRegDiff ppol r1 r2
prettyRegDiff _ [] [] = P.text ""

prettyMemDiff ppol ((i,d,l):m1) ((i', d', l'):m2)
    | i == i', d == d', l == l' =
        (P.char '[' P.<> P.integer i P.<> P.char ']' <+> P.text "<-" <+> pretty ppol d l)
        $$ prettyMemDiff ppol m1 m2
    | otherwise =
      (P.char '[' P.<> P.integer i P.<> P.char ']' <+> P.text "<-" <+> pretty ppol d l
      <||> P.char '[' P.<> P.integer i' P.<> P.char ']' <+> P.text "<-" <+> pretty ppol d' l')
      $$ prettyMemDiff ppol m1 m2
prettyMemDiff _ [] [] = P.text ""

instance CoupledPP (Maybe Instr_I) (Maybe Instr_I) where
  pretty ppol (Just i1) (Just i2)
    | i1 == i2  = pp ppol i1
    | otherwise = pp ppol i1 <||> pp ppol i2
  pretty _ Nothing Nothing = P.text "<Bad instr>"

instance CoupledPP Diff Diff where
  pretty ppol d1 d2 =
    P.hcat [ pad 15 (pretty ppol (d_pc d1) (d_pc d2))
           , P.text " "
           , pad 17 (pretty ppol (d_instr d1) (d_instr d2))
           , P.text "     "
           , prettyRegDiff ppol (d_reg d1) (d_reg d2)
           , prettyMemDiff ppol (d_mem d1) (d_mem d2)
           ]

-- TODO: The fact that we need this is a sad indication of how confused
-- everything is about whether pipe or machine states go first...
flipboth :: ((a,b),(a,b)) -> ((b,a),(b,a))
flipboth ((a1,b1),(a2,b2)) = ((b1,a1),(b2,a2))

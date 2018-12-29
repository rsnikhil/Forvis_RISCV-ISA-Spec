{-# LANGUAGE FlexibleInstances, TupleSections, MultiParamTypeClasses #-}
module TestHeapSafety where

import qualified Data.Map.Strict as Data_Map
import Data.Maybe
import qualified Data.Set as Data_Set
import Data.Set (Set)

import Bit_Utils
import Arch_Defs

-- Maybe?
import Machine_State
import Forvis_Spec
import Forvis_Spec_I
import GPR_File
import Memory

import Control.Exception.Base (assert)
import Debug.Trace

-- This might belong elsewhere
import Test.QuickCheck

--------------------------------------------------------
-- This belongs in /src!

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

reachableInOneStep :: MemT -> Set Color -> Set Color
reachableInOneStep m s =
  foldr (\t s -> 
           case t of 
             MTagM v l -> if Data_Set.member l s then Data_Set.insert v s else s
             _ -> s)
   s (Data_Map.elems $ unMemT m)

reachableLoop :: MemT -> Set Color -> Set Color
reachableLoop m s = 
  let s' = reachableInOneStep m s in
  if s == s' then s else reachableLoop m s'

registerColors :: PIPE_State -> Set Color 
registerColors pstate = 
  foldr (\t s -> case t of 
                   MTagR c -> Data_Set.insert c s 
                   _ -> error "Register tag should be an MTagR")
    Data_Set.empty (unGPR $ p_gprs pstate) 

reachable :: PIPE_State -> Set Color
reachable p = reachableLoop (p_mem p) (registerColors p)

sameReachablePart :: MStatePair -> Bool
sameReachablePart (M (s1, p1) (s2, p2)) =
  let r1 = reachable p1
      r2 = reachable p2

      filterAux [] _ = []
      filterAux _ [] = []
      filterAux ((i,d):ds) ((j,t):ts)
        | i == j =
            case t of
              MTagM v l | Data_Set.member l r1 -> d : filterAux ds ts
              _ -> filterAux ds ts
        | i < j = filterAux ds ((j,t):ts)
        | i > j = filterAux ((i,d):ds) ts

  in
    r1 == r2
    && (f_gprs s1 == f_gprs s2)
    && (filterAux (Data_Map.assocs $ f_dm $ f_mem s1) (Data_Map.assocs $ unMemT $ p_mem p1) ==
        filterAux (Data_Map.assocs $ f_dm $ f_mem s2) (Data_Map.assocs $ unMemT $ p_mem p2))

--- If you want reachability information, this needs to be before the prop_noninterference.
-- Shorthand for (indistinguishable) pairs of m- and p-states 
data MStatePair =
  M (Machine_State, PIPE_State) (Machine_State, PIPE_State)

-- BCP: This is almost a duplicate of [??]

instance PP MStatePair where
  pp (M (m1, p1) (m2, p2)) =
    P.vcat [ P.text "Reachable Colors:" <+> pretty (reachable p1) (reachable p2)
           , P.text "PC:" <+> pretty (f_pc m1, p_pc p1) (f_pc m2, p_pc p2)
           , P.text "Registers:" $$ P.nest 2 (pretty (f_gprs m1, p_gprs p1) (f_gprs m2, p_gprs p2))
           , P.text "Memories:" $$ P.nest 2 (pretty (f_mem m1, p_mem p1) (f_mem m2, p_mem p2))
           ]

verboseTracing = False

print_mstatepair :: MStatePair -> IO ()
print_mstatepair m = putStrLn $ P.render $ pp m

printTrace tr1 tr2 = putStrLn $ P.render $ prettyTrace tr1 tr2

prettyTrace :: [(PIPE_State, Machine_State)] -> [(PIPE_State, Machine_State)] -> Doc
prettyTrace [] [] = P.text ""
prettyTrace [(p1,m1)] [(p2,m2)] = pp (M (m1,p1) (m2,p2))
prettyTrace (tr1@((p1,m1):_)) (tr2@((p2,m2):_)) =
    pp (M (m1,p1) (m2,p2)) $$ P.text "------------------------------" $$ prettyDiffs tr1 tr2

prettyDiffs :: [(PIPE_State, Machine_State)] -> [(PIPE_State, Machine_State)] -> Doc
prettyDiffs ((p11,m11):(p12,m12):tr1) ((p21,m21):(p22,m22):tr2) =
  if verboseTracing then
     P.text "--------------------------------------------------------------------------"
  $$ P.nest 10 (P.text "Raw Machine 1 memory:" $$ P.nest 3 (P.text (show $ f_dm $ f_mem m12)))
  $$ P.nest 10 (P.text "Raw Machine 1 tags:" $$ P.nest 3 (P.text (show $ p_mem p12)))
  $$ P.nest 10 (P.text "Raw Machine 2 memory:" $$ P.nest 3 (P.text (show $ f_dm $ f_mem m22)))
  $$ P.nest 10 (P.text "Raw Machine 2 tags:" $$ P.nest 3 (P.text (show $ p_mem p22)))
  $$ P.nest 10 (P.text "Machine 1:" $$ P.nest 3 (pretty m12 p12) $$ P.text "Machine 2" $$ P.nest 3 (pretty m22 p22) )
  else P.empty
  $$ pretty (calcDiff (p11,m11) (p12,m12))
            (calcDiff (p21,m21) (p22,m22))
  $$ prettyDiffs ((p12,m12):tr1) ((p22,m22):tr2)
prettyDiffs [(p1,m1)] [(p2,m2)] =
  P.text "------------------------------" $$
  P.text "Final machine states:" $$ pp (M (m1,p1) (m2,p2))
prettyDiffs _ _ = P.text ""  

data Diff = Diff { d_pc :: (Integer, Tag)                  -- value and tag of the current PC
                 , d_instr :: Maybe Instr_I                -- current instruction
                 , d_reg :: Maybe (GPR_Addr, Integer, Tag) -- change in registers
                 , d_mem :: Maybe (Integer, Integer, Tag)  -- change in memory
                 }

calcDiff :: (PIPE_State, Machine_State) -> (PIPE_State, Machine_State) -> Diff
calcDiff (p1,m1) (p2,m2) =
  Diff { d_pc = (f_pc m1, p_pc p1)
       , d_instr =
           case fst $ instr_fetch m1 of
             Fetch u32 -> decode_I RV32 u32
             _ -> error "Bad instr fetch in calcDiff"
       , d_reg =
           let GPR_File r1 = f_gprs m1
               GPR_File r2 = f_gprs m2
               GPR_FileT t1 = p_gprs p1
               GPR_FileT t2 = p_gprs p2
               -- BCP: Something seems funny about these two filters:
               -- What will happen if the i1 and i2 are not the same
               -- at some point??  Seems like we will just ignore and
               -- go on.  We should at least error out (which I have now done). 
               reg_diff =
                 filter (\((i1,d1),(i2,d2)) -> assert (i1 == i2) $ d1 /= d2) (zip (Data_Map.assocs r1) (Data_Map.assocs r2))
               tag_diff =
                 filter (\((i1,l1),(i2,l2)) -> assert (i1 == i2) $ l1 /= l2) (zip (Data_Map.assocs t1) (Data_Map.assocs t2))
           in case (reg_diff, tag_diff) of
                ([], []) -> Nothing
                ([((i,_),(_,d))],[((j,_),(_,l))]) | i == j -> Just (i,d,l)
                ([((i,_),(_,d))],[]) ->
                  (i,d,) <$> Data_Map.lookup i t2
                ([],[((i,_),(_,l))]) ->
                  (i,,l) <$> Data_Map.lookup i r2
                _ -> error $ "More than one diff in register file:" ++ 
                             " registers = " ++ show reg_diff ++ 
                             " and tags = " ++ show tag_diff 
       , d_mem =
           let Mem dm1 _ = f_mem m1
               Mem dm2 _ = f_mem m2
               MemT pm1 = p_mem p1
               MemT pm2 = p_mem p2
               -- BCP: Ditto above concern:
               data_diff =
                 filter (\((i1,d1),(i2,d2)) ->
                           if i1 == i2 then d1 /= d2 else error $ "DIFF: " ++ show (i1, d1, i2, d2, dm1, dm2))
--                             assert (i1 == i2) $ d1 /= d2)
                        (zip (Data_Map.assocs dm1) (Data_Map.assocs dm2))
               tag_diff =
                 filter (\((i1,l1),(i2,l2)) -> assert (i1 == i2) $ l1 /= l2) (zip (Data_Map.assocs pm1) (Data_Map.assocs pm2))
           in case (data_diff, tag_diff) of
                ([], []) -> Nothing
                ([((i,_),(_,d))],[((j,_),(_,l))]) | i == j -> Just (i,d,l)
                ([((i,_),(_,d))],[]) ->
                  (i,d,) <$> Data_Map.lookup i pm2
                ([],[((i,_),(_,l))]) ->
                  (i,,l) <$> Data_Map.lookup i dm2
                _ -> error $ "More than one diff in memory file:" ++
                             " data = " ++ show data_diff ++ 
                             " and tags = " ++ show tag_diff 
       }

prettyRegDiff (Just (i,d,l)) (Just (i', d', l')) 
    | i == i', d == d', l == l' =
        P.char 'r' P.<> P.integer i <+> P.text "<-" <+> pretty d l
    | otherwise =
      P.char 'r' P.<> P.integer i <+> P.text "<-" <+> pretty d l <||> 
      P.char 'r' P.<> P.integer i' <+> P.text "<-" <+> pretty d' l'
prettyRegDiff Nothing Nothing = P.text ""

prettyMemDiff (Just (i,d,l)) (Just (i', d', l')) 
    | i == i', d == d', l == l' =
        P.char '[' P.<> P.integer i P.<> P.char ']' <+> P.text "<-" <+> pretty d l
    | otherwise =
      P.char '[' P.<> P.integer i P.<> P.char ']' <+> P.text "<-" <+> pretty d l
      <||> P.char '[' P.<> P.integer i' P.<> P.char ']' <+> P.text "<-" <+> pretty d' l'
prettyMemDiff Nothing Nothing = P.text ""

instance CoupledPP (Maybe Instr_I) (Maybe Instr_I) where
  pretty (Just i1) (Just i2)
    | i1 == i2  = pp i1
    | otherwise = pp i1 <||> pp i2
  pretty Nothing Nothing = P.text "<Bad instr>"

instance CoupledPP Diff Diff where
  pretty d1 d2 =
    P.hcat [ pad 6 (pretty (d_pc d1) (d_pc d2))
           , P.text " "
           , pad 17 (pretty (d_instr d1) (d_instr d2))
           , P.text "     "
           , prettyRegDiff (d_reg d1) (d_reg d2) 
           , prettyMemDiff (d_mem d1) (d_mem d2)
           ] 

prop_noninterference :: MStatePair -> Property
prop_noninterference (M (m1,p1) (m2,p2)) =
  let (r1,ss1') = run_loop 20 p1 m1
      (r2,ss2') = run_loop 20 p2 m2
      ss = zip (reverse ss1') (reverse ss2') 
      ((p1',m1'),(p2', m2')) = head $ reverse $ zip (reverse ss1') (reverse ss2') in
  whenFail (do putStrLn $ "Reachable parts differ after execution!"
               uncurry printTrace (unzip ss)
-- 
-- 
--               putStrLn $ "Original machines:"
--               print_mstatepair (M (m1,p1) (m2,p2))
--               putStrLn $ "After execution..."
--               print_mstatepair (M (m1', p1') (m2', p2'))
--               putStrLn "First One:"
--               print_coupled m1' p1'
--               putStrLn "Second One:"
--               print_coupled m2' p2'
           )
           $
--           collect (case fst $ instr_fetch m1' of 
--                      Fetch u32 -> decode_I RV32 u32)
             (sameReachablePart (M (m1', p1') (m2', p2')))



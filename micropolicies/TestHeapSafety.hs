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
instance PP MStatePair where
  pp (M (m1, p1) (m2, p2)) =
    P.vcat [ P.text "Reachable Colors:" <+> pretty (reachable p1) (reachable p2)
           , P.text "PC:" <+> pretty (f_pc m1, p_pc p1) (f_pc m2, p_pc p2)
           , P.text "Registers:" $$ P.nest 2 (pretty (f_gprs m1, p_gprs p1) (f_gprs m2, p_gprs p2))
           , P.text "Memories:" $$ P.nest 2 (pretty (f_mem m1, p_mem p1) (f_mem m2, p_mem p2))
           ]

print_mstatepair :: MStatePair -> IO ()
print_mstatepair m = putStrLn $ P.render $ pp m

prop_noninterference :: MStatePair -> Property
prop_noninterference (M (m1,p1) (m2,p2)) =
  let (r1,ss1') = run_loop 100 p1 m1
      (r2,ss2') = run_loop 100 p2 m2
      ((p1',m1'),(p2', m2')) = head $ reverse $ zip (reverse ss1') (reverse ss2') in
  whenFail (do putStrLn $ "Reachable parts differ after execution!"
               putStrLn $ "Original machines:"
               print_mstatepair (M (m1,p1) (m2,p2))
               putStrLn $ "After execution..."
               print_mstatepair (M (m1', p1') (m2', p2'))
--               putStrLn "First One:"
--               print_coupled m1' p1'
--               putStrLn "Second One:"
--               print_coupled m2' p2'
           )
           (collect (case fst $ instr_fetch m1' of Fetch u32 -> decode_I RV32 u32)
             (sameReachablePart (M (m1', p1') (m2', p2'))))



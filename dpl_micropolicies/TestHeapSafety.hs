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

import Debug.Trace
import Control.Monad.Reader
import Terminal 

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

-- The "heap." here is OK, I guess because we're in the heap safety policy... 
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

load_heap_policy = do
  ppol <- load_pipe_policy "heap.main"
  let pplus = PolicyPlus
        { policy = ppol
        , initGPR = fromExt [("heap.Pointer", Just 0)]
        , initMem =
            -- TODO: Might be better to make it some separate
            -- "Uninitialized" tag?
            fromExt [("heap.Cell", Just 0), ("heap.Pointer", Just 0)]
        , initPC = fromExt [("heap.Env", Nothing)]
        , initNextColor = 5
        , emptyInstTag = fromExt [("heap.Inst", Nothing)]
        , compareMachines = \pplus (M (m1,p1) (m2,p2)) -> 
            P.text "Reachable:"
              <+> pretty pplus (runReader (reachable p1) pplus) 
                               (runReader (reachable p2) pplus)
        }
  return pplus

-- prop_noninterference :: PolicyPlus -> MStatePair -> Property
-- prop_noninterference pplus (M (m1,p1) (m2,p2)) =
--   let (r1,ss1') = run_loop pplus 100 p1 m1
--       (r2,ss2') = run_loop pplus 100 p2 m2
--       ((p1',m1'),(p2', m2')) = head $ reverse $ zip (reverse ss1') (reverse ss2') in
--   whenFail (do putStrLnUrgent $ "Reachable parts differ after execution!"
--                putStrLn $ ""
--                -- putStrLnHighlight $ "Original machines:"
--                -- print_mstatepair ppol (M (m1,p1) (m2,p2))
--                -- putStrLn $ ""
--                -- putStrLnHighlight $ "After execution..."
--                -- print_mstatepair ppol (M (m1', p1') (m2', p2'))
--                -- putStrLn $ ""
--                -- putStrLnHighlight $ "Trace..."
--                let finalTrace = {- map flipboth $ -} reverse $ zip ss1' ss2'
--                uncurry (printTrace pplus) (unzip finalTrace)
-- --               putStrLn "First One:"
-- --               print_coupled m1' p1'
-- --               putStrLn "Second One:"
-- --               print_coupled m2' p2'
--            )
--            -- collect (case fst $ instr_fetch m1' of Fetch u32 -> decode_I RV32 u32) $
--              (runReader (sameReachablePart (M (m1', p1') (m2', p2'))) pplus)

prop_NI' pplus count maxcount trace (M (m1,p1) (m2,p2)) =
  let run_state1 = mstate_run_state_read m1
      run_state2 = mstate_run_state_read m2
      m1' = mstate_io_tick m1
      m2' = mstate_io_tick m2 
      trace' = ((m1,p1),(m2,p2)) : trace  in
  if count >= maxcount then 
    label "Out of gas" $ property True 
  -- TODO: Check for traps too
  else if run_state1 /= Run_State_Running || run_state2 /= Run_State_Running then 
    label (let (s1,s2) = (show run_state1, show run_state2) in
           if s1==s2 then s1 else (s1 ++ " / " ++ s2))
       $ property True
  else
    case (fetch_and_execute pplus m1' p1, fetch_and_execute pplus m2' p2) of
      (Right (m1r,p1r), Right (m2r, p2r)) ->
        (whenFail (do putStrLn $ "Reachable parts differ after execution!"
                      let finalTrace = reverse $ ((m1r,p1r), (m2r, p2r)) : trace'
                      uncurry (printTrace pplus) (unzip finalTrace)) $
           property $ (runReader (sameReachablePart (M (m1r,p1r) (m2r, p2r))) pplus))
        .&&. 
        prop_NI' pplus (count+1) maxcount trace' (M (m1r,p1r) (m2r, p2r))
      (Left s1, Left s2) ->
         label ("Pipe trap " ++ s1 ++ " / " ++ s2) $ property True
      (Left s1, _) ->
         label ("Pipe trap " ++ s1) $ property True
      (_, Left s2) ->
         label ("Pipe trap " ++ s2) $ property True

maxInstrsToGenerate :: Int
maxInstrsToGenerate = 10

prop_noninterference :: PolicyPlus -> MStatePair -> Property
prop_noninterference pplus ms = prop_NI' pplus 0 maxInstrsToGenerate [] ms


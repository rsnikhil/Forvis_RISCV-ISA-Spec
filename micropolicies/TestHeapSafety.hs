module TestHeapSafety where

import qualified Data.Map.Strict as Data_Map
import Data.Maybe
import qualified Data.Set as Data_Set
import Data.Set (Set)

import Bit_Utils
import Arch_Defs

-- Maybe?
import Machine_State
import Forvis_Spec_I
import GPR_File
import Memory

-- This might belong elsewhere
import Test.QuickCheck

--------------------------------------------------------
-- This belongs in /src!

import Data.Bits

import Printing
import PIPE
import Run_Program_PIPE
import Gen

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

reachable :: MemT -> Set Color -> Set Color
reachable m s = 
  let s' = reachableInOneStep m s in
  if s == s' then s else reachable m s'

registerColors :: PIPE_State -> Set Color 
registerColors pstate = 
  foldr (\t s -> case t of 
                   MTagR c -> Data_Set.insert c s 
                   _ -> error "Register tag should be an MTagR")
    Data_Set.empty (unGPR $ p_gprs pstate) 



{-

Fixpoint vuAux (r : Colors.t) (h : Memory) : G Memory :=
(* trace ("vuAux " ++ show (Colors.elements r) ++ " " ++ show h ++ newline)*) (
  match h with
    [] => ret []
  | (v, (HeapSafety.MTagM vt lt))::tl =>
      head <- (if Colors.mem lt r then (*trace ("SAME"++newline)*) (ret (v, (HeapSafety.MTagM vt lt)))
               else v' <- arbitrary ;; vt' <- arbitrary ;; 
               ret (v', (HeapSafety.MTagM vt' lt))) ;;
      tail <- vuAux r tl ;;
      ret (head :: tail)
  | _ => ret []  (* should not happen *)
  end).

Definition getReachable (s : MState) : Colors.t :=
  match stepResult reachable s with
    inl _ => Colors.empty
  | inr c => c
  end.

Definition varyUnreachable (s : MState) : G MState :=
  h <- vuAux (getReachable s) s.(memory) ;;
  NOTRACE ("vuAux of " ++ show s.(memory) ++ newline ++ 
           "     wrt " ++ show (Colors.elements (getReachable s)) ++ newline ++
           "      is " ++ show h ++ newline ++ newline) IN
  ret {| regs := s.(regs) ;
         memory := h ;
         pc := s.(pc);
         pstate := s.(pstate) |}.

Global Instance genMStatePair : Gen MStatePair := {|
  arbitrary := 
    s1 <- arbitrary ;;
    s2 <- varyUnreachable s1 ;;
    ret {| s1 := s1; s2 := s2 |}
|}.

Fixpoint srAux (r1 r2 : Colors.t) (h1 h2 : Memory) : bool :=
  match (h1,h2) with
    ([],[]) => true
  | ((v1, HeapSafety.MTagM _ t1)::rest1, 
    ((v2, (HeapSafety.MTagM _ t2))::rest2)) =>
      srAux r1 r2 rest1 rest2 
      && (negb (Colors.mem t1 r1 || Colors.mem t2 r2) || (v1 =? v2)%Z)
  | (_,_) => false
  end.

Definition sameReachablePart (states : MStatePair) : bool :=
  let r1 := getReachable states.(s1) in
  let r2 := getReachable states.(s2) in
     Colors.equal r1 r2 
  && ((map fst states.(s1).(regs)) = (map fst states.(s2).(regs))?)
  && (srAux r1 r2 states.(s1).(memory) states.(s2).(memory)).

(* Sample (arbitrary : G MStatePair). *)

Global Instance shrinkMStatePair : Shrink MStatePair := {|
  shrink p := 
    let candidates := 
      (r <- shrinkRegistersPair (p.(s1).(regs), p.(s2).(regs)) ;;
      ret {| s1 := {| regs   := (fst r); 
                      memory := p.(s1).(memory); 
                      pc     := p.(s1).(pc); 
                      pstate := p.(s1).(pstate) |};
             s2 := {| regs   := (snd r); 
                      memory := p.(s2).(memory); 
                      pc     := p.(s2).(pc); 
                      pstate := p.(s2).(pstate) |};
           |} )
     ++
      (h <- shrinkHeapPair (getReachable p.(s1))
                           (p.(s1).(memory), p.(s2).(memory)) ;;
      ret {| s1 := {| regs   := p.(s1).(regs); 
                      memory := (fst h); 
                      pc     := p.(s1).(pc); 
                      pstate := p.(s1).(pstate) |};
             s2 := {| regs   := p.(s2).(regs); 
                      memory := (snd h); 
                      pc     := p.(s2).(pc); 
                      pstate := p.(s2).(pstate) |};
           |} ) in
    filter sameReachablePart candidates
  |}%list.

Definition prop_varyUnreachable_ok (states : MStatePair) : Checker :=
  checker (sameReachablePart states).
(* QuickCheck prop_varyUnreachable_ok. *)

Definition prop_shrinkingPreservesReachability : Checker :=
  forAll arbitrary $ fun states : MStatePair => 
    let shrunk := shrink states in
    (* collect (show (List.length shrunk)) $ *)
    List.forallb sameReachablePart shrunk.
(* QuickCheck prop_shrinkingPreservesReachability. *)

-}

data MStatePair =
  M (Machine_State, PIPE_State) (Machine_State, PIPE_State)

sameReachablePart :: MStatePair -> Bool
sameReachablePart (M (s1, p1) (s2, p2)) =
  let r1 = reachable (p_mem p1) (registerColors p1)
      r2 = reachable (p_mem p2) (registerColors p2)

      filterReachable ms =
        map (fst . snd) $
        filter (\((i,d),(j,t)) ->
                  if i == j then
                    case t of
                      MTagM l v -> Data_Set.member l r1
                      _ -> False
                  else error "Address mismatch in sameReachable part"
               ) ms
      
  in
    r1 == r2
    && (f_gprs s1 == f_gprs s2)
    && (filterReachable (zip (Data_Map.assocs $ f_dm $ f_mem s1) (Data_Map.assocs $ unMemT $ p_mem p1)) ==
        filterReachable (zip (Data_Map.assocs $ f_dm $ f_mem s2) (Data_Map.assocs $ unMemT $ p_mem p2)))

prop_noninterference :: MStatePair -> Property
prop_noninterference (M (m1,p1) (m2,p2)) =
  let (r1,ss1') = run_loop 100 p1 m1
      (r2,ss2') = run_loop 100 p2 m2
      ((p1',m1'),(p2', m2')) = head $ reverse $ zip (reverse ss1') (reverse ss2') in
  whenFail (do putStrLn $ "Reachable parts differ after execution!"
               putStrLn "First One:"
               print_coupled m1' p1'
               putStrLn "Second One:"
               print_coupled m2' p2')
           (sameReachablePart (M (m1', p1') (m2', p2')))

instance Show Machine_State where
  show _ = ""

testHeapSafety =
  forAll genMachine $ \(m1, p1) ->
  forAll genMachine $ \(m2, p2) ->
  prop_noninterference (M (m1,p1) (m2, p2))


module PIPE where

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

--opcodeE x   = shiftL x 0
--rdE x       = shiftL x 7
--funct3E x   = shiftL x 12
--rs1E x      = shiftL x 15
--rs2E x      = shiftL x 20
--funct7E x   = shiftL x 25
--imm12E x    = shiftL x 20

encode_I :: RV -> Instr_I -> Instr_32b
encode_I rv (ADDI rd rs1 imm12) = mkInstr_I_type imm12 rs1 0 rd opcode_OP_IMM 
encode_I rv (LW rd rs1 imm12)   = mkInstr_I_type imm12 rs1 funct3_LW rd opcode_LOAD
encode_I rv (SW rs1 rs2 imm12)  = mkInstr_S_type imm12 rs2 rs1 funct3_SW opcode_STORE
encode_I rv (ADD rd rs1 rs2)    = mkInstr_R_type funct7_ADD rs2 rs1 funct3_ADD rd opcode_OP

mkInstr_I_type :: InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_I_type    imm12         rs1           funct3        rd            opcode =
  let
    legal  = (((   shiftR  imm12  12) == 0)
              && ((shiftR  rs1     5) == 0)
              && ((shiftR  funct3  3) == 0)
              && ((shiftR  rd      5) == 0)
              && ((shiftR  opcode  7) == 0))

    instr  = ((    shiftL  imm12   20)
              .|. (shiftL  rs1     15)
              .|. (shiftL  funct3  12)
              .|. (shiftL  rd       7)
              .|. opcode)
  in
    -- assert  legal  instr
    instr

mkInstr_R_type :: InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_R_type    funct7        rs2           rs1           funct3        rd            opcode =
  let
    legal  = (((   shiftR  funct7  7) == 0)
              && ((shiftR  rs2     5) == 0)
              && ((shiftR  rs1     5) == 0)
              && ((shiftR  funct3  3) == 0)
              && ((shiftR  rd      5) == 0)
              && ((shiftR  opcode  7) == 0))

    instr = ((   shiftL  funct7  25)
            .|. (shiftL  rs2     20)
            .|. (shiftL  rs1     15)
            .|. (shiftL  funct3  12)
            .|. (shiftL  rd       7)
            .|. opcode)
  in
    --    assert  legal  instr
    instr

{-# INLINE mkInstr_S_type #-}
mkInstr_S_type :: InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_S_type    imm12         rs2           rs1           funct3        opcode =
  let
    legal  = (((   shiftR  imm12  12) == 0)
              && ((shiftR  rs1     5) == 0)
              && ((shiftR  rs2     5) == 0)
              && ((shiftR  funct3  3) == 0)
              && ((shiftR  opcode  7) == 0))

    instr  = ((    shiftL  (bitSlice  imm12  11 5)  25)
              .|. (shiftL  rs2                      20)
              .|. (shiftL  rs1                      15)
              .|. (shiftL  funct3                   12)
              .|. (shiftL  (bitSlice  imm12   4 0)   7)
              .|. opcode)
  in
    instr

--------------------------------------------------------

-- Design decision: Do we want to write policies in Haskell, or in
-- RISCV machine instructions (compiled from C or something).  In this
-- experiment I'm assuming the former.  If we go for the latter, it's
-- going to require quite a bit of lower-level plumbing (including
-- keeping two separate copies of the whole RISCV machine state, and
-- making the explicit connections between them).  The latter is
-- probably what we really want, though.

newtype Color = C Int
                deriving (Eq, Show, Ord)

dfltcolor = C 0
othercolor = C 1

-- LATER: This should really be passed to the policy as a parameter
-- that is chosen by the test harness. 
initialColors = 5

data AllocOrNot = Alloc | NoAlloc deriving (Eq, Show)

data Tag = MTagP 
         | MTagI AllocOrNot 
         | MTagR Color
         | MTagM Color   -- contents (stored pointer)
                 Color   -- location
         deriving (Eq, Show)

initPC = MTagP
initStackTag = MTagM dfltcolor dfltcolor
initHeapTag = initStackTag
initR = MTagR dfltcolor
initR_SP = initR
plainInst = MTagI NoAlloc

---------------------------------

newtype GPR_FileT = GPR_FileT  { unGPR :: Data_Map.Map  InstrField  Tag }
  deriving (Eq, Show)

mkGPR_FileT :: GPR_FileT
mkGPR_FileT = GPR_FileT (Data_Map.fromList (zip [0..31] (repeat initR)))

gpr_readT :: GPR_FileT ->    GPR_Addr -> Tag
gpr_readT    (GPR_FileT dm)  reg = maybe (error (show reg)) id (Data_Map.lookup  reg  dm)

gpr_writeT :: GPR_FileT ->    GPR_Addr -> Tag -> GPR_FileT
gpr_writeT    (GPR_FileT dm)  reg         val =
    seq  val  (GPR_FileT (Data_Map.insert  reg  val  dm))

newtype MemT = MemT {unMemT :: Data_Map.Map Integer Tag}
  deriving (Eq, Show)

mkMemT = MemT (Data_Map.fromList [])

---------------------------------

data PIPE_Result = PIPE_Trap String
                 | PIPE_Success

data PIPE_State = PIPE_State {
  p_pc   :: Tag,
  p_gprs :: GPR_FileT,
  p_mem  :: MemT,
  p_nextcolor :: Int
  }
  deriving (Eq, Show)

init_pipe_state = PIPE_State {
  p_pc = initPC,
  p_gprs = mkGPR_FileT,
  p_mem = mkMemT,
  p_nextcolor = 1
  }

fresh_color :: PIPE_State -> (Color, PIPE_State)
fresh_color p = (C $ p_nextcolor p, p {p_nextcolor = p_nextcolor p + 1})

-- Should be done with lenses...
get_rtag :: PIPE_State -> GPR_Addr -> Tag
get_rtag p = gpr_readT (p_gprs p)

set_rtag :: PIPE_State -> GPR_Addr -> Tag -> PIPE_State
set_rtag p a t = p {p_gprs = gpr_writeT (p_gprs p) a t}

get_mtag :: PIPE_State -> Integer -> Tag
get_mtag p a = maybe (MTagM dfltcolor dfltcolor) id $ Data_Map.lookup a (unMemT $ p_mem p) 

set_mtag :: PIPE_State -> Integer -> Tag -> PIPE_State
set_mtag p a t = p { p_mem = MemT (Data_Map.insert a t (unMemT $ p_mem p)) }

---------------------------------

ok p = (p, PIPE_Success)
notok p s = (p, PIPE_Trap s)

exec_pipe :: PIPE_State -> Machine_State -> Integer -> (PIPE_State, PIPE_Result)
exec_pipe p m u32 =
  let rv  = mstate_rv_read  m
      res = decode_I rv u32
      ic = get_mtag p (f_pc m)
  in case res of
      Just i -> 
        case i of
          ADDI rd rs imm 
            | ic == MTagI NoAlloc -> ok $ set_rtag p rd $ get_rtag p rs
            | otherwise -> 
                let (c, p') = fresh_color p in
                ok $ set_rtag p rd (MTagR c)
          ADD rd rs1 rs2 -> ok $ set_rtag p rd $ get_rtag p rs1
          LW rd rs imm   -> 
            let rsc = get_rtag p rs
--                rs1_val  = mstate_gpr_read  mstate  rs1    -- address base
--                s_imm12  = sign_extend  12  xlen  imm12
--                eaddr1   = alu_add  xlen  rs1_val  s_imm12
--                addr   = if (rv == RV64) then eaddr1 else (eaddr1 .&. 0xffffFFFF)
                addr = mstate_gpr_read m rs
                memc = get_mtag p (addr+imm) in 
            case (rsc,memc) of
              (MTagR t1c, MTagM t2vc t2lc) 
                | t1c==t2lc -> ok $ set_rtag p rd (MTagR t2vc)
                | otherwise -> notok p $ "Different colors on Load: " ++ show t1c ++ " and " ++ show t2lc
              _ -> notok p $ "Mangled tags on Load: " ++ show rsc ++ " and " ++ show memc
          SW rs1 rs2 imm -> 
            let addr = mstate_gpr_read m rs1
                rs1c = get_rtag p rs1 
                rs2c = get_rtag p rs2 in 
            case (rs1c,rs2c) of
              (MTagR t1c, MTagR t2c) -> ok $ set_mtag p (addr+imm) (MTagM t2c t1c)
              _ -> notok p $ "Mangled tags on Store: " ++ show rs1c ++ " and " ++ show rs2c
          _ -> ok p 
      Nothing -> ok p

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

prop_noninterference :: Testable prop => MStatePair -> prop
prop_noninterference (M (m1,p1) (m2,p2)) =
  let (r1,p1',m1') = run_loop 100 p1 m1
      (r2,p2',m2') = run_loop 100 p2 m2

  let ss1' := runProg 100 p states.(s1) in
  let ss2' := runProg 100 p states.(s2) in
  let '(ss1',ss2') := trimToSameLength ss1' ss2' in
  let s1' := hd states.(s1) (rev ss1') in
  let s2' := hd states.(s2) (rev ss2') in
  let r1 := getReachable s1' in
  let r2 := getReachable s2' in
  if negb (sameReachablePart {|s1:=s1';s2:=s2'|}) then
    whenFail (newline ++
              "Reachable parts differ after execution!" ++ newline ++
              show s1' ++ newline ++ show s2' ++ newline)
             (checker false)
  else
    checker true.

testHeapSafety = quickCheck prop_noninterference


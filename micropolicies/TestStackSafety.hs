{-

    HEADER sequence:
        - SW SP RA 1    @H1    - store RA
        - ADDI SP SP 2  @H2    - increment SP
        - ADDI RA R0 0  @H3    - untag RA so others can use

    RETURN sequence: 
        - LW RA SP -1   @R1    - load return address
        - ADDI SP SP -2 @R2    - decrement SP
        - JALR RA RA    @R3    - jump back 

The main idea of the stack property:
  - We are always comparing one "main run" to a stack of "variant states" *
    <PC of JAL instructions> * <SP at JAL instructions>.
  - In the inefficient version, each execution step steps the main state and
    ALL the variant states
  - When the machines (all together) execute a call (whatever that exactly
    means), we add to the variant stack a scrambled version of the main
    state in which every inaccessible location is replaced by an arbitrary
    number, paired with the current PC
  - when we return, we throw away the top variant state
  - the invariant we expect is that
      - the accessible region of the main machine is the same as the
        accessible regions of all the variant machines after each step
      - the inaccessible region of each variant machine is unchanged by each
        step
  - a CALL is when the machine executes a JAL 
  - a RETURN is when the machine's PC is one more than the PC at the top of
    the stack of saved PCs and the stack pointer is equal to the top element
    of the stack of saved SPs.
-}

{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module TestStackSafety where

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

----------------------------------------------------------------------------------------
-- Accessibility

depthOf :: TagSet -> Maybe Color
depthOf t = 
  -- trace ("cellColorOf " ++ show t ++ " i.e. " ++ show (toExt t)) $
  join $ Data_List.lookup "stack.Depth" (toExt t)

getPCDepth :: PIPE_State -> Int
getPCDepth p = fromJust $ depthOf $ p_pc p

sameAccessiblePart :: MStatePair -> P Bool
sameAccessiblePart (M (s1, p1) (s2, p2)) = do
  let pcd1 = getPCDepth p1
      pcd2 = getPCDepth p2

      filterAux [] _ = return []
      filterAux _ [] = return []
      filterAux ((i,d):ds) ((j,t):ts)
        | i == j = do
            case depthOf t of
              Just c' | c' >= pcd1 -> (d :) <$> filterAux ds ts
              _ -> filterAux ds ts
        | i < j = error "Tag and data memory have different domains <" -- filterAux ds ((j,t):ts)
        | i > j = error "Tag and data memory have different domains >" -- filterAux ((i,d):ds) ts

  f1 <- filterAux (Data_Map.assocs $ f_dm $ f_mem s1) (Data_Map.assocs $ unMemT $ p_mem p1)
  f2 <- filterAux (Data_Map.assocs $ f_dm $ f_mem s2) (Data_Map.assocs $ unMemT $ p_mem p2)

  return $ (pcd1 == pcd2) && (f_gprs s1 == f_gprs s2) && (f1 == f2)

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

-- Figure out how to set this properly
maxImm = 40

-- Generate an immediate up to number
-- Multiple of 4
genImm :: Integer -> Gen InstrField
-- genImm n = (4*) <$> choose (0, n `div` 4)
genImm n = (4*) <$> choose (0, n `div` 4)  

{-
-- Picks out valid (data registers + content + min immediate + max immediate + tag),
--                 (jump registers + min immediate),
--                 integer registers
groupRegisters :: PolicyPlus -> GPR_File -> GPR_FileT -> PIPE_State ->
                  ([(GPR_Addr, Integer, Integer, Integer, TagSet)],
                   [(GPR_Addr, Integer)],
                   [GPR_Addr])
groupRegisters pplus (GPR_File rs) (GPR_FileT ts) ps =
  -- Assuming that the register files are same length and they have no holes
  let regs = Data_Map.assocs rs
      tags = Data_Map.assocs ts
      rts = zip regs tags

      validData ((reg_id,reg_content),(_reg_id, reg_tag)) 
        | reg_content >= dataMemLow pplus &&
          reg_content <= dataMemHigh pplus =
          -- && depthOf <target memory> >= depthOf <pc>
           Just (reg_id, reg_content, 0, dataMemHigh pplus - reg_content, reg_tag)
        | reg_content == 0  =  -- && isJust (pointerColorOf reg_tag)
        -- We can allow a 0 register by adding at least 4
           Just (reg_id, 0, dataMemLow pplus, dataMemHigh pplus, reg_tag)
        | otherwise =
           Nothing
        
      validJump ((reg_id,reg_content),(_, reg_tag))
        | reg_content < instrLow pplus && isJust (envColorOf reg_tag) =
          Just (reg_id, instrLow pplus - reg_content)
        | otherwise =
          Nothing

      dataRegs    = map (fromJust) $ filter (isJust) $ map validData rts
      controlRegs = map (fromJust) $ filter (isJust) $ map validJump rts
      arithRegs   = map fst regs
  in (dataRegs, controlRegs, arithRegs)

-- All locations that can be accessed using color 'c' between 'lo' and 'hi'
accessibleLocsBetween :: PolicyPlus -> Mem -> MemT -> Integer -> Integer -> TagSet -> [Integer]
accessibleLocsBetween pplus (Mem m _) (MemT pm) lo hi t =
  case pointerColorOf t of
    Just c -> 
      map fst $ filter (\(i,t) ->
                          case cellColorOf t of
                            Just c' -> c == c' && i >= lo && i <= hi
                            _ -> False
                       ) (Data_Map.assocs pm)
    _ -> []
-}

genInstr :: PolicyPlus -> Machine_State -> PIPE_State -> Gen (Instr_I, TagSet)
genInstr pplus ms ps =
  frequency [ (1,
               do -- ADDI
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm maxImm
                  let tag = emptyInstTag pplus                         
                  return (ADDI rd rs imm, tag))
            , (1,
               do -- LOAD
                  rs <- genSourceReg ms
                  rd <- genTargetReg ms
                  imm <- genImm maxImm
                  let tag = emptyInstTag pplus                         
                  return (LW rd rs imm, tag)
              )
            , (1,
               do -- STORE
                  rs <- genSourceReg ms
                  rd <- genSourceReg ms
                  imm <- genImm maxImm
                  let tag = emptyInstTag pplus                         
                  return (SW rd rs imm, tag))
            , (1,
               do -- ADD
                  rs1 <- genSourceReg ms
                  rs2 <- genSourceReg ms
                  rd <- genTargetReg ms
                  let tag = emptyInstTag pplus
                  return (ADD rd rs1 rs2, tag))
            ]

setInstrI :: Machine_State -> Instr_I -> Machine_State
setInstrI ms i =
  ms {f_mem = (f_mem ms) { f_dm = Data_Map.insert (f_pc ms) (encode_I RV32 i) (f_dm $ f_mem ms) } }

setInstrTagI :: Machine_State -> PIPE_State -> TagSet -> PIPE_State
setInstrTagI ms ps it =
  ps {p_mem = ( MemT $ Data_Map.insert (f_pc ms) (it) (unMemT $ p_mem ps) ) }

setInstrIAt :: Machine_State -> Instr_I -> Integer -> Machine_State
setInstrIAt ms i addr =
  ms {f_mem = (f_mem ms) { f_dm = Data_Map.insert addr (encode_I RV32 i) (f_dm $ f_mem ms) } }

setInstrTagIAt :: PIPE_State -> TagSet -> Integer -> PIPE_State
setInstrTagIAt ps it addr =
  ps {p_mem = ( MemT $ Data_Map.insert addr (it) (unMemT $ p_mem ps) ) }

genMStatePair_ :: PolicyPlus -> Gen MStatePair
genMStatePair_ pplus =
  let minit = initMachine 
      pinit = init_pipe_state pplus
      instrs = [ (0, JAL 0 100, emptyInstTag pplus)
               ]
      (m,p) = foldl (\(m',p') (addr,inst,tag) ->
                       (setInstrIAt m' inst addr, setInstrTagIAt p' tag addr)
                      )
                (minit,pinit) instrs
  in 

  return (M (m,p) (m,p))

----------------------------------------------------------------
-- OLD STUFF TO BE REVIVED


--------------------------------------------------
-- STOPPED HERE

{-
genColor :: Gen Color
genColor =
  frequency [ (1, pure $ 0)
            , (4, choose (0, 4)) ]

-- Only colors up to 2 (for registers)
genColorLow :: Gen Color
genColorLow = frequency [ (1, pure $ 0)
                        , (2, choose (0,2)) ]

-- Focus on unaccessible colors
genColorHigh :: Gen Color
genColorHigh =
  frequency [ (1, pure $ 0)
            , (1, choose (1,2) )
            , (3, choose (3,4) )
            ]

genMTagM :: PolicyPlus -> Gen TagSet
genMTagM pplus = do
  c1 <- genColor
  c2 <- genColor
  return $ fromExt [("heap.Cell", Just c1), ("heap.Pointer", Just c2)]

genDataMemory :: PolicyPlus -> Gen (Mem, MemT)
genDataMemory pplus = do
  let idx = [dataMemLow pplus, (dataMemLow pplus)+4..(dataMemHigh pplus)]
  combined <- mapM (\i -> do d <- genImm $ dataMemHigh pplus    -- BCP: This always puts 4 in every location!
                             t <- genMTagM pplus
                             return ((i, d),(i,t))) idx
  let (m,pm) = unzip combined
  return (Mem (Data_Map.fromList m) Nothing, MemT $ Data_Map.fromList pm)

setInstrI :: Machine_State -> Instr_I -> Machine_State
setInstrI ms i =
  ms {f_mem = (f_mem ms) { f_dm = Data_Map.insert (f_pc ms) (encode_I RV32 i) (f_dm $ f_mem ms) } }

setInstrTagI :: Machine_State -> PIPE_State -> TagSet -> PIPE_State
setInstrTagI ms ps it =
  ps {p_mem = ( MemT $ Data_Map.insert (f_pc ms) (it) (unMemT $ p_mem ps) ) }

genByExec :: PolicyPlus -> Int -> Machine_State -> PIPE_State -> Set GPR_Addr -> Gen (Machine_State, PIPE_State, Set GPR_Addr)
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

mkPointerTagSet pplus c = fromExt [("heap.Pointer", Just c)]

updTags :: PolicyPlus -> GPR_FileT -> Gen GPR_FileT
updTags pplus (GPR_FileT rs) = do
  [c1, c2, c3] <- (map $ mkPointerTagSet (policy pplus)) <$> (replicateM 3 genColorLow)
  
  let rs' :: Data_Map.Map Integer TagSet = Data_Map.insert 1 c1 $ Data_Map.insert 2 c2 $ Data_Map.insert 3 c3 rs
  return $ GPR_FileT rs'

genMachine :: PolicyPlus -> Gen (Machine_State, PIPE_State)
genMachine pplus = do
  -- registers
  (mem,pmem) <- genDataMemory pplus
  let ms = initMachine {f_mem = mem}
      ps = (init_pipe_state pplus){p_mem = pmem}
      ms2 = setInstrI ms (JAL 0 1000)
      ps2 = setInstrTagI ms ps (emptyInstTag pplus)  -- Needed??

  rs' <- updRegs $ f_gprs ms2
  ts' <- updTags pplus $ p_gprs ps2
  let ms' = ms2 {f_gprs = rs'}
      ps' = ps2 {p_gprs = ts'}
  
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

varyUnaccessibleMem :: PolicyPlus -> Set Color -> Mem -> MemT -> Gen (Mem, MemT)
varyUnaccessibleMem pplus r (Mem m ra) (MemT pm) = do
  combined <- mapM (\((i,d),(j,t)) -> do
                       case cellColorOf t of
                         Just c'
                           | Data_Set.member c' r -> return ((i,d),(j,t))
                           | otherwise -> do d' <- genImm 12 -- TODO: This makes no sense
                                             return ((i,d'),(j,t)) -- TODO: Here we could scramble v
                         _ -> return ((i,d),(j,t))
                    ) $ zip (Data_Map.assocs m) (Data_Map.assocs pm)
  let (m', pm') = unzip combined
  return (Mem (Data_Map.fromList m') ra, MemT (Data_Map.fromList pm'))

varyUnaccessible :: PolicyPlus -> (Machine_State, PIPE_State) -> Gen MStatePair
varyUnaccessible pplus (m, p) = do
  let r = runReader (accessible p) pplus
  (mem', pmem') <- varyUnaccessibleMem pplus r (f_mem m) (p_mem p)
  return $ M (m,p) (m {f_mem = mem'}, p {p_mem = pmem'})

genMStatePair_ :: PolicyPlus -> Gen MStatePair
genMStatePair_ pplus = 
  genMachine pplus >>= varyUnaccessible pplus

------------------------------------------------------------------------------------------
-- Shrinking

-- Tag shrinking basically amounts to shrinking the colors
-- of things to C 0. Assuming that C 0 is always accessible.
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
-- We also need the set of accessible things for data memories
-- INV: Original memories contain identical indices
shrinkMems :: PolicyPlus -> Set Color -> (Mem, MemT) -> (Mem, MemT) -> [((Mem, MemT), (Mem,MemT))]
shrinkMems pplus accessible (Mem m1 i1, MemT t1) (Mem m2 i2, MemT t2) = 
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
                -- Both accessible, everything should be identical
                | Data_Set.member loc1 accessible && Data_Set.member loc2 accessible && l1 == l2 && d1 == d2->
                  -- shrink the first and copy
                  -- Shrink data
                  [ (((j, d'), (j, l1)), ((j, d'),(j, l1))) | d' <- shrink d1 ]
                  ++ 
                  -- Or shrink tag 
                  [ (((j, d1), (j, l')), ((j, d2),(j, l'))) | l' <- shrinkTag l1 ]
                -- Both unaccessible, shrink independently
                | not (Data_Set.member loc1 accessible) && not (Data_Set.member loc2 accessible) ->
                  -- Shrink first data value 
                  [ (((j, d1'), (j, l1)), ((j, d2),(j, l2))) | d1' <- shrink d1 ]
                  ++
                  -- Shrink first tag to something unaccessible 
                  [ (((j, d1), (j, l1')), ((j, d2),(j, l2)))
                  | l1' <- shrinkTag l1,
                    not $ Data_Set.member (fromJust $ cellColorOf l1') accessible ]
                  ++
                  -- Shrink first tag to something accessible (and make sure first and second components are the same!)
                  [ (((j, d1), (j, l1')), ((j, d1),(j, l1')))
                  | l1' <- shrinkTag l1,
                    Data_Set.member (fromJust $ cellColorOf l1') accessible ]
                  ++
                  -- ... same for second register state
                  [ (((j, d1), (j, l1)), ((j, d2'),(j, l2))) | d2' <- shrink d2 ]
                  ++
                  [ (((j, d1), (j, l1)), ((j, d2),(j, l2')))
                  | l2' <- shrinkTag l2,
                    not $ Data_Set.member (fromJust $ cellColorOf l2') accessible ]
                  ++
                  [ (((j, d1), (j, l2')), ((j, d1),(j, l2')))
                  | l2' <- shrinkTag l2,
                    Data_Set.member (fromJust $ cellColorOf l2') accessible ]
                | otherwise -> error $ "Not both accessible or unaccessible?" ++ show (d1,l1,d2,l2)
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
 
      indexTagedIntsToMem :: _ -> [IndexedTaggedInt] -> (Mem, MemT)
      indexTagedIntsToMem i itis = ((flip Mem i) . Data_Map.fromList) *** (MemT . Data_Map.fromList) $ unzip itis
        
  in map (indexTagedIntsToMem i1 *** indexTagedIntsToMem i2) $ shrinkMemAux (zip m1' t1') (zip m2' t2')
        
shrinkMStatePair_ :: PolicyPlus -> MStatePair -> [MStatePair]
shrinkMStatePair_ pplus (M (m1,p1) (m2,p2)) =
  let r = runReader (accessible p1) pplus
      -- Shrink Memories
  in
     [ M (m1{f_mem = mem1},p1{p_mem = pmem1}) (m2{f_mem=mem2}, p2{p_mem=pmem2})
     | ((mem1, pmem1), (mem2, pmem2)) <- shrinkMems pplus r (f_mem m1, p_mem p1) (f_mem m2, p_mem p2) ]
  ++ [ M (m1{f_gprs = gpr1},p1{p_gprs = pgpr1}) (m2{f_gprs=gpr2}, p2{p_gprs=pgpr2})
     | ((gpr1, pgpr1), (gpr2, pgpr2)) <- shrinkGPRs pplus (f_gprs m1, p_gprs p1) (f_gprs m2, p_gprs p2) ]


------------------------------------------------------------------------------------------
-- Top-level non-interference policy
  
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
        (whenFail (do putStrLn $ "Accessible parts differ after execution!"
                      let finalTrace = reverse $ ((m1r,p1r), (m2r, p2r)) : trace'
                      uncurry (printTrace pplus) (unzip finalTrace)) $
           property $ (runReader (sameAccessiblePart (M (m1r,p1r) (m2r, p2r))) pplus))
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

prop_ :: PolicyPlus -> MStatePair -> Property
prop_ pplus ms = prop_NI' pplus 0 maxInstrsToGenerate [] ms
-}

prop_ :: PolicyPlus -> MStatePair -> Property
prop_ pplus (M (m1,p1) (m2,p2)) = undefined
{-
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
        (whenFail (do putStrLn $ "Accessible parts differ after execution!"
                      let finalTrace = reverse $ ((m1r,p1r), (m2r, p2r)) : trace'
                      uncurry (printTrace pplus) (unzip finalTrace)) $
           property $ (runReader (sameAccessiblePart (M (m1r,p1r) (m2r, p2r))) pplus))
        .&&. 
        prop_NI' pplus (count+1) maxcount trace' (M (m1r,p1r) (m2r, p2r))
      (Left s1, Left s2) ->
         label ("Pipe trap " ++ s1 ++ " / " ++ s2) $ property True
      (Left s1, _) ->
         label ("Pipe trap " ++ s1) $ property True
      (_, Left s2) ->
         label ("Pipe trap " ++ s2) $ property True
-}

--------------------------------------------------------------------------
-- The stack-safety policy
  
load_policy = do
  ppol <- load_pipe_policy "stack.main"
  let pplus = PolicyPlus
        { policy = ppol
        , initGPR = fromExt [("stack.Boring", Nothing)]
        , initMem = fromExt [("stack.Boring", Nothing)]
        , initPC  = fromExt [("stack.PC", Just 0)]
        , initNextColor = 1
        , emptyInstTag = fromExt [("stack.Boring", Nothing)]
        , instrLow = 0
--        , instrHigh = 499
--        , stackLow = 500
--        , stackHigh = 899
        , dataMemLow = 900
        , dataMemHigh = 920  
--        , compareMachines = \pplus (M (m1,p1) (m2,p2)) -> P.empty
        , shrinkMStatePair = (\_ _ -> [])
        , genMStatePair = genMStatePair_
        , prop = prop_
        }
  return pplus



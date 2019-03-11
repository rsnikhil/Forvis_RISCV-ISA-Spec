{-# LANGUAGE PartialTypeSignatures #-}
module Shrinking where

import Arch_Defs
import GPR_File
import Forvis_Spec_I
import Memory

import Data.Bits

import Encoder
import PIPE

import qualified Data.Map.Strict as Data_Map
import qualified Data.Set as Data_Set
import Data.Set (Set)

import Data.List (zip4,unzip4)
import Data.Maybe (fromJust)

import Machine_State

import Control.Arrow (second, (***))
import Test.QuickCheck
import TestHeapSafety
import Gen

import Control.Monad.Reader

-- INV: If we're shrinking registers, everything should already be equal.
shrinkRegister :: PolicyPlus -> (Integer, TagSet) -> [(Integer, TagSet)]
shrinkRegister pplus (d,t) = [(d',t') | d' <- shrink d, t' <- shrinkTag pplus t]

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
          ++ [ ((i1,v1),(j1,l'),(i2,v2),(j2,l')) | l' <- shrinkTag pplus l1 ]

-- To shrink an instruction, try converting it to a noop (ADD 0 0 0)
shrinkInstr :: Instr_I -> [Instr_I]
shrinkInstr (ADD 0 0 0)  = []
-- Do not shrink the initial JAL
shrinkInstr (JAL 0 1000) = []
shrinkInstr _ = [ADD 0 0 0]

type IndexedTagedInt = ((Integer,Integer),(Integer, TagSet))

-- Have to perform the same thing to both memories at once
-- We also need the set of reachable things for data memories
-- INV: Original memories contain identical indices
shrinkMems :: PolicyPlus -> Set Color -> (Mem, MemT) -> (Mem, MemT) -> [((Mem, MemT), (Mem,MemT))]
shrinkMems pplus reachable (Mem m1 i1, MemT t1) (Mem m2 i2, MemT t2) = 
  let m1' = Data_Map.assocs m1
      t1' = Data_Map.assocs t1
      m2' = Data_Map.assocs m2
      t2' = Data_Map.assocs t2

      isData  i = i >= dataMemLow && i <= dataMemHigh
      isInstr i = i == 0 || i >= instrLow
 
      shrinkMemLoc :: (Integer, Integer, TagSet) -> (Integer, Integer, TagSet) ->
                      [ (IndexedTagedInt, IndexedTagedInt) ]
      shrinkMemLoc (j,d1,l1) (_,d2,l2)
        | isInstr j =
          case (decode_I RV32 d1, decode_I RV32 d2) of
            -- Both (identical) instructions
            (Just i1, Just i2)
              | i1 == i2 && l1 == l2 ->
                -- Shrink instruction
                [ (((j, d'), (j, l1)), ((j, d'),(j, l1))) | d' <- encode_I RV32 <$> shrinkInstr i1] ++
                -- Or shrink tag (alloc)
                [ (((j, d1), (j, l')), ((j, d1),(j, l'))) | l' <- shrinkTag pplus l1 ]
              | otherwise -> error $ "Distinguishable memory locations: " ++ show (j,d1,l1,d2,l2)
            _ -> error "Instructions can't be decoded while shrinking"
        | otherwise =
            case (cellColorOf l1, pointerColorOf l1, cellColorOf l2, pointerColorOf l2) of 
--            case (l1, l2) of
              (Just loc1, Just v1, Just loc2, Just v2) 
--              (MTagM v1 loc1, MTagM v2 loc2)
                -- Both reachable, everything should be identical
                | Data_Set.member loc1 reachable && Data_Set.member loc2 reachable && l1 == l2 && d1 == d2->
                  -- shrink the first and copy
                  -- Shrink data
                  [ (((j, d'), (j, l1)), ((j, d'),(j, l1))) | d' <- shrink d1 ]
                  ++ 
                  -- Or shrink tag 
                  [ (((j, d1), (j, l')), ((j, d2),(j, l'))) | l' <- shrinkTag pplus l1 ]
                -- Both unreachable, shrink independently
                | not (Data_Set.member loc1 reachable) && not (Data_Set.member loc2 reachable) ->
                  -- Shrink first data value 
                  [ (((j, d1'), (j, l1)), ((j, d2),(j, l2))) | d1' <- shrink d1 ]
                  ++
                  -- Shrink first tag to something unreachable 
                  [ (((j, d1), (j, l1')), ((j, d2),(j, l2))) | l1' <- shrinkTag pplus l1,
                                                               not $ Data_Set.member (fromJust $ cellColorOf l1') reachable ]
                  ++
                  -- Shrink first tag to something reachable (and make sure first and second components are the same!)
                  [ (((j, d1), (j, l1')), ((j, d1),(j, l1'))) | l1' <- shrinkTag pplus l1,
                                                                Data_Set.member (fromJust $ cellColorOf l1') reachable ]
                  ++
                  -- ... same for second register state
                  [ (((j, d1), (j, l1)), ((j, d2'),(j, l2))) | d2' <- shrink d2 ]
                  ++
                  [ (((j, d1), (j, l1)), ((j, d2),(j, l2'))) | l2' <- shrinkTag pplus l2,
                                                               not $ Data_Set.member (fromJust $ cellColorOf l2') reachable ]
                  ++
                  [ (((j, d1), (j, l2')), ((j, d1),(j, l2'))) | l2' <- shrinkTag pplus l2,
                                                                Data_Set.member (fromJust $ cellColorOf l2') reachable ]
                | otherwise -> error $ "Not simultaneously reachable or unreachable?" ++ show (d1,l1,d2,l2)
              otherwise -> error "Data memory without cell or pointer color?"
 
      shrinkMemAux :: [ IndexedTagedInt ] -> [ IndexedTagedInt] -> [ ([IndexedTagedInt], [IndexedTagedInt]) ]
      shrinkMemAux [] [] = []
      shrinkMemAux (((j1,d1),(_,l1)):more1) (((j2,d2),(_,l2)):more2) =
        -- Shrink Current memory location and rebuild mem
        [ ((loc1':more1), (loc2':more2)) | (loc1', loc2') <- shrinkMemLoc (j1,d1,l1) (j2,d2,l2) ]
        ++
        -- Keep current memory location and shrink something later on
        [ ( ((j1,d1),(j1,l1)) : more1', ((j2,d2),(j2,l2)) : more2' )
        | (more1', more2') <- shrinkMemAux more1 more2 ]
 
      indexTagedIntsToMem :: _ -> [IndexedTagedInt] -> (Mem, MemT)
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

  

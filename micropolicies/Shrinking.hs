{-# LANGUAGE PartialTypeSignatures #-}
module Shrinking where

import Arch_Defs
import Forvis_Spec_I
import Memory

import Data.Bits

import Encoder
import PIPE

import qualified Data.Map.Strict as Data_Map
import qualified Data.Set as Data_Set
import Data.Set (Set)

import Machine_State

import Control.Arrow (second, (***))
import Test.QuickCheck
import TestHeapSafety

-- Tag shrinking basically amounts to shrinking the colors
-- of things to C 0. Assuming that C 0 is always reachable.
-- We can't change the Tag type. We can't change the Color
-- arbitrarily.
shrinkColor :: Color -> [Color]
shrinkColor (C 0) = []
shrinkColor (C _) = [C 0]

shrinkTag :: Tag -> [Tag]
shrinkTag (MTagI Alloc) = [MTagI NoAlloc]
shrinkTag (MTagR c) = [MTagR c' | c' <- shrinkColor c]
shrinkTag (MTagM c1 c2) = [MTagM c1' c2' | c1' <- shrinkColor c1, c2' <- shrinkColor c2]
shrinkTag _ = []

-- INV: If we're shrinking registers, everything should already be equal.
shrinkRegister :: (Integer, Tag) -> [(Integer, Tag)]
shrinkRegister (d,t) = [(d',t') | d' <- shrink d, t' <- shrinkTag t]

-- INV: The register files are also identical
--shrinkGPR :: (GPR_File, GPR_FileT) -> [(GPR_File, GPR_FileT)]
--shrinkGPR (d,t) =
--  let combined = zip (Data_Map.elems d) (Data_Map.elems t)
--      shrunk  = map shrinkRegister combined
--      indexed = zipWith ( [0..] shrunk 
--  [(Data_Map.fromList d', Data_Map.fromList t')
--  | dt' <- map shrinkRegister $ zip (Data_Map.assocs d) (Data_Map.elems t)

-- To shrink an instruction, try converting it to a noop (ADD 0 0 0)
shrinkInstr :: Instr_I -> [Instr_I]
shrinkInstr (ADD 0 0 0) = []
shrinkInstr _ = [ADD 0 0 0]


type IndexedTagedInt = ((Integer,Integer),(Integer, Tag))

-- Have to perform the same thing to both memories at once
-- We also need the set of reachable things for data memories
-- INV: Original memories contain identical indices
shrinkMems :: Set Color -> (Mem, MemT) -> (Mem, MemT) -> [((Mem, MemT), (Mem,MemT))]
shrinkMems reachable (Mem m1 i1, MemT t1) (Mem m2 i2, MemT t2) =
  let m1' = Data_Map.assocs m1
      t1' = Data_Map.assocs t1
      m2' = Data_Map.assocs m2
      t2' = Data_Map.assocs t2

      shrinkMemLoc :: (Integer, Integer, Tag) -> (Integer, Integer, Tag) -> [ (IndexedTagedInt, IndexedTagedInt) ]
      shrinkMemLoc (j,d1,l1) (_,d2,l2) =
        case (decode_I RV32 d1, decode_I RV32 d2) of
          -- Both (identical) instructions
          (Just i1, Just i2)
            | i1 == i2 && l1 == l2 ->
              -- Shrink instruction
              [ (((j, d'), (j, l1)), ((j, d'),(j, l1))) | d' <- encode_I RV32 <$> shrinkInstr i1] ++
              -- Or shrink tag (alloc)
              [ (((j, d1), (j, l')), ((j, d1),(j, l'))) | l' <- shrinkTag l1 ]
            | otherwise -> error $ "Distinguishable memory locations: " ++ show (j,d1,l1,d2,l2)
          -- TODO: Shrink data cells
          (Nothing, Nothing) ->
            case (l1, l2) of
              (MTagM v1 loc1, MTagM v2 loc2)
                -- Both reachable, everything should be identical
                | Data_Set.member loc1 reachable && Data_Set.member loc2 reachable && l1 == l2 && d1 == d2->
                  -- shrink the first and copy
                  -- Shrink data
                  [ (((j, d'), (j, l1)), ((j, d'),(j, l1))) | d' <- shrink d1 ]
                  ++ 
                  -- TODO: Or shrink tag 
                  []
                -- Both unreachable, shrink independently
                | not (Data_Set.member loc1 reachable) && not (Data_Set.member loc2 reachable) ->
                  -- Shrink data of one
                  [ (((j, d1'), (j, l1)), ((j, d2),(j, l2))) | d1' <- shrink d1 ]
                  ++
                  -- Shrink data of second
                  [ (((j, d1), (j, l1)), ((j, d2'),(j, l2))) | d2' <- shrink d2 ]
                  -- TODO: Shrink labels?
                | otherwise -> error "Not simultaneously reachable or unreachable?"
              otherwise -> error "Not MTagM's in data memory?"
          _ -> error "Invalid memory locs"

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
        
shrinkMStatePair :: MStatePair -> [MStatePair]
shrinkMStatePair (M (m1,p1) (m2,p2)) =
  let r = reachable p1 
      -- Shrink Memories
  in
  [ M (m1{f_mem = mem1},p1{p_mem = pmem1}) (m2{f_mem=mem2}, p2{p_mem=pmem2})
  | ((mem1, pmem1), (mem2, pmem2)) <- shrinkMems r (f_mem m1, p_mem p1) (f_mem m2, p_mem p2) ]

  

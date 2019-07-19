{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, FlexibleContexts #-}
module TestState where

import Data.Functor
import Control.Monad

import Control.Lens
import Control.Lens.Fold

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe

import Text.PrettyPrint (Doc, (<+>), ($$), fcat)
import qualified Text.PrettyPrint as P

import Machine_State
import GPR_File
import FPR_File
import CSR_File
import Forvis_Spec_Instr_Fetch
import Arch_Defs
import Forvis_Spec_I
import Memory

import MachineLenses
import PIPE

-- | Basic Definition and Lenses
--------------------------------
data RichState = Rich { _ms :: Machine_State
                      , _ps :: PIPE_State
                      }

data StackElem a = SE { _mp_state :: RichState
                      , _info :: a 
                      }

data TestState a = TS { _mp :: RichState
                      , _variants :: [StackElem a]
                      }

makeLenses ''RichState
makeLenses ''StackElem
makeLenses ''TestState

statePairs :: Traversal' (TestState a) RichState
statePairs f (TS mp vars) = TS <$> f mp <*> traverse (mp_state %%~ f) vars

-- | TODO: Organization. Should different functionalities be broken in different files?

-- | Diffing. Comparing one rich state to another and producing a small delta
data Diff = Diff { _d_pc    :: Maybe (Integer, TagSet)        -- value and tag of the current PC
                 , _d_instr :: Maybe Instr_I                  -- current instruction
                 , _d_reg   :: [(GPR_Addr, Integer, TagSet)]  -- change in registers
                 , _d_mem   :: [(Integer, Integer, TagSet)]   -- Change in memory
                 }

-- Generic "find diffs" function: Takes two association lists l1 and
-- l2, both assumed sorted by their keys and both representing
-- *infinite* maps with some default value d (passed as third
-- parameter), and returns a list of differences
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

eqOn l x y = x ^. l == y ^. l

mergeDiffs :: RichState -> RichState -> 
              [(Integer, (Integer, Integer))] -> [(Integer, (TagSet , TagSet ))] ->
              [Maybe (Integer, Integer, TagSet)]
mergeDiffs st1 st2 [] [] = []
mergeDiffs st1 st2 rd@((i,(d1,d2)):rs) td@((j,(l1,l2)):ts)
  | i == j = (Just (i,d2,l2)) : mergeDiffs st1 st2 rs ts
  | i <= j = ((i,d2,) <$> (st2 ^. ps . pgpr . at i)) : mergeDiffs st1 st2 rs td
  | i >= j = ((j,,l2) <$> (st2 ^. ms . fgpr . at j)) : mergeDiffs st1 st2 rd ts
mergeDiffs st1 st2 ((i,(d1,d2)):rs) [] = ((i,d2,) <$> (st2 ^. ps . pgpr . at i)) : mergeDiffs st1 st2 rs []
mergeDiffs st1 st2 [] ((j,(l1,l2)):ts) = ((j,,l2) <$> (st2 ^. ms . fgpr . at j)) : mergeDiffs st1 st2 [] ts

-- TODO: Default Tag
calcDiff :: PolicyPlus -> RichState -> RichState -> Diff
calcDiff pplus st1 st2 =
  Diff {
    _d_pc = guard (not $ eqOn (ms . fpc) st1 st2 && eqOn (ps . ppc) st1 st2)
            $> (st2 ^. ms . fpc, st2 ^. ps . ppc)
              
  , _d_instr =
      case (instr_fetch $ _ms st1, instr_fetch $ _ms st2) of
        ((Fetch u32,_), (Fetch u32',_))
          | u32 == u32' -> Nothing
          | otherwise   -> decode_I RV32 u32
        _ -> error "Bad instr fetch in calc diff"
  , _d_reg =
      let regDiff = diff (Map.assocs $ st1 ^. ms . fgpr)
                         (Map.assocs $ st2 ^. ms . fgpr) uninitialized_word
          tagDiff = diff (Map.assocs $ st1 ^. ps . pgpr)
                         (Map.assocs $ st2 ^. ps . pgpr) (initGPR pplus)

      in catMaybes $ mergeDiffs st1 st2 regDiff tagDiff

  , _d_mem =
      let dataDiff = diff (Map.assocs $ st1 ^. ms . fmem)
                          (Map.assocs $ st2 ^. ms . fmem) uninitialized_word
          tagDiff  = diff (Map.assocs $ st1 ^. ps . pmem)
                          (Map.assocs $ st2 ^. ps . pmem) (emptyInstTag pplus)
      in catMaybes $ mergeDiffs st1 st2 dataDiff tagDiff
  } 

-- | Printing | --
------------------
-- Helper Functions
x <|> y  = x P.<> P.text "\t|\t" P.<> y
x <:> y  = x P.<> P.text ": "    P.<> y
x <@> y  = x P.<> P.text " "     P.<> y
x <||> y = x P.<> P.text " || "  P.<> y

pad :: Int -> Doc -> Doc
pad i p = let s = show p in
          P.text (s ++ take (i - (length s)) (repeat ' '))

class Pretty a where
  pretty :: a -> Doc

instance Pretty TagSet where
  pretty t = P.text (showTagSet t)

instance Pretty Integer where
  pretty n = P.sizedText 2 $ show n

instance Pretty (Integer, TagSet) where
  pretty (n,t) = pretty n <@> pretty t

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing  = P.text "-"
  pretty (Just a) = pretty a

docPCs :: (Integer, TagSet) -> [Maybe (Integer,TagSet)] -> Doc
docPCs pc [] = pretty pc
docPCs pc pcs
  | all isNothing pcs = pretty pc
  | otherwise = foldl1 (<||>) (pretty pc : map pretty pcs)

destrAssocs :: Ord addr =>
  addr -> [[(addr, val, tag)]] -> ([Maybe (addr,val,tag)], [[(addr,val,tag)]])
destrAssocs addr [] = ([], [])
destrAssocs addr (avt:avts) =
  let (front, back) = destrAssocs addr avts in
  case avt of
    [] -> (Nothing:front, []:back)
    (a,v,t):rest
      | a == addr -> (Just (a,v,t) : front, rest : back)
      | a >= addr -> (Nothing      : front, avt  : back)
      | otherwise -> error "destrAssocs"

instance Pretty (Integer, Integer, TagSet) where
  pretty (a,v,t) = pretty v <@> pretty t

docAssocs :: [(Integer, Integer, TagSet)] -> [[(Integer, Integer, TagSet)]] -> Doc
docAssocs [] [] = P.empty
docAssocs ((addr,val,tag):rest) diffs =
  let (top, rem) = destrAssocs addr diffs in
  if all isNothing top then
    pretty addr <:> pretty (val, tag)
  else
    pretty addr <:> (foldl1 (<||>) (pretty (val,tag) : map pretty top))
    $$ docAssocs rest rem

docMaps :: (Map Integer Integer, Map Integer TagSet) -> [[(Integer, Integer, TagSet)]] -> Doc
docMaps (d, t) diffs =
  let assocs = zipWith (\(i,d) (j,t) -> (i,d,t)) (Map.assocs d) (Map.assocs t)
  in docAssocs assocs diffs

--docMems :: Mem -> MemT -> [[(Integer, Integer, TagSet)]] -> Doc
--docMems d t diffs =
--  let assocs =
--        zipWith (\(i,d) (j,t) -> (i,d,t)) (Map.assocs $ d ^. fmem_map)
--                                          (Map.assocs $ t ^. pmem_map)
--  in docAssocs assocs diffs

docRichStates :: RichState -> [Diff] -> Doc
docRichStates st diffs =
  P.vcat [ P.text "PC:" <+> docPCs (st ^. ms . fpc, st ^. ps . ppc)
                                   (map _d_pc diffs)
         , P.text "Registers:"
           $$ P.nest 2 (docMaps (st ^. ms . fgpr, st ^. ps . pgpr)
                                (map _d_reg diffs))
         , P.text "Memories:"
           $$ P.nest 2 (docMaps (st ^.  ms . fmem, st ^.  ps . pmem)
                                (map _d_mem diffs))
         ]

docTestState :: PolicyPlus -> TestState a -> Doc
docTestState pplus ts = docRichStates (ts ^. mp) (map (\x -> calcDiff pplus (ts ^. mp) (x ^. mp_state)) (ts ^. variants))






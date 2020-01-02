{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TupleSections, FlexibleInstances, MultiParamTypeClasses, MultiWayIf #-}

module TestStackSafety where

-- From Haskell libraries
import Control.Lens hiding (elements)

import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
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
import MachineLenses
import TestState

-- | Policy Specific generation

noTag = fromExt []
boringTag = fromExt [("stack.Boring", Nothing)]
spTag = fromExt [("stack.SP", Nothing)]
tagH1 = fromExt [("stack.H1", Nothing)] 
tagH2 = fromExt [("stack.H2", Nothing)]  
tagH3 = fromExt [("stack.H3", Nothing)]  
tagR1 = fromExt [("stack.R1", Nothing)]
tagR2 = fromExt [("stack.R2", Nothing)]  
tagR3 = fromExt [("stack.R3", Nothing)]  
stackTag n = fromExt [("stack.Stack"  , Just n)]
pcTag n = fromExt [("stack.PC"  , Just n)]

{-
HEADER sequence:
           SW SP RA 1     @H1    - store RA
           ADDI SP SP 2  @H2    - increment SP (by frame size)

RETURN sequence:
           LW RA SP -1    @R1    - load return address  (offset by one less than frame size)
           ADDI SP SP -2 @R2    - decrement SP (by frame size)
           JALR RA RA     @R3    - jump back
-}

headerSeq offset =
            [ (JAL ra offset, boringTag)
            , (SW sp ra 1  , tagH1)
            , (ADDI sp sp 2, tagH2)
            ]

returnSeq = [ (LW ra sp (-1), tagR1)
            , (ADDI sp sp 2 , tagR2)
            , (JALR ra ra 0 , tagR3)
            ]

{-
New memory layout:

Instr Memory

Stack

Data Memory
-}

load_policy = do
  ppol <- load_pipe_policy "stack.main"
  let pplus = PolicyPlus
        { policy = ppol
        , initGPR = noTag
        , initMem = stackTag 0
            -- TODO: Might be better to make it some separate
            -- "Uninitialized" tag?
        , initPC = pcTag 0
        , initNextColor = 1
        , instrLow = 0
        , instrHigh = 400
        , emptyInstTag = noTag
        , dataMemLow = 1000
        , dataMemHigh = 1020  -- Was 40, but that seems like a lot! (8 may be too little!)
        }
  return pplus

genMTag, genGPRTag :: PolicyPlus -> Gen TagSet 
genMTag pplus = frequency [(1, pure boringTag), (1, pure boringTag)]
genGPRTag = genMTag

dataP = const True
codeP = const True
callP t = t == tagH1

genITag _ = return boringTag

isSecretMP :: Machine_State -> PIPE_State -> TagSet -> Bool
isSecretMP _ _ t = t == boringTag

mkInfo :: Machine_State -> PIPE_State -> ()
mkInfo _ _ = ()

-- | Main

-- The real one
main_test = do
  pplus <- load_policy
  quickCheckWith stdArgs{maxSuccess=1000}
    $ forAllShrink (genVariationTestState pplus genMTag genGPRTag dataP codeP callP genITag isSecretMP mkInfo)
                   (\ts -> [] ) --shrinkMStatePair pplus mp 
--                   ++ concatMap (shrinkMStatePair pplus) (shrinkMStatePair pplus mp))
    $ \ts -> prop pplus ts

main = main_test

-- | Property

data DescTag = Stack Int
             | Instr
             deriving (Eq, Show)

data StateDesc = SD { pcdepth :: !Int
                    , memdepth :: Map Integer DescTag
                    , stack :: [(Integer, Integer)]
                    , callinstrs :: [Integer]
                    } 

initDesc :: Map Integer DescTag -> [Integer] -> StateDesc
initDesc memlayout calls = SD { pcdepth = 0
                              , memdepth = memlayout
                              , stack = []
                              , callinstrs = calls
                              } 

accessible :: Integer -> StateDesc -> Bool
accessible i sd =
  -- can error
  case memdepth sd Map.! i of
    (Stack n) -> n >= pcdepth sd
    _ -> False

-- TODO: Fix this
next_desc :: RichState -> StateDesc -> RichState -> StateDesc
next_desc s d s'
  | memdepth d Map.! (s ^. ms . fpc) == Instr =
    let isCall = elem (s ^. ms . fpc) (callinstrs d)
        isRet  = ((s' ^. ms . fpc) == 4 + fst (head $ stack d)) &&
                 (Just (snd (head $ stack d)) == (s ^. ms . fgpr . at sp))
        -- Should return Just (memory loc) if it is a write, Nothing otherwise
        isWrite = Nothing -- FIX
    in
    d { pcdepth =
          if isCall then
            pcdepth d + 1
          else if isRet then 
            pcdepth d - 1
          else pcdepth d
      , memdepth =
          case isWrite of
            Just loc -> Map.insert loc (Stack (pcdepth d)) (memdepth d)
            _ -> memdepth d
      , stack =
          if isCall then
            (s ^. ms . fpc, fromJust (s ^. ms . fgpr . at sp)) : stack d
          else if isRet then 
            tail $ stack d
          else stack d
      } 

-- TODO: Rephrase indistinguishability to only look at clean locs?
prop_NI :: PolicyPlus -> Int -> TestState () -> Property
prop_NI pplus maxCount ts =
--  whenFail (putStrLn $ printTestState pplus ts)
--           False
  let (trace,err) = traceExec pplus ts maxCount in
  whenFail (do putStrLn "Trace:"
               putStrLn $ printTrace pplus trace
           ) $ length trace <= 4
--  allWhenFail (\ts tss -> --tss is reversed here
--                 (whenFail (do putStrLn "Indistinguishable tags found!"
--                               putStrLn "Original Test State:"
--                               putStrLn $ printTestState pplus ts
--                               putStrLn " Trace:"
--                               putStrLn $ printTrace pplus $ reverse tss
--                           ) $ (indistinguishable (== taintTag) ts))
--                 .&&.
--                 (whenFail (do putStrLn $ "Clean tags set differs."
--                               putStrLn $ "Original: " ++ show clean
--                               putStrLn $ "Current:  " ++ show clean'
--                               putStrLn "Original Test State:"                               
--                               putStrLn $ printTestState pplus ts
--                               putStrLn " Trace:"                               
--                               putStrLn $ printTrace pplus $ reverse tss                               
--                           ) $ (clean == clean'))
--              ) (takeWhile pcInSync trace)

prop :: PolicyPlus -> TestState () -> Property
prop pplus ts = prop_NI pplus maxInstrsToGenerate ts



module Eval where

import System.IO
import Control.Monad (forM_, msum)
import qualified Data.Map as M
import qualified Data.Set as S

import AST
import Validate
import Generator
import PolicyModules
import Symbols
import CommonFn 

-- import qualified EvalSeq as ES
import qualified EvalSeq2 as ES2
import EvalCommon
-- import ASTPpr

-- fstModule (Right [x]) = x

type QPol = PolicyDecl QSym
type QPolMod = (ModName, QPol, ModSymbols)

loadPolicyAndModules :: String -> IO (Maybe QPolMod)
loadPolicyAndModules polName = do
  let opts = defaultOptions
  -- modules :: Right [ModuleDecl QSym]
  -- Does all the IO to load dependent modules
  parsedModules <- getAllModules opts [polName]
  case parsedModules of
    Left errs -> do
       pe "\nError during module loading."
       pe $ unlines errs
       pure Nothing
    Right modules ->
       case buildSymbolTables modules of
          Left errs -> do
             pe "\nError building Symbol Tables."
             pe $ unlines errs
             pure Nothing
          Right symbols -> do
             case locateMain [polName] symbols of
               Left errs -> do
                 pe "\nError unable to locate main policy."
                 pe $ unlines errs
                 pure Nothing
               Right (mainModule, mainPolicyDecl) -> do
                 case validateMain symbols mainModule mainPolicyDecl of
                   Left errs -> do
                     pe "\nError unable to validate main policy."
                     pe $ unlines errs
                     pure Nothing
                   Right _ -> 
                     pure $ Just (mainModule, mainPolicyDecl, symbols)


evalPolMod :: QPolMod -> TaggedInstr -> TagResult OperandTags
evalPolMod (mn,pd,sts) instr = ES2.evalPolicyExS sts mn instr pex
 where
  PolicyDecl _ _ _ pex = pd 
--  gds = concatMap (onlyGroupDecls . modDecl . snd) mods

{-
testMain :: IO ()
testMain = do
  Just pms <- loadPolicyAndModules "dpltests.hspec.failCk"
  Just cfiP <- loadPolicyAndModules "dpltests.cfi.cfiPol"

  -- pAll pms
  -- testFst pms
  testCfi cfiP

  pure ()
 where
  pAll (pol, mods) = do
    let myTags = allTags mods
    prints "mods" mods
    prints "tags" myTags
    prints "pol" [pol]

  testFst pms = do
    let
      rTag = QTag ["dpltests", "hspec", "R"]
      wTag = QTag ["dpltests", "hspec", "W"]
      instr = ("lw", wrapESKMap $ M.fromList [(RS1, S.empty), (Mem, S.singleton rTag)])
      instr2 = ("lw", wrapESKMap $ M.fromList [(RS1, S.empty), (Mem, S.empty)])
    print $ evalPolMod pms instr
    print $ evalPolMod pms instr2

  testCfi pms = do
    let
      jTag = QTag ["dpltests", "cfi", "Jumping"]
      tTag = QTag ["dpltests", "cfi", "Target"]
      instr = ("beq", M.fromList [
        (wrapTK RS1, S.empty),
        (wrapTK RS2, S.empty),
        (Right ESKCode, S.empty),
        (Right ESKEnv, S.empty)])
    print $ evalPolMod pms instr

-}

{-
	-- Expand named policy expressions.
elabPolicy :: [(ModName, SymbolTable QSym)] -> ModName -> PolicyDecl QSym -> PolicyDecl QSym
elabPolicy st mn (PolicyDecl p pl pqn ex) =  PolicyDecl p pl pqn $ elabPEx ex
   where
    elabPEx :: PolicyEx QSym -> PolicyEx QSym
    elabPEx (PERule sp clause) = PERule sp clause
    elabPEx (PECompModule sp lhs rhs)    = PECompModule sp (elabPEx lhs) (elabPEx rhs)
    elabPEx (PECompExclusive sp lhs rhs) = PECompExclusive sp (elabPEx lhs) (elabPEx rhs)
    elabPEx (PECompPriority sp lhs rhs)  = PECompPriority sp (elabPEx lhs) (elabPEx rhs)
    elabPEx (PEVar _ qn) = elabPEx rex
      where (_,PolicyDecl _ _ _ rex) = getPolicy st mn qn
-}

{-
-- Also qualify the opgroups for consumption for EvalSeq2.
onlyGroupDecls :: QModuleDecl -> [QGroupDecl]
onlyGroupDecls (ModuleDecl _ modName ss) = concatMap onlyG ss
 where
  onlyG = \case
    Groups gs -> map qG gs
    _ -> []

  qG (GroupDecl s gName srcs dsts isas) =
    GroupDecl s (qualifyQSym modName gName) srcs dsts isas
--  prep xs (QGroup ys) = QGroup $ xs ++ ys
-}


{-
allTags :: ModSymbols -> [TagDecl QSym]
allTags ms = map snd tags
  where
    tags = concatMap (tagSyms . snd) ms
-}
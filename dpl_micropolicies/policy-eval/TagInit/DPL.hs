{-# LANGUAGE TemplateHaskell #-}

-- Initial tags specified by DPL 

module TagInit.DPL where

import Control.Lens
import qualified Data.Map as M

import AST
import Symbols
import Eval
import EvalCommon
import CommonFn (parseDotName)

type QRequireDecl = RequireDecl QSym
type QInitSet = InitSet QSym

kRZeroKey = parseDotName "ISA.RISCV.Reg.RZero"
kRDefaultKey = parseDotName "ISA.RISCV.Reg.Default"
kCsrDefaultKey = parseDotName "ISA.RISCV.CSR.Default"
kPcKey = parseDotName "ISA.RISCV.Reg.Env"
kUserHeapKey = parseDotName "dover.Kernel.MemoryMap.UserHeap"

data MachTagInit a
  = MachTagInit {
    _mtiRegDefault :: a,
    _mtiCsrDefault :: a,
    _mtiRegZero :: a,
    _mtiPc :: a,
    _mtiUserHeap :: a
  }

makeLenses ''MachTagInit

emptyMachTagInit = MachTagInit Nothing Nothing Nothing Nothing Nothing

extractMachTagInit :: QPolMod -> MachTagInit (Maybe TagValue)
extractMachTagInit (_, _, mods)
  = foldr (.) id (map applyReq rds) emptyMachTagInit
 where
  rds = concatMap (onlyRequireDecls . modDecl . snd) mods
  applyReq (Init _ key (Just . iSetToTag -> t))
    | key == kRZeroKey = mtiRegZero .~ t
    | key == kRDefaultKey = mtiRegDefault .~ t
    | key == kCsrDefaultKey = mtiCsrDefault .~ t
    | key == kPcKey = mtiPc .~ t
    | key == kUserHeapKey = mtiUserHeap .~ t
    | otherwise = id

-- XXX: this throws away all TagFields
iSetToTag (ISExact _ ts) = M.fromList (zip (map qsym ts) (repeat Nothing))

onlyRequireDecls :: QModuleDecl -> [QRequireDecl]
onlyRequireDecls (ModuleDecl _ _modName ss) = concatMap onlyR ss
 where
  onlyR = \case
    Require gs -> gs
    _ -> []

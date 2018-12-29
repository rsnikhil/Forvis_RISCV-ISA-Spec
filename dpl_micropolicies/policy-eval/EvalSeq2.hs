module EvalSeq2 where

import EvalCommon
import EvalRule

import Debug.Trace
import System.IO
import Control.Monad (forM_, msum)
import qualified Data.Map as M
import qualified Data.Set as S

import AST
import Validate
import Generator
import PolicyModules
import Symbols


-- evalMainPolicyS = evalPolicyExS

evalPolicyExS :: ModSymbols 
                -- ^ Global symbol table
                -> ModName
                -- ^ Module name
                -> EvalArgs
                -- ^ Instr and tags
                -> PolicyEx QSym
                -- ^ policy expression
                -> TagResult EvalRes
evalPolicyExS sts mn ti@(instr,optags) = go 
 where 
  go (PECompPriority _ p1 p2) = go p1 `orI` go p2

  -- TODO: Split the ctx so that p1 and p2 only see
  -- their relevant tags and check really is no intersection in the result
  go (PECompExclusive _ p1 p2) = M.union <$> go p1 <*> go p2
  go (PECompModule _ p1 p2) = M.union <$> go p1 <*> go p2

  go (PERule _ rule@(RuleClause _ ruleGroup _ _ )) =
    case matchGroup sts mn ti ruleGroup of
      Just (ctx, putBack) -> evalRuleClause (resolveQSym sts mn) ctx rule >>= putBack
      Nothing -> failI
  go (PEVar _ qn) = go p
    where (_,PolicyDecl _ _ _ p) = getPolicy sts mn qn
  go (PENoChecks _) = pure optags -- maybe not quite the right thing to do
 
{- was:
evalPolicyExS ti gs pex = case pex of
  PECompPriority _ p1 p2 -> go p1 `orI` go p2
   -- TODO: Split the ctx so that p1 and p2 only see
   -- their relevant tags and check really is no intersection in the result
  PECompExclusive _ p1 p2 -> M.union <$> go p1 <*> go p2
  PECompModule _ p1 p2 -> M.union <$> go p1 <*> go p2
  PERule _ rule -> case matchGroup ti gs (ruleGroup rule) of
    Just (ctx, putBack) -> evalRuleClause ctx rule >>= putBack
    Nothing -> failI
 where
  go = evalPolicyExS ti gs
  ruleGroup (RuleClause _ name _ _) = name
-}

type PutBackS = TagContext -> TagResult OperandTags

matchGroup :: ModSymbols -> ModName
           -> TaggedInstr ->  QSym
           -> Maybe (TagContext, PutBackS) 
matchGroup sts mn ti@(instr, optags) groupExpected = res
 where
  res = do
    (ctx, putBack') <- runGD gd
    let
      putBack ctx = case putBack' ctx of
        Nothing -> failE $
          "missing required output for group " ++ show groupExpected
        Just x -> pure x
    pure (ctx, putBack)
  (_, gd) = getGroup sts mn groupExpected    
  runGD (GroupDecl _ _ srcs dsts isas) =
    -- trace (show gName) $
    if instr `elem` map isaInstr isas
      then pure (
        -- First fill in code/env, then fill other things
        foldr fillSrc (tcFromEskMap optags) srcs,
        -- First extract in other things, finally extract code/env
        \m -> M.union (tcToEskMap m) <$> extractDsts dsts m)
      else Nothing
  isaInstr (Asm _ instrName _) = instrName
  fillSrc (GroupParam _ loc nameInCtx) =
    M.insert nameInCtx (optags M.! (wrapTK loc))
  extractDsts dsts ctx = pure $ foldr (extractDst ctx) M.empty dsts
  extractDst ctx (GroupParam _ loc nameInCtx) =
    M.insert (wrapTK loc) (ctx M.! nameInCtx)

{-
-- Find the first opgroup that matches this instruction.
matchGroup :: TaggedInstr -> [QGroupDecl] -> QSym
           -> Maybe (TagContext, PutBackS) 
matchGroup ti@(instr, optags) gs groupExpected = res
 where
  res = do
    (ctx, putBack') <- runGs gs
    let
      putBack ctx = case putBack' ctx of
        Nothing -> failE $
          "missing required output for group " ++ show groupExpected
        Just x -> pure x
    pure (ctx, putBack)
  runGs gs = msum (map runGD gs)
  runGD (GroupDecl _ gName srcs dsts isas) =
    -- trace (show gName) $
    if instr `elem` map isaInstr isas
        && gName == groupExpected
      then pure (
        -- First fill in code/env, then fill other things
        foldr fillSrc (tcFromEskMap optags) srcs,
        -- First extract in other things, finally extract code/env
        \m -> M.union (tcToEskMap m) <$> extractDsts dsts m)
      else Nothing
  isaInstr (Asm _ instrName _) = instrName
  fillSrc (GroupParam _ loc nameInCtx) =
    M.insert nameInCtx (optags M.! (wrapTK loc))
  extractDsts dsts ctx = pure $ foldr (extractDst ctx) M.empty dsts
  extractDst ctx (GroupParam _ loc nameInCtx) =
    M.insert (wrapTK loc) (ctx M.! nameInCtx)

-}
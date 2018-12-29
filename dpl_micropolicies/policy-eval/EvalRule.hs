module EvalRule where

import qualified Data.Map as M
import Debug.Trace

import AST
import Validate
import Generator
import PolicyModules
import Symbols

import EvalCommon

evalRuleClause :: (QSym -> QSym) -> TagContext -> RuleClause QSym -> TagResult TagContext
evalRuleClause resolveTagQSym ctx (RuleClause _ _ pats res) =
  if all (matchBoundGroupPat resolveTagQSym ctx) pats
    then evalRuleResult resolveTagQSym ctx res
    -- Unmatched pattern is an implicit failure
    else failI

evalRuleResult resolveTagQSym ctx rr = case rr of
  -- Result result failure is explicit
  RRFail _ reason -> failE reason
  RRUpdate _ gexs -> do
    -- Can't do inplace update as rules might depend on overwritten values
    newCtx <- foldr (evalBoundGroupEx resolveTagQSym ctx) (pure M.empty) gexs
    pure $ M.union newCtx ctx  -- left-biased, i.e. prefer newCtx

evalBoundGroupEx resolveTagQSym ctx (BoundGroupEx _ key ex) mNewCtx = do
  newCtx <- mNewCtx
  r <- evalTagSetEx resolveTagQSym ctx ex
  pure (M.insert key r newCtx)

evalTagSetEx resolveTagQSym ctx ex = case ex of
  -- TODO: make this a static check.
  TSEVar _ v -> pure $ lookupOrThrow "evalTagSetEx" v ctx
  TSEExact _ ts -> M.fromList <$> mapM (evalTag resolveTagQSym) ts
  TSEModify _ tse mods -> foldr modify (go tse) mods
  TSEUnion _ t1 t2 -> M.union <$> go' t1 <*> go' t2
  TSEIntersect _ t1 t2 -> M.intersection <$> go' t1 <*> go' t2
 where
  go = evalTagSetEx resolveTagQSym ctx
  go' tse = do
    r <- go tse
    pure (noFieldOrThrow r)
  modify m ex = case m of
    -- shorthand
    TagEx _ t -> do
      (k, v) <- evalTag resolveTagQSym t
      modTag (M.insert k v)
    TagPlusEx _ t -> do
      (k, v) <- evalTag resolveTagQSym t
      modTag (M.insert k v)
    TagMinusEx _ t -> do
      (k, _) <- evalTag resolveTagQSym t
      modTag (M.delete k)
   where
    modTag f = f <$> ex


evalTag resolveTagQSym t = case t of
  Tag _ t [] -> pure (resolveTagQSym t, Nothing)
  Tag _ t [tf] -> do
    i <- evalTagField tf
    pure (resolveTagQSym t, Just i)
 where
  evalTagField = \case 
    TFTag {} -> failE "Nested TFTag not supported"
    TFVar {} -> failE "TFVar not supported yet" -- TODO
    TFAny {} -> failE "TFAny in expression position"
    TFNew _ -> mkUnique

matchBoundGroupPat resolveTagQSym ctx (BoundGroupPat _ key rule) =
  -- trace (show (ctx, key)) $
  matchTagSetPat resolveTagQSym (lookupOrThrow "matchBoundGroupPat" key ctx) rule

matchTagSetPat resolveTagQSym val pat = case pat of
  TSPAny _ -> True
  TSPExact _ ts -> val == M.fromList (zip (map (resolveTagQSym . extractTag) ts) (repeat Nothing))
  TSPAtLeast _ texs -> all (matchTSPAtLeast resolveTagQSym val) texs

matchTSPAtLeast resolveTagQSym val tex = case tex of
  TagEx _ t -> ckTag M.member (fmap resolveTagQSym t) -- shorthand
  TagPlusEx _ t -> ckTag M.member (fmap resolveTagQSym t)
  TagMinusEx _ t -> ckTag M.notMember (fmap resolveTagQSym t)
 where
  ckTag = matchTag val

-- TODO: match tagfields and record TFVar mapping
matchTag val ck t = ck (extractTag t) val

extractTag (Tag _ t _) = t

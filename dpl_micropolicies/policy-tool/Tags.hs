{-
 - Copyright © 2017-2018 The Charles Stark Draper Laboratory, Inc. and/or Dover Microsystems, Inc.
 - All rights reserved. 
 -
 - Use and disclosure subject to the following license. 
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 - 
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 - 
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 - LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 - OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 - WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 -}
{-# LANGUAGE RankNTypes, NamedFieldPuns #-}
module Tags (setTags,buildTagInfo,TagInfo(..)) where

-- -- import CommonTypes
import CommonFn
import AST
import Symbols
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Word
import Data.List (sort,foldl')
import Data.Maybe


-- This struct collects a bunch of info about the tags used by the current
-- policy (and its imports).  It is meant to be computed once and passed around.
--
-- XXX The policy masks should really be put in here and used as needed rather
-- than being defined as C variables.  Not doing this now because this version
-- of the language is unlikely to stick around.
data TagInfo =
  TagInfo {tiMaxTag :: Word32,
           -- The "biggest tag".  That is, the last position in the bitfield
           -- that is used.

           tiNumBitFields :: Word32,
           tiNumDataArgs  :: Word32,
           tiArrayLength    :: Word32,
           -- The number of Word32s used as bitfields and argument holders,
           -- respectively, in the tag struct's array.  The sum of these two
           -- numbers is the size of a tag struct, in words.

           tiTagNames :: [QSym],
           -- All tag names.  The domain of tiTagBitPositions

           tiGroupNames :: [QSym],
           -- All opgroup names.  A subset of tiTagNames.

           tiTagBitPositions :: M.Map QSym Word32,
           -- A map from tag names to the position of the bit that indicates the
           -- tag's presence.

           tiTagArgInfo :: M.Map QSym [(Word32,TypeDecl QSym)]
           -- A map from tag names to array indexes.  If a tag maps to
           -- [(i1,t1),(i2,t2)], then this tag has two data arguments, which are
           -- kept at positions i1 and i2 of the tag array and have types t1 and
           -- t2.
          }
  deriving (Show)

{-
usedModules :: ModSymbols -> ModName -> Maybe (PolicyDecl QSym) -> S.Set ModName
usedModules _ _ Nothing = S.empty
usedModules ms topMod(Just pd) = policyDeclModules ms topMod pd

policyDeclModules :: ModSymbols -> ModName -> PolicyDecl QSym -> S.Set ModName
policyDeclModules ms modN (PolicyDecl _ _ qs pe) =
  S.insert modN $ policyExModules ms modN pe

policyExModules :: ModSymbols -> ModName -> PolicyEx QSym -> S.Set ModName
policyExModules ms modN (PEVar _ v) = let (modN', p) = getPolicy ms modN v in  policyDeclModules ms modN' p
policyExModules ms modN (PECompExclusive _ pe1 pe2) =
  S.union (policyExModules ms modN pe1) (policyExModules ms modN pe2)
policyExModules ms modN (PECompPriority _ pe1 pe2) =
  S.union (policyExModules ms modN pe1) (policyExModules ms modN pe2)
policyExModules ms modN (PECompModule _ pe1 pe2) =
  S.union (policyExModules ms modN pe1) (policyExModules ms modN pe2)
policyExModules ms modN (PERule _ (RuleClause _ og _ _ )) = let (modN', p) = getPolicy ms modN og in S.singleton modN'
-}

setTags :: InitSet t -> [Tag t]
setTags (ISExact _ ts) = ts

-- This constructs the tag info.  It assumes the ModSymbols it is passed
-- contains only the relevant modules.  Any tags in the modules it is passed
-- will appear in the generated code.

buildTagInfo :: ModSymbols -> [(ModName, QSym)] -> TagInfo
buildTagInfo ms allSyms = 
  TagInfo {tiMaxTag,
           tiNumBitFields,
           tiNumDataArgs,
           tiArrayLength = tiNumBitFields + tiNumDataArgs,

           tiTagNames = map tagName declaredTags,
           tiGroupNames = map tagName ogFakeTagDecls,
           tiTagBitPositions = M.fromList $ zip (map tagName declaredTags)
                                                [minTagNumber..],
           tiTagArgInfo = dataArgInfo}
  where
    tagName (mn, td) = qualifyQSym mn $ qsym td
    tiMaxTag,tiNumBitFields,tiNumDataArgs :: Word32
    tiMaxTag = minTagNumber + (fromIntegral $ length declaredTags) - 1
    tiNumBitFields = 1 + (div tiMaxTag 32)
    tiNumDataArgs = fromIntegral $ length $
       concatMap (\(_, TagDecl _ _ args) -> args) declaredTags
    
    -- This is all the tags that are explicitly declared in modules that this
    -- policy uses bits from, plus fake declarations for the opgroups, since
    -- they are really tags.
    declaredTags :: [(ModName, TagDecl QSym)]
    declaredTags = sort $ actualDecls ++ ogFakeTagDecls
      where
        actualDecls :: [(ModName, TagDecl QSym)]
        actualDecls = map (tagDecl ms) $ usedTags  allSyms

    ogFakeTagDecls :: [(ModName, TagDecl QSym)]
    ogFakeTagDecls = mapMaybe grpDecls allSyms
      where
        grpDecls :: (ModName, QSym) -> Maybe (ModName, TagDecl QSym)
        grpDecls (mn, qg@(QGroup _)) = let (mn', gd) = getGroup ms mn qg in Just (mn', makeOGDecl gd)
        grpDecls _ = Nothing
        makeOGDecl :: GroupDecl a QSym -> TagDecl QSym
        makeOGDecl (GroupDecl sp nm _ _ _) = TagDecl sp (groupPrefix nm) []
        
--    relevantTags :: [TagDecl QSym]
--    relevantTags = sort $
--      mapMaybe (\tdcl -> if S.member (qsym tdcl) usedTags then Just tdcl else Nothing)
--               declaredTags

    findTypeDef :: ModName -> QSym -> TypeDecl QSym
    findTypeDef modN qs = let (_, td) = getType ms modN qs in td

    dataArgInfo :: M.Map QSym [(Word32,TypeDecl QSym)]
    dataArgInfo = fst $ foldl' folder (M.empty,tiNumBitFields) declaredTags
      where
        folder :: (M.Map QSym [(Word32,TypeDecl QSym)], Word32)
               -> (ModName, TagDecl QSym)
               -> (M.Map QSym [(Word32,TypeDecl QSym)], Word32)
        folder (accMap,nextPos) (modN, TagDecl _ nm typs) =
          (M.insert (qualifyQSym modN nm) (zip [nextPos..] (map (findTypeDef modN) typs)) accMap,
           (fromIntegral $ length typs) + nextPos)
        
minTagNumber :: Word32
minTagNumber = 16

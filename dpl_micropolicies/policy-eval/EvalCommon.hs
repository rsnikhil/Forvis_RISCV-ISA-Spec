module EvalCommon where

import System.IO
import Control.Monad (forM_, msum)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import AST
import Validate
-- import Generator
import PolicyModules
import Symbols

-- Actually should be named TagSetValue... Mapping from tag name
-- to its field. XXX: Can we have multiple fields?
type TagValue = M.Map QSym (Maybe Int)

-- Models failure and contains a unique int generator
type TagResult a = EitherT TagFailure (State Int) a

data TagFailure
  = TFExplicit String
  | TFImplicit
  deriving (Show, Eq)

failE :: String -> TagResult a
failE = left . TFExplicit

failI :: TagResult a
failI = left TFImplicit

orI :: TagResult a -> TagResult a -> TagResult a
orI ma mb = EitherT $ do
  a <- runEitherT ma
  case a of
    Left TFImplicit -> runEitherT mb
    _ -> pure a

mkUnique :: TagResult Int
mkUnique = do
  i <- lift $ get
  lift $ put (i + 1)
  pure i

justOrThrow :: Maybe a -> String -> a
justOrThrow m e = fromMaybe (error e) m

lookupOrThrow callsite k m = M.lookup k m `justOrThrow`
  (callsite ++ ": No such key " ++ show k)

noFieldOrThrow :: TagValue -> TagValue
noFieldOrThrow tvs = if hasField then error "throw: hasField" else tvs
 where
  hasField = any isJust $ M.elems tvs
  isJust (Just _) = True
  isJust _ = False

-- Map from operands to tags, used externally for instruction input and output
type OperandTags = M.Map OpTagKey TagValue

type OpTagKey = Either TagSpec ExtraStateKey

wrapTK :: TagSpec -> OpTagKey
wrapTK = Left

unwrapTK :: OpTagKey -> TagSpec
unwrapTK (Left x) = x

wrapESKMap :: Ord k => M.Map k a -> ESKMap k a
wrapESKMap = M.mapKeys Left

type ESKMap k a = M.Map (Either k ExtraStateKey) a

data ExtraStateKey = ESKCode | ESKEnv
  deriving (Show, Eq, Ord)

allEsk = [ESKCode, ESKEnv]

eskOnly :: Ord k2 => ESKMap k1 a -> ESKMap k2 a
eskOnly = M.foldrWithKey comb M.empty
 where
  comb k v = case k of
    Right x -> M.insert (Right x) v
    _ -> id

tagSpecOnly :: Ord k1 => ESKMap k1 a -> M.Map k1 a
tagSpecOnly = M.foldrWithKey comb M.empty
 where
  comb k v = case k of
    Left x -> M.insert x v
    _ -> id

tcFromEskMap :: OperandTags -> TagContext
tcFromEskMap = M.foldrWithKey comb M.empty
 where
  comb k v = case k of
    Right ESKCode -> M.insert (QVar ["code"]) v
    Right ESKEnv -> M.insert (QVar ["env"]) v
    _ -> id

tcToEskMap :: TagContext -> OperandTags
tcToEskMap = M.foldrWithKey comb M.empty
 where
  comb k v = case k of
    QVar ["code"] -> M.insert (Right ESKCode) v
    QVar ["env"] -> M.insert (Right ESKEnv) v
    _ -> id

-- Name, TagSpec->Tag
type TaggedInstr = (String, OperandTags)
type EvalArgs = TaggedInstr
type EvalRes = OperandTags

-- Map from names to tags, used internally by policy rules
type TagContext = M.Map QSym TagValue

-- Common type synonyms from policy-tool
type QGroupDecl = GroupDecl [ISA] QSym
type QModuleDecl = ModuleDecl QSym

runTagResult :: Int -> TagResult a -> (Either TagFailure a, Int)
runTagResult gen m = runState (runEitherT m) gen

pe = hPutStrLn stderr

prints tag xs = do
  pe $ "\n" ++ tag
  forM_ (zip [1..] xs) $ \(i, x) -> pe $ "\n[" ++ show i ++ "] " ++ tag ++ ": " ++ show x


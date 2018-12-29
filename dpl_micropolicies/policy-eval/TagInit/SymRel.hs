{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TagInit.SymRel where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Control.Lens
import qualified Data.Map as M

import EvalCommon
import AST

kSymRelKey :: T.Text
kSymRelKey = "symrels"

data SymRels
  = SymRels [SymRelTag]
  deriving (Show, Eq)

data SymRelTag
  = SymRelTag {
    -- | The name of the symbol.
    _srtName :: B.ByteString,

    -- | The nth instruction to tag. 0-based.
    _srtInstrIx :: Int,

    -- | The tag value to use.
    _srtTag :: TagValue
  }
  deriving (Show, Eq)

makeLenses ''SymRelTag

-- TODO: enrich this to at least the level of policy definition langauge
parseTag :: [String] -> TagValue
parseTag ts = M.singleton (QTag ts) Nothing

instance FromJSON SymRels where
  parseJSON (Y.Object v) = SymRels <$>
    v .: kSymRelKey
  parseJSON _ = fail "Expected Object for SymRels value"

instance FromJSON SymRelTag where
  parseJSON (Y.Object v) = SymRelTag <$>
    (B8.pack <$> v .: "name") <*>
    v .: "instrIx" <*>
    (parseTag <$> v .: "tag")
  parseJSON _ = fail "Expected Object for SymRelTag value"


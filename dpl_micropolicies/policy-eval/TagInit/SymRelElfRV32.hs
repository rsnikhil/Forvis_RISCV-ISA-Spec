module TagInit.SymRelElfRV32 where

import Data.Elf
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

getSymTab path = do
  bs <- B.readFile path
  let
    ef = parseElf bs
    (concat -> st) = parseSymbolTables ef
  pure st

entryHasName name (EST {..}) = mbs == Just bs
 where
  (_, mbs) = steName
  bs = B8.pack name

-- XXX: This is a linear scan. Consider populating a hashmap or bst (probably
-- in getSymTab) instead?
entryOffsetForName st name = steValue <$> entry
 where
  (listToMaybe -> entry) = filter (entryHasName name) st

-- Map instruction offset to byte offset.
-- XXX: Is RV instructions always 4 bytes wide?
ixToOffset ix = ix * 4


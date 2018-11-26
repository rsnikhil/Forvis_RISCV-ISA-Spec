module Elf where

-- This code is copied from MIT's riscv-semantics repo:
--     https://github.com/mit-plv/riscv-semantics
-- and edited here for personal style preferences.

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Elf
import Data.Word

-- External functions
-- read_elf returns the data loaded by an elf file as a list of tuples of byte
-- addresses and bytes of data.
read_elf :: FilePath -> IO [(Integer, Integer)]
read_elf f = do
    bytes <- B.readFile f
    return $ translate_elf $ parseElf bytes

-- read_elf_symbol takes a symbol name and returns its address if that symbol
-- exists in the elf file.
read_elf_symbol :: String -> FilePath -> IO (Maybe Integer)
read_elf_symbol symbolName f = do
    bytes <- B.readFile f
    return $ find_symbol_address  symbolName $ parseElf bytes

-- Internal functions
translate_elf :: Elf -> [(Integer, Integer)]
translate_elf e = concat $ map translate_elf_segment $ elfSegments e

translate_elf_segment :: ElfSegment -> [(Integer, Integer)]
translate_elf_segment s =
    if elfSegmentType s == PT_LOAD
        then address_each_byte (fromIntegral $ elfSegmentPhysAddr s) (B.unpack $ elfSegmentData s)
        else []

address_each_byte :: Integer -> [Word8] -> [(Integer, Integer)]
address_each_byte addr (b:bs) = (addr, fromIntegral b) : (address_each_byte (addr+1) bs)
address_each_byte addr [] = []

find_symbol_address :: String -> Elf -> Maybe Integer
find_symbol_address  symbol_name  elf =
    foldl mplus Nothing $ map (symbol_table_filter symbol_name) $ concat $ parseSymbolTables elf

symbol_table_filter :: String -> ElfSymbolTableEntry -> Maybe Integer
symbol_table_filter  symbol_name  ste =
    if (snd (steName ste)) == (Just $ BC8.pack  symbol_name)
        then Just $ fromIntegral (steValue ste)
        else Nothing

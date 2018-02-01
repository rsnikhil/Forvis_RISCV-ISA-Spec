module Main_RunProgram where

-- ================================================================
-- This is the 'main' function for the use-case where we simply
-- want to load and ELF file and run it to completion:
--   - Reads a list of filenames on the command-line
--         (RISC-V ELF executable files, or hex-mem images if ending with ".hex")
--   - Executes each RISC-V program using RunProgram()

-- ================================================================
-- Standard Haskell imports

import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.List
import Data.Word
import Data.Bits
import Numeric (showHex, readHex)

-- Project imports

import Elf
import ReadHexFile 

import ArchDefs64
import ArchState64
import RunProgram

-- ================================================================

usage_help :: String
usage_help = "This program expects one or more RISC-V ELF or .hex files as command-line arguments"

main_RunProgram :: IO ()
main_RunProgram = do
  args <- getArgs
  retval <- case args of
              [] -> do
                putStrLn usage_help
                return 1

              "-h":files -> do
                putStrLn usage_help
                return 1

              "-help":files -> do
                putStrLn usage_help
                return 1

              "--help":files -> do
                putStrLn usage_help
                return 1

              [file] -> runFile file

              files -> runFiles files

  exitWith (if retval == 0 then
              ExitSuccess
            else
              ExitFailure (fromIntegral retval))

-- Run all the RISC-V ELF/Hex programs in the list of file arguments

runFiles :: [String] -> IO Int64
runFiles (file:files) = do
    myreturn <- runFile file
    putStr (file ++ ": " ++ (show myreturn) ++ "\n")
    othersreturn <- runFiles files
    if myreturn /= 0
        then return myreturn
        else return othersreturn
runFiles [] = return 0

-- Run the RISC-V ELF/Hex program in file argument

runFile :: String -> IO Int64
runFile f = do
  addr_byte_list <- readProgram f

  -- Debug dump
  -- mapM_ (\(addr,byte) -> putStrLn (showHex addr ":" ++ showHex byte "")) addr_byte_list

  let initial_PC = 0x80000000
      astate1    = mkArchState64  initial_PC  addr_byte_list

  -- Set verbosity: 0: quiet (only console out); 1: also instruction trace; 2: also CPU arch state
  -- TODO: make this a command-line argument
  astate2 <- set_ArchState64_verbosity  astate1  0

  putStrLn "Running program up to 1,000,000 instructions"
  runProgram  1000000  astate2
  return 0

-- ================================================================
-- Read an RISC-V ELF/Hex file and return a memory (list of (addr,byte))

readProgram :: String -> IO ([(Int, Word8)])
readProgram f = do
  if ".hex" `isSuffixOf` f
    then do
    mem <- readHexFile f
    return mem
    else do
    mem <- readElf f
    return mem

-- ================================================================

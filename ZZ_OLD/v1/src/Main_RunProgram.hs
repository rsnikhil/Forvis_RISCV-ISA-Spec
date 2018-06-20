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
import System.Console.GetOpt    -- getOpt, usageInfo, ArgOrder, OptDescr
import System.Exit
import Data.Int
import Data.List
import Data.Word
import Data.Bits
import Numeric (showHex, readHex, readDec)

-- Project imports

import Elf
import ReadHexFile 

import ArchDefs
import ArchState
import RunProgram
import Memory

-- ================================================================

usage_help :: String
usage_help = "This program expects one or more RISC-V ELF or .hex files as command-line arguments"

main_RunProgram :: IO ()
main_RunProgram = do
  putStrLn "Type -h, -H or --help for help."
  progName <- getProgName
  args     <- getArgs
  let (opts, filenames, unrecognizeds, errs) = getOpt' RequireOrder options args
  {- DEBUG
      putStrLn ("Opts " ++ show opts)
      putStrLn ("Other " ++ show extras)
      putStrLn ("Unrecognizeds " ++ show unrecognizeds)
      putStrLn ("Errs " ++ show errs)
  -}

  let usage_header     = "Usage: " ++ progName ++ "  <flags>  <executable files>\n" ++
                         "  <executable files>:  one or more RISC-V ELF and/or Verilog mem-hex files\n" ++
                         "  <flags>:"
      usage_string     = usageInfo  usage_header   options
      has_help         = elem  Opt_Help  opts
      errs2            = [ err      | Opt_Err err <- opts]
      vs               = [ v | Opt_Verbosity v <- opts ]
      verbosity        = case vs of
                           []   -> 0
                           [v]  -> v
                           v:_  -> v
      nis              = [ n | Opt_Num_Instrs n <- opts ]
      num_instrs       = case nis of
                           []   -> 1000000
                           [n]  -> n
                           n:_  -> n
      rv32             =  elem  Opt_RV32  opts
      rv64             =  elem  Opt_RV64  opts

  if has_help then
    putStr usage_string

    else if unrecognizeds /=[] then
    do
      putStrLn "Command-line has Unrecognized flags:"
      mapM_ (\s -> putStrLn ("    " ++ s)) unrecognizeds

    else if errs /= [] then
    do
      putStrLn "Command-line has parse errors:"
      mapM_ (\s -> putStrLn ("    " ++ s)) errs

    else if errs2 /= [] then
    do
      putStrLn "Command-line hase parse errors:"
      mapM_ (\s -> putStrLn ("    " ++ s)) errs2

    else if length (filenames) == 0 then
    do
      putStrLn "Command-line has no filenames (expecting at least 1)"

    else if rv32 && rv64 then
    do
      putStrLn "Command-line species both RV32 and RV64 (expecting one of them)"

    else if (not rv32) && (not rv64) then
    do
      putStrLn "Command-line species neither RV32 nor RV64 (expecting one of them)"
    else
    do
      let rv = if rv32 then RV32 else RV64
      retval <- runFiles  rv  filenames  num_instrs  verbosity
      exitWith (if retval == 0 then
                   ExitSuccess
                else
                  ExitFailure (fromIntegral retval))

-- ================================================================
-- These are options for System.Console.GetOpt/getOpt

-- Haskell notes: in type 'OptDescr a', the parameter 'a' is a type for the
-- values associated with options. Here, a = MyOptS

data MyOpt = Opt_Help
           | Opt_RV32
           | Opt_RV64
           | Opt_Verbosity   Int
           | Opt_Num_Instrs  Int
           | Opt_Err         String
  deriving (Eq, Show);

-- Haskell notes: type 'ArgDescr a' (3rd arg to Option constructor) has constructors:
--    NoArg x::a
--        The option flag has no option value string following it
--        and it returns the option value of type 'a' provided
--    ReqArg f::(String -> a) s:String
--        The option flag MUST have an option value string following it
--        'f option_value_string' will produce the option value
--        's' is a string is used in the 'usage' message as a place-holder
--            for the option value string
--    OptArg f::(Maybe String -> a) s:String
--        The option flag MAY have an option value string following it
--        'f (Just option_value_string)/Nothing' will produce the option value
--        's' is a string is used in the 'usage' message as a place-holder
--            for the option value string

options :: [OptDescr MyOpt]
options =
     [ Option ['h', 'H']  ["help"]       (NoArg  Opt_Help)                  "help"
     , Option []          ["RV32"]       (NoArg  Opt_RV32)                  "executables are for RV32"
     , Option []          ["RV64"]       (NoArg  Opt_RV64)                  "executables are for RV64"
     , Option ['v']       ["verbosity"]  (ReqArg to_Opt_Verbosity  "<int>") "0 quiet, 1 instr trace, 2 more info"
     , Option ['n']       ["num_instrs"] (ReqArg to_Opt_Num_Instrs "<int>") "max instrs executed (default 1,000,000)"
     ]

to_Opt_Verbosity :: String -> MyOpt
to_Opt_Verbosity s = case (readDec s) of
                       [(n,"")] -> Opt_Verbosity n
                       _        -> Opt_Err s

to_Opt_Num_Instrs :: String -> MyOpt
to_Opt_Num_Instrs s = case (readDec s) of
                        [(n,"")] -> Opt_Num_Instrs n
                        _        -> Opt_Err s

-- ================================================================
-- Run all the RISC-V ELF/Hex programs in the list of file arguments

runFiles :: RV -> [String] -> Int -> Int -> IO Int64
runFiles  rv  (file:files)  num_instrs  verbosity = do
    putStrLn ("Running file: '" ++ file ++ "'")
    myreturn <- runFile  rv  file  num_instrs  verbosity
    putStrLn ("Exitvalue from running file '" ++ file ++ "' is: " ++ (show myreturn))
    othersreturn <- runFiles  rv  files  num_instrs  verbosity
    if myreturn /= 0
        then return myreturn
        else return othersreturn
runFiles _  []  _  _ = return 0

-- Run a single RISC-V ELF/Hex program in file argument

runFile :: RV -> String -> Int -> Int -> IO Int64
runFile  rv  filename  num_instrs  verbosity = do
  addr_byte_list <- readProgram  filename
  let min_addr = minimum (map  (\(x,y) -> x)  addr_byte_list)
      max_addr = maximum (map  (\(x,y) -> x)  addr_byte_list)

  putStrLn ("Initial addr range: " ++ (showHex min_addr "") ++ ".." ++ (showHex max_addr ""))

  -- Debug dump
  -- mapM_ (\(addr,byte) -> putStrLn (showHex addr ":" ++ showHex byte "")) addr_byte_list

  let initial_PC = 0x80000000
      astate1    = mkArchState  rv  initial_PC  addr_byte_list

  -- dumpMem  astate1  0x80002000  0x80002010

  -- Set verbosity: 0: quiet (only console out); 1: also instruction trace; 2: also CPU arch state
  -- TODO: make this a command-line argument
  astate2 <- archstate_verbosity_write  astate1  verbosity

  putStrLn ("Running program up to " ++ show (num_instrs) ++ " instructions")
  runProgram  num_instrs  astate2
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
-- For debugging

dumpMem :: ArchState -> UInt -> UInt -> IO ()
dumpMem  astate  start  end = do
  let f addr = do
          let (load_result, astate') = archstate_mem_read8  astate  addr
              val = case load_result of
                      LoadResult_Ok  x          ->  showHex x ""
                      LoadResult_Err trap_cause ->  show trap_cause
          putStrLn (showHex addr ":" ++ val)
  putStrLn "Memory contents"
  mapM_  f  [start..end]

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

import ArchDefs64
import ArchState64
import RunProgram
import Memory

-- ================================================================

usage_help :: String
usage_help = "This program expects one or more RISC-V ELF or .hex files as command-line arguments"

main_RunProgram :: IO ()
main_RunProgram = do
  putStrLn "Type -h, -H or --help for help."
  args <- getArgs
  let (opts, filenames, unrecognizeds, errs) = getOpt' RequireOrder options args
  {- DEBUG
      putStrLn ("Opts " ++ show opts)
      putStrLn ("Other " ++ show extras)
      putStrLn ("Unrecognizeds " ++ show unrecognizeds)
      putStrLn ("Errs " ++ show errs)
  -}

  let usage_string     = usageInfo  "Optional command line arguments:"   options
      has_help         = elem  Opt_Help  opts
      errs2            = [ err      | Opt_Err err <- opts]
      verbosities      = [ v | Opt_Verbosity v <- opts ]
      verbosity        = case verbosities of
                           []   -> 0
                           [v]  -> v
                           v:_  -> v

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
           putStrLn "Command-line has no filenames (expecting at least 1)"
    else
    do
      retval <- runFiles  filenames  verbosity
      exitWith (if retval == 0 then
                   ExitSuccess
                else
                  ExitFailure (fromIntegral retval))

{-
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

              [file] -> runFile  file  verbosity

              files -> runFiles  files  verbosity

  exitWith (if retval == 0 then
              ExitSuccess
            else
              ExitFailure (fromIntegral retval))
-}

-- ================================================================
-- These are options for System.Console.GetOpt/getOpt

-- Note: in type 'OptDescr a', the parameter 'a' is a type for the
-- values associated with options. Here, a = MyOptS

data MyOpt = Opt_Help
           | Opt_Verbosity  Int
           | Opt_Err        String
  deriving (Eq, Show);

-- Note: type 'ArgDescr a' (3rd arg to Option constructor) has constructors:
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
     [ Option ['v']       ["verbosity"] (ReqArg to_Opt_Verbosity "<int>") "0 quiet, 1 instr trace, 2 more info"
     , Option ['h', 'H']  ["help"]      (NoArg  Opt_Help)                 "help"
     ]

to_Opt_Verbosity :: String -> MyOpt
to_Opt_Verbosity s = case (readDec s) of
                       [(n,"")] -> Opt_Verbosity n
                       _        -> Opt_Err s

-- ================================================================
-- Run all the RISC-V ELF/Hex programs in the list of file arguments

runFiles :: [String] -> Int -> IO Int64
runFiles  (file:files)  verbosity = do
    putStrLn ("Running file: '" ++ file ++ "'")
    myreturn <- runFile  file  verbosity
    putStrLn ("Exitvalue from running file '" ++ file ++ "' is: " ++ (show myreturn))
    othersreturn <- runFiles  files  verbosity
    if myreturn /= 0
        then return myreturn
        else return othersreturn
runFiles []  verbosity = return 0

-- Run a single RISC-V ELF/Hex program in file argument

runFile :: String -> Int -> IO Int64
runFile  filename  verbosity = do
  addr_byte_list <- readProgram  filename
  let min_addr = minimum (map  (\(x,y) -> x)  addr_byte_list)
      max_addr = maximum (map  (\(x,y) -> x)  addr_byte_list)

  putStrLn ("Initial addr range: " ++ (showHex min_addr "") ++ ".." ++ (showHex max_addr ""))

  -- Debug dump
  -- mapM_ (\(addr,byte) -> putStrLn (showHex addr ":" ++ showHex byte "")) addr_byte_list

  let initial_PC = 0x80000000
      astate1    = mkArchState64  initial_PC  addr_byte_list

  -- dumpMem  astate1  0x80002000  0x80002010

  -- Set verbosity: 0: quiet (only console out); 1: also instruction trace; 2: also CPU arch state
  -- TODO: make this a command-line argument
  astate2 <- set_ArchState64_verbosity  astate1  verbosity

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
-- For debugging

dumpMem :: ArchState64 -> WordXLEN -> WordXLEN -> IO ()
dumpMem  astate  start  end = do
  let f addr = do
          let (load_result, astate') = get_ArchState64_mem8  astate  addr
              val = case load_result of
                      LoadResult_Ok  x          ->  showHex x ""
                      LoadResult_Err trap_cause ->  show trap_cause
          putStrLn (showHex addr ":" ++ val)
  putStrLn "Memory contents"
  mapM_  f  [start..end]

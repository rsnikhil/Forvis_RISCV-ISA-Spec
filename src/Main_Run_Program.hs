-- See LICENSE for license details

module Main_Run_Program where

-- ================================================================
-- This is the 'main' function for the use-case where we simply
-- want to load and ELF file and run it sequentially to completion:
--   - Reads a list of filenames on the command-line
--         (RISC-V ELF executable files, or hex-mem images if ending with ".hex")
--   - Executes each RISC-V program using run_program()

-- ================================================================
-- Standard Haskell imports

import System.IO
import System.Environment
import System.Console.GetOpt    -- getOpt, usageInfo, OptDescr
import System.Exit

import Control.Monad
import Data.Int
import Data.List
import Data.Word
import Data.Bits
import Numeric (showHex, readHex, readDec)

-- Project imports

import Elf
import Read_Hex_File 

import Arch_Defs
import Machine_State
import Run_Program
import Mem_Ops
import Memory
import MMIO
import Address_Map

-- ================================================================

usage_help :: String
usage_help = "This program expects one or more RISC-V ELF or .hex files as command-line arguments"

main_run_program :: IO ()
main_run_program = do
  putStrLn "Type -h, -H or --help for help."
  progName <- getProgName
  args     <- getArgs
  let (opts, filenames, unrecognizeds, errs) = System.Console.GetOpt.getOpt'  RequireOrder  options  args
  {- DEBUG
      putStrLn ("Opts " ++ show opts)
      putStrLn ("Other " ++ show extras)
      putStrLn ("Unrecognizeds " ++ show unrecognizeds)
      putStrLn ("Errs " ++ show errs)
  -}

  let usage_header     = "Usage: " ++ progName ++ "  <flags>  <executable files>\n" ++
                         "  <executable files>:  one or more RISC-V ELF and/or Verilog mem-hex files\n" ++
                         "  <flags>:"
      usage_string     = System.Console.GetOpt.usageInfo  usage_header   options
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
      retval <- run_program_from_files  rv  filenames  num_instrs  verbosity
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

options :: [System.Console.GetOpt.OptDescr  MyOpt]
options =
     [ Option ['h', 'H'] ["help"]       (NoArg  Opt_Help)                  "help"
     , Option []         ["RV32"]       (NoArg  Opt_RV32)                  "executables are for RV32"
     , Option []         ["RV64"]       (NoArg  Opt_RV64)                  "executables are for RV64"
     , Option ['v']      ["verbosity"]  (ReqArg to_Opt_Verbosity  "<int>") "0 quiet, 1 instr trace, 2 more info"
     , Option ['n']      ["num_instrs"] (ReqArg to_Opt_Num_Instrs "<int>") "max instrs executed (default 1,000,000)"
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
-- Run RISC-V program specified in the ELF/Hex file arguments

run_program_from_files :: RV -> [String] -> Int -> Int -> IO Int
run_program_from_files  rv  files  num_instrs  verbosity = do
  -- Read the ELF and Memhex files
  (m_tohost_addr, addr_byte_list) <- read_files  files

  -- Debug only: dump mem
  -- mapM_ (\(addr,byte) -> putStrLn (showHex addr ":" ++ showHex byte "")) addr_byte_list

  -- Create the initial machine state with initial memory contents
  let mstate1        = mkMachine_State  rv  pc_reset_value  addr_ranges  addr_byte_list

      -- Set verbosity: 0: quiet (only console out); 1: also instruction trace; 2: also CPU arch state
      mstate2        = mstate_verbosity_write  mstate1  verbosity

  -- Run the program that is in memory, and report PASS/FAIL
  putStrLn ("PC reset: 0x" ++ showHex  pc_reset_value "" ++
            "; " ++ show (rv) ++ 
            "; instret limit: " ++ show (num_instrs))
  (exit_value, mstate3) <- run_program  num_instrs  m_tohost_addr  mstate2    -- For ISA tests, test_hello,
  -- (exit_value, mstate3) <- run_program  num_instrs  Nothing  mstate2    -- For Linux

  if (exit_value == 0)
    then putStrLn  ("PASS")
    else putStrLn  ("FAIL: test " ++ show exit_value)
  -- putStrLn ("  Files")
  -- mapM_ (\filename -> putStrLn ("    " ++ filename))  files

  -- When verbosity > 0 we repeat all console output here for
  -- convenience since console output would have been interleaved with
  -- trace messages and may have been difficult to read.
  when (verbosity > 0) (do
                           let all_console_output = mstate_mem_all_console_output  mstate3
                           putStrLn "Repeating all console output:"
                           putStr   (if (all_console_output == "") then "--none--\n" else all_console_output))

  -- Info only: show final mtime
  let (mem_result, _) = mstate_mem_read  mstate3  exc_code_load_access_fault  funct3_LD  addr_mtime
  case mem_result of
    Mem_Result_Ok   t -> putStrLn ("Final value of mtime: " ++ show t)
    Mem_Result_Err  _ -> return ()

  return exit_value
  
-- ================================================================
-- Read a list of ELF or Memhex files
-- and return possibly a 'tohost' address
--        and memory contents (list of (addr, byte))
-- If more than one file contains a 'tohost' addr, the first one is returned

read_files :: [String] -> IO (Maybe Word64, [(Int, Word8)])
read_files  []           = return (Nothing, [])
read_files  (file:files) = do
  -- putStrLn ("Reading file " ++ file)
  (m_tohost_addr_1, mem_contents_1) <- read_file   file
  (m_tohost_addr_2, mem_contents_2) <- read_files  files

  let mem_contents = mem_contents_1 ++ mem_contents_2
  case (m_tohost_addr_1, m_tohost_addr_2) of
    (Nothing, _) -> return (m_tohost_addr_2, mem_contents)
    (Just a1, _) -> return (m_tohost_addr_1, mem_contents)

-- ----------------
-- Read a single ELF or Memhex file
-- and return possibly a 'tohost' address
--        and memory contents (list of (addr, byte))

-- Berkeleley ISA tests typically have tohost_addr = (0x80001000 :: Word64)

read_file :: String -> IO (Maybe Word64, [(Int, Word8)])
read_file filename = do
  (m_tohost_addr, addr_byte_list) <- do
    if (".hex8"  `isSuffixOf`  filename)
      then (do
               mem <- read_hex8_file  filename
               return (Nothing, mem))

      else if (".hex32"  `isSuffixOf`  filename)
      then (do
               mem <- read_hex32_file  filename
               return (Nothing, mem))

      else (do
               mem <- read_elf  filename
               m_tohost_addr <- read_elf_symbol  "tohost"  filename
               return (m_tohost_addr, mem))

  let min_addr = minimum (map  (\(x,y) -> x)  addr_byte_list)
      max_addr = maximum (map  (\(x,y) -> x)  addr_byte_list)

  -- Print file info
  putStrLn ("Input file: " ++ filename)
  putStrLn ("    Addr range: " ++ (showHex min_addr "") ++ ".." ++ (showHex max_addr ""))
  case m_tohost_addr of
    Nothing -> putStrLn ("    tohost addr: none")
    Just x  -> putStrLn ("    tohost addr: " ++ showHex  x  "")

  return (m_tohost_addr, addr_byte_list)

-- ================================================================
-- For debugging

dump_mem :: Machine_State -> Word64 -> Word64 -> IO ()
dump_mem  mstate  start  end = do
  let f addr = do
          let (load_result, mstate') = mstate_mem_read  mstate  exc_code_load_access_fault  funct3_LB  addr
              val = case load_result of
                      Mem_Result_Ok  x          ->  showHex x ""
                      Mem_Result_Err trap_cause ->  show trap_cause
          putStrLn (showHex addr ":" ++ val)
  putStrLn "Memory contents"
  mapM_  f  [start..end]

-- ================================================================

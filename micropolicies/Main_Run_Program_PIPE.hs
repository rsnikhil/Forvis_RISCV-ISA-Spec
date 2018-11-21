-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Main_Run_Program_PIPE where

-- ================================================================
-- This is the 'main' function for the use-case where we simply
-- want to load and ELF file and run it sequentially to completion:
--   - Reads a list of filenames on the command-line
--         (RISC-V ELF executable files, or hex-mem images if ending with ".hex")
--   - Executes each RISC-V program using run_loop()

-- ================================================================
-- Standard Haskell imports

import System.IO
import System.Environment
import System.Console.GetOpt    -- getOpt, usageInfo, OptDescr
import System.Exit

import Control.Monad
import Data.Int
import Data.List
import Data.Bits
import Data.Char (toLower, ord)
import Numeric (showHex, readHex, readDec)

-- Project imports

import Elf
import Read_Hex_File 

import Arch_Defs
import Machine_State
import Mem_Ops
import Memory
import MMIO
import Address_Map

import Run_Program_PIPE
import PIPE

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
  putStrLn ("Filenames " ++ show filenames)
  putStrLn ("Unrecognizeds " ++ show unrecognizeds)
  putStrLn ("Errs " ++ show errs)
  -}

  let usage_header     = "Usage: " ++ progName ++ "  <flags>  <executable files>\n" ++
                         "  <executable files>:  one or more RISC-V ELF and/or Verilog mem-hex files\n" ++
                         "  <flags>:"
      usage_string     = System.Console.GetOpt.usageInfo  usage_header   options
      has_help         = elem  Opt_Help  opts
      errs2            = [ err | Opt_Err err <- opts]

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

      archs            = [ (rv, arch) | Opt_Arch  rv  arch <- opts ]

      tohost           =  elem  Opt_ToHost  opts

  if has_help then
    putStr usage_string

    else if unrecognizeds /= [] then
    do
      putStrLn  "Command-line has unrecognized flags:"
      mapM_ (\s -> putStrLn ("    " ++ s)) unrecognizeds

    else if errs /= [] then
    do
      putStrLn  "Command-line has parse errors:"
      mapM_ (\s -> putStrLn ("    " ++ s)) errs

    else if errs2 /= [] then
    do
      putStrLn  "Command-line has parse errors:"
      mapM_ (\s -> putStrLn ("    " ++ s)) errs2

    else if (length  filenames) == 0 then
      putStrLn  "Command-line has no filenames (expecting at least 1)"

    else if (length  archs) == 0 then
      putStrLn  "Command-line has no 'arch' argument"

    else if (length  archs) > 1 then
      putStrLn  "Command-line has more than one 'arch' argument (expecting just one)"

    else
    do
      let [(rv,  misa)] = archs
      putStrLn  ("DEBUG: rv = " ++ show rv ++ (show_misa  misa))
      retval <- run_program_from_files  rv  misa  filenames  num_instrs  tohost  verbosity
      exitWith (if retval == 0 then
                   ExitSuccess
                else
                   ExitFailure (fromIntegral retval))

-- ================================================================
-- These are options for System.Console.GetOpt/getOpt

-- Haskell notes: in type 'OptDescr a', the parameter 'a' is a type for the
-- values associated with options. Here, a = MyOptS

data MyOpt = Opt_Help
           | Opt_Arch        RV  Integer    -- RV32/RV64    misa
           | Opt_ToHost
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
     , Option ['a']      ["arch"]       (ReqArg to_Opt_Arch  "<string>")   "architecture (ISA, e.g., RV32IMAFD)"
     , Option []         ["tohost"]     (NoArg  Opt_ToHost)                "watch <tohost> location"
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

to_Opt_Arch :: String -> MyOpt
to_Opt_Arch s =
  let
    mk_misa  :: RV -> String -> Integer -> MyOpt
    mk_misa     rv    s         misa =
      case s of
        []                                    -> (let
                                                           misa_mxl | rv == RV32 = (shiftL  xl_rv32  misa_MXL_bitpos_RV32)
                                                                    | rv == RV64 = (shiftL  xl_rv64  misa_MXL_bitpos_RV64)
                                                           misa' = (misa_mxl .|. misa)
                                                        in
                                                           Opt_Arch  rv  misa')

        (ch:s1)  | ('a' <= ch) && (ch <= 'z') -> (let
                                                           bit = shiftL  1  ((ord ch) - (ord 'a'))
                                                           misa' = misa .|. bit
                                                        in
                                                           mk_misa  rv  s1  misa')

        _                                     -> Opt_Err  "Illegal --arch command-line arg"

    s1    = take 4 (map  toLower  s)
    s2    = drop 4 (map  toLower  s)
    misa0 = 0
  in
    if (s1 == "rv32") then
      mk_misa  RV32  s2  misa0
    else if (s1 == "rv64") then
      mk_misa  RV64  s2  misa0
    else
      Opt_Err  "Illegal --arch command-line arg"

-- ================================================================
-- Run RISC-V program specified in the ELF/Hex file arguments

run_program_from_files :: RV -> Integer -> [String] -> Int ->      Bool ->       Int -> IO Int
run_program_from_files    rv    misa       files       num_instrs  watch_tohost  verbosity = do
  -- Read the ELF and Memhex files
  (m_tohost_addr, addr_byte_list) <- read_files  files

  let m_tohost_addr1 = if (watch_tohost) then m_tohost_addr    -- e.g., for ISA tests, test_hello, etc.
                       else Nothing                            -- e.g., for Linux kernel boot 

  -- Debug only: dump mem
  -- mapM_ (\(addr,byte) -> putStrLn (showHex addr ":" ++ showHex byte "")) addr_byte_list

  -- Create the initial machine state with initial memory contents
  let mstate1        = mkMachine_State  rv  misa  pc_reset_value  addr_ranges  addr_byte_list

      -- Set verbosity: 0: quiet (only console out); 1: also instruction trace; 2: also CPU arch state
      mstate2        = mstate_verbosity_write  mstate1  verbosity

  -- Run the program that is in memory, and report PASS/FAIL
  putStrLn ("PC reset: 0x" ++ showHex  pc_reset_value "" ++
            "; " ++ show (rv) ++ (show_misa  misa) ++
            "; instret limit: " ++ show (num_instrs))
  (exit_value, pipe_state1, mstate3) <- run_loop num_instrs m_tohost_addr1 init_pipe_state mstate2

  if (exit_value == 0)
    then putStrLn  ("PASS")
    else putStrLn  ("FAIL: test " ++ show exit_value)
  -- putStrLn ("  Files")
  -- mapM_ (\filename -> putStrLn ("    " ++ filename))  files

  -- When verbosity > 0 we repeat all console output here for
  -- convenience since console output would have been interleaved with
  -- trace messages and may have been difficult to read.
  when (verbosity > 0) (do
                           let (all_console_output_a,
                                all_console_output_b) = mstate_mem_all_console_output  mstate3
                               all_console_output     = all_console_output_a ++ all_console_output_b
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

read_files :: [String] -> IO (Maybe Integer, [(Integer, Integer)])
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

-- Berkeleley ISA tests typically have tohost_addr = (0x8000_1000)

read_file :: String -> IO (Maybe Integer, [(Integer, Integer)])
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

dump_mem :: Machine_State -> Integer -> Integer -> IO ()
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

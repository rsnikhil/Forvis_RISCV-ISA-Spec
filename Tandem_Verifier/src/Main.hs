-- Copyright (c) 2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Main where

-- ================================================================
-- Standard Haskell imports

import Numeric (showHex, readHex, readDec)
import Data.Char (toUpper, ord)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.List (nub, isSuffixOf)
import Control.Monad (when)
import System.IO                -- for IOMode, hFileSize
import System.Console.GetOpt    -- getOpt, usageInfo, OptDescr
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))

import qualified Data.ByteString.Lazy as BSL

import Verify

-- ================================================================
-- Project imports

import Arch_Defs (RV(..),
                  Mem_Result(..),
                  funct3_LB,
                  exc_code_load_access_fault)
import Machine_State (Machine_State(..),
                      mkMachine_State,
                      mstate_mem_read,
                      mstate_mem_read_mtime,
                      mstate_verbosity_write,
                      mstate_mem_all_console_output)
import CSR_File (xl_rv32,
                 xl_rv64,
                 show_misa,
                 misa_MXL_bitpos_RV32,
                 misa_MXL_bitpos_RV64)
import Address_Map (pc_reset_value, addr_ranges)
import Run_Program_Sequential (run_loop, fetch_and_execute)

import Elf (read_elf, read_elf_symbol)
import Read_Hex_File (read_hex8_file, read_hex32_file)

import Parse_Trace_Data

-- ================================================================

main :: IO ()
main = do
  putStrLn "Type -h, -H or --help for help."
  progName <- getProgName
  args     <- getArgs
  let (opts, filenames, unrecognizeds, errs) = System.Console.GetOpt.getOpt'  RequireOrder  options  args

  {- Debug: show getOpt results
  putStrLn ("Opts " ++ show opts)
  putStrLn ("Filenames " ++ show filenames)
  putStrLn ("Unrecognizeds " ++ show unrecognizeds)
  putStrLn ("Errs " ++ show errs)
  -}

  let usage_header     = "Usage: " ++ progName ++ "  <flags>  <ELF file>  <trace data file>\n" ++
                         "  <ELF file>:         that was executed by the DUT to produce trace data\n" ++
                         "  <trace data file>:  the tandem-verification trace data file produced by the DUT\n" ++
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

      archs            = [ arch | Opt_Arch  arch <- opts ]

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

    else if (length  filenames) /= 3 then
      putStrLn  ("Command-line does not have 3 filenames: " ++ show filenames)

    else if (length  archs) == 0 then
      putStrLn  "Command-line has no 'arch' argument"

    else if (length  archs) > 1 then
      putStrLn  "Command-line has more than one 'arch' argument (expecting just one)"

    else
    do
      let [arch]                = archs
          [boot_rom_filename, elf_filename, trace_data_filename] = filenames
      putStrLn  ("Architecture    : " ++ arch)
      putStrLn  ("Instr limit     : " ++ show num_instrs ++ " instructions")
      putStrLn  ("Verbosity       : " ++ show verbosity)
      putStrLn  ("Boot ROM file   : " ++ boot_rom_filename)
      putStrLn  ("ELF file        : " ++ elf_filename)
      putStrLn  ("Trace data file : " ++ trace_data_filename)

      -- Initialize Forvis' machine state
      let (rv,misa) = arch_string_to_misa  arch
      mstate <- initialize_machine_state  rv  misa  [boot_rom_filename, elf_filename]  verbosity

      -- Parse the Trace Data file
      let extns = drop 4 arch
          xlen1 = if (rv == RV32) then 32 else 64
          flen1 = if ('D' `elem` extns) then 64
                  else if ('F' `elem` extns) then 32
                       else 0
          ilen1 = 32
          mlen1 = if (rv == RV32) then
                    if ('S' `elem` extns) then 34
                    else 32
                  else
                    64
          tv_parse_state = State {xlen=xlen1, flen=flen1, ilen=ilen1, mlen=mlen1, in_group=False}
      tv_items <- parse_trace_file  tv_parse_state  trace_data_filename

      -- Verify the Trace Data
      let inum = 1
      retval <- verify_loop  verbosity  inum  tv_items  mstate
      exitWith (if retval == 0 then
                   ExitSuccess
                else
                   ExitFailure (fromIntegral retval))

      {-
      putStrLn ">================================================================"
      putStrLn "Run program"

      let tohost    = True
      retval <- run_program_from_files  rv  misa  [boot_rom_filename, elf_filename]  num_instrs  tohost  verbosity

      putStrLn ">================================================================"
      putStrLn "Show trace file"

      show_trace_file  state  trace_data_filename
      -}

-- ================================================================
-- These are options for System.Console.GetOpt/getOpt

-- Haskell notes: in type 'OptDescr a', the parameter 'a' is a type for the
-- values associated with options. Here, a = MyOptS

data MyOpt = Opt_Help
           | Opt_Arch        String    -- misa
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
    -- RV32/RV64 header
    rvXX  = take 4 (map  toUpper  s)

    -- e.g., IMAFDC
    letters     = drop 4 (map  toUpper  s)
    bad_letters = [ bad_ch | bad_ch <- letters, ((bad_ch < 'A') || ('Z' < bad_ch))]
    has_G       = 'G' `elem` letters
    letters1    = if has_G then
                    "IMAFD" ++ [ ch | ch <- letters, (ch /= 'G') ]
                  else
                    letters
    letters2    = nub letters1    -- remove duplicates
  in
    if ((rvXX /= "RV32") && (rvXX /= "RV64")) then
      Opt_Err ("Illegal --arch command-line arg '" ++ s ++ "' does not begin with RV32 or RV64")
    else if (bad_letters /= []) then
      Opt_Err ("Illegal --arch command-line arg '" ++ s ++ "' has unrecognized letter(s): " ++ bad_letters)
    else
      Opt_Arch  (rvXX ++ letters2)

-- ================================================================
-- Convert architecture string (like R64ACDFIMSU) into RV and MISA

is_misa_opt :: Char -> Bool
#ifdef FLOAT
is_misa_opt  ch = (('A' <= ch) && (ch <= 'Z'))
#else
is_misa_opt  ch = (('A' <= ch) && (ch <= 'Z') && (ch /= 'D') && (ch /= 'F'))
#endif

arch_string_to_misa :: String -> (RV, Integer)
arch_string_to_misa    arch_string =
  let
    mk_misa  :: RV -> String -> Integer -> (RV, Integer)
    mk_misa     rv    s         misa =
      case s of
        []                          -> (let
                                           misa_mxl | rv == RV32 = (shiftL  xl_rv32  misa_MXL_bitpos_RV32)
                                                    | rv == RV64 = (shiftL  xl_rv64  misa_MXL_bitpos_RV64)
                                           misa' = (misa_mxl .|. misa)
                                         in
                                           (rv,  misa'))

        (ch:s1)  | is_misa_opt (ch) -> (let
                                           bit = shiftL  1  ((ord ch) - (ord 'A'))
                                           misa' = misa .|. bit
                                        in
                                           mk_misa  rv  s1  misa')

        _                           -> error ("Illegal arch_string: " ++ arch_string)

    s1    = take 4 (map  toUpper  arch_string)
    s2    = drop 4 (map  toUpper  arch_string)
    misa0 = 0
  in
    if (s1 == "RV32") then
      mk_misa  RV32  s2  misa0
    else if (s1 == "RV64") then
      mk_misa  RV64  s2  misa0
    else
      error  ("Illegal --arch command-line arg: " ++ arch_string)

-- ================================================================
-- Initialize Machine State and load ELF and MemHex files

initialize_machine_state :: RV -> Integer -> [String] -> Int -> IO Machine_State
initialize_machine_state    rv    misa       files       verbosity = do
  -- Read the ELF and Memhex files
  (m_tohost_addr, addr_byte_list) <- read_files  files

  -- Create the initial machine state with initial memory contents
  let mstate1        = mkMachine_State  rv  misa  pc_reset_value  addr_ranges  addr_byte_list

      -- Set verbosity: 0: quiet (only console out); 1: also instruction trace; 2: also CPU arch state
      mstate2        = mstate_verbosity_write  verbosity  mstate1

  return mstate2

-- ================================================================
-- Opens the trace data file, parses it, and verifies it

parse_trace_file :: State -> String -> IO [Trace_Item]
parse_trace_file    state    filename = do
  handle <- openBinaryFile  filename  ReadMode
  size   <- hFileSize  handle

  putStrLn $ "Opened file '" ++ filename ++ "' for reading trace_data. Size: " ++ show size ++ " bytes"
  items <- parse_trace_data  state  handle
  -- print_items  items
  return items
  
-- ================================================================
-- Verify items in trace file

verify_loop :: Int ->     Int -> [Trace_Item] -> Machine_State -> IO Int
verify_loop    verbosity  inum   []              mstate = (do
                                                              putStrLn ("verify_loop exit: " ++
                                                                        show inum ++ " instrs")
                                                              return 0)
verify_loop    verbosity  inum   (item:items)    mstate =
  if (not (item_has_instruction  item))
  then do
    when (verbosity > 0) (putStrLn ("    TV: inum:" ++ show inum ++
                                    " " ++ show  item ++ "; Ignoring (no instruction)"))
    verify_loop  verbosity  inum  items  mstate
  else do
    mstate1 <- fetch_and_execute  mstate
    ok <- verify_instr  mstate  mstate1  item
    if (not ok) then
      do
        putStrLn ("    TV: inum:" ++ show inum ++ " " ++ show  item ++ "; TV failure")
        return 1
      else
      do
        when (verbosity > 0) (putStrLn ("    TV: inum:" ++ show inum ++
                                        " " ++ show  item ++ "; OK"))
        verify_loop  verbosity  (inum+1)  items  mstate1

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
      mstate2        = mstate_verbosity_write  verbosity  mstate1

  -- Run the program that is in memory, and report PASS/FAIL
  putStrLn ("PC reset: 0x" ++ showHex  pc_reset_value "" ++
            "; " ++ show (rv) ++ (show_misa  misa) ++
            "; instret limit: " ++ show (num_instrs))
  (exit_value, mstate3) <- run_loop  num_instrs  m_tohost_addr1  mstate2

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
  let t = mstate_mem_read_mtime  mstate3
  putStrLn ("Final value of mtime: " ++ show t)

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
          let (load_result, mstate') = mstate_mem_read  exc_code_load_access_fault  funct3_LB  addr  mstate
              val = case load_result of
                      Mem_Result_Ok  x          ->  showHex x ""
                      Mem_Result_Err trap_cause ->  show trap_cause
          putStrLn (showHex addr ":" ++ val)
  putStrLn "Memory contents"
  mapM_  f  [start..end]

-- ================================================================
-- Opens a trace data file, parses it, and prints the items

show_trace_file :: State -> String -> IO ()
show_trace_file    state    filename = do
  handle <- openBinaryFile  filename  ReadMode
  size   <- hFileSize  handle

  putStrLn $ "Opened file '" ++ filename ++ "' for reading trace_data. Size: " ++ show size ++ " bytes"
  items <- parse_trace_data  state  handle
  print_items  items
  
-- ================================================================
-- This function is not used; is available for debugging.
-- Just dumps the bytes of a binary file (e.g., the trace data file)
-- in hex format, 16 bytes per line.

dump_trace_file :: Handle -> IO ()
dump_trace_file    handle = do
  eof <- hIsEOF  handle
  when (not eof) (do
                     bs    <- BSL.hGet  handle  16
                     let len = BSL.length  bs
                     mapM_  (\j -> putStr $ "  0x" ++ (showHex  (BSL.index bs j)  ""))  [0..len-1]
                     putStrLn ""
                     dump_trace_file  handle)

-- ================================================================

module Main_TandemVerifier (main_TandemVerifier) where

-- ================================================================
-- This is the 'main' function for the use-case where we use the
-- formal spec as a tandem verifier.  Here, it receives commands
-- on stdin and produces responses on stdout.

-- ================================================================
-- Standard Haskell imports

import System.IO
import Numeric (showHex, readHex)
import System.Exit

-- Project imports

import BitManipulation
import ArchDefs
import ArchState
import RunProgram

-- ================================================================

main_TandemVerifier :: IO ()
main_TandemVerifier = do
  -- Parse commands from stdin
  all_input <- getContents
  let ws     = words  all_input
      ws'    = skip_until_cmd_start  ws
      cmds   = split_cmds  []  ws'

  -- Create initial architectural state
  let initial_PC     = 0
      addr_byte_list = []
      astate         = mkArchState  RV64  initial_PC  addr_byte_list

  putStrLn "Initial arch state"
  print_ArchState "____"  astate

  -- Process the commands against the architectural state
  astate1 <- process_cmds  astate  cmds

  putStrLn "Final arch state"
  print_ArchState "____"  astate1

-- ================================================================
-- Parsing stdin into commands

cmd_start :: String
cmd_start = "$"

-- Establishes invariant for arg of split_cmds
-- namely: list of strings is either empty, or first string is the command-start word
skip_until_cmd_start :: [String] -> [String]
skip_until_cmd_start []                      = []
skip_until_cmd_start (x:xs) | x == cmd_start = (x:xs)

-- Invariant: 'cmds' is either empty or starts with the cmd_start word
-- Split list of lines (line = list of words) into commands
-- where each command begins with cmd_start word.
-- The cmd_start word is dropped.
split_cmds :: [[String]] -> [String] -> [[String]]
split_cmds  cmds  []                        =  cmds
split_cmds  cmds  (x:xs) | x == cmd_start = collect_cmd  cmds  []  xs
                         | True           = error ("split_cmds arg does not start with cmd_start word: " ++ cmd_start)

-- The cmd_start word has been consumed by split_cmds
-- Collect the rest of the commmand (until next cmd_start word or end of input)
-- and resume splitting rest of input into commands
collect_cmd :: [[String]] -> [String] -> [String] -> [[String]]
collect_cmd  cmds  cmd  []       = cmds ++ [cmd]
collect_cmd  cmds  cmd  (x:xs) | x == cmd_start = split_cmds  (cmds ++ [cmd])  ("$":xs)
                               | True           = collect_cmd  cmds (cmd ++ [x])  xs

-- ================================================================
-- Processing commands against the architectural state

process_cmds :: ArchState -> [[String]] -> IO ArchState
process_cmds  astate  []         = return astate
process_cmds  astate  (cmd:cmds) = do
  astate1 <- process_cmd  astate  cmd
  hFlush  stdout
  process_cmds  astate1  cmds

-- ================
-- Process one command

process_cmd :: ArchState -> [String] -> IO ArchState

-- ================
-- Read/Write PC.

process_cmd  astate  ["read_PC"] = do
  putStrLn ("    Doing read_PC")
  let pc_val = archstate_pc_read  astate
  putStrLn ("OK " ++ show pc_val)
  return astate

process_cmd  astate  ["write_PC", v_s] = do
  putStrLn ("    Doing write_PC " ++ v_s)
  let v = fromIntegral (read_hex  32  v_s)
  astate1 <- archstate_pc_write  astate  v
  putStrLn "OK"
  return astate1

-- ================
-- Read/Write GPR

process_cmd  astate  ["read_GPR", r_s] = do
  putStrLn ("    Doing read_GPR " ++ r_s)
  let r       = toEnum (fromIntegral (read_hex  5  r_s))
      gpr_val = archstate_gpr_read  astate  r
  putStrLn ("OK " ++ show gpr_val)
  return astate

process_cmd  astate  ["write_GPR", r_s, v_s] = do
  putStrLn ("    Doing write_GPR " ++ r_s ++ " " ++ v_s)
  let r = toEnum (fromIntegral (read_hex   5  r_s))
      v = fromIntegral (read_hex  32  v_s)
  astate1 <- archstate_gpr_write  astate  r  v
  putStrLn "OK"
  return astate1

-- ================
-- Read/Write CSR

process_cmd  astate  ["read_CSR", csr_addr_s] = do
  putStrLn ("    Doing read_CSR " ++ csr_addr_s)
  let csr_addr = fromIntegral (read_hex  12  csr_addr_s)
      csr_val  = archstate_csr_read  astate  csr_addr
  putStrLn ("OK " ++ show csr_val)
  return astate

process_cmd  astate  ["write_CSR", csr_addr_s, v_s] = do
  putStrLn ("    Doing write_CSR " ++ csr_addr_s ++ " " ++ v_s)
  let csr_addr = fromIntegral (read_hex  12  csr_addr_s)
      v        = fromIntegral (read_hex  32  v_s)
  astate1 <- archstate_csr_write  astate  csr_addr  v
  putStrLn "OK"
  return astate1

-- ================
-- Read mem bytes
-- TODO: Read all data before responding, check for errors

process_cmd  astate  ["read_mem_8", n_s, addr_s] = do
  putStrLn ("    Doing read_mem_8 " ++ n_s ++ " " ++ addr_s)
  putStr "OK"
  let n    = read_hex  32  n_s
      addr = read_hex  32  addr_s
      read_bytes :: ArchState -> Integer -> IO ()
      read_bytes  astate  j | j == n = return ()
                            | True   = do
                                          let addr_j           = fromIntegral (addr + j)
                                              (res_j, astate') = archstate_mem_read8  astate  addr_j
                                          case res_j of
                                            LoadResult_Err cause ->
                                              do
                                                putStrLn ("ERROR: read_mem_8 encountered LoadResult_Err: " ++ show cause)
                                                exitWith (ExitFailure 1)
                                            LoadResult_Ok val_j ->
                                              do
                                                putStr (" " ++ (showHex val_j ""))
                                                read_bytes  astate'  (j+1)
  read_bytes  astate  0
  putStrLn  ""
  return astate

-- ================
-- Write mem bytes
-- TODO: Check for errors

process_cmd  astate  ("write_mem_8": addr_s: val_ss) = do
  putStr ("    Doing write_mem_8 " ++ addr_s)
  mapM_ (\val_s -> putStr  (" " ++ val_s))  val_ss
  putStrLn ""

  let addr = read_hex  32  addr_s
      write_bytes :: ArchState -> Integer -> [String] -> IO ArchState
      write_bytes  astate  j  []               = return astate
      write_bytes  astate  j  (val_s:val_ss) = do
        let addr_j = addr + j
            val_j  = read_hex 32 val_s
        putStrLn ("(" ++ (showHex addr_j "") ++ "," ++ (showHex val_j "") ++ ")")
        astate1 <- archstate_mem_write8  astate  (fromIntegral addr_j)  (fromIntegral val_j)
        write_bytes  astate1  (j+1)  val_ss

  astate1 <- write_bytes  astate  0  val_ss
  putStrLn "OK"
  return astate1

-- ================
-- Read mem 32b words
-- TODO: Read all data before responding, check for errors

process_cmd  astate  ["read_mem_32", n_s, addr_s] = do
  putStrLn ("    Doing read_mem_32 " ++ n_s ++ " " ++ addr_s)
  putStr "OK"
  let n    = read_hex  32  n_s
      addr = read_hex  32  addr_s
      read_words :: ArchState -> Integer -> IO ()
      read_words  astate  j | j == n = return ()
                            | True   = do
                                          let addr_j           = fromIntegral (addr + j*4)
                                              (res_j, astate') = archstate_mem_read32  astate  addr_j
                                          case res_j of
                                            LoadResult_Err cause ->
                                              do
                                                putStrLn ("ERROR: read_mem_32 encountered LoadResult_Err: " ++ show cause)
                                                exitWith (ExitFailure 1)
                                            LoadResult_Ok val_j ->
                                              do
                                                putStr (" " ++ (showHex val_j ""))
                                                read_words  astate'  (j+1)
  read_words  astate  0
  putStrLn  ""
  return astate

-- ================
-- Write mem 32b words
-- TODO: Check for errors

process_cmd  astate  ("write_mem_32": addr_s: val_ss) = do
  putStr ("    Doing write_mem_32 " ++ addr_s)
  mapM_ (\val_s -> putStr  (" " ++ val_s))  val_ss
  putStrLn ""

  let addr = read_hex  32  addr_s
      write_words :: ArchState -> Integer -> [String] -> IO ArchState
      write_words  astate  j  []               = return astate
      write_words  astate  j  (val_s:val_ss) = do
        let addr_j = addr + j*4
            val_j  = read_hex 32 val_s
        putStrLn ("(" ++ (showHex addr_j "") ++ "," ++ (showHex val_j "") ++ ")")
        astate1 <- archstate_mem_write32  astate  (fromIntegral addr_j)  (fromIntegral val_j)
        write_words  astate1  (j+1)  val_ss

  astate1 <- write_words  astate  0  val_ss
  putStrLn "OK"
  return astate1

-- ================
-- Execute N instructions and optionally return Tandem-Verification packets
-- TODO: return Tandem-Verification packets

process_cmd  astate  ["exec", n_s, tv_s] = do
  putStrLn ("    Doing exec " ++ n_s ++ " " ++ tv_s)
  let n = read  n_s
  astate1 <- runProgram  (fromIntegral n)  astate
  let stop_reason = archstate_stop_read  astate1
  putStrLn ("OK " ++ (show stop_reason))
  return astate1

-- ================
-- Read/Write trace verbosity
-- TODO: Clip to legal values

process_cmd  astate  ["read_verbosity"] = do
  putStrLn ("    Doing read_verbosity")
  let verbosity = archstate_verbosity_read  astate
  putStrLn ("OK " ++ show (verbosity))
  return astate

process_cmd  astate  ["write_verbosity", v_s] = do
  putStrLn ("    Doing write_verbosity " ++ v_s)
  let verbosity = read_hex  1  v_s
  astate1 <- archstate_verbosity_write  astate  (fromIntegral verbosity)
  putStrLn "OK"
  return astate1

-- ================
-- Unrecognized command

process_cmd  astate   cmd  = do
  putStr "    Doing "
  mapM_ (\val_s -> putStr (" " ++ val_s)) cmd
  putStrLn ""
  putStrLn "NOT_OK"
  return astate

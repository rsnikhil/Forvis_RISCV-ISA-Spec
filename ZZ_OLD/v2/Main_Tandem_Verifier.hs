-- Copyright (c) 2018 Rishiyur S. Nikhil
-- See LICENSE for license details

module Main_Tandem_Verifier where

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

import Bit_Manipulation
import Arch_Defs
import Machine_State
import Run_Program

-- ================================================================

main_tandem_verifier :: IO ()
main_tandem_verifier = do
  -- Parse commands from stdin
  all_input <- getContents
  let ws     = words  all_input
      ws'    = skip_until_cmd_start  ws
      cmds   = split_cmds  []  ws'

  -- Create initial architectural state
  let initial_PC     = 0
      mem_base       = 0x80000000
      mem_size       = 0x80000000    -- 1 GiB
      addr_byte_list = []
      mstate         = mkMachine_State  RV64  initial_PC  [(mem_base, mem_base + mem_size)]  addr_byte_list

  putStrLn "Initial arch state"
  mstate_print  "____"  mstate

  -- Process the commands against the architectural state
  mstate1 <- process_cmds  mstate  cmds

  putStrLn "Final arch state"
  mstate_print  "____"  mstate1

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

funct3_LB  :: InstrField;    funct3_LB  = 0x0     -- 3'b_000
funct3_LW  :: InstrField;    funct3_LW  = 0x2     -- 3'b_010

funct3_SB :: InstrField;    funct3_SB = 0x0     -- 3'b_000
funct3_SH :: InstrField;    funct3_SH = 0x1     -- 3'b_001
funct3_SW :: InstrField;    funct3_SW = 0x2     -- 3'b_010
funct3_SD :: InstrField;    funct3_SD = 0x3     -- 3'b_011

-- ================================================================
-- Processing commands against the architectural state

process_cmds :: Machine_State -> [[String]] -> IO Machine_State
process_cmds  mstate  []         = return mstate
process_cmds  mstate  (cmd:cmds) = do
  mstate1 <- process_cmd  mstate  cmd
  hFlush  stdout
  process_cmds  mstate1  cmds

-- ================
-- Process one command

process_cmd :: Machine_State -> [String] -> IO Machine_State

-- ================
-- Read/Write PC.

process_cmd  mstate  ["read_PC"] = do
  putStrLn ("    Doing read_PC")
  let pc_val = mstate_pc_read  mstate
  putStrLn ("OK " ++ show pc_val)
  return mstate

process_cmd  mstate  ["write_PC", v_s] = do
  putStrLn ("    Doing write_PC " ++ v_s)
  let v = fromIntegral (read_hex  32  v_s)
      mstate1 = mstate_pc_write  mstate  v
  putStrLn "OK"
  return mstate1

-- ================
-- Read/Write GPR

process_cmd  mstate  ["read_GPR", r_s] = do
  putStrLn ("    Doing read_GPR " ++ r_s)
  let r       = toEnum (fromIntegral (read_hex  5  r_s))
      gpr_val = mstate_gpr_read  mstate  r
  putStrLn ("OK " ++ show gpr_val)
  return mstate

process_cmd  mstate  ["write_GPR", r_s, v_s] = do
  putStrLn ("    Doing write_GPR " ++ r_s ++ " " ++ v_s)
  let r = toEnum (fromIntegral (read_hex   5  r_s))
      v = fromIntegral (read_hex  32  v_s)
      mstate1 = mstate_gpr_write  mstate  r  v
  putStrLn "OK"
  return mstate1

-- ================
-- Read/Write CSR

process_cmd  mstate  ["read_CSR", csr_addr_s] = do
  putStrLn ("    Doing read_CSR " ++ csr_addr_s)
  let csr_addr = fromIntegral (read_hex  12  csr_addr_s)
      csr_val  = mstate_csr_read  mstate  csr_addr
  putStrLn ("OK " ++ show csr_val)
  return mstate

process_cmd  mstate  ["write_CSR", csr_addr_s, v_s] = do
  putStrLn ("    Doing write_CSR " ++ csr_addr_s ++ " " ++ v_s)
  let csr_addr = fromIntegral (read_hex  12  csr_addr_s)
      v        = fromIntegral (read_hex  32  v_s)
      mstate1  = mstate_csr_write  mstate  csr_addr  v
  putStrLn "OK"
  return mstate1

-- ================
-- Read mem bytes
-- TODO: Read all data before responding, check for errors

process_cmd  mstate  ["read_mem_8", n_s, addr_s] = do
  putStrLn ("    Doing read_mem_8 " ++ n_s ++ " " ++ addr_s)
  putStr "OK"
  let n    = read_hex  32  n_s
      addr = read_hex  32  addr_s
      read_bytes :: Machine_State -> Integer -> IO ()
      read_bytes  mstate  j | j == n = return ()
                            | True   = do
                                          let addr_j           = fromIntegral (addr + j)
                                              (res_j, mstate') = (mstate_mem_read
                                                                  mstate
                                                                  exc_code_load_access_fault
                                                                  funct3_LB
                                                                  addr_j)
                                          case res_j of
                                            Mem_Result_Err cause ->
                                              do
                                                putStrLn ("ERROR: read_mem_8 encountered Mem_Result_Err: " ++ show cause)
                                                exitWith (ExitFailure 1)
                                            Mem_Result_Ok val_j ->
                                              do
                                                putStr (" " ++ (showHex val_j ""))
                                                read_bytes  mstate'  (j+1)
  read_bytes  mstate  0
  putStrLn  ""
  return mstate

-- ================
-- Write mem bytes
-- TODO: Check for errors

process_cmd  mstate  ("write_mem_8": addr_s: val_ss) = do
  putStr ("    Doing write_mem_8 " ++ addr_s)
  mapM_ (\val_s -> putStr  (" " ++ val_s))  val_ss
  putStrLn ""

  let addr = read_hex  32  addr_s
      write_bytes :: Machine_State -> Integer -> [String] -> IO Machine_State
      write_bytes  mstate  j  []             = return mstate
      write_bytes  mstate  j  (val_s:val_ss) = do
        let addr_j = addr + j
            val_j  = read_hex 32 val_s
            (st_result, mstate1) = mstate_mem_write  mstate  funct3_SB  (fromIntegral addr_j)  (fromIntegral val_j)
        putStrLn ("(" ++ (showHex addr_j "") ++ "," ++ (showHex val_j "") ++ ")")
        write_bytes  mstate1  (j+1)  val_ss

  mstate1 <- write_bytes  mstate  0  val_ss
  putStrLn "OK"
  return mstate1

-- ================
-- Read mem 32b words
-- TODO: Read all data before responding, check for errors

process_cmd  mstate  ["read_mem_32", n_s, addr_s] = do
  putStrLn ("    Doing read_mem_32 " ++ n_s ++ " " ++ addr_s)
  putStr "OK"
  let n    = read_hex  32  n_s
      addr = read_hex  32  addr_s
      read_words :: Machine_State -> Integer -> IO ()
      read_words  mstate  j | j == n = return ()
                            | True   = do
                                          let addr_j           = fromIntegral (addr + j*4)
                                              (res_j, mstate') = (mstate_mem_read
                                                                   mstate
                                                                   exc_code_load_access_fault
                                                                   funct3_LW
                                                                   addr_j)
                                          case res_j of
                                            Mem_Result_Err cause ->
                                              do
                                                putStrLn ("ERROR: read_mem_32 encountered Mem_Result_Err: " ++ show cause)
                                                exitWith (ExitFailure 1)
                                            Mem_Result_Ok val_j ->
                                              do
                                                putStr (" " ++ (showHex val_j ""))
                                                read_words  mstate'  (j+1)
  read_words  mstate  0
  putStrLn  ""
  return mstate

-- ================
-- Write mem 32b words
-- TODO: Check for errors

process_cmd  mstate  ("write_mem_32": addr_s: val_ss) = do
  putStr ("    Doing write_mem_32 " ++ addr_s)
  mapM_ (\val_s -> putStr  (" " ++ val_s))  val_ss
  putStrLn ""

  let addr = read_hex  32  addr_s
      write_words :: Machine_State -> Integer -> [String] -> IO Machine_State
      write_words  mstate  j  []               = return mstate
      write_words  mstate  j  (val_s:val_ss) = do
        let addr_j = addr + j*4
            val_j  = read_hex 32 val_s
            (st_result, mstate1) = mstate_mem_write  mstate  funct3_SW  (fromIntegral addr_j)  (fromIntegral val_j)
        putStrLn ("(" ++ (showHex addr_j "") ++ "," ++ (showHex val_j "") ++ ")")
        write_words  mstate1  (j+1)  val_ss

  mstate1 <- write_words  mstate  0  val_ss
  putStrLn "OK"
  return mstate1

-- ================
-- Execute N instructions and optionally return Tandem-Verification packets
-- TODO: return Tandem-Verification packets

process_cmd  mstate  ["exec", n_s, tv_s] = do
  putStrLn ("    Doing exec " ++ n_s ++ " " ++ tv_s)
  let n             = read  n_s
      m_tohost_addr = Nothing
  (exit_value, mstate1) <- run_loop  (fromIntegral n)  m_tohost_addr  mstate
  let run_state = mstate_run_state_read  mstate1
  putStrLn ("OK " ++ (show run_state))
  return mstate1

-- ================
-- Read/Write trace verbosity
-- TODO: Clip to legal values

process_cmd  mstate  ["read_verbosity"] = do
  putStrLn ("    Doing read_verbosity")
  let verbosity = mstate_verbosity_read  mstate
  putStrLn ("OK " ++ show (verbosity))
  return mstate

process_cmd  mstate  ["write_verbosity", v_s] = do
  putStrLn ("    Doing write_verbosity " ++ v_s)
  let verbosity = read_hex  1  v_s
      mstate1   = mstate_verbosity_write  mstate  (fromIntegral verbosity)
  putStrLn "OK"
  return mstate1

-- ================
-- Unrecognized command

process_cmd  mstate   cmd  = do
  putStr "    Doing "
  mapM_ (\val_s -> putStr (" " ++ val_s)) cmd
  putStrLn ""
  putStrLn "NOT_OK"
  return mstate

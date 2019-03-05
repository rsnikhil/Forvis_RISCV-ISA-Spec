-- Copyright (c) 2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Verify where

-- ================================================================
-- Standard Haskell imports

import Numeric (showHex)
import Data.Word

-- ================================================================
-- Project imports

import Parse_Trace_Data (Trace_Item (..), TI_Addl_State_t (..))
import RegNames

import Machine_State
import Arch_Defs
import Forvis_Spec_Instr_Fetch

-- ================================================================

verify_instr :: Machine_State -> Machine_State -> Trace_Item -> IO Bool
verify_instr    mstate1          mstate2          item =
  case item of
    TI_Group items                         -> (do
                                                  oks <- mapM  (verify_instr  mstate1  mstate2)  items
                                                  return (all  id  oks))
    TI_Incr_PC                             -> verify_TBD "verify_instr: TI_Incr_PC not yet handled"
    TI_Full_Reg      reg  val              -> verify_reg_write  mstate1  mstate2  reg  val
    TI_Incr_Reg_Add  reg  offset           -> verify_TBD "verify_instr: TI_Incr_Reg_Add not yet handled"
    TI_Incr_Reg_Or   reg  ormask           -> verify_TBD "verify_instr: TI_Incr_Reg_Or not yet handled"
    TI_Addl_State    addl_state            -> verify_addl_state  mstate1  mstate2  addl_state
    TI_Mem_Req       addr  op  size  wdata -> verify_TBD "verify_instr: TI_Mem_Req not yet handled"
    TI_Mem_Rsp       size  result  rdata   -> verify_TBD "verify_instr: TI_Mem_Rsp not yet handled"
    TI_Hart_Reset                          -> return True    -- TODO: reset mstate?
    TI_State_Init                          -> return True
    TI_Instr16       instr16               -> verify_instr16  mstate1  mstate2  instr16
    TI_Instr32       instr32               -> verify_instr32  mstate1  mstate2  instr32

-- ================================================================

verify_addl_state :: Machine_State -> Machine_State -> TI_Addl_State_t -> IO Bool
verify_addl_state    mstate1          mstate2          addl_state =
  case addl_state of
    TI_Addl_State_priv      priv    -> (do
                                           let priv1 = mstate_priv_read  mstate2
                                           if (fromIntegral priv == priv1) then
                                             return True
                                             else
                                             do
                                               putStrLn  ("Privilege level mismatch:")
                                               putStrLn  ("    Forvis value: " ++ showHex priv1 "")
                                               putStrLn  ("    DUT    value: " ++ showHex priv  "")
                                               return False)

    TI_Addl_State_paddr     paddr   -> verify_TBD "verify_addl_state: TI_Addl_State_paddr not handled yet"

    TI_Addl_State_eaddr     eaddr   -> (do
                                           let eaddr1 = mstate_eaddr_read  mstate2
                                           if (fromIntegral eaddr == eaddr1) then
                                             return True
                                             else
                                             do
                                               putStrLn  ("Effective Address mismatch:")
                                               putStrLn  ("    Forvis value: " ++ showHex eaddr1 "")
                                               putStrLn  ("    DUT    value: " ++ showHex eaddr  "")
                                               return False)

    TI_Addl_State_wdata8    wdata8  -> (do
                                           let wdata8_a = mstate_wdata_read  mstate2
                                           if (fromIntegral wdata8 == wdata8_a) then
                                             return True
                                             else
                                             do
                                               putStrLn  ("Write-Data8 mismatch:")
                                               putStrLn  ("    Forvis value: " ++ showHex wdata8_a "")
                                               putStrLn  ("    DUT    value: " ++ showHex wdata8   "")
                                               return False)

    TI_Addl_State_wdata16   wdata16 -> (do
                                           let wdata16_a = mstate_wdata_read  mstate2
                                           if (fromIntegral wdata16 == wdata16_a) then
                                             return True
                                             else
                                             do
                                               putStrLn  ("Write-Data16 mismatch:")
                                               putStrLn  ("    Forvis value: " ++ showHex wdata16_a "")
                                               putStrLn  ("    DUT    value: " ++ showHex wdata16   "")
                                               return False)

    TI_Addl_State_wdata32   wdata32 -> (do
                                           let wdata32_a = mstate_wdata_read  mstate2
                                           if (fromIntegral wdata32 == wdata32_a) then
                                             return True
                                             else
                                             do
                                               putStrLn  ("Write-Data32 mismatch:")
                                               putStrLn  ("    Forvis value: " ++ showHex wdata32_a "")
                                               putStrLn  ("    DUT    value: " ++ showHex wdata32   "")
                                               return False)

    TI_Addl_State_wdata64   wdata64 -> (do
                                           let wdata64_a = mstate_wdata_read  mstate2
                                           if (fromIntegral wdata64 == wdata64_a) then
                                             return True
                                             else
                                             do
                                               putStrLn  ("Write-Data64 mismatch:")
                                               putStrLn  ("    Forvis value: " ++ showHex wdata64_a "")
                                               putStrLn  ("    DUT    value: " ++ showHex wdata64   "")
                                               return False)

    TI_Addl_State_mtime     mtime   -> verify_TBD "verify_addl_state: TI_Addl_State_mtime not handled yet"
    TI_Addl_State_pc_paddr  pc_paddr -> verify_TBD "verify_addl_state: TI_Addl_State_pc_paddr not handled yet"
    TI_Addl_State_pc        pc       -> (do
                                            let pc1 = mstate_pc_read  mstate2
                                            if (fromIntegral pc == pc1) then
                                              return True
                                              else
                                              do
                                                putStrLn  ("PC mismatch:")
                                                putStrLn  ("    Forvis value: " ++ showHex pc1 "")
                                                putStrLn  ("    DUT    value: " ++ showHex pc  "")
                                                return False)

-- ================================================================

verify_reg_write :: Machine_State -> Machine_State -> Word64 -> Word64 -> IO Bool
verify_reg_write    mstate1          mstate2          reg       val = do
  if (reg < 0x1000)
    then
    do
      let val1 = mstate_csr_read  (fromIntegral  reg)  mstate2
      if (fromIntegral val == val1) then
        return True
        else
        do
          putStrLn  ("CSR-write mismatch for csr " ++ (name_of_csr_with_addr  reg) ++ " (0x" ++ showHex reg ")")
          putStrLn  ("    Forvis value: " ++ showHex val1 "")
          putStrLn  ("    DUT    value: " ++ showHex val  "")
          return False

    else if (reg < 0x1020)
    then
    do
      let gpr = reg - 0x1000
          val1 = mstate_gpr_read  (fromIntegral  gpr)  mstate2
      if (fromIntegral val == val1) then
        return True
        else
        do
          putStrLn  ("GPR-write mismatch for gpr " ++ (name_of_gpr_with_addr  gpr) ++ " (" ++ show gpr ++ ")")
          putStrLn  ("    Forvis value: " ++ showHex val1 "")
          putStrLn  ("    DUT    value: " ++ showHex val  "")
          return False

    else if (reg < 0x1040)
    then
    do
      let fpr = reg - 0x1020
          val1 = mstate_fpr_read  (fromIntegral  fpr)  mstate2
      if (fromIntegral val == val1) then
        return True
        else
        do
          putStrLn  ("FPR-write mismatch for fpr " ++ show fpr)
          putStrLn  ("    Forvis value: " ++ showHex val1 "")
          putStrLn  ("    DUT    value: " ++ showHex val  "")
          return False

    else
    do
      putStrLn ("WARNING: verify_reg_write: unknown register number: " ++ showHex reg "; ignoring")
      return True

-- ================================================================

verify_instr32 :: Machine_State -> Machine_State -> Word64 -> IO Bool
verify_instr32    mstate1          mstate2          instr32 = do
  let (fetch_result, mstate') = instr_fetch  mstate1
  case fetch_result of
    Fetch       instr32_a  -> if (fromIntegral instr32 == instr32_a) then
                                return True
                              else
                                do
                                  putStrLn  ("verify_instr32: mismatch")
                                  putStrLn  ("    Forvis instr32: " ++ showHex instr32_a "")
                                  putStrLn  ("    DUT    instr32: " ++ showHex instr32   "")
                                  return False

    Fetch_C     instr16_a  -> (do
                                  putStrLn  ("verify_instr32: mismatch:")
                                  putStrLn  ("    Forvis instr16 (compressed): " ++ showHex instr16_a "")
                                  putStrLn  ("    DUT    instr32             : " ++ showHex instr32   "")
                                  return False)

    Fetch_Trap  trap_cause -> (do
                                  putStrLn ("verify_instr32: " ++ show_trap_exc_code trap_cause)
                                  putStrLn  ("    DUT    instr32: " ++ showHex instr32   "")
                                  return False)


verify_instr16 :: Machine_State -> Machine_State -> Word64 -> IO Bool
verify_instr16    mstate1          mstate2          instr16 = do
  let (load_result, mstate') = instr_fetch  mstate1
  case load_result of
    Fetch_C  instr16_a    -> if (fromIntegral instr16 == instr16_a) then
                               return True
                             else
                               do
                                 putStrLn  ("verify_instr16: mismatch")
                                 putStrLn  ("    Forvis instr16: " ++ showHex instr16_a "")
                                 putStrLn  ("    DUT    instr16: " ++ showHex instr16   "")
                                 return False

    Fetch  instr32_a      -> (do
                                  putStrLn  ("verify_instr16: mismatch:")
                                  putStrLn  ("    Forvis instr32             : " ++ showHex instr32_a "")
                                  putStrLn  ("    DUT    instr16 (compressed): " ++ showHex instr16   "")
                                  return False)

    Fetch_Trap trap_cause -> (do
                                 putStrLn ("verify_instr16: memory read trap " ++ show trap_cause)
                                 putStrLn  ("    DUT    instr16: " ++ showHex instr16   "")
                                 return False)
  

-- ================================================================

report_states :: Machine_State -> Machine_State -> IO ()
report_states    mstate1          mstate2 = do
  putStrLn  ">================================================================"
  putStrLn  "State before instruction"
  mstate_print  ""  mstate1
  putStrLn  ">================================================================"
  putStrLn  "State after instruction"
  mstate_print  ""  mstate2
  putStrLn  ">================================================================"

-- ================================================================

verify_TBD :: String -> IO Bool
verify_TBD    message = do
  putStrLn  ("TBD: " ++ message)
  return True

-- ================================================================

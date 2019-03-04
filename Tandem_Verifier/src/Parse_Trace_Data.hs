-- Copyright (c) 2018-2019 Rishiyur S. Nikhil
-- See LICENSE for license details

module Parse_Trace_Data where

-- ================================================================
-- Standard Haskell imports

import Data.Bits
import Data.Word

import Numeric (showHex)

import Control.Monad (when)

import System.IO                -- for IOMode, hFileSize

import qualified Data.ByteString.Lazy as BSL

-- Project imports

import RegNames

-- ================================================================
-- Binary trace data encodings

te_opcode_begin_group     :: Word64;     te_opcode_begin_group      = 1
te_opcode_end_group       :: Word64;     te_opcode_end_group        = 2
te_opcode_incr_pc         :: Word64;     te_opcode_incr_pc          = 3
te_opcode_full_reg        :: Word64;     te_opcode_full_reg         = 4
te_opcode_incr_reg_add    :: Word64;     te_opcode_incr_reg_add     = 5
te_opcode_incr_reg_or     :: Word64;     te_opcode_incr_reg_or      = 6
te_opcode_addl_state      :: Word64;     te_opcode_addl_state       = 7
te_opcode_mem_req         :: Word64;     te_opcode_mem_req          = 8
te_opcode_mem_rsp         :: Word64;     te_opcode_mem_rsp          = 9
te_opcode_hart_reset      :: Word64;     te_opcode_hart_reset       = 10
te_opcode_state_init      :: Word64;     te_opcode_state_init       = 11
te_opcode_instr16         :: Word64;     te_opcode_instr16          = 16
te_opcode_instr32         :: Word64;     te_opcode_instr32          = 17

te_mem_req_size_8         :: Word64;     te_mem_req_size_8          = 0
te_mem_req_size_16        :: Word64;     te_mem_req_size_16         = 1
te_mem_req_size_32        :: Word64;     te_mem_req_size_32         = 2
te_mem_req_size_64        :: Word64;     te_mem_req_size_64         = 3

te_mem_req_op_LOAD        :: Word64;     te_mem_req_op_LOAD         = 0
te_mem_req_op_STORE       :: Word64;     te_mem_req_op_STORE        = 1
te_mem_req_op_LR          :: Word64;     te_mem_req_op_LR           = 2
te_mem_req_op_SC          :: Word64;     te_mem_req_op_SC           = 3
te_mem_req_op_AMO_swap    :: Word64;     te_mem_req_op_AMO_swap     = 4
te_mem_req_op_AMO_add     :: Word64;     te_mem_req_op_AMO_add      = 5
te_mem_req_op_AMO_xor     :: Word64;     te_mem_req_op_AMO_xor      = 6
te_mem_req_op_AMO_and     :: Word64;     te_mem_req_op_AMO_and      = 7
te_mem_req_op_AMO_or      :: Word64;     te_mem_req_op_AMO_or       = 8
te_mem_req_op_AMO_min     :: Word64;     te_mem_req_op_AMO_min      = 9
te_mem_req_op_AMO_max     :: Word64;     te_mem_req_op_AMO_max      = 10
te_mem_req_op_AMO_minu    :: Word64;     te_mem_req_op_AMO_minu     = 11
te_mem_req_op_AMO_maxu    :: Word64;     te_mem_req_op_AMO_maxu     = 12
te_mem_req_op_IFETCH      :: Word64;     te_mem_req_op_IFETCH       = 13

te_mem_req_result_success :: Word64;    te_mem_req_result_success  = 0
te_mem_req_result_failure :: Word64;    te_mem_req_result_failure  = 1

te_uarch_state_priv       :: Word64;    te_uarch_state_priv      = 1
te_uarch_state_paddr      :: Word64;    te_uarch_state_paddr     = 2
te_uarch_state_eaddr      :: Word64;    te_uarch_state_eaddr     = 3
te_uarch_state_wdata8     :: Word64;    te_uarch_state_wdata8    = 4
te_uarch_state_wdata16    :: Word64;    te_uarch_state_wdata16   = 5
te_uarch_state_wdata32    :: Word64;    te_uarch_state_wdata32   = 6
te_uarch_state_wdata64    :: Word64;    te_uarch_state_wdata64   = 7
te_uarch_state_mtime      :: Word64;    te_uarch_state_mtime     = 8
te_uarch_state_pc_paddr   :: Word64;    te_uarch_state_pc_paddr  = 9
te_uarch_state_pc         :: Word64;    te_uarch_state_pc        = 10

show_mem_op :: String -> Word64 -> String
show_mem_op    prefix    op
  | (op == te_mem_req_op_LOAD)     = prefix ++ "LOAD"
  | (op == te_mem_req_op_STORE)    = prefix ++ "STORE"
  | (op == te_mem_req_op_LR)       = prefix ++ "LR"
  | (op == te_mem_req_op_SC)       = prefix ++ "SC"
  | (op == te_mem_req_op_AMO_swap) = prefix ++ "AMO_swap"
  | (op == te_mem_req_op_AMO_add)  = prefix ++ "AMO_add"
  | (op == te_mem_req_op_AMO_xor)  = prefix ++ "AMO_xor"
  | (op == te_mem_req_op_AMO_and)  = prefix ++ "AMO_and"
  | (op == te_mem_req_op_AMO_or)   = prefix ++ "AMO_or"
  | (op == te_mem_req_op_AMO_min)  = prefix ++ "AMO_min"
  | (op == te_mem_req_op_AMO_max)  = prefix ++ "AMO_max"
  | (op == te_mem_req_op_AMO_minu) = prefix ++ "AMO_minu"
  | (op == te_mem_req_op_AMO_maxu) = prefix ++ "AMO_maxu"
  | (op == te_mem_req_op_IFETCH)   = prefix ++ "IFETCH"
  | True = error $ "ERROR: unknown mem op: " ++ show  op

show_mem_size :: String -> Word64 -> String
show_mem_size    prefix    size
  | (size == te_mem_req_size_8)  = "Size8"
  | (size == te_mem_req_size_16) = "Size16"
  | (size == te_mem_req_size_32) = "Size32"
  | (size == te_mem_req_size_64) = "Size64"
  | True = error $ "ERROR: unknown mem data size: " ++ show  size

show_mem_result :: String -> Word64 -> String
show_mem_result    prefix    result
  | (result == te_mem_req_result_success) = "Success"
  | (result == te_mem_req_result_failure) = "Failure"
  | True = error $ "ERROR: unknown mem result code: " ++ show  result

-- ================================================================
-- Abstract Syntax Tree of items

data State = State {xlen              :: Int,
                    flen              :: Int,
                    ilen              :: Int,
                    mlen              :: Int,
                    in_group          :: Bool}

data Trace_Item  = TI_Group         [Trace_Item]
                 | TI_Incr_PC
                 | TI_Full_Reg      Word64  Word64                     -- reg-addr, value
                 | TI_Incr_Reg_Add  Word64  Word64                     -- reg-addr, offset
                 | TI_Incr_Reg_Or   Word64  Word64                     -- reg addr, or-mask
                 | TI_Addl_State    TI_Addl_State_t
                 | TI_Mem_Req       Word64  Word64   Word64  Word64    -- addr, op, size, wdata
                 | TI_Mem_Rsp       Word64   Word64   Word64           -- size, result, rdata
                 | TI_Hart_Reset
                 | TI_State_Init
                 | TI_Instr16       Word64
                 | TI_Instr32       Word64
                 deriving (Eq)

data TI_Addl_State_t = TI_Addl_State_priv      Word64    -- priv
                     | TI_Addl_State_paddr     Word64    -- MLEN bits
                     | TI_Addl_State_eaddr     Word64    -- XLEN bits
                     | TI_Addl_State_wdata8    Word64
                     | TI_Addl_State_wdata16   Word64
                     | TI_Addl_State_wdata32   Word64
                     | TI_Addl_State_wdata64   Word64
                     | TI_Addl_State_mtime     Word64    -- 64 bits
                     | TI_Addl_State_pc_paddr  Word64    -- MLEN bits
                     | TI_Addl_State_pc        Word64    -- XLEN bits
                     deriving (Eq)

instance  Show  Trace_Item  where
  show  (TI_Group         items)          = "{ " ++ concat (map show items) ++ " }"
  show  TI_Incr_PC                        = "Incr_PC"
  show  (TI_Full_Reg  addr  w64)          = "[" ++ show_regname addr ++ " " ++ showHex w64 "]"
  show  (TI_Incr_Reg_Add  addr  w64)      = "[" ++ show_regname addr ++ " " ++ showHex w64 "]"
  show  (TI_Incr_Reg_Or  addr  w64)       = "[" ++ show_regname addr ++ " " ++ showHex w64 "]"
  show  (TI_Addl_State  addl_state)       = show addl_state
  show  (TI_Mem_Req  addr  op  size  w64) = ("Mem_Req[0x" ++ showHex w64 "]"
                                             ++ show_mem_op  " " op
                                             ++ show_mem_size " " size
                                             ++ " 0x" ++ showHex w64 "")
  show  (TI_Mem_Rsp  size  result  w64)   = ("Mem_Rsp"
                                             ++ show_mem_size " " size
                                             ++ show_mem_result  " " result
                                             ++ " 0x" ++ showHex w64 "")
  show  TI_Hart_Reset                     = "RESET"
  show  TI_State_Init                     = "State_Init"
  show  (TI_Instr16  w64)                 = "[C.instr " ++ showHex_in_width w64 4 ++ "]"
  show  (TI_Instr32  w64)                 = "[instr " ++ showHex_in_width w64 8 ++ "]"


instance  Show TI_Addl_State_t  where
  show (TI_Addl_State_priv      w64) = "[priv "     ++ show w64 ++ "]"
  show (TI_Addl_State_paddr     w64) = "[paddr "    ++ showHex w64 "]"
  show (TI_Addl_State_eaddr     w64) = "[eaddr "    ++ showHex w64 "]"
  show (TI_Addl_State_wdata8    w64) = "[wdata8 "  ++ showHex w64 "]"
  show (TI_Addl_State_wdata16   w64) = "[wdata16 " ++ showHex w64 "]"
  show (TI_Addl_State_wdata32   w64) = "[data32 "  ++ showHex w64 "]"
  show (TI_Addl_State_wdata64   w64) = "[data64 "  ++ showHex w64 "]"
  show (TI_Addl_State_mtime     w64) = "[mtime "    ++ showHex w64 "]"
  show (TI_Addl_State_pc_paddr  w64) = "[pc_paddr " ++ showHex w64 "]"
  show (TI_Addl_State_pc        w64) = "[pc "       ++ showHex w64 "]"

-- ================================================================

parse_trace_data :: State -> Handle -> IO [Trace_Item]
parse_trace_data    state   handle  = do
  items <- parse_main  state  handle  Nothing
  return  items

parse_main :: State -> Handle -> Maybe Trace_Item -> IO [Trace_Item]
parse_main    state    handle    m_last_item = do
  eof <- hIsEOF  handle
  if eof then
    return []
    else
    do
      bs <- BSL.hGet  handle  1
      let te_opcode = fromIntegral (BSL.index  bs  0)
      if te_opcode == te_opcode_end_group then
        if in_group state then
          return []
        else do
          putStrLn $ "WARNING: 'end group' encountered while not in a group; ignoring"
          parse_main  state  handle  Nothing
        else
        do
          let maybe_n_bytes = expected_mem_rsp_num_bytes  m_last_item
              parse_fn      = select_parse_fn  te_opcode  maybe_n_bytes

          item <- parse_fn  state  handle
          -- Debugging: putStrLn $ show item
          items <- parse_main  state  handle  (Just item)
          return (item:items)
            
select_parse_fn :: Word64 ->  Maybe Int -> (State -> Handle -> IO Trace_Item)
select_parse_fn    te_opcode  m_num_bytes
  | (te_opcode == te_opcode_begin_group)  = parse_begin_group
  | (te_opcode == te_opcode_incr_pc)      = parse_incr_pc
  | (te_opcode == te_opcode_full_reg)     = parse_full_reg
  | (te_opcode == te_opcode_incr_reg_add) = parse_incr_reg_add
  | (te_opcode == te_opcode_incr_reg_or)  = parse_incr_reg_or
  | (te_opcode == te_opcode_addl_state)   = parse_addl_state
  | (te_opcode == te_opcode_mem_req)      = parse_mem_req
  | (te_opcode == te_opcode_mem_rsp)      = parse_mem_rsp  m_num_bytes
  | (te_opcode == te_opcode_hart_reset)   = parse_hart_reset
  | (te_opcode == te_opcode_state_init)   = parse_state_init
  | (te_opcode == te_opcode_instr16)      = parse_instr16
  | (te_opcode == te_opcode_instr32)      = parse_instr32
  | True = error ("ERROR: unknown opcode 0x" ++ showHex te_opcode "")

-- 'expected_mem_rsp_num_bytes  m_last_item' returns
--    'Just n_bytes' if last item is a memory request expecting data of 'size' (1,2,4,8) in response
--    'Just 0'       if last item is a memory request expecting no data in response
--    'Nothing'   otherwise
-- 'ritems' is the reversed list of items, with the most recent item as its head.

expected_mem_rsp_num_bytes :: Maybe  Trace_Item -> Maybe Int
expected_mem_rsp_num_bytes    Nothing = Nothing
expected_mem_rsp_num_bytes    (Just item) =
  case item of
    TI_Mem_Req  addr  op  rsize  _ -> if ((op == te_mem_req_op_LOAD)
                                          || (op == te_mem_req_op_LR)
                                          || (op == te_mem_req_op_AMO_swap)
                                          || (op == te_mem_req_op_AMO_add)
                                          || (op == te_mem_req_op_AMO_xor)
                                          || (op == te_mem_req_op_AMO_and)
                                          || (op == te_mem_req_op_AMO_or)
                                          || (op == te_mem_req_op_AMO_min)
                                          || (op == te_mem_req_op_AMO_max)
                                          || (op == te_mem_req_op_AMO_minu)
                                          || (op == te_mem_req_op_AMO_maxu)
                                          || (op == te_mem_req_op_IFETCH)) then
                                        Just (size_code_to_num_bytes  rsize)
                                      else
                                        Just 0    -- STORE and SC
    _ -> Nothing

-- ================================================================

parse_begin_group :: State -> Handle -> IO Trace_Item
parse_begin_group    state   handle = do
  let state1 = state {in_group = True}
  ritems1 <- parse_main  state1  handle  Nothing
  return $ TI_Group ritems1

-- ================================================================

parse_incr_pc :: State -> Handle -> IO Trace_Item
parse_incr_pc    state   handle = do
  return TI_Incr_PC

-- ================================================================

parse_full_reg :: State -> Handle -> IO Trace_Item
parse_full_reg    state   handle = do
  bs1 <- BSL.hGet  handle  2
  bs2 <- BSL.hGet  handle  (if xlen state == 32 then 4 else 8)
  return $ TI_Full_Reg  (concat_bytes  bs1)  (concat_bytes  bs2)

-- ================================================================

parse_incr_reg_add :: State -> Handle -> IO Trace_Item
parse_incr_reg_add    state   handle = do
  bs1 <- BSL.hGet  handle  2
  bs2 <- BSL.hGet  handle  1
  return $ TI_Incr_Reg_Add  (concat_bytes  bs1)  (concat_bytes  bs2)

-- ================================================================

parse_incr_reg_or :: State -> Handle -> IO Trace_Item
parse_incr_reg_or    state   handle = do
  bs1 <- BSL.hGet  handle  2
  bs2 <- BSL.hGet  handle  1
  return $ TI_Incr_Reg_Or  (concat_bytes  bs1)  (concat_bytes  bs2)

-- ================================================================

parse_addl_state :: State -> Handle -> IO Trace_Item
parse_addl_state    state   handle = do
  bs <- BSL.hGet  handle  1
  let te_uarch_state_id = fromIntegral (BSL.index  bs  0)
      parse_fn | (te_uarch_state_id == te_uarch_state_priv)     = parse_priv
               | (te_uarch_state_id == te_uarch_state_paddr)    = parse_paddr
               | (te_uarch_state_id == te_uarch_state_eaddr)    = parse_eaddr
               | (te_uarch_state_id == te_uarch_state_wdata8)   = parse_wdata8
               | (te_uarch_state_id == te_uarch_state_wdata16)  = parse_wdata16
               | (te_uarch_state_id == te_uarch_state_wdata32)  = parse_wdata32
               | (te_uarch_state_id == te_uarch_state_wdata64)  = parse_wdata64
               | (te_uarch_state_id == te_uarch_state_mtime)    = parse_mtime
               | (te_uarch_state_id == te_uarch_state_pc_paddr) = parse_pc_paddr
               | (te_uarch_state_id == te_uarch_state_pc)       = parse_pc
               | True = error ("ERROR: unknown te_uarch_state id code 0x" ++ showHex  te_uarch_state_id "")
  parse_fn  state  handle

parse_priv :: State -> Handle -> IO Trace_Item
parse_priv    state   handle = do
  bs <- BSL.hGet  handle  1
  return $ TI_Addl_State  (TI_Addl_State_priv  (concat_bytes  bs))

parse_paddr :: State -> Handle -> IO Trace_Item
parse_paddr    state   handle = do
  bs <- BSL.hGet  handle  (if xlen state == 32 then 4 else 8)
  return $ TI_Addl_State  (TI_Addl_State_paddr  (concat_bytes  bs))

parse_eaddr :: State -> Handle -> IO Trace_Item
parse_eaddr    state   handle = do
  bs <- BSL.hGet  handle  (if xlen state == 32 then 4 else 8)
  return $ TI_Addl_State  (TI_Addl_State_eaddr  (concat_bytes  bs))

parse_wdata8 :: State -> Handle -> IO Trace_Item
parse_wdata8    state   handle = do
  bs <- BSL.hGet  handle  1
  return $ TI_Addl_State  (TI_Addl_State_wdata8  (concat_bytes  bs))

parse_wdata16 :: State -> Handle -> IO Trace_Item
parse_wdata16    state   handle = do
  bs <- BSL.hGet  handle  2
  return $ TI_Addl_State  (TI_Addl_State_wdata16  (concat_bytes  bs))

parse_wdata32 :: State -> Handle -> IO Trace_Item
parse_wdata32    state   handle = do
  bs <- BSL.hGet  handle  4
  return $ TI_Addl_State  (TI_Addl_State_wdata32  (concat_bytes  bs))

parse_wdata64 :: State -> Handle -> IO Trace_Item
parse_wdata64    state   handle = do
  bs <- BSL.hGet  handle  8
  return $ TI_Addl_State  (TI_Addl_State_wdata64  (concat_bytes  bs))

parse_mtime :: State -> Handle -> IO Trace_Item
parse_mtime    state   handle = do
  bs <- BSL.hGet  handle  8
  return $ TI_Addl_State  (TI_Addl_State_mtime  (concat_bytes  bs))

parse_pc_paddr :: State -> Handle -> IO Trace_Item
parse_pc_paddr    state   handle = do
  bs <- BSL.hGet  handle  (if xlen state == 32 then 4 else 8)
  return $ TI_Addl_State  (TI_Addl_State_pc_paddr  (concat_bytes  bs))

parse_pc :: State -> Handle -> IO Trace_Item
parse_pc    state   handle = do
  bs <- BSL.hGet  handle  (if xlen state == 32 then 4 else 8)
  return $ TI_Addl_State  (TI_Addl_State_pc  (concat_bytes  bs))

-- ================================================================

parse_mem_req :: State -> Handle -> IO Trace_Item
parse_mem_req    state    handle = do
  bs1 <- BSL.hGet  handle  (mlen_to_num_bytes  (mlen state))
  let addr = (concat_bytes  bs1)

  bs2 <- BSL.hGet  handle  1
  let byte = (BSL.index  bs2  0) :: Word8
      op   = (fromIntegral ((shiftR  byte  4) .&. 0xF)) :: Word64 
      size = (fromIntegral              (byte .&. 0xF)) :: Word64 

  bs3 <- BSL.hGet  handle  (size_code_to_num_bytes  size)

  return $ TI_Mem_Req  addr  op  size  (concat_bytes  bs3)

-- ================================================================

parse_mem_rsp :: Maybe Int      -> State -> Handle -> IO Trace_Item
parse_mem_rsp    Nothing           state    handle = error $ "ERROR: memory response without preceding memory request"
parse_mem_rsp    (Just num_bytes)  state    handle = do
  bs1 <- BSL.hGet  handle  1
  let byte   = (BSL.index  bs1  0) :: Word8
      size   = (fromIntegral ((shiftR  byte  4) .&. 0xF)) :: Word64 
      result = (fromIntegral              (byte .&. 0xF)) :: Word64

      num_bytes_actual = size_code_to_num_bytes  size
  when (num_bytes_actual /= num_bytes)
    (do
        putStrLn $ "ERROR: unexpected memory response size"
        putStrLn $ "    Expected     " ++ show num_bytes        ++ " bytes"
        putStrLn $ "    Response has " ++ show num_bytes_actual ++ " bytes"
        error "Aborting")

  bs2 <- BSL.hGet  handle  num_bytes
  return $ TI_Mem_Rsp  size  result  (concat_bytes  bs2)

-- ================================================================

parse_hart_reset :: State -> Handle -> IO Trace_Item
parse_hart_reset    state   handle = return TI_Hart_Reset

-- ================================================================

parse_state_init :: State -> Handle -> IO Trace_Item
parse_state_init    state   handle = return TI_State_Init

-- ================================================================

parse_instr16 :: State -> Handle -> IO Trace_Item
parse_instr16    state   handle = do
  bs <- BSL.hGet  handle  2
  return $ TI_Instr16 (concat_bytes  bs)

-- ================================================================

parse_instr32 :: State -> Handle -> IO Trace_Item
parse_instr32    state   handle = do
  bs <- BSL.hGet  handle  4
  return $ TI_Instr32 (concat_bytes  bs)

-- ================================================================
-- Print out a list of items

print_items :: [Trace_Item] -> IO ()
print_items    items = print_items_aux  items  1

print_items_aux :: [Trace_Item] -> Int  ->IO ()
print_items_aux    []              inum = return ()
print_items_aux    (item:items)    inum = do
  putStr (show  item)
  if (item_has_instruction  item) then
    do
      putStrLn (" inum " ++ show inum)
      print_items_aux  items  (inum + 1)
    else
    do
      putStrLn ""
      print_items_aux  items  inum

item_has_instruction :: Trace_Item -> Bool
item_has_instruction    (TI_Instr16  _)  = True
item_has_instruction    (TI_Instr32  _)  = True
item_has_instruction    (TI_Group items) = any  item_has_instruction  items
item_has_instruction    _                = False

-- ================================================================
-- Utilities

concat_bytes :: BSL.ByteString -> Word64
concat_bytes    bs =
  BSL.foldr  (\ w8  w64 -> ((shiftL  w64  8) .|. (fromIntegral w8)))  0  bs

mlen_to_num_bytes :: Int -> Int
mlen_to_num_bytes    mlen
  | (mlen == 32) = 4
  | (mlen == 34) = 5
  | (mlen == 64) = 8
  | True = error $ "INTERNAL ERROR: unknown MLEN: " ++ show mlen

size_code_to_num_bytes :: Word64 -> Int
size_code_to_num_bytes    size
  | (size == te_mem_req_size_8)  = 1
  | (size == te_mem_req_size_16) = 2
  | (size == te_mem_req_size_32) = 4
  | (size == te_mem_req_size_64) = 8
  | True = error $ "INTERNAL ERROR: unknown mem req size code: " ++ show size

showHex_in_width :: Word64 -> Int -> String
showHex_in_width    w64       width =
  let
    s1 = showHex  w64  ""
    len = length s1
  in
    if len >= width then s1
    else
      let
        s2 = take  (width - len)  (repeat '0')
      in
        s2 ++ s1

-- ================================================================

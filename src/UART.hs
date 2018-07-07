module UART where

-- ================================================================
-- This is (very partial) model of a classic National Semiconductor
-- NS16550A UART

-- ================================================================
-- Standard Haskell imports

import Data.Word
import Data.Bits
import Data.Char

-- Project imports

-- none

-- ================================================================
-- UART registers and their relative addresses

addr_UART_txd  = 0x00 :: Word64    -- transmit data
addr_UART_rxd  = 0x00 :: Word64    -- receive data
addr_UART_ier  = 0x08 :: Word64    -- interrupt enable register
addr_UART_iir  = 0x10 :: Word64    -- interrupt id register
addr_UART_lcr  = 0x18 :: Word64    -- line control reg
addr_UART_mcr  = 0x20 :: Word64    -- modem control reg
addr_UART_lsr  = 0x28 :: Word64    -- line status reg
addr_UART_msr  = 0x30 :: Word64    -- modem status reg
addr_UART_scr  = 0x38 :: Word64    -- scratch pad reg

-- Bit fields of LSR
uart_lsr_dr    = 0x01 :: Word8
uart_lsr_oe    = 0x02 :: Word8
uart_lsr_pe    = 0x04 :: Word8
uart_lsr_fe    = 0x08 :: Word8
uart_lsr_bi    = 0x10 :: Word8
uart_lsr_thre  = 0x20 :: Word8
uart_lsr_temt  = 0x40 :: Word8
uart_lsr_rxfe  = 0x80 :: Word8

-- ================================================================
-- UART representation
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data UART_NS16550A = UART_NS16550A {
  -- f_rxd_a:  uart input that has already been consumed
  -- f_rxd_b:  uart input that nas not yet been consumed
  -- f_rxd_a ++ f_rxd_b:  all uart intput
  f_rxd_a :: String,
  f_rxd_b :: String,

  -- f_txd_a:  uart output that has already been consumed
  -- f_txd_b:  uart output that nas not yet been consumed
  -- f_txd_a ++ f_txd_b:  all uart output
  f_txd_a :: String,
  f_txd_b :: String,

  f_ier      :: Word8,
  f_iir      :: Word8,
  f_lcr      :: Word8,
  f_mcr      :: Word8,
  f_lsr      :: Word8,
  f_msr      :: Word8,
  f_scr      :: Word8
  }

-- ================================================================
-- Create a UART

mkUART :: UART_NS16550A
mkUART =  UART_NS16550A { f_rxd_a = "",
                          f_rxd_b = "",

                          f_txd_a = "",
                          f_txd_b = "",

                          f_ier = 0,
                          f_iir = 0,
                          f_lcr = 0,
                          f_mcr = 0,
                          f_lsr = (uart_lsr_temt .|. uart_lsr_thre),
                          f_msr = 0,
                          f_scr = 0 }

-- ================================================================
-- CPU read from UART

uart_read :: UART_NS16550A -> Word64 -> (Word8, UART_NS16550A)
uart_read  uart  addr =
  if (addr == addr_UART_rxd) then
    case (f_rxd_b  uart) of
      []     -> (0, uart)
      (c:cs) -> (let
                    v     = fromIntegral (ord  c)
                    uart' = uart {f_rxd_a = (f_rxd_a  uart) ++ [c],
                                  f_rxd_b = cs }
                  in
                    (v, uart'))

  else if (addr == addr_UART_ier) then (f_ier  uart,  uart)
  else if (addr == addr_UART_iir) then (f_iir  uart,  uart)
  else if (addr == addr_UART_lcr) then (f_lcr  uart,  uart)
  else if (addr == addr_UART_mcr) then (f_mcr  uart,  uart)
  else if (addr == addr_UART_lsr) then
    let
      lsr1 = f_lsr  uart
      lsr2 = if ((f_rxd_b  uart) /= []) then (lsr2 .|. uart_lsr_dr) else lsr1
    in
      (lsr2, uart)
  else if (addr == addr_UART_msr) then (f_msr  uart,  uart)
  else if (addr == addr_UART_scr) then (f_scr  uart,  uart)
  else (0,  uart)

-- ================================================================
-- CPU write to UART

uart_write :: UART_NS16550A -> Word64 -> Word64 -> UART_NS16550A
uart_write  uart  addr  val =
  if (addr == addr_UART_txd) then
    let
      txd_b = f_txd_b  uart
      char  = chr (fromIntegral val)
      uart' = uart { f_txd_b = txd_b ++ [char] }
    in
      uart'

  else if (addr == addr_UART_ier) then uart { f_ier = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_iir) then uart { f_iir = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_lcr) then uart { f_lcr = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_mcr) then uart { f_mcr = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_lsr) then uart { f_lsr = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_msr) then uart { f_msr = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_scr) then uart { f_scr = fromIntegral (val .&. 0xFF) }
  else uart

-- ================================================================
-- System append input from console into UART

uart_enq_input :: UART_NS16550A -> String -> UART_NS16550A
uart_enq_input  uart  s =
  case s of
    [] -> uart
    _  -> uart { f_rxd_b = (f_rxd_b  uart) ++ s }

-- ================================================================
-- System consume output from UART for output to console

uart_deq_output :: UART_NS16550A -> (String, UART_NS16550A)
uart_deq_output  uart =
  let
    txd_b = f_txd_b  uart
  in
    case  txd_b  of
      [] -> ("", uart)
      cs -> (txd_b, uart {f_txd_a = (f_txd_a  uart) ++ txd_b,
                          f_txd_b = ""})

-- ================================================================
-- Read all UART input and output

uart_all_input :: UART_NS16550A -> String
uart_all_input  uart = (f_rxd_a  uart) ++ (f_rxd_b  uart)

uart_all_output :: UART_NS16550A -> String
uart_all_output  uart = (f_txd_a  uart) ++ (f_txd_b  uart)

-- ================================================================

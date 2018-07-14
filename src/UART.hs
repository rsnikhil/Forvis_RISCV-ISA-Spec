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

addr_UART_rxd  = 0x00 :: Word64    -- receive data
addr_UART_txd  = 0x00 :: Word64    -- transmit data
addr_UART_dll  = 0x00 :: Word64    -- divisor latch low

addr_UART_ier  = 0x08 :: Word64    -- interrupt enable register
addr_UART_dlh  = 0x08 :: Word64    -- divisor latch high

addr_UART_iir  = 0x10 :: Word64    -- interrupt id register
addr_UART_fcr  = 0x10 :: Word64    -- fifo control reg
addr_UART_efr  = 0x10 :: Word64    -- ??

addr_UART_lcr  = 0x18 :: Word64    -- line control reg
addr_UART_mcr  = 0x20 :: Word64    -- modem control reg
addr_UART_lsr  = 0x28 :: Word64    -- line status reg
addr_UART_msr  = 0x30 :: Word64    -- modem status reg
addr_UART_scr  = 0x38 :: Word64    -- scratch pad reg

-- Bit fields of LCR
uart_lcr_dlab  = 0x80 :: Word8     -- Divisor latch access bit
uart_lcr_bc    = 0x40 :: Word8     -- Break control
uart_lcr_sp    = 0x20 :: Word8     -- Stick parity
uart_lcr_eps   = 0x10 :: Word8     -- Even parity
uart_lcr_pen   = 0x08 :: Word8     -- Parity enable
uart_lcr_stb   = 0x04 :: Word8     -- # of stop bits (0=1b,1=2b)
uart_lcr_wls   = 0x03 :: Word8     -- word len (0:5b,1:6b,2:7b,3:8b)

-- Bit fields of LSR
uart_lsr_rxfe  = 0x80 :: Word8
uart_lsr_temt  = 0x40 :: Word8
uart_lsr_thre  = 0x20 :: Word8
uart_lsr_bi    = 0x10 :: Word8
uart_lsr_fe    = 0x08 :: Word8
uart_lsr_pe    = 0x04 :: Word8
uart_lsr_oe    = 0x02 :: Word8
uart_lsr_dr    = 0x01 :: Word8

-- ================================================================
-- UART representation
-- This is a private internal representation that can be changed at
-- will; only the exported API can be used by clients.

data UART_NS16550A = UART_NS16550A {
  -- f_rxd_a ++ f_rxd_b:  all uart intput
  f_rxd_a :: String,      -- consumed
  f_rxd_b :: String,      -- 0, pending

  -- f_txd_a ++ f_txd_b:  all uart output
  f_txd_a :: String,      -- sent
  f_txd_b :: String,      -- 0, pending

  f_dll      :: Word8,    -- 0    if LCR.DLAB=1

  f_ier      :: Word8,    -- 1
  f_dlh      :: Word8,    -- 1    if LCR.DLAB=1

  f_iir      :: Word8,    -- 2    Read
  f_fcr      :: Word8,    -- 2    Write
  f_efr      :: Word8,    -- 2    Write

  f_lcr      :: Word8,    -- 3
  f_mcr      :: Word8,    -- 4
  f_lsr      :: Word8,    -- 5
  f_msr      :: Word8,    -- 6
  f_scr      :: Word8     -- 7
  }

-- ================================================================
-- Create a UART

mkUART :: UART_NS16550A
mkUART =  UART_NS16550A { f_rxd_a = "",
                          f_rxd_b = "",

                          f_txd_a = "",
                          f_txd_b = "",

                          f_dll = 0,

                          f_ier = 0,
                          f_dlh = 0,

                          f_iir = 1,    -- no interrupt pending
                          f_fcr = 0,
                          f_efr = 0,

                          f_lcr = 0,
                          f_mcr = 0,
                          f_lsr = (uart_lsr_temt .|. uart_lsr_thre),
                          f_msr = 0,
                          f_scr = 0 }

-- ================================================================
-- Periodic action on UART
-- Raise interrupt if interrupts enabled and input is available

uart_tick :: UART_NS16550A -> (Bool, UART_NS16550A)
uart_tick  uart =
  let
    ier   = f_ier    uart
    rxd_b = f_rxd_b  uart
    iir   = f_iir    uart
    iir'  = (iir .&. 0xFE)    -- raise interrupt
  in
    if (((ier .&. 0x1) /= 0) && (rxd_b /= "")) then
      (True, uart {f_iir = iir'})
    else
      (False, uart)

-- ================================================================
-- CPU read from UART

uart_read :: UART_NS16550A -> Word64 -> (Word8, UART_NS16550A)
uart_read  uart  addr =
  if (addr == addr_UART_rxd) then
    if (((f_lcr  uart) .&. uart_lcr_dlab) /= 0) then
      (f_dll uart, uart)
    else
      case (f_rxd_b  uart) of
        []     -> (0, uart)
        (c:cs) -> (let
                      v     = fromIntegral (ord  c)
                      uart' = uart {f_rxd_a = (f_rxd_a  uart) ++ [c],
                                    f_rxd_b = cs }
                   in
                     (v, uart'))

  else if (addr == addr_UART_ier) then
    if (((f_lcr  uart) .&. uart_lcr_dlab) /= 0) then
      (f_dlh uart, uart)
    else
      (f_ier  uart,  uart)

  else if (addr == addr_UART_iir) then
    if (f_lcr uart == 0xBF) then
      (f_efr  uart,  uart)
    else
      let
        iir  = f_iir  uart
        iir' = iir .|. 0x1    -- clear interrupt
        uart' = uart { f_iir = iir' }
      in
        (iir,  uart')

  else if (addr == addr_UART_lcr) then (f_lcr  uart,  uart)
  else if (addr == addr_UART_mcr) then (f_mcr  uart,  uart)
  else if (addr == addr_UART_lsr) then
    let
      lsr1 = f_lsr  uart
      lsr2 = if ((f_rxd_b  uart) /= "") then (lsr1 .|. uart_lsr_dr) else lsr1
    in
      (lsr2, uart)
  else if (addr == addr_UART_msr) then (f_msr  uart,  uart)
  else if (addr == addr_UART_scr) then (f_scr  uart,  uart)
  else (0xFF,  uart)    -- Arbitrary 0xFF; TODO: raise an access fault

-- ================================================================
-- CPU write to UART

uart_write :: UART_NS16550A -> Word64 -> Word64 -> UART_NS16550A
uart_write  uart  addr  val =
  if (addr == addr_UART_txd) then
    if (((f_lcr  uart) .&. uart_lcr_dlab) /= 0) then
      uart { f_dll = fromIntegral (val .&. 0xFF) }
    else
      let
        txd_b = f_txd_b  uart
        char  = chr (fromIntegral val)
        iir   = f_iir  uart
        iir'  = if ((f_ier uart .&. 0x2) /= 0) then
                  (iir .&. 0xFE)    -- raise interrupt
                else
                  iir
        uart' = uart { f_txd_b = txd_b ++ [char],
                       f_iir   = iir'}
      in
        uart'

  else if (addr == addr_UART_ier) then
    if (((f_lcr  uart) .&. uart_lcr_dlab) /= 0) then
      uart { f_dlh = fromIntegral (val .&. 0xFF) }
    else
      uart { f_ier = fromIntegral (val .&. 0xFF) }

  else if (addr == addr_UART_fcr) then
    if ((f_lcr  uart) == 0xBF) then
      uart { f_efr = fromIntegral (val .&. 0xFF) }
    else
      uart { f_fcr = fromIntegral (val .&. 0xFF) }

  else if (addr == addr_UART_lcr) then uart { f_lcr = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_mcr) then uart { f_mcr = fromIntegral (val .&. 0xFF) }
  else if (addr == addr_UART_lsr) then uart    -- LSR is read-only
  else if (addr == addr_UART_msr) then uart    -- MSR is read-only
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

uart_all_input :: UART_NS16550A -> (String, String)
uart_all_input  uart = ((f_rxd_a  uart),  (f_rxd_b  uart))

uart_all_output :: UART_NS16550A -> (String, String)
uart_all_output  uart = ((f_txd_a  uart), (f_txd_b  uart))

-- ================================================================

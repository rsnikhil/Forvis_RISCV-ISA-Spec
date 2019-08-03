module Encoder where

import Bit_Utils
import Arch_Defs
import Forvis_Spec_I

import Data.Bits

-------------------------------------------------------
-- This belongs in /src!
--
-- Encoding of instructions. Use old functionality in ZZ_OLD/v3/ArchDefs
-- to encode instructions.

--opcodeE x   = shiftL x 0
--rdE x       = shiftL x 7
--funct3E x   = shiftL x 12
--rs1E x      = shiftL x 15
--rs2E x      = shiftL x 20
--funct7E x   = shiftL x 25
--imm12E x    = shiftL x 20

encode_I :: RV -> Instr_I -> Instr_32b
encode_I rv (ADDI rd rs1 imm12) = mkInstr_I_type imm12 rs1 0 rd opcode_OP_IMM 
encode_I rv (LW rd rs1 imm12)   = mkInstr_I_type imm12 rs1 funct3_LW rd opcode_LOAD
encode_I rv (SW rs1 rs2 imm12)  = mkInstr_S_type imm12 rs2 rs1 funct3_SW opcode_STORE
encode_I rv (ADD rd rs1 rs2)    = mkInstr_R_type funct7_ADD rs2 rs1 funct3_ADD rd opcode_OP
encode_I rv (JAL rd imm21)      = mkInstr_J_type imm21 rd opcode_JAL
encode_I rv (BLT rs1 rs2 imm13) = mkInstr_B_type imm13 rs1 funct3_BLT rs2 opcode_BRANCH

mkInstr_J_type :: InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_J_type    imm21         rd            opcode =
  let
    legal  = (((   shiftR  imm21  21) == 0)
              && ((shiftR  rd      5) == 0)
              && ((shiftR  opcode  7) == 0))

    bits_31_12 = ((    shiftL  (bitSlice  imm21  20  20)  31)
                  .|. (shiftL  (bitSlice  imm21  10   1)  21)
                  .|. (shiftL  (bitSlice  imm21  11  11)  20)
                  .|. (shiftL  (bitSlice  imm21  19  12)  12))

    instr  = (bits_31_12  .|.  (shiftL  rd  7)  .|.  opcode)
  in
    instr

mkInstr_U_type  :: InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_U_type     imm20         rd            opcode =
  let
    legal = (((   shiftR  imm20  20) == 0)
             && ((shiftR  rd      5) == 0)
             && ((shiftR  opcode  7) == 0))

    instr = ((    shiftL  imm20  12)
             .|. (shiftL  rd      7)
             .|.  opcode)
  in
    instr


mkInstr_I_type :: InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_I_type    imm12         rs1           funct3        rd            opcode =
  let
    legal  = (((   shiftR  imm12  12) == 0)
              && ((shiftR  rs1     5) == 0)
              && ((shiftR  funct3  3) == 0)
              && ((shiftR  rd      5) == 0)
              && ((shiftR  opcode  7) == 0))

    instr  = ((    shiftL  imm12   20)
              .|. (shiftL  rs1     15)
              .|. (shiftL  funct3  12)
              .|. (shiftL  rd       7)
              .|. opcode)
  in
    -- assert  legal  instr
    instr

-- branch?
mkInstr_B_type :: InstrField -> InstrField -> InstrField ->  InstrField -> InstrField -> Instr_32b
mkInstr_B_type    imm13         rs1           funct3        rs2            opcode =
  let
    legal = True -- TODO
    imm12 = imm13 --`div` 2
    instr  = ((    shiftL  (bitSlice imm12 12 12) 31)
              .|. (shiftL  (bitSlice imm12 10  5) 25)
              .|. (shiftL  (bitSlice imm12  4  1)  8)
              .|. (shiftL  (bitSlice imm12 11 11)  7)                           
              .|. (shiftL  rs1     15)
              .|. (shiftL  funct3  12)
              .|. (shiftL  rs2     20)
              .|. opcode)
  in
    instr

mkInstr_R_type :: InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_R_type    funct7        rs2           rs1           funct3        rd            opcode =
  let
    legal  = (((   shiftR  funct7  7) == 0)
              && ((shiftR  rs2     5) == 0)
              && ((shiftR  rs1     5) == 0)
              && ((shiftR  funct3  3) == 0)
              && ((shiftR  rd      5) == 0)
              && ((shiftR  opcode  7) == 0))

    instr = ((   shiftL  funct7  25)
            .|. (shiftL  rs2     20)
            .|. (shiftL  rs1     15)
            .|. (shiftL  funct3  12)
            .|. (shiftL  rd       7)
            .|. opcode)
  in
    --    assert  legal  instr
    instr

{-# INLINE mkInstr_S_type #-}
mkInstr_S_type :: InstrField -> InstrField -> InstrField -> InstrField -> InstrField -> Instr_32b
mkInstr_S_type    imm12         rs2           rs1           funct3        opcode =
  let
    legal  = (((   shiftR  imm12  12) == 0)
              && ((shiftR  rs1     5) == 0)
              && ((shiftR  rs2     5) == 0)
              && ((shiftR  funct3  3) == 0)
              && ((shiftR  opcode  7) == 0))

    instr  = ((    shiftL  (bitSlice  imm12  11 5)  25)
              .|. (shiftL  rs2                      20)
              .|. (shiftL  rs1                      15)
              .|. (shiftL  funct3                   12)
              .|. (shiftL  (bitSlice  imm12   4 0)   7)
              .|. opcode)
  in
    instr

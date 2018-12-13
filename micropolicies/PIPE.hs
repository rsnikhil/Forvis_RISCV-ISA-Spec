module PIPE where

import qualified Data.Map.Strict as Data_Map
import Data.Maybe

import Bit_Utils
import Arch_Defs

-- Maybe?
import Machine_State
import Forvis_Spec_I
import GPR_File
import Memory

--------------------------------------------------------
-- This belongs in /src!

import Data.Bits

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
encode_I rv (SW rd rs1 imm12)   = mkInstr_I_type imm12 rs1 funct3_SW rd opcode_STORE
encode_I rv (ADD rd rs1 rs2)    = mkInstr_R_type funct7_ADD rs2 rs1 funct3_ADD rd opcode_OP

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



--------------------------------------------------------

-- Design decision: Do we want to write policies in Haskell, or in
-- RISCV machine instructions (compiled from C or something).  In this
-- experiment I'm assuming the former.  If we go for the latter, it's
-- going to require quite a bit of lower-level plumbing (including
-- keeping two separate copies of the whole RISCV machine state, and
-- making the explicit connections between them).  The latter is
-- probably what we really want, though.

newtype Tag = Tag ()
  deriving (Eq, Show)
foo = Tag ()

---------------------------------

newtype GPR_FileT = GPR_FileT  (Data_Map.Map  InstrField  Tag)
  deriving (Eq, Show)

mkGPR_FileT :: GPR_FileT
mkGPR_FileT = GPR_FileT (Data_Map.fromList (zip [0..31] (repeat foo)))

gpr_readT :: GPR_FileT ->    GPR_Addr -> Tag
gpr_readT    (GPR_FileT dm)  reg = fromMaybe foo (Data_Map.lookup  reg  dm)

gpr_writeT :: GPR_FileT ->    GPR_Addr -> Tag -> GPR_FileT
gpr_writeT    (GPR_FileT dm)  reg         val =
    seq  val  (GPR_FileT (Data_Map.insert  reg  val  dm))


newtype MemT = MemT (Data_Map.Map Integer Tag)
  deriving (Eq, Show)

mkMemT = MemT (Data_Map.fromList [])

---------------------------------

data PIPE_Result = PIPE_Trap String
                 | PIPE_Success

data PIPE_State = PIPE_State {
  p_pc   :: Tag,
  p_gprs :: GPR_FileT,
  p_mem  :: MemT
  }
  deriving (Eq, Show)

init_pipe_state = PIPE_State {
  p_pc = foo,
  p_gprs = mkGPR_FileT,
  p_mem = mkMemT
  }

---------------------------------

exec_pipe :: PIPE_State -> Machine_State -> Machine_State -> Integer -> (PIPE_State, PIPE_Result)
exec_pipe p m m' u32 =
  let rv  = mstate_rv_read  m
      res = decode_I rv u32
  in case res of
       Just (ADDI _ _ _) -> (p,PIPE_Trap "ADDI")
       _ -> (p,PIPE_Success)

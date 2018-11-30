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

opcodeE x   = shiftL x 0
rdE x       = shiftL x 7
funct3E x   = shiftL x 12
rs1E x      = shiftL x 15
rs2E x      = shiftL x 20
funct7E x   = shiftL x 25
imm12E x    = shiftL x 20

encode_I :: RV -> Instr_I -> Instr_32b
encode_I rv (ADDI rd rs1 imm12) = opcodeE opcode_OP_IMM .|. rdE rd .|. rs1E rs1 .|. imm12E imm12

--------------------------------------------------------

-- Design decision: Do we want to write policies in Haskell, or in
-- RISCV machine instructions (compiled from C or something).  In this
-- experiment I'm assuming the former.  If we go for the latter, it's
-- going to require quite a bit of lower-level plumbing (including
-- keeping two separate copies of the whole RISCV machine state, and
-- making the explicit connections between them).  The latter is
-- probably what we really want, though.

newtype Tag = Tag ()
foo = Tag ()

---------------------------------

newtype GPR_FileT = GPR_FileT  (Data_Map.Map  InstrField  Tag)

mkGPR_FileT :: GPR_FileT
mkGPR_FileT = GPR_FileT (Data_Map.fromList (zip [0..31] (repeat foo)))

gpr_readT :: GPR_FileT ->    GPR_Addr -> Tag
gpr_readT    (GPR_FileT dm)  reg = fromMaybe foo (Data_Map.lookup  reg  dm)

gpr_writeT :: GPR_FileT ->    GPR_Addr -> Tag -> GPR_FileT
gpr_writeT    (GPR_FileT dm)  reg         val =
    seq  val  (GPR_FileT (Data_Map.insert  reg  val  dm))


newtype MemT = MemT (Data_Map.Map Integer Tag)

mkMemT = MemT (Data_Map.fromList [])

---------------------------------

data PIPE_State = PIPE_State {
  p_pc   :: Tag,
  p_gprs :: GPR_FileT,
  p_mem  :: MemT
  }

init_pipe_state = PIPE_State {
  p_pc = foo,
  p_gprs = mkGPR_FileT,
  p_mem = mkMemT
  }

---------------------------------

exec_pipe :: PIPE_State -> Machine_State -> Machine_State -> Integer -> IO (PIPE_State, Bool)
exec_pipe p m m' u32 =
  let rv  = mstate_rv_read  m
      res = decode_I rv u32
  in case res of
       Just (ADDI _ _ _) -> return (p,True)
       _ -> return (p,False)

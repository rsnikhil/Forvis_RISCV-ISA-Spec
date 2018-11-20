module PIPE where

import Bit_Utils
import Arch_Defs
import Machine_State
import Decode

type PIPE_State = ()

init_pipe_state = ()

exec_pipe :: PIPE_State -> Machine_State -> Machine_State -> Integer -> IO (PIPE_State, Bool)
exec_pipe p m m' u32 = return ((),True)
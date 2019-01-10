DPL STUFF

PIPE.hs
heap.dpl












MORE

Nikhil: I've added the 'last_instr_trapped' flag we discussed in email yesterday.
Please see 'git log' for details.

Integrate Andrew's policy language interpreter

make a PR for the /src and / changes

Printing
- Double-check that trace printing is really working: E.g., what will
  happen if the two traces are running completely different
  programs?  (This should be allowed, at least by the printing stuff!)
- When printing reachable colors, we should include (C 0) (if it is reachable!)

Shrink colors in memory locations (and eventually registers)  (Done yet?)

Stack stuff:
  - "stack clean" is too aggressive.  Ideally, we want to remove just PIPE.o!
  - try moving all the stack stuff inside the micropolicies / dpl* directories

___________________________________________________________
BEFORE JANUARY PI MEETING

replace haskell policy by policy interpreter
(Andrew)

get a heap safety policy running using the interpreter
(All)

________________________
AFTER PI MEETING

start thinking about stack safety!
  - look at the policy in the draper repo

improve mutation testing (BCP / Leo)
  - run cpp separately so that we don't recompile everything every time

haskell-mode for emacs!  (on BCP's work laptop)


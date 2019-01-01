TODAY

Try to add some more mutants to the heap safety policy

Integrate Andrew's poolicy language interpreter

Printing
- Double-check that trace printing is really working: E.g., what will
  happen if the two traces are running completely different
  programs?  (This should be allowed, at least by the printing stuff!)
- When printing reachable colors, we should include (C 0) (if it is reachable!)

Use Nikhil's new "has the machine trapped" bit

Shrink colors in memory locations (and eventually registers)  (Done yet?)

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


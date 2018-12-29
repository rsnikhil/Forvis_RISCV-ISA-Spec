TODAY

Check for property failure after each step, not just at the end
  (hopefully this will not slow down testing too much!)

Printing
- can we remove the @ when what's after it is blank?  (Mostly done, but there are a few more pesky ones)
- when printing traces, show the tag of each instruction that gets executed (Done?)
- Double-check that trace printing is really working: E.g., what will
  happen if the two traces are running completely different
  programs?  (This should be allowed, at least by the printing stuff!)
- When printing reachable colors, we should include (C 0) (if it is reachable!)

Falling off the end of memory is not a very interesting behavior
- Look for it, halt execution, traps are not interesting (yet)

there are too mamy magic constants saying how many instructions to generate / execute!
- name them

are we generating too many "interesting" immediate fields?  (How would we tell?)

Put data in initial registers
- Have to think about multiples of 4

Shrink colors in memory locations (and eventually registers)

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


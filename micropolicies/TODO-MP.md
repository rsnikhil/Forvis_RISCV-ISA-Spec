TODAY

Printing
- can we remove the @ when what's after it is blank?  (Done, I think)
- Don't print mem/reg diffs if they're empty (Done, I think)
- Each trace step one line  (Done, I think)
- use the "pad" function to improve / squash memory printing
- when printing traces, show the tag of each instruction that gets executed

Can we suppress the warning when compiling Shrinking.hs?

Falling off the end of memory is not a very interesting behavior -- generate it less often or maybe explicitly look for it and halt execution
- Look for it, halt execution, traps are not interesting (yet)

there are too mamy magic constants saying how many instructions to generate / execute!
- name them

are we generating too many "interesting" immediate fields?  (How would we
tell?)

Put data in initial registers
- Have to think about multiples of 4

Shrink colors in memory locations (and eventually registers)

___________________________________________________________
BEFORE JANUARY PI MEETING

replace haskell policy by policy interpreter
(Andrew)

copy over all the mutants from the Coq version
(BCP)

get the heap safety policy running using the interpreter
(All)

________________________
AFTER PI MEETING

start thinking about stack safety!
  - look at the policy in the draper repo

improve mutation testing (BCP / Leo)
  - run cpp separately so that we don't recompile everything every time

haskell-mode for emacs!  (on BCP's work laptop)


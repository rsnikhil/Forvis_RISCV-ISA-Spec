TODAY

Try to write an explicit test that exposes the second mutant (to make sure
it really is one!!)

Try the other mutants

can we remove the @ when what's after it is blank?

Falling off the end of memory is not a very interesting behavior -- generate it less often or maybe explicitly look for it and halt execution

there are too mamy magic constants saying how many instructions to generate / execute!

are we generating too many "interesting" immediate fields?  (How would we
tell?)

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


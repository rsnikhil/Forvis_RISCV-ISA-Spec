BEFORE JANUARY PI MEETING

there are too mamy magic constants saying how many instructions to generate / execute!

fix
  - at end of gen by exec, we have the state at the end of the run, not the beginning
  - grab the instructions only and copy them over into the arguments to
  - take the delta in the instruction memories
  - change the name of ms_fin'

replace haskell policy by policy interpreter
(Andrew)

copy over all the mutants from the Coq version
(BCP)

get the heap safety policy running using the interpreter
(All)

improve mutation testing (BCP / Leo)
  - run cpp separately so that we don't recompile everything every time

________________________

start thinking about stack safety!
  - look at the policy in the draper repo

haskell-mode for emacs!  (on BCP's work laptop)


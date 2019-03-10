* URGENT

fold our changes back into master when Nikhil says it's time

(Leo) improve the printing of diffs in registers (calcDiff)

* IDEAS FOR WORKING WITH MULTIPLE POLICIES

- First try:
    - make a Policy typeclass with members
        - Tag -- a type that isn't actually used for anything except to
          control which 

* NOW

beautify the code so others can use it
  move some printing support from TestHeapSafety to Printing
  use the P monad uniformly for printing, I guess
  tidy Terminal.hs

Find a more permanent fix for the JAL issue
  see the "DPL rules for JAL" email thread

___________________

more permanent fix for the issue with misaligned accesses
  we want a PIPE_trap, not a fatal error

put in a flag controlling whether misaligned accesses are allowed
  and try to discover the bug!

________________________________________________________
* SOON

"JAL r0" is probably bad style (we use it in our first instr)

Falling off the end of memory is not a very interesting behavior --
generate it less often or maybe explicitly look for it and halt
execution

there are too many magic constants saying how many instructions to
generate / execute!

are we generating too many "interesting" immediate fields?

switch to Chris's new syntax for the policy tool when Andrew is ready
  and then
  remove the sed hack in the makefile

________________________________________________________
* LATER

start thinking about stack safety!
  - look at the policy in the draper repo

haskell-mode for emacs!  (on BCP's work laptop)


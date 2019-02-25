* URGENT

fold our changes back into master after 2/16

* NOW

more mutants

get Shrinking fully working

put in a flag controlling whether misaligned accesses are allowed
  and try to discover the bug!

beautify the code so others can use it
  use the P monad uniformly for printing, I guess

"JAL r0" is probably bad style (we use it in our first instr)

more permanent fix for the issue with misaligned accesses
  we want a PIPE_trap, not a fatal error

Improve printing
  
remove / hide the old micropolicies directory

Find a more permanent fix for the JAL issue
  see the "DPL rules for JAL thread"

switch to Chris's new syntax for the policy tool when Andrew is ready
  and then
  remove the sed hack in the makefile

________________________________________________________
* SOON

Falling off the end of memory is not a very interesting behavior --
generate it less often or maybe explicitly look for it and halt
execution

Try to write an explicit test that exercises the second mutant

there are too many magic constants saying how many instructions to
generate / execute!

are we generating too many "interesting" immediate fields?

________________________________________________________
* LATER

start thinking about stack safety!
  - look at the policy in the draper repo

haskell-mode for emacs!  (on BCP's work laptop)


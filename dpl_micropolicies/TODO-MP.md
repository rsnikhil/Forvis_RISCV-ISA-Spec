* TODAY

replace Shrinking stuff with Leo's updated version
  and get it working
  and remove / hide the old micropolicies directory

put in a flag controlling whether misaligned accesses are allowed
  and try to discover the bug!

beautify the code so others can use it
  use the P monad uniformly for printing, I guess

"JAL r0" is probably bad style (we use it in our first instr)

merge in Nikhil's master (from time to time, as needed)
  and fold our changes back into master pretty soon

more permanent fix for the issue with misaligned accesses
  we want a PIPE_trap, not a fatal error

Improve printing
  
Find a more permanent fix for the JAL issue
  see the "DPL rules for JAL thread"

after we update to Chris C's new parser
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

delete policy-* when finished using them for reference

________________________________________________________
* LATER

start thinking about stack safety!
  - look at the policy in the draper repo

haskell-mode for emacs!  (on BCP's work laptop)


* TODAY

think about JAL

implement mutation testing

we should perhaps stop using mkTagSet, since I don’t think you should need it any
more given the lower-level fromExt function.

improve printing 
  replace ppol arguments with our own information (e.g., for defaults)

replace Shrinking stuff with Leo's updated version
  and get it working
  and remove / hide the old micropolicies directory

merge in Nikhil's master (as needed)
  and fold our changes back into master pretty soon

beautify the code so others can use it

"JAL r0" is probably bad style (we use it in our first instr)

more permanent fix for the issue with misaligned accesses
  we want a PIPE_trap, not a fatal error

Improve printing
  
Find a more permanent fix for the JAL issue
  see the "DPL rules for JAL thread"

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


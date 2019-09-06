* NEXT STEPS

- printing:
  + space in registers
  + address printed twice in diff
  + remove prefix from tags
  + print affected registers before each instruction
  
- finish the stack policy (both)
- finish generalization (Leo)
  + Works (mostly) for taint
  + TODO: other policies
  + TODO: printing
  + TODO: shrinking

* POSSIBLE PAPER TARGETS

- CSF (February 2020).
  + Noninterference: Too Weak and Too Strong
  + Interfering with noninterference
    ++ Taint policy (weaker)
    ++ Stack policy (stronger)
    ++ Testing Framework encompassing both (generalization)

* NOTES ON OTHER POLICIES

** RWX
- init sections have wr + ex
- test example: rwx policy with allgrp and no negative rules allows bad reads and writes
- test example: simplificatoins without breaking: remove [+Rd] and friends from last three rules.

** Heap
- example: are special rules still needed?  Other policies too
- antipointer rules can go?
- draper should try lazy heap
- If you added integrity to exisitng heap property, then OMIT_TAGGING_ON_STORE would be a bug

** FreeRTOS
- Want to avoid testing boot every time.  Can we save "loaded state".
-- Use policy tests as starting point?  Is freertos just an elf file?
+ Question to ask:
  Are there any policies that depend (in an interesting way) on the fact that they run over RTOS instead of a bare machine?

** CFI
- BCP and Leo: got this policy and a simple property working, but we are
  puzzled how to write the property for real: 
    - The property really describes a constraint on the compiler: The actual
      CFG of the program (taking into account the branches that will be
      trapped by the micropolicy because of missing Target tags) is a
      subgraph of the one that it predicts.  
    - So what we're really testing is the _combination_ of the static
      (compiler) and dynamic (micropolicy) parts of an enforcement
      mechanism. 
    - But we don't have a compiler to test.  We're testing just the dynamic
      part, under some _assumption_ about how the static part behaves.
    - So what should we test about the dynamic part in isolation?  And how
      do we generate things that satisfy the assumption about the static
      part? 
    - In some sense, it seems what we want is to generate "adversarial
      programs" -- i.e. programs (and predicted CFGs) that could be output
      by a reasonable (and reasonably correct) compiler but that are subtly
      wrong, e.g. because they contain the possibility of self-modifying
      code. 
    - One stumbling block is that, with our current model of generating and
      testing programs, it doesn't make sense to talk about "possibility" of
      self-modifying code: a program is either self modifying on its one and
      only execution trace, or it is not.
    - One simple (?) way of modeling code modiication exploits is to
      occasionally generate a block of instructions that explicitly writes
      some random instructions into a random block of locations in the
      instruction memory.  One issue with this is that we are not so good at
      generating blocks of instructions, just single ones.  This abstracts
      away from the details of control-flow exploits and just gives the
      attacker a quite powerful ability to change the program.
    - But it's still not very clear what sort of wrong programs we'd be
      hoping to generate.  Generation by execution seems to be an awkward
      model for this sort of testing.
    - More fundamental problem: We're trying to test something imaginary!
      (Namely the compiler.)
    - Bottom line:
       - In order to claim that the _policy_ (as opposed to the
         policy+compiler) is "wrong", we need to demonstrate an example where
         we give the policy a predicted CFG and a set of tags on the program
         and it "does the wrong thing."  But what is the wrong thing?  It is
         allowing a branch that is not in the graph.  But, if there is no
         assumption about the _relation_ between the tags and the graph, then
         such a counterexample is trivial to find: just contruct an empty graph
         and a program with one branch.  So we need to make _some_ assumption
         on the relation that will be guaranteed by the compiler.  What should
         it be??  We don't know.
       - Conclusion: CFI is not a very promising thing to test!  It seems we
         really need to be testing the compiler+policy, but this involves (1) a
         sophisticated compiler and source language (sophisticated enough to be
         vulnerable to code injection attacks of some form) and (2) a test
         generator for _source_ programs that is sophisticated enough to
         generate such attacks.  Sounds hard!
- Take out None.
- Example: mret?  Only interrupts?  More complex uses?
- Questions: NoCFI - allow branching to or branch from assembly?  Do we ever branch into the middle of assembly code?
-- Jumping into assembly and then falling out is currently rejected.
- CJC: does the DW integration report talk about this?
- Does the compiler always know that it's jumping to inline assembly.
- BUGS:
  -- (liveness, not security) compiler writer provided incomplete graph to policy
  -- going any place that is not in the graph
    --- generate unrelated program and graph
    --- traditional buffer overflows
    --- forgetting the "bounce" rules
    ---- jump that is in graph leads to jump not in graph
    ---- every time you generate jump, flip a coin about whether to include edge in the graph

** TAINT
- DONE! (ish)
  * Found a couple of real bugs (loading/storing through tainted pointers)
  * A spurious rule? (storing a tainted value to a clean location rule never fires)
  * Many mutants added.
- TODO:
  * Fix shrinking
  * prettyRegDiff / pretty printing in general
- Ideas
    - Property: for programs that don't branch on tainted data, we have non-interference.  Bake this into the policy?
    - Is this a better answer to traditional complaints that taint tracking doesn't provide a property?
    - Mutating programs by replacing branches on sensitive data with direct jumps should preserve stronger property.
- Proposal:
    - Generate initial memories with Tainted and Untainted tags
    - Variant initial memory scrambles the Tainted locations
    - Property: Locations marked Untainted are identical at every step (as
      long as we never branch on tainted data, which is true for the current
      generation [and could be enforced by a more sophisticated generator
      even with indirect branches, just by stopping execution and declaring
      success if we ever do try to branch on tainted data]) 
- Seems reasonably easy.  Most of the infrastructure is already there in the
  heap policy.  Would be interesting to try to share as much code as
  possible between this and the heap policy by parameterizing, e.g., the
  instructions to the scrambler.

* SMALLER POLICIES

- figure out what the userType policy is actually intended to
    - guarantee once we know this, then make sure that we can find
      policy violations that involve overwriting code!!

- simple: "cells marked in a certain way can only be written once"

- similar: "cells marked in a certain way can only be written by code
  marked a certain way (at the beginning of the run!)"

* SELF MODIFYING POLICY

- tried: policy checking that a particular istruction (JAL) always follows another (ADD)
  Wasn't clear why self modifying code was needed. Simpler bugs violate it

- idea: at the beginning, some memory cells are marked "sensitive" and some are marked "blessed"
  property:
    cells marked "sensitive" AT THE BEGINNING can only be changed by instructions "blessed" AT THE BEGINNING
    where the instruction is in a location marked "blessed" at the beginning and its opcode/registers are unchanged

* NEXT

- (Leo) improve the printing of diffs in registers (calcDiff) in TestHeapSafety

- default tag for memory needs to be generalized a bit

* STACK POLICY

- we finished writing a strawman policy (but not yet property)

- get TestStackSafety minimally compiling

- stack policy should add to depth instead of making fresh colors
  (to avoid polluting rule cache)

- next step: write a simple prop_ in TestStackSafety that just runs the program

- then we need to
  - copy over printing stuff, etc., from TestHeapSafety to TestStackSafety
  - finish writing an example that should pass the policy and try to run it
  - write an example that should NOT pass the policy and try to run it
  - write the property and make sure it makes the right prediction on these two
  - work on generation

questions to think about
  - should we set up the machine so that the stack can run into the
    heap (to create opportunities for interesting bugs)?

* NOW

beautify the code some more
  use the P monad uniformly for printing, I guess

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


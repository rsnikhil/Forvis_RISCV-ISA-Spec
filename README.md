# RISCV-ISA-Spec

This is a formal (and executable) specification for the RISC-V ISA
(Instruction Set Architecture), written in "Elementary" Haskell.

This version is inspired by and informed by the version at <https://github.com/mit-plv/riscv-semantics> [1].

That version [1] uses several advanced Haskell concepts, idioms and
styles which may be difficult for people who are not familiar with
Haskell.

This version tries to be more approachable, using very basic vanilla
Haskell concepts, idioms and styles.  A strong goal here is that this
should be readable and understandable by people who have no prior
exposure to Haskell (and indeed may have no interest in learning
Haskell).  This includes architects/designers of RISC-V CPUs, and
people writing compilers and system code for RISC-V, i.e., people
using it "in anger".  People who are extending the RISC-V instruction
set may also use this as a basis to produce formal specs for their
extensions.

This is a work in progress by the "ISA Formal Specification" Technical
Group constituted by The RISC-V Foundation (<https://riscv.org>).
Neither this work nor [1] is (yet) an official semantics of the RISC-V ISA.

This code formalizes only basic RV32I and RV64I instructions so far.

----------------------------------------------------------------

### The code

The specification code is contained in:

        app/Main.hs
        src/*.hs

For those new to the code, a good reading order is:

        ArchDefs64.hs
        ArchState64.hs

        Decode.hs

        GPRFile.hs
        CSRFile.hs
        Memory.hs
        MMIO.hs

        ExecuteInstr.hs
        RunProgram.hs

`RunProgram.hs` contains the FETCH-DECODE-EXECUTE loop.

`Main.hs` is a driver program that reads RISC-V binaries, initializes
architecture state, and calls `RunProgram`.

`BitManipulation.hs` contains utilities for bit manipulation, including
sign- and zero-extension, truncation, conversion, etc. that are
relevant for these semantics.

`Elf.hs` and `ReadHexFile.hs` are not part of the semantics per se;
the executable uses them to read ELF files and "Hex Memory" files,
respectively.


----------------------------------------------------------------

### How to run this code on RISC-V binaries

This formal spec is executable as a standard Haskell program.  It uses
the standard Haskell tool "stack" to build it and execute it.  Several
files in this top-level directory are intended for the stack tool.

If you don't already have stack installed on your computer, please see 
<https://docs.haskellstack.org/en/stable/README>

On most Linux systems, you can install stack with the command:

        $ curl -sSL https://get.haskellstack.org/ | sh

Once you have installed stack, you can build the executable for the
formal spec like so:

        $ stack build

This will create a `.stack-work/` directory and, somewhere within, an
executable `RISCV-ISA-Spec-exe`.  Once you have built the executable,
you can run it on two provided test programs, like so:

        $ stack exec RISCV-ISA-Spec-exe TestPrograms/hello64
        Running program up to 1,000,000 instructions
        Hello, world!
        Reached jump-to-self infinite loop; instret = 446; exiting

        $ stack exec RISCV-ISA-Spec-exe TestPrograms/thuemorse64
        Running program up to 1,000,000 instructions
        01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001
        Reached jump-to-self infinite loop; instret = 25323; exiting

The two test programs are standard RISC-V ELF files, compiled from C
programs using gcc for RISC-V.  Note: these programs are "bare metal"
RISC-V programs using just user-level RV64I instructions; they assume
a certain starting address; they assume a certain address for console
output.  If you compile other C programs to run on this executable,
you may have to adjust things accordingly.

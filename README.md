# RISCV-ISA-Spec

Forvis: A Formal RISC-V ISA Specification
-----------------------------------------

This is a formal (and executable) specification for the RISC-V ISA
(Instruction Set Architecture), written in "extremely elementary" Haskell.

This is a work-in-progress, as part of the "ISA Formal Specification"
Technical Group constituted by The RISC-V Foundation
(<https://riscv.org>).

Feature coverage:

- Implements RV32 and RV64, I, M, A, Privilege Levels M and U.

- This spec can be executed sequentially (one-instruction-at-a-time)
    as a Haskell program, which in turn executes RISC-V ELF binaries.

- Passes all RISC-V ISA tests in the following sets:
   - `rv32ui-p-*`, `rv64ui-p-*`
   - `rv32um-p-*`, `rv64um-p-*`
   - `rv32ua-p-*`, `rv64ua-p-*`
   - `rv32mi-p-*`, `rv64mi-p-*`

Future Plans:

- We will be adding Priv Mode S and extensions, C, F, and D.

- We will be adding an alternative interpreter exhibiting concurrency
    and integration with RISC-V's RVWMO Weak Memory Model.

----------------------------------------------------------------

### Reading the code

The specification is expressed in Haskell; the code is in:

        src/*.hs

The entire ISA specification is in `Forvis_Spec.hs`.  It specifies
instruction-fetch, and the execution of each kind of instruction.
Everything else is just support to enable executing it as a Haskell
program.

A reading guide for the code is in `Doc/forvis_reading_guide.pdf`

The document suggests reading the code in this order:

        `Arch_Defs.hs`
        `Machine_State.hs`

        `Forvis_Spec.hs`

        `GPR_File.hs`
        `CSR_File.hs`
        `Mem_Ops.hs`
        `Memory.hs`
        `MMIO.hs`

        `Run_Program.hs`
        `Main_RunProgram.hs`
        `Main_TandemVerifier.hs`

`Main.hs` is a driver program that just dispatches to one of two
use-cases, `Main_RunProgram.hs` (free-running) or `Main_TandemVerifier.hs`
(Tandem Verification).

`Main_RunProgram.hs` reads RISC-V binaries (ELF), initializes
architecture state and memory, and calls `RunProgram` to run the
loaded program, up to a specified maximum number of instructions.

`Run_Program.hs` contains the FETCH-EXECUTE loop.

`Main_TandemVerifier.hs` sets up the formal spec to be a slave to a
tandem verifier, receiving commands on stdin and sending responses on
stdout.  The commands allow a tandem verifier to initialize
architecture state, execute 1 or more instructions, and query
archtectural state. Responses include tandem verification packets
which the verifier can use to check an implementation.

`Bit_Manipulation.hs` contains utilities for bit manipulation, including
sign- and zero-extension, truncation, conversion, etc. that are
relevant for these semantics.

`Elf.hs` and `Read_Hex_File.hs` are not part of the semantics per se;
the executable uses them to read ELF files and "Hex Memory" files,
respectively.

----------------------------------------------------------------

### How to run build and this code on RISC-V binaries

This formal spec is executable as a standard Haskell program.  If you
do not already have the standard Haskell compiler `ghc` installed, you
will need to to do so.  It is available as a standard package inon
most Linux distributions.  For example, on Debian and Ubuntu systems,
you can say:

        $ apt-get  install  ghc

Then, you can build the Forvis executable (`forvis_exe`) with:

        $ make

Run the following to see command-line options on the executable:

        $ ./forvis_exe  --help

Then, try the following tests to execute the standard RISC-V ISA test
rv32ui-p-add on the Forvis executable at verbosity 0, 1 and 2
respectively.

        $ make test
        $ make test_v1
        $ make test_v2

Look at the commands in the Makefile that these execute.  If you
substitute "64" for "32" you'll run the RV64 version of the test.

You can also run two pre-compiled C programs:

        $ make test_hello
        $ make test_thue


### Running all RISC-V ISA tests:

The directory:    `TestPrograms/riscv-tests/isa/`
contains pre-compiled versions of all the "ISA Tests" that one
gets when one follows the directions at:

        https://riscv.org/software-tools/

and builds the riscv-tools downloaded from:

        https://github.com/riscv/riscv-tools.git

The following runs a Python script that runs forvis_exe on all of them:

        $ cd Regression_Testing
        $ make

----------------------------------------------------------------

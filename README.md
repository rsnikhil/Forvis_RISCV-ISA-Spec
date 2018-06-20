# RISCV-ISA-Spec

Forvis: A Formal RISC-V ISA Specification
-----------------------------------------

Author: Rishiyur S. Nikhil, Bluespec, Inc.

This is a formal (and executable) specification for the RISC-V ISA
(Instruction Set Architecture), written in "extremely elementary" Haskell.

This is a work-in-progress, one of several similar concurrent efforts
within the "ISA Formal Specification" Technical Group constituted by
The RISC-V Foundation (<https://riscv.org>).  We welcome your
feedback, comments and suggestions.

----------------------------------------------------------------

### Current status

- RV32 and RV64, I, M, A, Privilege Levels M and U.

   - RV32 and RV64 are supported simultaneously, e.g., a program can
       have parts running in RV64 mode at Machine privilege and other
       parts running in Supervisor or User privilege in RV32 mode, by
       setting MISA.MXL, MSTATUS.SXL and MSTATUS.UXL appropriately.

- Forvis can be executed today as a Haskell program, which in turn
    executes RISC-V ELF binaries.  This is a sequential
    interpretation: one-instruction-at-a-time, sequential memory
    model.

- Passes all RISC-V ISA tests in the following sets:
   - `rv32ui-p-*`, `rv64ui-p-*`
   - `rv32um-p-*`, `rv64um-p-*`
   - `rv32ua-p-*`, `rv64ua-p-*`
   - `rv32mi-p-*`, `rv64mi-p-*`

   These tests are all provided in this repo, along with a script to
   run them as a regression.

### What's coming soon (target: July/August 2018)

- S privilege level (Supervisor) with Sv32, Sv39 and Sv48 Virtual Memory schemes.

- RISC-V extensions C (compressed), F (single precision floating
    point) and D (double precision floating point)

### What's coming next (target: December 2018)

- Interpreter supporting concurrency (modeling out-of-order execution,
    pipelining, speculation, multi-hart and more), and integration
    with RISC-V's RVWMO Weak Memory Model.

----------------------------------------------------------------

### Reading the code

We expect that many people might use this as a reading reference
(whether or not they build and execute it) to clarify their
understanding of RISC-V ISA semantics.

The file `src/Forvis_Spec.hs` is the central file to read, containing
essentially the entire ISA specification.  It specifies
instruction-fetch, and the execution of each kind of instruction.

That file, and all the remaining files, are expressed in Haskell, and
can be found in:

        src/*.hs

A PDF reading guide is provided in `Doc/forvis_reading_guide.pdf`.  It
is intended to be used as a reference while perusing the actual
Haskell code, and is not a standalone document.  It displays code
fragments automatically extracted from the actual code.  We suggest
reading the code in this order:

>         Arch_Defs.hs
>         Machine_State.hs
>
>         Forvis_Spec.hs
>
>         GPR_File.hs
>         CSR_File.hs
>         Mem_Ops.hs
>         Memory.hs
>         MMIO.hs
>
>         Run_Program.hs
>         Main_RunProgram.hs
>         Main_TandemVerifier.hs

----------------------------------------------------------------

### How to build Forvis and run it on RISC-V binaries

One way to execute Forvis (exhibiting sequential,
one-instruction-at-a-time semantics) is to build and execute it as a
standard Haskell program.  If you do not already have the standard
Haskell compiler `ghc` installed, you will need to to do so.  It is
available as a standard package in most Linux distributions.  For
example, on Debian and Ubuntu systems, you can say:

        $ apt-get  install  ghc

(The version of ghc should not matter, since Forvis is written in
"extremely elementary" Haskell that has been stable for more than a
decade.)

Then, you can build the Forvis executable (`forvis_exe`) with:

        $ make

Run the following to see command-line options on the executable:

        $ ./forvis_exe  --help

Then, try the following tests to execute the standard RISC-V ISA test
`rv32ui-p-add` on the Forvis executable at verbosity 0, 1 and 2
respectively.

        $ make test
        $ make test_v1
        $ make test_v2

Look at the commands in the `Makefile` that these execute.  If you
substitute "64" for "32" you'll run the `rv64ui-p-add` test.

You can also run two pre-compiled C programs--the standard "Hello
World!" program and the "Thuemorse" program:

        $ make test_hello
        $ make test_thue

You can follow the template of any of the above tests to execute any
of the many pre-compiled RISC-V ELF binaries found in
`Test_Programs/`.  Please see `README.txt` in that directory for more
information on the binaries.

----------------------------------------------------------------

### Running all RISC-V ISA tests:

Please see `Regression_Testing/README.txt` for how to automatically
run Forvis on all ELF files in a directory tree such as `Test_Programs/`.

----------------------------------------------------------------

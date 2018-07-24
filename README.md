# RISCV-ISA-Spec

Forvis: A Formal RISC-V ISA Specification
-----------------------------------------

Copyright (c) Rishiyur S. Nikhil, Bluespec, Inc.

See LICENSE for license details.

This is a formal (and executable) specification for the RISC-V ISA
(Instruction Set Architecture), written in "extremely elementary" Haskell.

This is a work-in-progress, one of several similar concurrent efforts
within the "ISA Formal Specification" Technical Group constituted by
The RISC-V Foundation (<https://riscv.org>).  We welcome your
feedback, comments and suggestions.

----------------------------------------------------------------

### Current status

- **July 24, 2018: Now booting an RV64 Linux kernel! Try it! See below.**

- Forvis supports the following features
    - Base instruction sets: RV32I and RV64I
        - RV32 and RV64 are supported simultaneously, e.g., a program
          can have parts running in RV64 mode at Machine privilege and
          other parts running in Supervisor or User privilege in RV32
          mode, by setting MISA.MXL, MSTATUS.SXL and MSTATUS.UXL
          appropriately.
    - Standard extension M (integer multiply/divide)
    - Standard extension A (atomic memory ops)
    - Privilege Level M (Machine)
    - Privilege Level S (Supervisor)
        - Virtual Memory schemes SV32, SV39 and SV48
    - Privilege Level U (User)

- Forvis can be executed today as a Haskell program, which in turn
    executes RISC-V ELF binaries.  This is a sequential
    interpretation: one-instruction-at-a-time, sequential memory
    model (a concurrent interpreter will follow, later).

- Passes all RISC-V ISA tests in the following sets (currently 310 tests):

    - `rv32ui-p-*`, `rv64ui-p-*`    (Base instruction set)
    - `rv32um-p-*`, `rv64um-p-*`    (M extension)
    - `rv32ua-p-*`, `rv64ua-p-*`    (A extension)
    - `rv32mi-p-*`, `rv64mi-p-*`    (Machine privilege)
    - `rv32ui-v-*`, `rv64ui-v-*`    (Base instruction set in virtual memory)
    - `rv32um-v-*`, `rv64um-v-*`    (M extension in virtual memory)
    - `rv32ua-v-*`, `rv64ua-v-*`    (A extension in virtual memory)
    - `rv32si-p-*`, `rv64si-p-*`    (Supervisor privilege)

   In this repo we provide pre-compiled binaries (ELF files) for all
   these tests, a script to run them as a regression, and sample
   output logs.

### What's coming soon (target: July/August 2018)

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
>         Virtual_Mem.hs
>
>         GPR_File.hs
>         CSR_File.hs
>         Mem_Ops.hs
>         Memory.hs
>         MMIO.hs
>
>         Run_Program.hs
>         Main_Run_Program.hs
>         Main.hs

You can ignore the following, which are used for testing virtual
memory translation and Tandem Verification, respectively (you're
welcome to read them, if you're curious):

>         Main_Test_Virtual_Mem.hs
>         Main_Tandem_Verifier.hs

----------------------------------------------------------------

### How to build Forvis and run it on RISC-V binaries

Forvis can be executed as a sequential RISC-V simulator (sequential,
one-instruction-at-a-time semantics), by building and executing it as
a standard Haskell program.

You will need the standard Haskell compiler `ghc` which is available
for Linux, MacOS and Windows, along with its standard `elf` library
for parsing ELF files.  These are available as standard packages in
most Linux distributions.  For example, on Debian and Ubuntu systems,
you can say:

        $ apt-get  install  ghc
        $ apt-get  install  cabal
        $ cabal install elf

The version of ghc should not matter, since Forvis is written in
"extremely elementary" Haskell that has been stable for more than a
decade.  You can do the analogous package-install on other Linux
distributions using their native package mechanisms, and use Macports
on Apple OS X.  Full details can be found at `haskell.org`.

Then, you can build the Forvis executable (`forvis_exe`) with:

        $ make  exe

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

### Booting an RV64 Linux kernel:

We have provided a pre-built Linux kernel ELF file in: `Test_Programs/Linux_kernel/rv64-vmlinux.elf`

We have provided an RV64 boot ROM containing a compiled device tree in: `Test_Programs/boot_ROM_RV64.hex32`

The kernel has been built for RV64IMAUS.  It expects to access the following:

- A boot ROM with a compiled device tree;
- A 256 MiB memory;
- A timer implementing the memory-mapped MTIME and MTIMECMP locations, capable of generating timer interrupts;
- A memory-mapped location MSIP for generaing software interrupts;
- A model of the National Semiconductor NS16550 UART for console I/O.

All these are supplied in the source code and incorporated into the Forvis executable.

To boot the Linux kernel, in the top-level directory, after you have
created the Forvis executable (see above):

        $ make test_linux_boot

This takes about 8 minutes on a 2.60GHz Intel Core i7-6700HQ with 6
MiB cache, with 16 GiB of memory (Forvis, when running, takes about
3.1 GiB of memory).  Please see `Linux_boot_log.txt` for an example of
what you should see on your console.  At the end of the boot sequence,
you will see a prompt:

        ... <various boot progress messages> ...
        [    0.080000] This architecture does not have kernel memory protection.

        Please press Enter to activate this console. 

Press the <ENTER> key, and after a short delay you should see a shell
prompt, at which you can type various shell commands.  When done,
press Control-C to exit.  Examples:

        / # ls
        bin      etc      linuxrc  root     sys      usr
        dev      init     proc     sbin     tmp

        / # cat  /proc/cpuinfo
        cat  /proc/cpuinfo
        hart    : 0
        isa     : rv64imaus
        mmu     : sv39

        / # cat  /etc/inittab
        cat  /etc/inittab
        ::sysinit:/etc/init.d/rcS
        console::askfirst:/bin/sh

        / # ^C

Caveat: the shell response is sluggish; please wait for a bit for each response.

----------------------------------------------------------------

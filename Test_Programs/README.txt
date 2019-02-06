>----------------------------------------------------------------
The directory:    `Test_Programs/riscv-tests/isa/`
contains ELF binaries for all the "ISA Tests" that from:

        https://github.com/riscv/riscv-tests

Please consult riscv.org for directions on how to download and build
the RISC-V toolchain (including the gcc compiler and linker).

The directory also contains, for each ELF binary, a corresponding
".dump" file showing the disassembly of the ELF binary, by applying
the standard GNU 'objdump' program in the toolchain.

These ISA tests are compiled during the toolchain-build using RISC-V
gcc.  The original assembly source codes can be found in the
riscv-tools repo at:

        riscv-tests/isa/

>----------------------------------------------------------------
The directory:    'Test_Programs/MIT'
contains ELF binaries and corresponding ".dump" disassemblies of two
programs written in C and compiled using RISC-V gcc:

    Classical "Hello World!" C program.

    "Thuemorse"

>----------------------------------------------------------------

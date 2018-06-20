>----------------------------------------------------------------
The directory:    `Test_Programs/riscv-tests/isa/`
contains ELF binaries for all the "ISA Tests" that one
gets when one follows the directions given at:

        https://riscv.org/software-tools/

to download riscv-tools from:

        https://github.com/riscv/riscv-tools.git

and build the RISC-V toolchain.

It also contains, for each ELF binary, a corresponding ".dump" file
showing the disassembly of the ELF binary.

These ISA tests are compiled during the toolchain-build using RISC-V
gcc.  The original assembly source codes can be found in the
riscv-tools repo at:

        riscv-tools/riscv-tests/isa/

>----------------------------------------------------------------
The directory:    'Test_Programs/MIT'
contains ELF binaries and corresponding ".dump" disassemblies of two
programs written in C and compiled using RISC-V gcc:

    Classical "Hello World!" C program.

    "Thuemorse"

>----------------------------------------------------------------

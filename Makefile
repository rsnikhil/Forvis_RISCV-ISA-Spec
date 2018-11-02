# Copyright (c) 2018 Rishiyur S. Nikhil
# See LICENSE for license details

FORVIS_EXE = forvis_exe

# Compiles the Haskell source files in SRC_DIR into the FORVIS_EXE using
# the ghc Haskell compiler, placing compiler-intermediate files in
# TMP_DIR.

.PHONY: default
default:
	@echo "    make exe           Compile Haskell source files from '$(SRC_DIR)' into Forvis executable."
	@echo "    make test          Run $(FORVIS_EXE) as a RISC-V simulator on sample ISA test $(SAMPLE_ISA_TEST)."
	@echo "    make test_v1       -- ditto, with verbosity 1 (+ print instruction trace)."
	@echo "    make test_v2       -- ditto, with verbosity 2 (+ print arch state after each instr)."
	@echo "    make test_hello    Run $(FORVIS_EXE) as a RISC-V simulator on classic 'Hello World!' C program"
	@echo "                           compiled with gcc into a RISC-V ELF binary."
	@echo "    make test_thue     Run $(FORVIS_EXE) as a RISC-V simulator on compiled Thuemorse C program."

# ================================================================
# Compile Haskell source files from SRC_DIR into Forvis executable
# using the ghc Haskell compiler.
# Compiler-intermediate files are placed in TMP_DIR.

SRC_DIR  = ./src
TMP_DIR  = tmp_haskell

.PHONY: exe
exe:
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  $(FORVIS_EXE)  -O2  -i$(SRC_DIR)  -outputdir  $(TMP_DIR)  -rtsopts  Main \
		csrc/softfloat_wrappers.c \
		/usr/lib/libsoftfloat.so

# ================================================================
# Running a sample ISA test
# (Substitute SAMPLE_ISA_TEST, SAMPLE_ISA_TEST_RV, SAMPLE_ISA_TEST_ARCH for a different ISA test.)
# (See Regression_Testing/Makefile for running all the ISA tests)

TEST_PROGRAMS ?= Test_Programs

SAMPLE_ISA_TEST_RV   ?= RV64
SAMPLE_ISA_TEST_ARCH ?= RV64UI
SAMPLE_ISA_TEST      ?= rv64ui-p-add

# SAMPLE_ISA_TEST_RV   ?= RV32
# SAMPLE_ISA_TEST_ARCH ?= RV32UI
# SAMPLE_ISA_TEST      ?= rv32ui-p-add

N = 100000

# Run SAMPLE_ISA_TEST
.PHONY: test
test: $(FORVIS_EXE)
	./$(FORVIS_EXE)  --arch $(SAMPLE_ISA_TEST_ARCH)  --tohost  --n $(N) \
		$(TEST_PROGRAMS)/boot_ROM_$(SAMPLE_ISA_TEST_RV).hex32 \
		$(TEST_PROGRAMS)/riscv-tests/isa/$(SAMPLE_ISA_TEST)

# Same, with verbosity 1
.PHONY: test_v1
test_v1: $(FORVIS_EXE)
	./$(FORVIS_EXE)  --arch $(SAMPLE_ISA_TEST_ARCH)  --tohost  --n $(N)  --verbosity 1  \
		$(TEST_PROGRAMS)/boot_ROM_$(SAMPLE_ISA_TEST_RV).hex32 \
		$(TEST_PROGRAMS)/riscv-tests/isa/$(SAMPLE_ISA_TEST)

# Same, with verbosity 2
.PHONY: test_v2
test_v2: $(FORVIS_EXE)
	./$(FORVIS_EXE)  --arch $(SAMPLE_ISA_TEST_ARCH)  --tohost  --n $(N)  --verbosity 2  \
		$(TEST_PROGRAMS)/boot_ROM_$(SAMPLE_ISA_TEST_RV).hex32 \
		$(TEST_PROGRAMS)/riscv-tests/isa/$(SAMPLE_ISA_TEST)

# ================================================================
# Running sample C programs compiled by gcc to ELF files

# Standard C program that prints "Hello World!\n"
.PHONY: test_hello
test_hello: $(FORVIS_EXE)
	./$(FORVIS_EXE)  --arch RV64AIMSU  --tohost  \
		$(TEST_PROGRAMS)/boot_ROM_RV64.hex32 \
		$(TEST_PROGRAMS)/MIT/rv64-hello

# Same, with verbosity 1
.PHONY: test_hello_v1
test_hello_v1: $(FORVIS_EXE)
	./$(FORVIS_EXE)  --arch RV64AIMSU  --tohost  --verbosity 1  \
		$(TEST_PROGRAMS)/boot_ROM_RV64.hex32 \
		$(TEST_PROGRAMS)/MIT/rv64-hello

# Same, with verbosity 2
.PHONY: test_hello_v2
test_hello_v2: $(FORVIS_EXE)
	./$(FORVIS_EXE)  --arch RV64AIMSU  --tohost  --verbosity 2  \
		$(TEST_PROGRAMS)/boot_ROM_RV64.hex32 \
		$(TEST_PROGRAMS)/MIT/rv64-hello

# C program that does some computation and prints out a string of 0s and 1s
.PHONY: test_thue
test_thue: $(FORVIS_EXE)
	./$(FORVIS_EXE)  --arch RV64AIMSU  --tohost  \
		$(TEST_PROGRAMS)/boot_ROM_RV64.hex32 \
		$(TEST_PROGRAMS)/MIT/rv64-thuemorse

# Linux kernel boot
.PHONY: test_linux_boot
test_linux_boot: $(FORVIS_EXE)
	nice -n19  ./$(FORVIS_EXE)  +RTS -K10M -M3G -RTS\
		--arch RV64AIMSU  -n 400000000 \
		$(TEST_PROGRAMS)/boot_ROM_RV64.hex32 \
		$(TEST_PROGRAMS)/Linux_kernel/rv64-vmlinux.elf

# FreeRTOS kernel boot
.PHONY: test_freeRTOS_boot
test_freeRTOS_boot: $(FORVIS_EXE)
	nice -n19  ./$(FORVIS_EXE)  +RTS -K10M -M3G -RTS\
		--arch RV32AIMSU  -n 400000000 \
		$(TEST_PROGRAMS)/boot_ROM_RV32.hex32 \
		$(TEST_PROGRAMS)/FreeRTOS_kernel/riscv-spike.elf

# ================================================================
# Cleanup

.PHONY: clean
clean:
	rm  -r -f  *~  $(SRC_DIR)/*~  $(TMP_DIR)  *.hi *.o

.PHONY: full_clean
full_clean:
	rm  -r -f  *~  $(SRC_DIR)/*~  $(TMP_DIR)  *.hi *.o  $(FORVIS_EXE)

# ================================================================

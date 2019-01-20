# Copyright (c) 2018-2019 Rishiyur S. Nikhil
# See LICENSE for license details

FORVIS_EXE = forvis_exe

# Compiles the Haskell source files in SRC_DIR into the FORVIS_EXE using
# the ghc Haskell compiler, placing compiler-intermediate files in
# TMP_DIR.

.PHONY: default
default:
	@echo "Usage:"
	@echo "    make build_softfloat    Do this once, before making anything else."
	@echo "                            Downloads and builds the softloat lib for IEEE floating point emulation,"
	@echo "                            which is used for the RISC-V 'F' and 'D' extensions."
	@echo ""
	@echo "    make exe                Compile Haskell source files from '$(SRC_DIR)' into Forvis executable."
	@echo ""
	@echo "    make test               Run $(FORVIS_EXE) as a RISC-V simulator on sample ISA test $(SAMPLE_ISA_TEST)."
	@echo "    make test_v1            -- ditto, with verbosity 1 (+ print instruction trace)."
	@echo "    make test_v2            -- ditto, with verbosity 2 (+ print arch state after each instr)."
	@echo ""
	@echo "    make test_hello         Run $(FORVIS_EXE) as a RISC-V simulator on classic 'Hello World!' C program"
	@echo "                                compiled with gcc into a RISC-V ELF binary."
	@echo "    make test_hello_v1      -- ditto, with verbosity 1 (+ print instruction trace)."
	@echo "    make test_hello_v2      -- ditto, with verbosity 2 (+ print arch state after each instr)."
	@echo ""
	@echo "    make test_thue          Run $(FORVIS_EXE) as a RISC-V simulator on compiled Thuemorse C program."
	@echo "    make test_linux_boot    Run $(FORVIS_EXE) as a RISC-V simulator on a pre-built Linux kernel"

# ================================================================
# By default, we exclude the 'F' and 'D' RISC-V extensions (single-
# and double-precision floating point) because compilation is more
# complex, involving a git submodule with a Haskell foreign-function
# interface to C code (Berkeley 'softfloat' library).

# The following def of FLOAT should be commented-out to EXCLUDE F and D
# The following def of FLOAT should be uncommented   to INCLUDE F and D (or define FLOAT=yes on command line)
# FLOAT := yes

UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
SOFTFLOAT_LIBPATH=/usr/lib/libsoftfloat.so
endif

ifeq ($(UNAME), Darwin)
SOFTFLOAT_LIBPATH=/usr/local/lib/libsoftfloat.dylib
endif

ifneq ($(FLOAT),)
FLOAT_EXTRAS := -isubmodules/softfloat-hs/src \
		-Isubmodules/softfloat-hs/include \
		submodules/softfloat-hs/csrc/softfloat_wrappers.c \
		$(SOFTFLOAT_LIBPATH)
FLOATARG := -DFLOAT
endif

# ================================================================
# Compile Haskell source files from SRC_DIR into Forvis executable
# using the ghc Haskell compiler.
# Compiler-intermediate files are placed in TMP_DIR.

SRC_DIR  = ./src
TMP_DIR  = tmp_haskell

.PHONY: exe
exe:
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  $(FORVIS_EXE)  -O2  -i$(SRC_DIR) \
	        -outputdir  $(TMP_DIR)  -rtsopts \
	        -cpp $(FLOATARG) \
		Main \
		$(FLOAT_EXTRAS)

# ================================================================
# This section downloads, into 'submodules/', a git submodule: 'softfloat-hs'
# That, in turn, contains a git submodule: 'berkeley-softfloat-3/'
# Then, it compiles and installs the berkeley-softfloat-3 library

.PHONY: build_softfloat
build_softfloat:  build_softfloat_step_1  build_softfloat_step_2

.PHONY: build_softfloat_step_1
build_softfloat_step_1:
	@echo "build_softfloat_step_1: Downloading berkeley-softfloat-3 submodule"
	cd submodules/softfloat-hs; git submodule update --init --recursive

.PHONY: build_softfloat_step_2
build_softfloat_step_2:
	@echo "build_softfloat_step_2: Using softfloat-hs's Makefile to compiling and install berkeley-softfloat-3 submodule"
	@echo "WARNING: uses 'sudo' to install files into system areas like /usr/include, /usr/lib/, etc."
	make  COMPILE_TYPE=RISCV  -C submodules/softfloat-hs  softfloat
	make  COMPILE_TYPE=RISCV  -C submodules/softfloat-hs  install

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

N ?= 100000

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

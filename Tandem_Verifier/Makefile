# Copyright (c) 2018-2019 Rishiyur S. Nikhil
# See LICENSE for license details

EXE = tv

# Compiles the Haskell source files in SRC_DIR into the FSMC using
# the ghc Haskell compiler, placing compiler-intermediate files in
# TMP_DIR.

.PHONY: default
default:
	@echo "Usage:"
	@echo "    make exe    Compile Haskell source files from '$(SRC_DIR)' into executable $(EXE)."

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
FLOAT_EXTRAS := -i../submodules/softfloat-hs/src \
		-I../submodules/softfloat-hs/include \
		../submodules/softfloat-hs/csrc/softfloat_wrappers.c \
		$(SOFTFLOAT_LIBPATH)
FLOATARG := -DFLOAT
endif

# ================================================================
# Compile Haskell source files from SRC_DIRs into $(EXE) executable
# using the ghc Haskell compiler.
# Compiler-intermediate files are placed in TMP_DIR.

SRC_DIR1  = ./src
SRC_DIR2  = ../src
TMP_DIR  = tmp_haskell

.PHONY: exe
exe: src/Main.hs  src/Parse_Trace_Data.hs  src/RegNames.hs  src/Verify.hs
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  $(EXE)  -O2  -i$(SRC_DIR1)  -i$(SRC_DIR2) \
	        -outputdir  $(TMP_DIR)  -rtsopts \
	        -cpp $(FLOATARG) \
		Main \
		$(FLOAT_EXTRAS)

# ================================================================
# TV an ISA for RV64AIMSU

VERBOSITY    ?= 0
TEST         ?= rv64ui-p-add
ISA_TESTS_DIR = ~/GitHub/Flute/Tests/isa
TRACE_FILE    = ~/Projects/RISCV/Devel/Builds_Non_Std/RV64AIMSU_Flute_bluesim_tv/trace_out.dat

.PHONY: tv_isa
tv_isa:
	./$(EXE)  --verbosity  $(VERBOSITY)  --arch RV64AIMSU  $(BOOT_ROM_RV64)  $(ISA_TESTS_DIR)/$(TEST)  $(TRACE_FILE)

# ================================================================
# TV an ISA

TEST_PROGRAMS ?= ../Test_Programs
BOOT_ROM_RV32 = $(TEST_PROGRAMS)/boot_ROM_RV32.hex32
BOOT_ROM_RV64 = $(TEST_PROGRAMS)/boot_ROM_RV64.hex32

ELF_FILE1        = $(TEST_PROGRAMS)/riscv-tests/isa/rv32ui-p-add
TRACE_DATA_FILE1 = ~/Projects/RISCV/Devel/Builds_Non_Std/RV32ACIMU_Flute_bluesim_tv/trace_out.dat
ARGS1            = --arch RV32ACIMU  $(BOOT_ROM_RV32)  $(ELF_FILE1)  $(TRACE_DATA_FILE1)

.PHONY: test1
test1:
	./$(EXE)  $(VERBOSITY)  $(ARGS1)

# ================================================================
# Test

ELF_FILE2        = $(TEST_PROGRAMS)/riscv-tests/isa/rv64ui-p-add
TRACE_DATA_FILE2 = ~/Projects/RISCV/Devel/Builds_Non_Std/RV64ACDFIMSU_Flute_bluesim_tv/trace_out.dat
ARGS2            = --arch RV64ACDFIMSU  $(BOOT_ROM_RV64)  $(ELF_FILE2)  $(TRACE_DATA_FILE2)

.PHONY: test2
test2:
	./$(EXE)  --verbosity $(VERBOSITY)  $(ARGS2)

# ================================================================
# Cleanup

.PHONY: clean
clean:
	rm  -r -f  *~  $(SRC_DIR)/*~  $(TMP_DIR)  *.hi *.o

.PHONY: full_clean
full_clean:
	rm  -r -f  *~  $(SRC_DIR)/*~  $(TMP_DIR)  *.hi *.o  $(EXE)

# ================================================================

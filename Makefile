SRCDIR  = src
TMPDIR  = tmp_haskell
EXEFILE = forvis_exe

# Compiles the Haskell source files in SRCDIR into the EXEFILE using
# the ghc Haskell compiler, placing compiler-intermediate files in
# TMPDIR.

.PHONY: default
default:
	mkdir -p  $(TMPDIR)
	ghc  -o  $(EXEFILE)  -i$(SRCDIR)  -outputdir  $(TMPDIR)  Main

.PHONY: clean
clean:
	rm  -r -f  *~  $(SRCDIR)/*~  $(TMPDIR)  *.hi *.o

.PHONY: full_clean
full_clean:
	rm  -r -f  *~  $(SRCDIR)/*~  $(TMPDIR)  *.hi *.o  $(EXEFILE)

# ================================================================
# Running a sample ISA test
# (see Regression_Testing/ for running all ISA tests)

TEST_PROGRAMS = TestPrograms

# Run rv32ui-p-add ISA test with verbosity 0
.PHONY: test
test:
	./$(EXEFILE)  --RV32                 --n 500  $(TEST_PROGRAMS)/riscv-tests/isa/rv32ui-p-add

# Run rv32ui-p-add ISA test with verbosity 1: prints instruction trace
.PHONY: test_v1
test_v1:
	./$(EXEFILE)  --RV32  --verbosity 1  --n 500  $(TEST_PROGRAMS)/riscv-tests/isa/rv32ui-p-add

# Run rv32ui-p-add ISA test with verbosity 2: prints insttuction trace
# and full CPU architectural state after each instruction
.PHONY: test_v2
test_v2:
	./$(EXEFILE)  --RV32  --verbosity 2  --n 500  $(TEST_PROGRAMS)/riscv-tests/isa/rv32ui-p-add

# ================================================================
# Running sample C programs compiled by gcc to ELF files

# Standard program that prints "Hello World!\n"
.PHONY: test_hello
test_hello:
	./$(EXEFILE)  --RV64  $(TEST_PROGRAMS)/MIT/hello64

# Prints out a string of 0's and 1's
.PHONY: test_thue
test_thue:
	./$(EXEFILE)  --RV64  $(TEST_PROGRAMS)/MIT/thuemorse64

# ================================================================

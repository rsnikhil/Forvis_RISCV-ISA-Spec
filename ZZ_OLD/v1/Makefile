# This Makefile is just to document/automate how to stack build, stack exec, etc.

V    ?= 0
N    ?= 500
TEST ?= TestPrograms/riscv-tests/isa/rv32ui-p-add.hex
RV   ?= RV32

# ================================================================

.PHONY: help
help:
	@echo "Usage:"
	@echo "  make  build                // recompile"
	@echo "  make  test_hello_hex       // Run test on Hello Word    RV64 mem-hex file"
	@echo "  make  test_hello_elf       // Run test on Hello Word    RV64 ELF file"
	@echo "  make  test_thue_hex        // Run test on Thuemorse     RV64 mem-hex file"
	@echo "  make  test_thue_elf        // Run test on Thuemores     RV64 ELF file"
	@echo "  make  test_add_hex         // Run test on rv32ui-p-add  RV32 mem-hex file"
	@echo "or supply args:"
	@echo "  make  test  V=<verbosity>  N=<num_instrs>  RV=RV32/RV64  TEST=testfile"
	@echo "  make  test                 // Default V=$(V)  N=$(N)  RV=$(RV)  TEST=$(TEST)"

.PHONY: build
build:
	stack build

.PHONY: test_hello_hex
test_hello_hex:
	stack exec RISCV-ISA-Spec-exe  -- --RV64  TestPrograms/MIT/hello64.hex

.PHONY: test_hello_elf
test_hello_elf:
	stack exec RISCV-ISA-Spec-exe  -- --RV64  TestPrograms/MIT/hello64

.PHONY: test_thue_elf
test_thue_elf:
	stack exec RISCV-ISA-Spec-exe  -- --RV64  TestPrograms/MIT/thuemorse64

.PHONY: test_thue_hex
test_thue_hex:
	stack exec RISCV-ISA-Spec-exe  -- --RV64  TestPrograms/MIT/thuemorse64.hex

.PHONY: test_add_hex
test_add_hex:
	stack exec RISCV-ISA-Spec-exe  -- --RV32  TestPrograms/riscv-tests/isa/rv32ui-p-add.hex

.PHONY: test
test:
	stack exec RISCV-ISA-Spec-exe -- --verbosity  $(V)  -n $(N)  --$(RV)  $(TEST)

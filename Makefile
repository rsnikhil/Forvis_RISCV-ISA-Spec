# This Makefile is just to document/automate how to stack build, stack exec, etc.

.PHONY: build
build:
	stack build

.PHONY: test_hello_hex
test_hello_hex:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/MIT/hello64.hex

.PHONY: test_hello_elf
test_hello_elf:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/MIT/hello64

.PHONY: test_thue_elf
test_thue_elf:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/MIT/thuemorse64

.PHONY: test_thue_hex
test_thue_hex:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/MIT/thuemorse64.hex

.PHONY: test_add_hex
test_add_hex:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/riscv-tests/isa/rv32ui-p-add.hex

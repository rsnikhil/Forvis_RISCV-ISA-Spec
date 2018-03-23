# This Makefile is just to document/automate how to stack build, stack exec, etc.

.PHONY: build
build:
	stack build

.PHONY: test_hello
test_hello:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/MIT/hello64

.PHONY: test_thue
test_thue:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/MIT/thuemorse64

.PHONY: test_add
test_add:
	stack exec RISCV-ISA-Spec-exe  TestPrograms/riscv-tests/isa/rv32ui-p-add.hex

# This def will likely be different in your installation
# It's the directory where you built the riscv-tests from riscv.org

RISCV_TESTS_ISA_ELFS_DIR = ~/Projects/RISCV/2018-03-04_tests/riscv-tests/isa/

ELF_TO_HEX = ../../../elf_to_hex/elf_to_hex

.PHONY: all
all:
	cp -p  $(RISCV_TESTS_ISA_ELFS_DIR)/rv32ui-p-add.dump  .
	$(ELF_TO_HEX)  $(RISCV_TESTS_ISA_ELFS_DIR)/rv32ui-p-add  rv32ui-p-add.hex
	cp -p  $(RISCV_TESTS_ISA_ELFS_DIR)/rv32ui-p-lw.dump  .
	$(ELF_TO_HEX)  $(RISCV_TESTS_ISA_ELFS_DIR)/rv32ui-p-lw  rv32ui-p-lw.hex
	cp -p  $(RISCV_TESTS_ISA_ELFS_DIR)/rv32ui-p-sw.dump  .
	$(ELF_TO_HEX)  $(RISCV_TESTS_ISA_ELFS_DIR)/rv32ui-p-lw  rv32ui-p-sw.hex

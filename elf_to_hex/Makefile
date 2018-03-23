# Makefile to create an elf_to_hex executable
# where the hex file contains 8b bytes

CC = gcc

.PHONY: default
default: elf_to_hex

elf_to_hex: elf_to_hex.c
	gcc -g  -o elf_to_hex  elf_to_hex.c  -lelf

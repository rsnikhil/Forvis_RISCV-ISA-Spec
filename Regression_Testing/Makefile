# Copyright (c) 2018 Rishiyur S. Nikhil.  All Rights Reserved.

# See LICENSE in this repo for license details.

# Runs a Python script
#    which runs the Forvis executable on all ELF tests in ../Test_Programs/
#    placing the output log for each test into Logs/
#    (see Python code for tests ignored because features are not yet implemented)

FORVIS_EXE = ../forvis_exe

.PHONY: default
default: $(FORVIS_EXE)
	mkdir  -p  Logs
	./Run_all_tests.py  ../Test_Programs  Logs

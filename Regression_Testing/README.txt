In this directory you can run a "regression", i.e.,

    Run the Forvis executable as a RISC-V simulator
    on each of the ELF files in the Test_Programs/ directory and sub-directories,
    placing a log for each in the Logs/ directory
    and printing out a final summary of number of tests run and number of tests that "passed".

To run, first ensure that you have created the Forvis executable
forvis_exe in the top-level directory, then:

    $ cd  Regression_Testing
    $ make

For reference:
    The Logs_sample/ directory is a copy of Logs/ from a previous run.

    make_transcript.txt
    make_nofloat_transcript.txt

        are transcripts of running 'make' on executables built with
	and without support for the RISC-V F and D extensions (single-
	and double-precision floating point).  The former shows all
	tests passing; the latter shows failures on all tests
	involving F or D.

Note: the regression is run by the Python program 'Run_all_tests.py'

    Caveat: This is a Python 3 program, so please make sure you have
    Python 3 installed (many places still use Python 2 as the
    default).

    Running it by itself with --help will list its command-line arguments:
        $ Run_all_tests.py  --help

    You can modify the program to change its functionality.

    In particular:

    - It has an 'ignore_list' to filter out certain files in the
        Test_Programs directory, such as tests for features that Forvis
        does not yet implement

    - It has some ad hoc and fragile ways to ignore .dump files
        (ignores all files with any extension including .dump), and
        looks for the string "32" or "64" in the ELF filename to
        decide whether it should be run in RV32 or RV64 mode.

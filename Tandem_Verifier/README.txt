Copyright (c) 2018-2019 Rishiyur S. Nikhil, Bluespec, Inc.
See LICENSE for license details.

This directory (Tandem_Verifier) is a work in progress.

Briefly: it is a standalone program ('tv', built in this directory)
that is built on top of Forvis code, to 'tandem verify' an
implementation.

- A RISC-V implementation (hardware or software) outputs a 'Trace
    Data' file that records architectural state updates on each
    instruction.  The Trace Data file is a binary file, in a format
    defined by Bluespec, Inc.  Please ask the author or Bluespec,
    Inc. for a copy of the Trace Data format specification, if you
    need it.

    Bluespec's own processors all (optionally) output such trace data.

- This program uses Forvis to verify that a given trace data file
    shows correct RISC-V execution and, if not, identifies the exact
    instruction where there was a divergence from the Forvis spec.

This program is still in development; it has so far been used to
verify Bluespec's Piccolo and Flute implementations on small programs
(the riscv ISA tests).  Once it is smoothly verifying larger programs
(C-compiled programs, Linux, FreeRTOS), we will provide more details.

[ Interestingly, devloping this tandem verifier has uncovered a few
  small bugs in Forvis as well, which have been fixed. ]

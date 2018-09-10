This directory contains OBSOLETE code and can be safely ignored.

Each of these obsolete directories is a copy of the repo at some point
in time, placed here for reference due to some major rewrite, just in
case we want to conveniently refer back to it (instead of checking out
some particular git commit).

v1: Obsolete since around March 2018.  Had explicit Decode function;
encoded '64' or '32' into the type and so could only support either
RV32 or RV64 but not simultaneously; did not support UART and boot ROM
functionality; did not support S Privilige or Linux boot, etc.

v2: Obsolete since September 9, 2018.  Was using fixed-width integers
(libs Data.Word and Data.Int) to represent registers, memory, CSRs,
etc.  Supports 'S' Privilege, boots Linux kernel (7 minutes), etc.
Replaced by new version that replaces fixed-width integers by
'Integer' (unbounded integers) as these are more amenable to reasoning
in Coq.

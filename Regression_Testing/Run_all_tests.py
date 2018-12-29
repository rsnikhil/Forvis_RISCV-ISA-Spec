#!/usr/bin/python3

# Copyright (c) 2018-2019 Rishiyur S. Nikhil
# See LICENSE for license details

usage_line = (
    "Usage:\n"
    "    $ CMD    <root-dir-for-ISA-tests>    <logs_dir>\n"
)

help_lines = (
    "  Runs a Forvis executable  on each ELF file in <root-dir> and its sub-directories.\n"
    "  For each source file FOO, saves output log in <logs_dir>/FOO.log. \n"
)

import sys
import os
import stat
import subprocess

# ================================================================
# Ignores files with the following in their names

ignore_list = ["."                    # Files with extensions (e.g., foo.dump)
]

# ================================================================

forvis_exe = "../forvis_exe"

num_executed = 0
num_passed   = 0

# ================================================================

def main (argv = None):
    print ("Use flag --help  or --h for a help message")
    if ((len (argv) <= 1) or
        (argv [1] == '-h') or (argv [1] == '--help') or
        (len (argv) != 3)):

        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        sys.stdout.write (help_lines.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 0

    elfs_path = os.path.abspath (os.path.normpath (argv [1]))
    logs_path = os.path.abspath (os.path.normpath (argv [2]))
    max_level = 20
    traverse (max_level, 0, elfs_path, logs_path, 0)

    sys.stdout.write ("Executed: {0} tests\n".format (num_executed))
    sys.stdout.write ("PASS:     {0} tests\n".format (num_passed))

# ================================================================
# Recursively traverse the dir tree below elf_path and process each file

def traverse (max_level, level, elfs_path, logs_path, verbosity):
    st = os.stat (elfs_path)
    is_dir = stat.S_ISDIR (st.st_mode)
    is_regular = stat.S_ISREG (st.st_mode)
    do_foreachfile_function (level, is_dir, is_regular, elfs_path, logs_path)
    if is_dir and level < max_level:
        for entry in os.listdir (elfs_path):
            elfs_path1 = os.path.join (elfs_path, entry)
            traverse (max_level, level + 1, elfs_path1, logs_path, verbosity)
    return 0

# ================================================================
# This function is applied to every path in the
# recursive traversal

def do_foreachfile_function (level, is_dir, is_regular, path, logs_path):
    prefix = ""
    for j in range (level): prefix = "  " + prefix

    # directories
    if is_dir:
        print ("%s%d dir %s" % (prefix, level, path))

    # regular files
    elif is_regular:
        dirname  = os.path.dirname (path)
        basename = os.path.basename (path)
        # print ("%s%d %s" % (prefix, level, path))
        do_regular_file_function (level, dirname, basename, logs_path)

    # other files
    else:
        print ("%s%d Unknown file type: %s" % (prefix, level, os.path.basename (path)))

# ================================================================
# For each ELF file, execute it in the Forvis simulator

def do_regular_file_function (level, dirname, basename, logs_path):
    global num_executed
    global num_passed

    # Ignore file if filename matches something in the ignore-list
    for x in ignore_list:
        if basename.find (x) != -1: return

    arch = mkArchString (basename)
    if arch == None: return

    elf_file = os.path.join (dirname, basename)

    # For debugging only
    # prefix = ""
    # for j in range (level): prefix = "  " + prefix
    # sys.stdout.write ("{0}{1} ACTION:    {2}\n".format (prefix, level, elf_file))

    boot_rom_file = "../Test_Programs/boot_ROM_{0}.hex32".format (arch [0:4])

    # Compose command to be run in sub-process
    command = [forvis_exe, "--arch", arch,  "--tohost",  boot_rom_file,  elf_file]

    # Show command to be executed in sub-process, for info.
    sys.stdout.write ("Test {0}\n".format (basename))
    sys.stdout.write ("    Exec: {0}\n".format (command [0]))
    sys.stdout.write ("        {0}  {1}  {2}\n".format (command [1], command [2], command [3]))
    for x in command [4:]:
        sys.stdout.write ("        {0}\n".format (x))

    # Run command command as a sub-process
    completed_process = run_command (command)
    num_executed = num_executed + 1
    passed = completed_process.stdout.find ("PASS") != -1
    if passed:
        sys.stdout.write ("    PASS")
        num_passed = num_passed + 1
    else:
        sys.stdout.write ("    FAIL")

    log_filename = os.path.join (logs_path, basename + ".log")
    sys.stdout.write ("      Writing log: {0}.log\n".format (basename))

    fd = open (log_filename, 'w')
    fd.write (completed_process.stdout)
    fd.close ()

    return

# ================================================================
# Make an architecture string (e.g., RV64AIMSU) from the test name
# WARNING: this is somewhat fragile in that the test name sometimes does not give full info

def mkArchString (basename):
    basename = basename.lower()
    j = basename.find ("rv32") 
    if j != -1:
        arch = "RV32"
    else:
        j = basename.find ("rv64")
        if j != -1:
            arch = "RV64"
        else:
            return None

    s = basename [j+4:]
    extns = ["I", "U"]

    if   s.startswith ("mi-"):
        # Some 'mi' tests need 'S'!
        extns.append ("S")

    elif s.startswith ("si-") and not ("S" in extns):  extns.append ("S")

    elif s.startswith ("ua-"):  extns.append ("A")
    elif s.startswith ("uc-"):  extns.append ("C")
    elif s.startswith ("ud-"):
        extns.append ("F")
        extns.append ("D")
    elif s.startswith ("uf-"):  extns.append ("F")
    elif s.startswith ("um-"):  extns.append ("M")

    s = s[2:]

    if s.startswith ("-v-") and not ("S" in extns): extns.append ("S")

    # Finally, sort the set of extensions in decreasing alphabetic order
    # and add them to the arch string
    extns.sort (reverse = True)
    for extn in extns:
        arch = arch + extn

    return arch

# ================================================================
# This is a wrapper around 'subprocess.run' because of an annoying
# incompatible change in moving from Python 3.5 to 3.6

def run_command (command):
    python_minor_version = sys.version_info [1]
    if python_minor_version < 6:
        # Python 3.5 and earlier
        result = subprocess.run (args = command,
                                 bufsize = 0,
                                 stdout = subprocess.PIPE,
                                 stderr = subprocess.STDOUT,
                                 universal_newlines = True)
    else:
        # Python 3.6 and later
        result = subprocess.run (args = command,
                                 bufsize = 0,
                                 stdout = subprocess.PIPE,
                                 stderr = subprocess.STDOUT,
                                 encoding='utf-8')
    return result

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main (sys.argv))

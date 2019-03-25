# A version that takes tag definitions and rules from a .dpl file

Compiles for me with ghc 8.2.2
You will need at least the following cabal packages:
megaparsec-5.3.1
mainland-pretty-0.6.1
either-4.4.1.1
language-c-quote-0.12.1
yaml-0.8.25
(Some of these could be dispensed with by removing unnecessary parts of
the policy-tool/ subdirectory, but I prefered to keep it as an exact copy
for now.)

# Installation

cabal install split QuickCheck
make

# Notes

examples of real policies can be found here:
  hope-policies-osv  (git-repo)
look for .dpl files

documentation of the policy language is here
  hope/docs
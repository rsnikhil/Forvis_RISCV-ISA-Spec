# forvis
# Created using:
# stack new forvis-stack simple from ..
# Manually edited forvis-stack.cabal
# Added softfloat-hs as a dependency and in the submodules folder

# ---------------------------------
# Steps to build FORVIS using Stack
# ---------------------------------
# From this directory:
# Create a submodules directory where stack looks for the submodules of the
# project
# mkdir submodules
# cd submodules
# 
# Clone the softfloat repository (please note --recursive)
# git clone --recursive https://github.com/GaloisInc/softfloat-hs.git

# Build FORVIS
# cd ..
# stack build --ghc-options=-threaded

# Executable created at:
# ./.stack-work/install/x86_64-linux/lts-12.9/8.4.3/bin/forvis-stack

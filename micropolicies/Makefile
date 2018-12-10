SRC_DIR  = ../src
FORVIS_EXE = forvis_exe
TMP_DIR  = ../tmp_haskell

# Why don't we use stack (or even cabal) again?
QC_LIB = ~/.cabal/lib/x86_64-linux-ghc-8.4.4/QuickCheck-2.11.3-4Q6LQ7mN6Tj8otrLuHN0se

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
SOFTFLOAT_LIBPATH=/usr/lib/libsoftfloat.so
endif
ifeq ($(UNAME), Darwin)
SOFTFLOAT_LIBPATH=/usr/local/lib/libsoftfloat.dylib
endif

all:
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  ../$(FORVIS_EXE)  -O2  -i$(SRC_DIR)  -outputdir  $(TMP_DIR)  -rtsopts \
		Main_PIPE \
		-i../submodules/softfloat-hs/src \
		-I../submodules/softfloat-hs/include \
		../submodules/softfloat-hs/csrc/softfloat_wrappers.c \
		$(SOFTFLOAT_LIBPATH)

pipe_hello: all
	$(MAKE) -C .. test_hello

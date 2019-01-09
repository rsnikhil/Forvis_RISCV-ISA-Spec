SRC_DIR  = ../src
FORVIS_EXE = forvis_exe
TMP_DIR  = ../tmp_haskell

all:
	mkdir -p  $(TMP_DIR)
	stack build --ghc-options="-cpp" :micropolicies-exe 

# TODO: "stack clean" is too aggressive.  Ideally, we want to remove just PIPE.o!
mutant:
	@echo "[37;41m--------- $(MUT) -----------[0m"
	stack clean
	stack build --ghc-options="-cpp -D$(MUT)" :micropolicies-exe 
	time stack exec micropolicies-exe

mutants:
	make mutant MUT=M_WRONG_STORE
	make mutant MUT=M_WRONG_ADD
	make mutant MUT=M_WRONG_ADDI
	make mutant MUT=M_FRESH_COLOR
	make mutant MUT=M_LW_NOCHECK

pipe_hello: all
	$(MAKE) -C .. test_hello

pipe_qc: all
	../forvis_exe

stack:
	stack build --ghc-options="-cpp" :micropolicies-exe

##############################################################################
# Probably the rest can be deleted:

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
SOFTFLOAT_LIBPATH=/usr/lib/libsoftfloat.so
endif
ifeq ($(UNAME), Darwin)
SOFTFLOAT_LIBPATH=/usr/local/lib/libsoftfloat.dylib
endif

with-softfloat:
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  ../$(FORVIS_EXE)  -O2  -i$(SRC_DIR)  -outputdir  $(TMP_DIR)  -rtsopts \
		Main_PIPE \
		-i../submodules/softfloat-hs/src \
		-I../submodules/softfloat-hs/include \
		../submodules/softfloat-hs/csrc/softfloat_wrappers.c \
		$(SOFTFLOAT_LIBPATH)


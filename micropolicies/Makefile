SRC_DIR  = ../src
FORVIS_EXE = forvis_exe
TMP_DIR  = ../tmp_haskell

all:
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  ../$(FORVIS_EXE)  -O2  -i$(SRC_DIR)  \
	   -outputdir  $(TMP_DIR)  -rtsopts -cpp \
	   Main_PIPE 
# temporary
	../forvis_exe

mutant:
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  ../$(FORVIS_EXE)  -O2  -i$(SRC_DIR)  \
	   -outputdir  $(TMP_DIR)  -rtsopts -cpp -D$(MUT) \
	   Main_PIPE 
# temporary
	time ../forvis_exe

temp:
	make mutant MUT=M_WRONG_MOVE

mutants:
	make mutant MUT=M_WRONG_MOVE
	make mutant MUT=M_FRESH_COLOR
	make mutant MUT=M_LW_NOCHECK

pipe_hello: all
	$(MAKE) -C .. test_hello

pipe_qc: all
	../forvis_exe


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


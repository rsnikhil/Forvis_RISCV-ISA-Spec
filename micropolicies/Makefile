SRC_DIR  = ../src
FORVIS_EXE = forvis_exe
TMP_DIR  = ../tmp_haskell

all:
	mkdir -p  $(TMP_DIR)
	ghc  -dynamic  -threaded  -o  ../$(FORVIS_EXE)  -O2  -i$(SRC_DIR)  \
	   -outputdir  $(TMP_DIR)  -rtsopts -cpp \
	   Main_PIPE 

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


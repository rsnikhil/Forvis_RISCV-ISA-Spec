all: build taint

taint: build
	$(MAKE) taintmutants unmutated POLICY=taint

cfi: build
	$(MAKE) cfimutants unmutated POLICY=cfi

stack: build
	$(MAKE) stackmutants unmutated POLICY=stack

heap: build
	$(MAKE) heapmutants unmutated POLICY=heap

writeonce: build
	$(MAKE) writeoncemutants unmutated POLICY=writeonce

build:
	@stack build :dpl-exe 

unmutated: build
	@make mutant MUT=NO_MUTATIONS

ifeq ($(shell uname),Darwin)
  SED = sed -i '' '/^\#/d' 
else
  SED = sed -i '/^\#/d' 
endif

V=@

mutant:
	$Vecho 
	$Vecho "[37;46m"
	$Vecho "                                  $(MUT)"
	$Vecho 
	$Vecho "[0m"
	$Vmkdir -p .mutants
	$Vcpp -D$(MUT) $(POLICY).dpl > .mutants/$(POLICY).dpl
	$V$(SED) .mutants/$(POLICY).dpl
	$V(cd .mutants; time stack exec dpl-exe)

writeoncemutants:
	$Vmake mutant MUT=IGNORE_WRITEONCE
	$Vmake mutant MUT=IGNORE_WRITENEVER

stackmutants:

cfimutants:
	$Vmake mutant MUT=CFI_BAD_JUMP_TO_LOAD
	$Vmake mutant MUT=CFI_BAD_JUMP_TO_STORE
	$Vmake mutant MUT=CFI_BAD_JUMP_TO_IMMARITH
	$Vmake mutant MUT=CFI_BAD_JUMP_TO_ARITH
	$Vmake mutant MUT=CFI_BAD_JUMP_TO_BRANCH
	$Vmake mutant MUT=CFI_BAD_JUMP_TO_JUMP
	$Vmake mutant MUT=CFI_BAD_JUMP_TO_JUMPREG

heapmutants:
	$Vmake mutant MUT=OMIT_CHECKS_ON_LOAD

taintmutants:
	$Vmake mutant MUT=NO_ADDR_TAINT_CHECK_STORE
	$Vmake mutant MUT=NO_ADDR_TAINT_CHECK_LOAD

# Not a bug, in the simple heap safety policy
#	@make mutant MUT=OMIT_TAGGING_ON_STORE

clean:
	rm -rf .mutants

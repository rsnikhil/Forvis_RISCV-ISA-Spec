all: build heap

stack:
	$(MAKE) stackmutants unmutated POLICY=stack

heap:
	$(MAKE) heapmutants unmutated POLICY=heap

build:
	@stack build :dpl-exe 

unmutated:
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

stackmutants:

heapmutants:
	$Vmake mutant MUT=OMIT_CHECKS_ON_LOAD

# Not a bug, in the simple heap safety policy
#	@make mutant MUT=OMIT_TAGGING_ON_STORE

clean:
	rm -rf .mutants

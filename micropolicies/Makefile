all: build writeonce

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

heapmutants:
	$Vmake mutant MUT=OMIT_CHECKS_ON_LOAD

# Not a bug, in the simple heap safety policy
#	@make mutant MUT=OMIT_TAGGING_ON_STORE

clean:
	rm -rf .mutants

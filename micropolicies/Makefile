all: build stack

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

mutant:
	@echo 
	@echo "[37;46m"
	@echo "                                  $(MUT)"
	@echo 
	@echo "[0m"
	@mkdir -p .mutants
	@cpp -D$(MUT) $(POLICY).dpl > .mutants/$(POLICY).dpl
	@$(SED) .mutants/$(POLICY).dpl
	@(cd .mutants; time stack exec dpl-exe)

stackmutants:

heapmutants:
	@make mutant MUT=OMIT_CHECKS_ON_LOAD

# Not a bug, in the simple heap safety policy
#	@make mutant MUT=OMIT_TAGGING_ON_STORE



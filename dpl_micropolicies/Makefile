all: build mutants unmutated

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
	@echo "                   $(MUT)"
	@echo 
	@echo "[0m"
	@mkdir -p .mutants
	@cpp -D$(MUT) heap.dpl > .mutants/heap.dpl
	@$(SED) .mutants/heap.dpl
	@(cd .mutants; time stack exec dpl-exe)

mutants:
	@make mutant MUT=OMIT_CHECKS_ON_LOAD
	@make mutant MUT=OMIT_TAGGING_ON_STORE



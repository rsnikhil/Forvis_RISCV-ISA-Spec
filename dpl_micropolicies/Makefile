all: build mutants unmutated

build:
	stack build :dpl-exe 

unmutated:
	make mutant MUT=NO_MUTATIONS

mutant:
	@echo "[37;41m--------- $(MUT) -----------[0m"
	@mkdir -p .mutants
	cpp -D$(MUT) heap.dpl > .mutants/heap.dpl
	sed -i '' '/^#/d' .mutants/heap.dpl
	(cd .mutants; time stack exec dpl-exe)

mutants:
	make mutant MUT=BUG1



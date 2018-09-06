# Adjust this
HS_TO_COQ_DIR = /Users/sweirich/github/deepspec/hs-to-coq

include $(HS_TO_COQ_DIR)/common.mk

OUT=lib

# Handwritten modules (usually by modification of generated version)
#
# These are axiomatizations of library definitions used by the
# generated files below
HANDMOD = \
  IO \
  Numeric \



# Generated modules
MODULES =          \
  Address_Map      \
  Arch_Defs        \
  Bit_Manipulation \
  CSR_File         \
  Forvis_Spec      \
  GPR_File         \
  MMIO             \
  Machine_State    \
  Main_Test_Virtual_Mem \
  Mem_Ops          \
  Memory           \
  Run_Program      \
  UART             \
  Virtual_Mem      \

## The files in the src/ directory that we do not (yet) translate are: 
##    Elf Main Main_Run_Program Main_Tandem_Verifier Read_Hex_File

VFILES_GEN = $(addprefix $(OUT)/,$(addsuffix .v,$(MODULES)))
VFILES_MAN = $(addprefix $(OUT)/,$(addsuffix .v,$(HANDMOD)))

VFILES = $(VFILES_GEN) $(VFILES_MAN)

all: vfiles coq

vfiles: $(OUT)/edits $(OUT)/README.md $(OUT)/Makefile $(VFILES)

$(OUT)/_CoqProject : Makefile $(OUT)/README.md
	> $@
	echo '-Q . ""' >> $@
	echo '-R $(HS_TO_COQ_DIR)/base ""' >> $@
	echo '-R $(HS_TO_COQ_DIR)/examples/containers/lib ""' >> $@
	echo $(addsuffix .v,$(HANDMOD)) >> $@
	echo $(addsuffix .v,$(MODULES)) >> $@

$(OUT)/Makefile: $(OUT)/_CoqProject $(VFILES) $(OUT)/README.md
	cd $(OUT); coq_makefile -f _CoqProject -o Makefile

$(OUT)/edits: $(OUT)/README.md
	ln -fs ../edits $(OUT)/edits

$(OUT)/README.md:
	mkdir -p $(OUT)
	> $@
	echo 'This directory contains a Coq’ified version of ....' >> $@
	echo 'Do not edit files here!' >> $@

coq: $(OUT)/Makefile $(VFILES)
	$(MAKE) -C $(OUT) -f Makefile OPT=$(COQFLAGS)

HS_TO_COQ_OPTS := \
  -e $(HS_TO_COQ_DIR)/base/edits \
  -e edits \
  --iface-dir $(HS_TO_COQ_DIR)/base/ \
  --iface-dir $(HS_TO_COQ_DIR)/examples/containers/lib/ \
  --iface-dir $(OUT) \
  -N \
  -i ../src/ \

.SECONDEXPANSION:
$(VFILES_GEN): $(OUT)/%.v : $$(wildcard module-edits/$$*/preamble.v) $$(wildcard module-edits/$$*/midamble.v)  $$(wildcard module-edits/$$*/edits) $(wildcard module-edits/$$*/flags) edits $(OUT)/README.md
	$(HS_TO_COQ) $(addprefix -e , $(wildcard module-edits/$*/edits)) \
	             $(addprefix -p , $(wildcard module-edits/$*/preamble.v)) \
	             $(addprefix --midamble , $(wildcard module-edits/$*/midamble.v)) \
	             $(addprefix `cat , $(addsuffix ` , $(wildcard module-edits/$*/flags))) \
		     $(HS_TO_COQ_OPTS) \
	             -o $(OUT) \
	             ../src/$*.hs
	test -e $@


$(VFILES_MAN): $(OUT)/%.v : manual/%.v
	mkdir -p "$$(dirname $(OUT)/$*.v)"
	rm -f $@
	lndir ../manual $(OUT)/

clean:
	rm -rf $(OUT)

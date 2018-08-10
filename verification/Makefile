# Adjust this
HS_TO_COQ_DIR = /home/jojo/uni/UPenn/hs-to-coq

include $(HS_TO_COQ_DIR)/common.mk

OUT=lib

# Handwritten modules (usually by modification of generated version)
HANDMOD = \

# Generated modules
MODULES =       \
  Arch_Defs     \
  Machine_State

VFILES_GEN = $(addprefix $(OUT)/,$(addsuffix .v,$(MODULES)))
VFILES_MAN = $(addprefix $(OUT)/,$(addsuffix .v,$(HANDMOD)))

VFILES = $(VFILES_GEN) $(VFILES_MAN)

all: vfiles coq

vfiles: $(OUT)/edits $(OUT)/README.md $(OUT)/Makefile $(VFILES)

$(OUT)/_CoqProject : Makefile $(OUT)/README.md
	> $@
	echo '-Q . ""' >> $@
	echo '-R $(HS_TO_COQ_DIR)/base ""' >> $@
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

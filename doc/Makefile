PDFTEX ?= pdftex

all: calling-convention.pdf isa-insn.pdf
.PHONY: all

%.pdf: %.tex
	$(PDFTEX) $^
	# Then do it again to make sure the TOC was built correctly
	$(PDFTEX) $^

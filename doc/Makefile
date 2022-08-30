PDFTEX ?= pdftex

all: ass.pdf calling-convention.pdf isa-insn.pdf crap-1.pdf
.PHONY: all

ass.pdf: ass.tex *.tex
	$(PDFTEX) $^
	$(PDFTEX) $^

%.pdf: %.tex _macros.tex
	$(PDFTEX) $^
	# Then do it again to make sure the TOC was built correctly
	$(PDFTEX) $^

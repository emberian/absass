PDFTEX ?= pdftex
PDFS = calling-convention.pdf isa-insn.pdf crap-1.pdf params.pdf

all: $(PDFS) ass.pdf
.PHONY: all

clean:
	rm $(PDFS) ass.pdf ||:
.PHONY: clean

%.pdf: %.tex _macros.tex
	# Do it this many times to be sure there's a valid TOC
	$(PDFTEX) $*.tex
	$(PDFTEX) '\let\nogentoc=\relax\input $*'

# The whole document depends on its parts
ass.pdf: ass.tex $(patsubst %.pdf,%.tex,$(PDFS))

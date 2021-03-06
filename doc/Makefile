# Copyright (C) 2004, 2005, 2006, 2007 Philipp Benner <pbenner@uos.de>
#
# This makefile is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This makefile is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this makefile; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# There are quite some problems one has to deal with when using LaTeX and
# gnu make. First, there is no easy way to calculate dependencies for
# tex files. Second, several programs generate pdf output, hence there
# is no distinct target rule for pdf files. What one needs is a make
# system that knows how to distinguish between e.g. a pdf target generated
# from a dot graph or a tex file. Furthermore, the make system should
# know which files depend on each other according to their file names,
# to avoid having to parse tex files for dependencies. Another issue
# is that some files might exist or not. E.g. a tex document might have
# a bibliography based on bibtex and if that is the case the make
# system should recognize the corresponding bibtex file as dependency.

SHELL		= /bin/sh -e

LATEX		= pdflatex -halt-on-error -shell-escape
TEXI2DVI	= texi2dvi -p
MAKEINDEX	= makeindex
BIBTEX		= bibtex
DOT		= dot

TEXINPUTS	= .:../share:../../share:

L2HTML		= latex2html
L2HTMLFLAGS	= -no_math -html_version 4.0,math -accent_images textrm -scalable_fonts -split 0

DOT_OUTPUT	= $(patsubst %.dot,%.pdf,$(wildcard *.dot))
TEX_OUTPUT	= $(patsubst %.tex,%.pdf,$(wildcard *.tex))

## default build rule
all:
	@echo "Usage: make <pdf-output>"
	@echo "       make html"

## dynamic dependency calculation
# naming scheme:
# -> <basename>.tex depends on
# -> <basename>.bib and
# -> <basename>_*.dot/texinput/pdf
define DYN_dep
$(1): $(patsubst %.dot,%.pdf,$(wildcard $(basename $(1))_*.dot))
$(1): $(wildcard $(basename $(1))_*.texinput)
$(1): $(wildcard $(basename $(1))_*.pdf)
$(1): $(shell if [ -f $(1:.pdf=.bib) ]; then echo $(1:.pdf=.bib); fi)
endef

$(foreach target,$(TEX_OUTPUT),$(eval $(call DYN_dep,$(target))))

## latex&co build rules
$(TEX_OUTPUT): %.pdf: %.tex
# texi2dvi runs latex as often as needed and generates a
# bibliography by invoking bibtex
	TEXINPUTS=$(TEXINPUTS) PDFLATEX="$(LATEX)" $(TEXI2DVI) $<
	if [ -f $*.nlo ]; then $(MAKE) $*.nls; fi
	TEXINPUTS=$(TEXINPUTS) $(LATEX) $*

$(DOT_OUTPUT): %.pdf: %.dot
	$(DOT) -Tpdf $< > $@

.SUFFIXES: .nlo .nls
.nlo.nls:
	$(MAKEINDEX) $< -s nomencl.ist -o $@
#	$(MAKEINDEX) $< -s nomentbl.ist -o $@

## latex2html
html: $(TEX_OUTPUT) $(DOT_OUTPUT)
	$(L2HTML) $(L2HTMLFLAGS) $(TEX_OUTPUT:.pdf=.tex)

## clean rules
clean:
	$(RM) -r $(TEX_OUTPUT:.pdf=)
	$(RM) -r input/auto
	$(RM) -r auto
	$(RM) tmp.inputs
	$(RM) *.table
	$(RM) *.gnuplot
	$(RM) *.aux
	$(RM) *.blg
	$(RM) *.bbl
	$(RM) *.dvi
	$(RM) *.log
	$(RM) *.ps
	$(RM) *.toc
	$(RM) *.out
	$(RM) *.nav
	$(RM) *.snm
	$(RM) *.nlo
	$(RM) *.nls
	$(RM) *.ilg
	$(RM) *.idx
	$(RM) *.ind

distclean: clean
	$(RM) $(TEX_OUTPUT)
	$(RM) $(DOT_OUTPUT)

.PHONY: all clean distclean html

LHS2TEX = lhs2TeX
PDFLATEX = pdflatex
LATEX = latex
BIBTEX = bibtex
RM=rm
AOUT=GUIMain

GHC = ghc
GHCFLAGS = -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleInstances

PAPER=GUIMain

all: executable clean

default:
	$(LHS2TEX) -o $(PAPER).tex $(PAPER).lhs
	$(LATEX) $(PAPER).tex
	$(BIBTEX) $(PAPER)
	$(LATEX) $(PAPER).tex
	$(PDFLATEX) $(PAPER).tex

executable:
	$(GHC) $(GHCFLAGS) -o $(AOUT) $(PAPER).lhs

clean:
	$(RM) -f $(PAPER).tex
	$(RM) -f $(PAPER).log
	$(RM) -f $(PAPER).aux
	$(RM) -f $(PAPER).bbl
	$(RM) -f $(PAPER).blg
	$(RM) -f $(PAPER).ptb
	$(RM) -f *.o
	$(RM) -f *.hi
	$(RM) -f *.dvi

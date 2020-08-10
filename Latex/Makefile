FIGUREDIR = figures
CACHEDIR = cache

pdf: nvuniverbation.bbl nvuniverbation.pdf

all: pod cover

complete: index nvuniverbation.pdf

index:  nvuniverbation.snd

nvuniverbation_biber.bib: nvuniverbation.bcf
	biber nvuniverbation --output_format bibtex

nvuniverbation.pdf: nvuniverbation.aux
	xelatex nvuniverbation

nvuniverbation.aux: nvuniverbation.tex $(wildcard local*.tex)
	xelatex -no-pdf nvuniverbation

# Before the XeTeX make begins, we need to make a TeX file from a Rnw file.
nvuniverbation.tex: nvuniverbation.Rnw $(wildcard sections/*.Rnw) $(wildcard sections/*.tex)
	Rscript \
	  -e "library(knitr)" \
	  -e "knitr::knit('$<','$@')"

# Make R files.
%.R: %.Rnw
	Rscript -e "Sweave('$^', driver=Rtangle())"

# Create only the book.
nvuniverbation.bbl: nvuniverbation.tex nvuniverbation.bib
	xelatex -no-pdf nvuniverbation
	biber nvuniverbation


nvuniverbation.snd: nvuniverbation.bbl
	touch nvuniverbation.adx nvuniverbation.sdx nvuniverbation.ldx
	sed -i s/.*\\emph.*// nvuniverbation.adx
	sed -i 's/hyperindexformat{\\\(infn {[0-9]*\)}/\1/' nvuniverbation.sdx
	sed -i 's/hyperindexformat{\\\(infn {[0-9]*\)}/\1/' nvuniverbation.adx
	sed -i 's/hyperindexformat{\\\(infn {[0-9]*\)}/\1/' nvuniverbation.ldx
# 	python3 fixindex.py
# 	mv mainmod.adx nvuniverbation.adx
	makeindex -o nvuniverbation.and nvuniverbation.adx
	makeindex -o nvuniverbation.lnd nvuniverbation.ldx
	makeindex -o nvuniverbation.snd nvuniverbation.sdx
	xelatex nvuniverbation


clean:
	rm -f nvuniverbation.tex nvuniverbation-concordance.tex *.bcf *.bak *~ *.backup *.tmp \
	*.adx *.and *.idx *.ind *.ldx *.lnd *.sdx *.snd *.rdx *.rnd *.wdx *.wnd \
	*.log *.blg *.ilg \
	*.aux *.toc *.cut *.out *.tpm *.bbl *-blx.bib *_tmp.bib \
	*.glg *.glo *.gls *.wrd *.wdv *.xdv *.mw *.clr \
	*.run.xml nvuniverbation.tex nvuniverbation.pgs nvuniverbation.bcf \
	sections/*aux sections/*~ sections/*.bak sections/*.backup \
	langsci/*/*aux langsci/*/*~ langsci/*/*.bak langsci/*/*.backup

realclean: clean
	rm -f *.dvi *.ps *.pdf \
	cache/* figures/* cache*.*


FORCE:

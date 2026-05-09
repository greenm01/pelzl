# Orpie documentation makefile
all:	manual.pdf manual.html post-build-cleanup pelzl.1 pelzlrc.5 pelzl-curses-keys.1

manual.ps: dvi
	dvips -tletterSize -Ppdf -G0 manual.dvi

dvi: manual.tex
	latex manual.tex
	latex manual.tex

manual.tex: manual.tex.in
	latex2man -CLATEX -L manual.tex.in manual.tex

manual.pdf: manual.ps
	ps2pdf13 manual.ps

manual.html: manual.tex
	hevea -fix manual.tex
	hevea -fix manual.tex

manual.tex.stripped: manual.tex.in
	python remove-tt.py manual.tex.in manual.tex.stripped

pelzl.1: manual.tex.stripped
	latex2man -M manual.tex.stripped pelzl.1

pelzlrc.5: manual.tex.stripped
	latex2man -CORPIERC -M manual.tex.stripped pelzlrc.5

pelzl-curses-keys.1: pelzl-curses-keys.tex
	latex2man -M pelzl-curses-keys.tex pelzl-curses-keys.1

post-build-cleanup: manual.pdf manual.html pelzl.1 pelzlrc.5 pelzl-curses-keys.1
	rm -f *.aux *.log *.toc *.haux *.htoc *.dvi *.ps *.stripped manual.tex

clean:
	rm -f manual.tex *.aux *.log *.toc *.haux *.htoc *.dvi *.ps *.pdf *.html *.stripped pelzl.1 pelzlrc.5 pelzl-curses-keys.1


# arch-tag: DO_NOT_CHANGE_a5d62ea3-3a73-4de2-a2b3-a70bb310823f 

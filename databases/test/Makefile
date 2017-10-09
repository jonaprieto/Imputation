.PHONY: default
default:
	python graphics.py
	latexmk -pdf main.tex
	make clean

.PHONY : clean
clean :
	cat .gitignore | xargs echo rm -f | sh


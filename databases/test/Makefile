.PHONY: default
default:
	python graphics.py
	make clean
	latexmk -pdf main.tex

.PHONY : clean
clean :
	rm main.synctex.gz*
	latexmk -C
	cat .gitignore | xargs echo rm -f | sh


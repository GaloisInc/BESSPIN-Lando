all: grammar-v2.pdf well-formedness-v2.pdf grammar-v1.pdf typesystem-v1.pdf

%.pdf: %.tex
	pdflatex $<

grammar-v2.pdf: grammar-v2.tex grammar-macros-v2.tex

well-formedness-v2.pdf: well-formedness-v2.tex

grammar-v1.pdf: grammar-v1.tex grammar-macros-v1.tex

typesystem-v1.pdf: typesystem-v1.tex typesystem-macros-v1.tex

clean:
	rm -f *.aux *.log grammar-v2.pdf well-formedness-v2.pdf grammar-v1.pdf typesystem-v1.pdf



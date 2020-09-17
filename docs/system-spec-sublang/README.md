# Lando System Specification (Sub)Language 

Contents:

- `grammar2.tex` - EBNF presentation of LSSL version 2. Should accurately reflect version 2 parser.

- `wf2.tex` - Formal presentation of well-formedness criteria for LSSL version 2.  Should accurately reflect behavior of version 2 checker.

- `example.lando` - An example file illustrating some of the features of version 2.

- `grammar1.tex` - EBNF presentation of LSSL version 1. Should accurately reflect version 1 parser. 

- `old_*.tex` - original description of LSSL version 1; implemented parser has many differences from this grammar, and implemented checker is not related to this type system.

- To build pdf for any of these files, e.g. `foo.tex`, run 
```
pdflatex foo.tex
pdflatex foo.tex
```

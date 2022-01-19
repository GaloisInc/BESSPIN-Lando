# Lando System Specification (Sub)Language (LSSL)

## Contents

- `grammar-v2.tex` - EBNF presentation of LSSL version 2 (v2).  This
  document should accurately reflect the v2 parser.

- `well-formedness-v2.tex` - Formal presentation of well-formedness
  criteria for LSSL version 2, including the LSSL v2 type system,
  written by Andrew Tolmach.  This document should accurately reflect
  behavior of the v2 checker.

- `example.lando` - An example file illustrating some of the features
  of Lando v2.

- `grammar-v1.tex` - EBNF presentation of LSSL version 1 (v1). Should
  accurately reflect v1 parser.

- `typesystem-v1` - A draft typesystem for LSSL v1 written by Hari
  Menon.  This type system was never implemented.

- `grammar-pre-v1.tex` - The original description of LSSL v1.  The
  implemented parser at the time of this documents creation had many
  differences from this grammar, and the implemented checker is not
  related to this type system.

- To hand-build a PDF for any of these files, e.g. `foo.tex`, run:
```
pdflatex foo.tex && pdflatex foo.tex
```
  A `Makefile` is also provided to automatically build all
  documentation.

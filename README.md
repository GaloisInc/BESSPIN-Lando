```
This material is based upon work supported by the Defense Advanced
Research Project Agency (DARPA) under Contract No. HR0011-18-C-0013. 
Any opinions, findings, conclusions or recommendations expressed in
this material are those of the author(s) and do not necessarily
reflect the views of DARPA.

Distribution Statement "A" (Approved for Public Release, Distribution
Unlimited)
```

# Lando

This repository is organized as follows:

- `source/lobot` - A parser, type checker, and instance generator for the Lobot
  sublanguage of Lando. See `source/lobot/README.md` for more details.
  
- `source/lando` - A parser and command line tool for the Lando System
  Specification (Sub)Language (SSL).
  
  To run the SSL parser on a `.lando` file, first run `./lando.sh -r` to build
  the executable, then run:
  ```
  $ ./lando.sh -f path/to/source.lando
  ```
  Use `-h` to view the available options. See `source/lando/README.md` for more
  details.
  
- `docs` - Formal description of Lobot and the Lando SSL.

### Status of un-merged branches

This section gives short summaries of the branches which were still in-progress as of September 2020.

- `lssl-v2` - Implements the changes to the Lando SSL discussed in [this Google Doc](https://docs.google.com/document/d/1dNa6TtV8_ON2jmsPzbp2ZXbtNIaBm_WG0y_Nea4SkQI/edit), along with a well-formedness checker for this version 2 of SSL and documents in `docs/system-spec-sublang` giving a formal presentation of its grammar and well-formedness criteria. This branch may be ready to merge once it is tested.

- `lssl-well-formed` - Adds a document in `docs/system-spec-sublang` giving a formal presentation of the well-formedness criteria of the Lando SSL v1. If `lssl-v2` is merged, this branch could be merged in renaming `wf.tex` to `old_wf.tex`.

- `feature/constrained-functions` - Work in progress on all three tasks in #56 (adding support for abstract functions with constraints in Lobot), but is currently missing support for functions which contain other function calls in their argument constraints. Before this can be merged some deep thought needs to be given as to whether these changes are really what we want.
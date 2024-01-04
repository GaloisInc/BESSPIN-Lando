# Lando
## Copyright (C) 2018-2022 Galois
## PI: Joe Kiniry <kiniry@galois.com>a
## Contributors: Ben Selfridge, Matt Yacavone, Andrew Tolmach

**Lando** is a system specification language that, for the most part,
looks like normal natural language.  It permits one to specify several aspects
of a system at a high level, including:
 0. its *domain model* (in the domain engineering sense),
 1. its overall *static architecture* (subsystems and their components),
 2. its *features* via 
   - a *feature modeling* sublanguage called **Lobot** and 
   - in component specifications using the augmented
   Command/Query/Constraint principle,
 3. its *requirements* (in the requirements engineering sense),
 4. its *events* (roughly equivalent to internal and external atomic
    transitions), and
 5. its *scenarios* (roughly equivalent to *use cases*, as seen in
    other languages).

Lando is designed to work well for specifying systems in any
combination of software, firmware, hardware, or systems engineering
processes or methodologies.  It has been designed to refine into
formal, mechanized models, in both a model-based engineering sense and
a formal methods sense.  We use it with models expressed in AADL,
SysML (version 1 and 2), Cryptol, Alloy, JML, ACSL, F*, Coq, PVS,
Isabelle/HOL, Lean, and more.

## Dependencies

Lando is implemented in Kotlin and Lobot is implemented in Haskell.
Lando dependencies are in the [pom.xml](./source/lando/pom.xml) file, while Lobot dependencies are in [lobol.cabal](./source/lobot/lobot.cabal) file.

To get started with Lando:

* You will need maven, currently tested with `maven:3.8.5-openjdk-11` (available as a docker image)
* run `mvn package -f source/lando/pom.xml`
* you will get a java binary `source/lando/target/lando-$LANDO)VERSION-jar-with-dependencies.jar`

To get started with Lobot:
* follow the instructions in [source/lobot/README.md](./source/lobot/README.md)

## Example of Use

Lando has been used to specify several small and medium-sized systems,
including cyber-physical systems, software-only systems, hardware
systems (fabricated ASIC designs and FPGA-based soft core processors
and accelerators), and embedded systems that include novel and reused
software, firmware, and hardware.

## Requirements

1. Lando shall enable the user to rapidly develop a domain engineering model
2. Lando shall enable the user to quickly write up requirements and scenarios relevant for the domain engineering model
3. Lando shall implement the well-formedness rules described in the [well-formedness document](./docs/system-spec-sublang/well-formedness-v2.pdf)
4. Lando shall export the domain engineering model, requirements and scenarios into markdown
5. Lando shall provide simple API for adding custom backends to the parsed AST, such SysMLv2, Clafer, or AADL export
6. Lando shall provide syntax highlighting and error reporting, for easy debugging

## Project Organization

Development of Lando is hosted on a private project on the Galois
GitLab-ext server.  https://gitlab-ext.galois.com/ssith/lando/

Public releases are made via GitHub.
https://github.com/GaloisInc/BESSPIN-Lando/

This repository is organized as follows:

- `BONc` is a snapshot of Lando's grandfather, the `bonc` tool.  See
  the KindSoftware web site for more information.
  https://www.kindsoftware.com/products/opensource/bonc/

- `docs` contains a formal description of the Lando and Lobot
  languages, including their grammars and type systems.
  
- `gf` contains GF-based grammars for parsing natural language
  specifications embedded in the Lando specifications into refinement
  types.

- `source/lobot` - A parser, type checker, and instance generator for
  the Lobot sublanguage of Lando.  See `source/lobot/README.md` for
  more details.  Lobot is implemented in Haskell.
  
- `source/lando` - A parser and command line tool for the Lando System
  Specification (Sub)Language (SSL).  Lando is implemented in Kotlin.
  
  To run the SSL parser on a `.lando` file, first run `./lando.sh -r`
  to build the executable, then run:
  ```
  $ ./lando.sh -f path/to/source.lando
  ```
  Use `-h` to view the available options. See `source/lando/README.md`
  for more details.
  
### Status of Un-merged Branches

This section gives short summaries of the branches which were still
in-progress as of September 2020, at which time DARPA halted the
development of tools in the BESSPIN project for the SSITH program.

- `lssl-v2` - Implements the changes to the Lando SSL discussed in
  [this Google
  Doc](https://docs.google.com/document/d/1dNa6TtV8_ON2jmsPzbp2ZXbtNIaBm_WG0y_Nea4SkQI/edit),
  along with a well-formedness checker for this version 2 of SSL and
  documents in `docs/system-spec-sublang` giving a formal presentation
  of its grammar and well-formedness criteria. This branch may be
  ready to merge once it is tested.

- `lssl-well-formed` - Adds a document in `docs/system-spec-sublang`
  giving a formal presentation of the well-formedness criteria of the
  Lando SSL v1. If `lssl-v2` is merged, this branch could be merged in
  renaming `wf.tex` to `old_wf.tex`.

- `feature/constrained-functions` - Work in progress on all three
  tasks in #56 (adding support for abstract functions with constraints
  in Lobot), but is currently missing support for functions which
  contain other function calls in their argument constraints. Before
  this can be merged some deep thought needs to be given as to whether
  these changes are really what we want.


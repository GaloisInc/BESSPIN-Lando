# Introduction

This document offers the basics of building and running Lobot. For a more
in-depth introduction to the language and its use, see docs/UserGuide.md.

# Building

This is under active development, and much of this library and its associated
tooling will change rapidly.

First, clone this repository (which you may have done already). Then, initialize
its submodules via

```bash
git submodule update --init
```

In order to build Lobot, you must obtain `ghc` and `cabal`, the compiler and
build tool for Haskell. GHC must be at least version 8.8, which is the latest
stable release.

Once those tools are installed, the following should \"just work\":

```bash
cabal v2-build
```

This will build the `lobot` tool in some hidden subdirectory, which you can
`find` via:

```bash
find $(pwd) -type f -name lobot
```

Now you have the path to the executable, and can `alias` to it in order to run
it easily.

Before running `lobot`, install the `z3` tool, or you will get a runtime error.
You can download the binary [here](https://github.com/Z3Prover/z3/releases).
Simply download, unzip, and move the `bin/z3` executable anywhere on your path.
Alternatively, use your favorite package manager (brew, apt-get, etc.) to
install it.

# Running

The Lobot user\'s guide doesn\'t exist yet, as various aspects of the language
have not yet been solidified. See the `examples` directory for various examples
(some of which will work out of the box, others of which will require a bit of
system configuration before running).

The basic way you \"run\" Lobot is by invoking `lobot`:

```bash
lobot --help
```

See the user's guide for more details.

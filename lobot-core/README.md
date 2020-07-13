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

This will build the `lobotIG` tool in some hidden subdirectory, which you can
`find` via:

```bash
find $(pwd) -type f -name lobotIG
```

Now you have the path to the executable, and can `alias` to it in order to run
it easily.

# Running

The Lobot user\'s guide doesn\'t exist yet, as various aspects of the language
have not yet been solidified. See the `examples` directory for various examples
(some of which will work out of the box, others of which will require a bit of
system configuration before running).

The basic way you \"run\" Lobot is by invoking `lobotIG`:

```bash
lobotIG examples/example2.lobot
```

Currently, the tool simply loads in a Lobot file and enumerates all instances of
the final declaration in the file. It only enumerates 100 instances; this limit
is somewhat arbitrary, but is not configurable at the command line. It is
important to have some kind of limit, since data types can have infinitely many
instances.


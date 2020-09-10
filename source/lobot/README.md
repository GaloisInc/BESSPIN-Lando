# Introduction

This document offers the basics of building and running Lobot. For a more
in-depth introduction to the language and its use, see docs/UserGuide.md.

# Building

This is under active development, and much of this library and its associated
tooling will change rapidly.

First, clone this repository (which you may have done already).

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

Alternatively, the built executable can be accessed directly through cabal as
follows:

```bash
cabal v2-exec -- lobot [options]
```

Before running `lobot`, install the `z3` tool, or you will get a runtime error.
You can download the binary [here](https://github.com/Z3Prover/z3/releases).
Simply download, unzip, and move the `bin/z3` executable anywhere on your path.
Alternatively, use your favorite package manager (brew, apt-get, etc.) to
install it.

# Running

The basic way you \"run\" Lobot is by invoking `lobot`:

```bash
lobot --help
```

See the user's guide for more details, or the `examples` and `tests`
directories for various examples (some of which will work out of the box,
others of which will require a bit of system configuration before running).

# Testing

To run the test suite, located in the `tests` directory, use `cabal` as
follows:

```bash
cabal v2-test
```

To add a new test, add the relevant Lobot file to `tests/lobot-files` and
any functions on which it depends to `tests/functions`, then run the above
command to accept the output of the `lobot` tool on the last kind declaration
in this file as the expected output. To update this expected output at a later
point, run:

```bash
cabal v2-test --test-options=--accept
```

To create a more consistent environment for testing, a `tmp/tests` directory
is removed and re-created at the start of each test. Thus, any file paths used
in functions in `test/lobot-files` should begin with `tests/tmp/` (see
`tests/functions/tmp_file`, for example).

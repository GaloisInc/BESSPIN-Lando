# Lobot (v0.0.1)

Lobot is a language for defining constrained data structures -- data structures
with arbitrary logical constraints attached to them. The Lobot instance
generator can enumerate all instances of a data structure and check that they
satisfy arbitrary properties.

Lobot will be used as an IR for features models in Lando. Most of the core
functionality of Lando will be implemented at the level of Lobot.

This release is a development snapshot. Please contact Ben Selfridge
(benselfridge@galois.com) and Matt Yacavone (myac@galois.com) if you have any
questions about it.

## Introductory example

This example is taken from examples/example1.lobot.

Suppose I wanted to define a datatype that represents all pairs of positive
integers that sum to 10. I could do that in lobot by first defining the concept
of a positive integer:

```
-- Positive integers.
posint : kind of int where 1 <= self
```

Then, I can create pairs of positive integers by creating a `struct`:

```
-- Unique pairs of positive integers
unique_posint_pair : kind of struct
  with x : posint
       y : posint
  where x <= y
```

Notice the `x <= y` constraint, which helps avoid getting the same pair, just in
a different order. Now, we can define one more datatype that further constrains
`unique_posint_pair`:

```
-- Pairs that sum to 10
posint_sum_10 : kind of unique_posint_pair
  where x + y = 10
```

If I type the above definitions into a file, `posint_sum_10.lobot`, and then fire up
the Lobot instance generator via

```bash
lobotIG posint_sum_10.lobot
```

I get the following output:

```bash
Last kind in posint_sum_10.lobot:
----------------
posint_sum_10 : kind of struct
  with x : int, 
       y : int
  where (x + y) = 10, 
        x <= y, 
        1 <= x, 
        1 <= y
----------------
Generating instances...
5 valid instances, enumerated 5
Instance 1/5:
posint_sum_10 with {x = 5, y = 5}
Press enter to see the next instance.
```

Every time I type enter, I see a new instance of `posint_sum_10`. There are five
in total.

## Building

This is under active development, and much of this library and its associated
tooling will change rapidly. Contact benselfridge@galois.com and 

First, clone this repository (which you may have done already). Then, initialize
its submodules via

```bash
git submodule update --init
```

In order to build Lobot, you must obtain `ghc` and `cabal`, the compiler and
build tool for Haskell. GHC must be at least version 8.8, which is the latest
stable release.

Once those tools are installed, the following should "just work":

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

## Running

The Lobot user's guide doesn't exist yet, as various aspects of the language
have not yet been solidified. See the `examples` directory for various examples
(some of which will work out of the box, others of which will require a bit of
system configuration before running).

The basic way you "run" Lobot is by invoking `lobotIG`:

```bash
lobotIG examples/example2.lobot
```

Currently, the tool simply loads in a Lobot file and enumerates all instances of
the final declaration in the file. It only enumerates 100 instances; this limit
is somewhat arbitrary, but is not configurable at the command line. It is
important to have some kind of limit, since data types can have infinitely many
instances.


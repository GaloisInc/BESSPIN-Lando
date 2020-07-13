# Introduction

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
integers that sum to 100. I could do that in lobot by first defining the concept
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
-- Pairs that sum to 100
posint_sum_100 : kind of unique_posint_pair
  where x + y = 100
```

If I type the above definitions into a file, `posint_sum_100.lobot`, and then fire up
the Lobot tool via

```bash
$ lobot posint_sum_100.lobot
```

I get the following output:

```bash
All checks pass. (File had no checks)
```

This means that the file was syntactically valid and well-typed, which is great!
It also doesn't give us any information about the types we defined. We can count
the number of instances of one of our `kind`s in the following way:

```bash
$ lobot -c posint_sum_100 example1.lobot
Generating instances...
Found 50 valid instances, generated 0 invalid instances
```

Lobot determined that there are 50 instances of the `posint_sum_100` kind. If we
want to enumerate them, we can use the `-e` option:

```bash
$ lobot -e posint_sum_100 example1.lobot
Generating instances...
Instance 1:
  posint_sum_100 with {x = 50, y = 50}
Press enter to see the next instance
```

Each time we hit enter, we see a new instance.

What if we want to verify that some property always holds of our
`posint_sum_100` kind? For instance, we might like to ensure that for any
instance `p` of `posint_sum_100`, `p.x <= 50`. We can encode that as a `check`
command in the Lobot source file:

```
-- Check that all instances satisfy p.x <= 50
check1 : check
  on p : posint_sum_100
  that p.x <= 50
```

We run this check via the `-r` option:

```bash
$ lobot -r check1 ../examples/example1.lobot
Generating instances...
Check 'check1' holds. (Generated 0 instances)
```

Lobot determined that the check holds for all instances of the `posint_sum_100`
kind. What if we change the check condition `p.x <= 50` to `p.x < 50`?

```bash
$ lobot -r check1 ../examples/example1.lobot
Generating instances...
Check 'check1' failed with counterexample:
  p = struct with {x = 50, y = 50}
```

Lobot fails, and finds a counterexample for us. Great!

# The language of Lobot

## Types

## Kinds

## Abstract functions and types

# Introduction

Lobot is a language for defining _constrained data types_ -- data types
with arbitrary logical constraints attached to them. The Lobot instance
generator can enumerate all instances of a data structure and check that they
satisfy arbitrary properties.

Lobot will be used as an IR for features models in Lando. Most of the core
functionality of Lando will be implemented at the level of Lobot.

This release is a development snapshot. Please contact Ben Selfridge
(benselfridge@galois.com) and Matt Yacavone (myac@galois.com) if you have any
questions about it.

## What is Lobot?

Lobot is a language for describing data types (ints, structs, etc.) with
constraints attached to them. It is also a tool for enumerating the values of
those types, and checking that the enumerated values satisfy certain properties.

Lobot uses an SMT solver to enumerate and check constrained types (called
_kinds_), and it has very similar expressive power to constraint programming
languages (like MiniZinc). However, Lobot does more than model a problem; it can
directly interact with real-world applications and incorporate them into its
reasoning engine. In other words, we can call a command line tool with Lobot and
use it as part of a constraint in a kind we are defining. This allows us to talk
about properties of external software and actually verify that various
properties hold about that software.

Before we dive into the core concepts of Lobot, let's look at a couple of
examples.

## Example 1: pairs that sum to 100

Suppose I wanted to define a data type that represents all pairs of positive
integers that sum to 100. I could do that in Lobot by first defining the concept
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
a different order. Now, we can define one more data type that further constrains
`unique_posint_pair`:

```
-- Pairs that sum to 100
posint_sum_100 : kind of unique_posint_pair
  where x + y = 100
```

If I type the above definitions into a file, `posint_sum_100.lobot`, and then fire up
the Lobot tool via

```
$ lobot posint_sum_100.lobot
```

I get the following output:

```
All checks pass. (File had no checks)
```

This means that the file was syntactically valid and well-typed, which is great!
It also doesn't give us any information about the types we defined. We can count
the number of instances of one of our `kind`s in the following way:

```
$ lobot -c posint_sum_100 posint_sum_100.lobot
Generating instances...
Found 50 valid instances, generated 0 invalid instances
```

Lobot determined that there are 50 instances of the `posint_sum_100` kind. If we
want to enumerate them, we can use the `-e` option:

```
$ lobot -e posint_sum_100 posint_sum_100.lobot
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

```
$ lobot -r check1 posint_sum_100.lobot
Generating instances...
Check 'check1' holds. (Generated 0 instances)
```

Lobot determined that the check holds for all instances of the `posint_sum_100`
kind. What if we change the check condition `p.x <= 50` to `p.x < 50`?

```
$ lobot -r check1 posint_sum_100.lobot
Generating instances...
Check 'check1' failed with counterexample:
  p = struct with {x = 50, y = 50}
```

Lobot fails, and finds a counterexample for us. Great!

## Example 2: The real world

Lobot is designed to directly interact with applications as a core language
feature. For instance, suppose I have a shell command called `add1` that works
like this:

```
$ add1 5
6
$ add1 -1000000
-999999
```

Context clues lead us to the conclusion that the `add1` command is probably
supposed to add 1 to its input. In Lobot, we can actually import functions from
the command-line environment and invoke them when defining new kinds. However,
we do need to provide small wrapper scripts for Lobot to be able to call them;
Lobot passes arguments to these commands by translating them to JSON and passing
them to stdin, and then getting the JSON-encoded results from stdout. Below is
an example implementation of `add1` in python that is compatible with Lobot's
API:

```python
#!/usr/bin/python3

import json
import sys

json_str = ""

for line in sys.stdin:
  json_str += line

# Incoming JSON data has this format:
# [ { "variant": "int", "value": <value> } ]
json_data = json.loads(json_str)

i = json_data[0]['value']
json_data[0]['value'] = i + 1

print(json.dumps(json_data[0]))
```

After adding this to our `$PATH`, we can call it on the command line:

```
$ echo "[{\"variant\": \"int\", \"value\": 4}]" | add1
{"variant": "int", "value": 5}
```

This is, admittedly, not as clean to interact with directly as the original
`add1` function. However, it is straightforward to "wrap" real-world commands
with scripts that provide the JSON-based API that Lobot requires.

Now that `add1` is on our path, we can use it in Lobot. In a new file named
`add1.lobot`, we write

```
abstract add1 : int -> int
```

This is a _function declaration_. We have declared to Lobot that there exists a
function called `add1` mapping integers to integers. Now, we are free to use
this function as part of a new kind definition:

```
add1_0 : kind of int where self = add1(0)
```

Just like any other kind, we can enumerate instances one-by-one, or we can count
the number of instances. Let's enumerate `add1_0`:

```
Instance 1:
  1
Press enter to see the next instance

Enumerated all 1 valid instances, generated 1 invalid instances
```

Lobot has determined that there is exactly one instance of `add1_0`, which is
not terribly impressive, since every function has exactly one value when you
call it. Let's see what happens if we try the inverse problem of finding the
solution to `add1(x) = 0`:

```
add1_is_0 : kind of int where add1(self) = 0
```

Let's enumerate this kind:

```
$ lobot -e add1_is_0 add1.lobot
Generating instances of 'add1_is_0'...
```

Uh-oh! Lobot hangs when we ask it to enumerate instances of this kind. Let's use
the `-v` option to try and figure out what's going on:

```
$ lobot -e add1_is_0 -v add1.lobot
Generated an invalid instance:
  1
The constraints that failed were:
  add1(self) = 0
Learned the values of the following function calls:
  add1(1) = 2
Press enter to see the next instance

Generated an invalid instance:
  3
The constraints that failed were:
  add1(self) = 0
Learned the values of the following function calls:
  add1(3) = 4
Press enter to see the next instance

Generated an invalid instance:
  5
The constraints that failed were:
  add1(self) = 0
Learned the values of the following function calls:
  add1(5) = 6
Press enter to see the next instance
```

Here, we see that Lobot is effectively attempting to find a solution to `add1(x)
= 0` by enumerating positive values of `x` until it finds one (for some reason,
it is only checking odd values). However, this strategy will never yield
a valid instance, because the solution to this equation is `-1`, which is
negative. We can help Lobot out by constraining the search space a bit:

```
add1_is_0 : kind of int
  where add1(self) = 0
        -10 <= self
        self <= 10
```

Now, when we attempt to enumerate values of this kind, we have more success:

```
$ lobot -e add1_is_0 add1.lobot 
Instance 1:
  -1
Press enter to see the next instance

Enumerated all 1 valid instances, generated 20 invalid instances
```

Not only has Lobot found the solution, `-1`, it has determined that it is the
only solution in the range `-10 <= x <= 10` via exhaustively checking every
value in that range. Since `add1` could have been any function, and Lobot knows
nothing about its behavior, we couldn't reasonably expect it to do anything more
clever than an exhaustive search.

In general, Lobot knows nothing about real-world functions until it actually
evaluates calls to it for particular values. Therefore, we need to be careful
when enumerating instances of kinds that are constrained by such functions.

# The Lobot language

## Types, type synonyms, and kinds

The base types of Lobot are:

* booleans
* integers
* enums
* enumsets
* structs
* abstract types

### Type synonyms

Before we discuss these types individually, it will be helpful to introduce
_type synonyms_. We can declare a new type synonym:

```
type <name> = <type>
```

where `<name>` is the name of the type synonym, and `<type>` is a base type.
Type names must start with a lowercase letter. For instance, we could alias
`int` with `ident`:

```
type ident = int
```

We can now use `ident` as an alternative name for `int`.

### Booleans

The built-in keyword `bool` signifies the type of boolean values. We can
enumerate `bool` instances by declaring a synonym in lobot:

```
-- file: bool.lobot
type my_bool = bool
```

```
$ lobot -e my_bool bool.lobot
Instance 1:
  false
Press enter to see the next instance

Instance 2:
  true
Press enter to see the next instance

Enumerated all 2 valid instances, generated 0 invalid instances
```

If we are in an introductory logic course and are assigned the following
homework problem:

```
Fill out the truth table of the proposition "P => ((Q | R) => (R => !P))".
```

We can use Lobot to cheat:

```
-- file: logic_homework1.lobot
problem1 : kind of struct
  with p : bool, q : bool, r : bool
  where p => ((q | r) => (r => not p))
```

```
$ lobot -e problem1 logic_homework1.lobot
Instance 1:
  problem1 with {p = false, q = false, r = false}
Press enter to see the next instance

Instance 2:
  problem1 with {p = false, q = false, r = true}
Press enter to see the next instance

Instance 3:
  problem1 with {p = false, q = true, r = true}
Press enter to see the next instance

Instance 4:
  problem1 with {p = false, q = true, r = false}
Press enter to see the next instance

Instance 5:
  problem1 with {p = true, q = false, r = false}
Press enter to see the next instance

Instance 6:
  problem1 with {p = true, q = true, r = false}
Press enter to see the next instance

Enumerated all 6 valid instances, generated 0 invalid instances
```

### Integers

Similarly to `bool`, `int` signifies the type of integer values.

```
-- file: int.lobot
type my_int = int
```

We can attempt to count the integers (rather than enumerate them one-by-one)
with the `-c` option:

```
lobot -c my_int int.lobot
Hit instance limit of 100!
Found 100 valid instances, generated 0 invalid instances
```

Lobot informs us that we have hit the built-in instance limit of `100`,
resulting in `100` instances of `int`. We can increase this limit with `-l`:

```
lobot -c my_int -l 10000 int.lobot
Hit instance limit of 10000!
Found 10000 valid instances, generated 0 invalid instances
```

After a minute or two, Lobot finally determines that there are at least `10000`
instances of `int`. So, in case you didn't realize it -- there are more than ten
thousand integers!

If we are in a high school algebra class, we can use Lobot to do our homework on
linear systems of equations for us:

```
type pair = struct with x : int, y : int

problem1 : kind of pair
  where x - 7 * y = -11
        5 * x + 2 * y = -18
```

```
lobot -e problem1 algebra_homework1.lobot
Instance 1:
  problem1 with {x = -4, y = 1}
Press enter to see the next instance

Enumerated all 1 valid instances, generated 0 invalid instances
```

This of course assumes that the linear system has an integer solution (which
they usually do when they arrive in the form of algebra homework).

### Enumerations and enumeration sets

Enumerations are like `enum`s in the C programming language, or "sum types" in
Haskell. They are finite, user-defined types that encode a variety of different
choices.

```
type abc = {A, B, C}
```

```
$ lobot -e abc enum.lobot
Instance 1:
  A
Press enter to see the next instance

Instance 2:
  B
Press enter to see the next instance

Instance 3:
  C
Press enter to see the next instance

Enumerated all 3 valid instances, generated 0 invalid instances
```

Here, we introduce an enum type called `abc` with three constructors, `A`, `B`,
and `C`. The constructors of an enum must start with capital letters. We can
also use subsets of enumerations:

```
type abc_set = subset abc
```

```
$ lobot -e abc_set enum.lobot
Instance 1:
  {}
Press enter to see the next instance

Instance 2:
  {A}
Press enter to see the next instance

Instance 3:
  {A, B}
Press enter to see the next instance

Instance 4:
  {A, B, C}
Press enter to see the next instance

Instance 5:
  {B, C}
Press enter to see the next instance

Instance 6:
  {C}
Press enter to see the next instance

Instance 7:
  {A, C}
Press enter to see the next instance

Instance 8:
  {B}
Press enter to see the next instance

Enumerated all 8 valid instances, generated 0 invalid instances
```

We can create more constrained kinds of subsets:

```
a_implies_c : kind of abc_set
  where A in self => C in self

doesnt_have_c : kind of abc_set
  where not (C in self)

both : kind of a_implies_c doesnt_have_c
```

```
$ lobot -e both enum.lobot
Instance 1:
  {}
Press enter to see the next instance

Instance 2:
  {B}
Press enter to see the next instance

Enumerated all 2 valid instances, generated 0 invalid instances
```

### Structs

In Lobot, _structs_ are the basic way we form compound types and package
multiple values into a single data structure. We have seen several examples of
structs already. Here's another one:

```
even_int : kind of struct
  with i : int
       j : int
  where i = 2 * j

odd_int : kind of struct
  with i : int
       j : int
  where i = 2 * j + 1

-- Check that even and odd ints are distinct.
even_odd_check : check
  on e : even_int
     o : odd_int
  that not (e.i = o.i)
```

### Abstract types

We can declare brand new types in Lobot, without specifying the values they
take:

```
abstract type t

type two_ts = struct
  with t1 : t
       t2 : t
```

However, if we try to generate instances of `two_ts`, something interesting
happens:

```
$ lobot -e two_ts abstract.lobot
Cannot generate instances of abstract type.
```

Abstract types cannot be enumerated by Lobot, because we know nothing about
their value set. So, why are they part of Lobot? Because they can be used as
return values and arguments to _abstract functions_.

## Abstract functions

## JSON API

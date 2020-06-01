# Contributing to lando-core

The following is a set of guidelines for contributing to Lando.

#### Table Of Contents

[Styleguide](#styleguide)

## Styleguide

### Import declarations

Import declarations should be grouped into three main categories: internal
imports, qualified external imports, and unqualified external imports. All
imports of modules internal to this project should be at the top of the import
list, followed by the qualified external imports, followed by the unqualified
external imports. Example:

```
import Lando.Core.Kind

import qualified What4.Expr.Builder as WB
import qualified What4.Solver.Z3    as WS

import Data.List (intercalate)
import Data.Parameterized.Nonce
import Data.Parameterized.Some
```

Internal imports should be unqualified in most cases, except when intentional
name clashes exist.

All qualified imports (internal and external) should be all-uppercase
abbreviations of the module they represent, although it may not be necessary to
abbreviate every single intermediate module. In the above example,
`What4.Expr.Builder` gets abbreviated as `WB`. It's a bit of a stylistic
decision. As much as possible, use the same abbreviation across the project when
importing the same module in multiple different places.

All qualified imports should have the qualification prefixes lined up in a
column, and should be alphabetized *by prefix*, rather than by module path. This
allows one to see a qualification prefix in the code and quickly scan the
qualified imports list to discover what that prefix abbreviates.

When importing entire modules, most external imports should be imported
qualified. The only exceptions to this rule currently are the functions in
`parameterized-utils`, as we consider those part of our conceptual `Prelude`.
Even so, we sometimes have name clashes between `parameterized-utils` and
`Prelude`, so it may be necessary to import something qualified, or to hide
something from `Prelude`; both are acceptable, and discretion should be used to
determine what the best way to do it is.

We also allow importing specific declarations from external modules other than
`parameterized-utils`; for instance, in the above example we import the function
`intercalate` from `Data.List`. This is fine.

When importing the same module both qualified and unqualified, the unqualified
import must not be the whole module, but must contain a parenthesized list of
specific types, functions, etc. that will be imported unqualified.

### Function and variable naming

Functions must be camel case -- they cannot ever contain underscores. Variables
may contain underscores, although camel case is usually preferred. One instance
where underscores may be preferable is indicating indexes -- so if `as :: [a]`,
then we might declare `as_3 = as !! 3`, which is a bit more readable than `as3 =
as !! 3`.

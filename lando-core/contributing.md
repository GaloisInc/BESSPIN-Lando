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
import qualified What4.Solver.Z3 as WS

import Data.List (intercalate)
```

Internal imports should be unqualified in most cases, except when intentional
name clashes exist.

All qualified imports (internal and external) should be all-uppercase
abbreviations of the module they represent, although it may not be necessary to
abbreviate every single intermediate module. In the above example,
`What4.Expr.Builder` gets abbreviated as `WB`. It's a bit of a stylistic
decision. As much as possible, use the same abbreviation across the project when
importing the same module in multiple different places.

Most external imports should be imported qualified. The only exceptions to this
rule currently are the functions in `parameterized-utils`, as we consider those
part of our conceptual `Prelude`. Even so, we sometimes have name clashes
between `parameterized-utils` and `Prelude`, so it may be necessary to import
something qualified, or to hide something from `Prelude`; both are acceptable,
and discretion should be used to determine what the best way to do it is.

When importing the same module both qualified and unqualified, the unqualified
import must not be the whole module, but must contain a parenthesized list of
specific types, functions, etc. that will be imported unqualified.

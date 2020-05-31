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

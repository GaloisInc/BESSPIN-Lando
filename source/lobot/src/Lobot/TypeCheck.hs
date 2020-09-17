{-# LANGUAGE GADTs #-}

{-|
Module      : Lobot.TypeCheck
Description : The LOBOT type checker.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module exposes the type checking algorithm for the Lobot AST.

Type checking transforms a list of untyped declarations into a function
environment, a list of kinds in that environment, and a list of checks in that
environment (see 'TypeCheckResult'). Because the types of these resulting
kinds and checks depend on the resulting function environment, we cannot just
directly map each untyped declaration to a typed one. Instead, we have two
options:

1. Type check all declarations in one pass, building a function environment as
   we go. The downside of this is that every time we encounter a new function
   declaration, all previously type checked kinds and checks now need to be
   traversed in order to update their function environment type parameter.

2. Noticing that only the constraints of a kind or check depend on the
   function environment (e.g. see 'K.kindConstraints' in 'K.Kind'), the
   unnecessary traversals mentioned above can be avoided by instead doing type
   checking in two passes. In the first pass, the type corresponding to each
   kind is determined, which can then be used to determine the type of each
   function, and thus a function environment. This can then be used in a
   second pass to type check all kind and check constraints.

We chose the second option. The algorithm can be summarized as follows:

@
Untyped syntax --[First pass]--> (Function env., IDecls) --[Second pass]--> Fully typed syntax
@

"Lobot.TypeCheck.IDecls" defines an intermediate notion of declaration,
similar to 'Decl' from "Lobot.Syntax" but with 'Type' from "Lobot.Syntax"
replaced with 'TypeRepr' from "Lobot.Types". The first pass produces these
intermediate declarations, and is located in "Lobot.TypeCheck.FirstPass". The
second pass is located in "Lobot.TypeCheck.SecondPass".
"Lobot.TypeCheck.Monad" defines the monad 'TCM' used in both passes of type
checking.

See all the modules mentioned above for more detail on each respective part
of type checking.

-}

module Lobot.TypeCheck
  ( typeCheck
  , TypeCheckResult(..)
  -- ** Re-exports from "Lobot.TypeCheck.Monad"
  , TypeError(..)
  , ppTypeError
  , TypeWarning(..)
  , ppTypeWarning
  , SomeTypeOrString(..)
  , ppSomeTypeOrString
  ) where

import Data.Parameterized.Some
import Data.Parameterized.Context
import Control.Monad.Writer (runWriterT)
import qualified Data.Set as Set

import Lobot.Kind as K
import Lobot.Syntax as S

import Lobot.Utils (mapSnd)
import Lobot.TypeCheck.Monad
import Lobot.TypeCheck.FirstPass
import Lobot.TypeCheck.SecondPass

-- | The result of type checking a list of untyped declarations is a function
-- environment, a list of kinds in that environment and a list of checks in
-- that environment.
data TypeCheckResult where
  TypeCheckResult :: Assignment (ConstrainedFunction env) env
                  -> [Some (K.Kind env)]
                  -> [Some (K.Check env)]
                  -> TypeCheckResult

-- | Given a list of declarations, produce a 'TypeCheckResult'.
typeCheck :: [S.Decl] -> Either TypeError (TypeCheckResult, [TypeWarning])
typeCheck decls = fmap (mapSnd Set.toAscList) . runWriterT $ do
  (Some env, idecls) <- firstPass decls
  (ks, cks, cfnEnv) <- secondPass env idecls
  pure (TypeCheckResult cfnEnv ks cks)

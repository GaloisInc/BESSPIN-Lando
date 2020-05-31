{-|
Module      : Lando.Core.TypeCheck
Description : The LOBOT type checker.
Copyright   : (c) Matt Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines the type checking algorithm for the Lobot AST.
-}

module Lando.Core.TypeCheck where

import Lando.Core.Kind   as K
import Lando.Core.Syntax as S

data TypeError = TypeError

-- | Given a list of kind declarations, produce a list of typed kinds.
typeCheck :: [S.KindDecl] -> Either TypeError [Some K.Kind]
typeCheck = error "type checker unimplemented"

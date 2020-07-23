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
-}

module Lobot.TypeCheck
  ( typeCheck
  , TypeCheckResult(..)
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
import Lobot.Types as T

import Lobot.Utils (mapSnd)
import Lobot.TypeCheck.Monad
import Lobot.TypeCheck.FirstPass
import Lobot.TypeCheck.SecondPass

data TypeCheckResult where
  TypeCheckResult :: Assignment FunctionTypeRepr env
                  -> [Some (K.Kind env)]
                  -> [Some (K.Check env)]
                  -> TypeCheckResult

-- | Given a list of declarations, produce a list of typed kinds.
typeCheck :: [S.Decl] -> Either TypeError (TypeCheckResult, [TypeWarning])
typeCheck decls = fmap (mapSnd Set.toAscList) . runWriterT $ do
  (Some env, idecls) <- firstPass decls
  (ks, cks) <- secondPass env idecls
  pure (TypeCheckResult env ks cks)

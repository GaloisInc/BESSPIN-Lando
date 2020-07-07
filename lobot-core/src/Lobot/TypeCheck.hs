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

import Lobot.Kind as K
import Lobot.Syntax as S
import Lobot.Types as T

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
typeCheck decls = runWriterT $ do
  FirstPassResult env ks cks st <- firstPass decls
  SecondPassResult ks' cks' <- secondPass env ks cks st
  pure $ TypeCheckResult env ks' cks'

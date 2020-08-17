{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Lobot.TypeCheck.IDecls
Description : An intermediate untyped syntax used during typechecking.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galoicom
Stability   : experimental
Portability : POSIX

This module defines an intermediate version 'S.Decl' from "Lobot.Syntax" for
use between the first and second passes of type checking. Relative to
"Lobot.Syntax", the definition of 'Decl' in this module replaces every 'Type'
from "Lobot.Syntax" with a 'TypeRepr' from "Lobot.Types", a list of
'DerivedConstraint's, and a set of enum names this type should bring into
scope. See "Lobot.TypeCheck" and "Lobot.TypeCheck.FirstPass" for broader
context about this type.

-}

module Lobot.TypeCheck.IDecls
  ( -- ** Intermediate declarations
    Decl(..)
  , Kind(..)
  , EnumNameSet
  , Check(..)
  , CheckField(..)
  , FunctionType(..)
    -- ** Derived Constraints
  , DerivedConstraint(..)
  , getDerivedConstraintKinds
  ) where

import Data.Text (Text)
import qualified Data.HashSet as HS
import Data.List.NonEmpty
import Data.Functor.Const
import Data.Parameterized.Classes
import Data.Parameterized.Some
import Data.Parameterized.Context

import Lobot.Lexer
import qualified Lobot.Syntax as S
import qualified Lobot.Types as T

data Decl = KindDecl Kind
          | CheckDecl Check
          | TypeSynDecl LText (Some T.TypeRepr) EnumNameSet
          | FunctionDecl LText FunctionType

data Kind where
  Kind :: { kindName :: LText
          , kindType ::  T.TypeRepr tp
          , kindConstraints :: [S.LExpr]
          , kindDerivedConstraints :: [DerivedConstraint]
          , kindInScopeEnums :: EnumNameSet
          } -> Kind

deriving instance Show Kind

type EnumNameSet = HS.HashSet Text

data CheckField tp =
  CheckField { checkFieldName :: LText
             , checkFieldType :: T.TypeRepr tp
             , checkFieldDerivedConstraints :: [DerivedConstraint]
             }
  deriving (Show)

instance ShowF CheckField

data Check where
  Check :: { checkName :: LText
           , checkFields :: Assignment CheckField tps
           , checkConstraints :: [S.LExpr]
           , checkRequirements :: [S.LExpr]
           , checkInScopeEnums :: EnumNameSet
           } -> Check

deriving instance Show Check

-- | Unlike 'S.FunctionType', this saves the 'DerivedConstraint's on the
-- function's argument and return types, though they are currently not
-- used for anything.
data FunctionType where 
  FunType :: { funArgTypes :: Assignment T.TypeRepr args
             , funRetType  :: T.TypeRepr ret
             , funArgConstraints :: Assignment (Const [DerivedConstraint]) args
             , funRetConstraints :: [DerivedConstraint]
             , funArgInScopeEnums :: [EnumNameSet]
             , funRetInScopeEnums :: EnumNameSet
             } -> FunctionType

deriving instance Show FunctionType

-- | After the first pass of typechecking, all kind names have been resolved
-- in types, but no expressions (and thus no kind constraints) have been
-- checked yet. Instead of copying over the untyped constraints, which could
-- create strange error reporting w.r.t. source locations, we save
-- 'DerivedConstraint's, which are essentially pointers to where to get
-- additional constraints in the second pass, once they're typechecked.
data DerivedConstraint = FromKind LText
                       | FromField LText (NonEmpty DerivedConstraint)
                       deriving Show

-- | The list of all kind names in a derived constraint.
getDerivedConstraintKinds :: DerivedConstraint -> NonEmpty LText
getDerivedConstraintKinds (FromKind k) = k :| []
getDerivedConstraintKinds (FromField _ dcns) = dcns >>= getDerivedConstraintKinds

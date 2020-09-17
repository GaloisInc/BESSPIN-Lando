{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Lobot.Kind
Description : The core data type for representing feature models in Lobot.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the core data type for representing a feature model in
Lobot.
-}
module Lobot.Kind
  ( -- * Kinds
    Kind(..)
  , KindExpr
  , pattern SelfExpr
  , addConstraints
  , derivedKind
  , liftConstraints
  , giveSelf
  , liftExpr
    -- * Checks
  , Check(..)
  , NamedType(..)
    -- * Constrained functions
  , ConstrainedFunction(..)
  , cfunType'
  , envTypes
  ) where

import Lobot.Expr
import Lobot.Types

import Data.Functor.Const
import Data.List.NonEmpty hiding ((!!))
import Data.Text (Text)
import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.TraversableFC
import Prelude hiding ((!!))

type KindExpr env tp = Expr env (EmptyCtx ::> tp)

-- | Representation of a constrained Lobot function, which consists of a
-- function name, argument types, a return type, argument names, constraints
-- on those arguments which must be satisfied every time the function is
-- called, and constraints on the return type which are assumed to be
-- satisfied for every call of the function.
data ConstrainedFunction (env :: Ctx FunctionType) (fntp :: FunctionType) where
  CFun :: { cfunType :: FunctionTypeRepr (FunType nm args ret)
          , cfunArgNames :: Assignment (Const Text) args
          , cfunArgConstraints :: [Expr     env args BoolType]
          , cfunRetConstraints :: [KindExpr env ret  BoolType]
          } -> ConstrainedFunction env (FunType nm args ret)

cfunType' :: ConstrainedFunction env fntp -> FunctionTypeRepr fntp
cfunType' (CFun ftp _ _ _) = ftp

-- | Given a function environment of 'ConstrainedFunction's, return the
-- associated function environment of 'FunctionTypeRepr's.
envTypes :: Assignment (ConstrainedFunction env') env
         -> Assignment FunctionTypeRepr env
envTypes = fmapFC cfunType'

deriving instance Show (ConstrainedFunction env fntp)
instance ShowF (ConstrainedFunction env)

-- | Representation of a Lobot feature model, which we term a 'Kind' for
-- brevity. A kind consists of a name, a type, a function environment, and a
-- list of constraints that must hold for all instances of this kind.
data Kind (env :: Ctx FunctionType) (tp :: Type) = Kind
  { kindName :: Text
  , kindType :: TypeRepr tp
  , kindFunctionEnv :: Assignment FunctionTypeRepr env
  , kindConstraints :: [KindExpr env tp BoolType]
  }
  deriving Show
instance ShowF (Kind env)

data NamedType tp = NamedType { namedTypeName :: Text
                              , namedTypeType :: TypeRepr tp
                              }

data Check (env :: Ctx FunctionType) (tps :: Ctx Type) = Check
  { checkName :: Text
  , checkFields :: Assignment NamedType tps
  , checkFunctionEnv :: Assignment FunctionTypeRepr env
  , checkConstraints :: [Expr env tps BoolType]
  , checkRequirements :: [Expr env tps BoolType]
  }

-- | Augment a 'Kind' with some additional constraints.
addConstraints :: Kind env tp -> [KindExpr env tp BoolType] -> Kind env tp
addConstraints kd cs =
  kd { kindConstraints = kindConstraints kd ++ cs }

-- | Given a set of parent 'Kind's of the same type, create a derived 'Kind'.
derivedKind :: NonEmpty (Kind env tp)
            -> Text
            -> [KindExpr env tp BoolType]
            -> Kind env tp
derivedKind kds@(kd :| _) nm cs =
  kd { kindName = nm
     , kindConstraints = concatMap kindConstraints kds ++ cs
     }

-- | Lift the constraints of a kind @K'@ into a parent kind @K@ that contains
-- @K'@. This should only be called once for a given child.
liftConstraints :: Index ktps '(nm, tp)
                -> Kind env tp
                -> Kind env (StructType ktps)
                -> Kind env (StructType ktps)
liftConstraints i k' k = addConstraints k (liftExpr i <$> kindConstraints k')

-- | Substitute a value for 'self' in a kind expression.
giveSelf :: Expr env ctx a -> KindExpr env a b -> Expr env ctx b
giveSelf s = subst (Empty :> s)

singletonIndexRefl :: forall tp tp' . Index (EmptyCtx ::> tp') tp -> tp :~: tp'
singletonIndexRefl i = case viewIndex knownSize i of
  IndexViewLast _ -> Refl
  IndexViewInit i' -> case viewIndex knownSize i' of { }

-- | Convenient pattern for matching on 'KindExpr' variables.
pattern SelfExpr :: () => (tp' ~ tp) => KindExpr env tp' tp
pattern SelfExpr <- VarExpr (singletonIndexRefl -> Refl)
  where SelfExpr = VarExpr baseIndex
{-# COMPLETE
    LiteralExpr
  , StructExpr
  , SelfExpr
  , FieldExpr
  , ApplyExpr
  , EqExpr
  , LteExpr
  , LtExpr
  , GteExpr
  , GtExpr
  , PlusExpr
  , MinusExpr
  , TimesExpr
  , ModExpr
  , DivExpr
  , AbsExpr
  , NegExpr
  , MemberExpr
  , NotMemberExpr
  , SubsetExpr
  , NonEmptyExpr
  , SizeExpr
  , IntersectExpr
  , UnionExpr
  , SymDiffExpr
  , DiffExpr
  , ComplementExpr
  , AndExpr
  , OrExpr
  , XorExpr
  , ImpliesExpr
  , IffExpr
  , NotExpr #-}

-- | Lift an expression about a kind @K'@ into an expression about a kind @K@ which
-- contains @K'@.
liftExpr :: Index ftps '(nm, tp)
         -> KindExpr env tp tp'
         -> KindExpr env (StructType ftps) tp'
liftExpr i = giveSelf (FieldExpr (VarExpr baseIndex) i)

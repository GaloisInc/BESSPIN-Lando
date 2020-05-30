{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lando.Core.Kind
Description : The core data type for representing feature models in Lando.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the core data type for representing a feature model in Lando.
-}
module Lando.Core.Kind
  ( Kind(..)
  , Instance(..)
    -- * Fields
  , FieldRepr(..)
  , Type(..), BoolType, EnumType, SetType, KindType
  , TypeRepr(..)
  , FieldValue(..)
    -- * Expressions
  , Expr(..)
  , Literal(..)
  , evalExpr
  ) where

import qualified Data.Kind as K

import Data.List (find)
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.SymbolRepr
import Prelude hiding ((!!))

-- | Representation of a Lando feature model, which we term a 'Kind' for
-- brevity. A kind consists of a name, a list of typed fields, and a list of
-- constraints that must hold.
data Kind (ktps :: [(Symbol, Type)]) = Kind
  { kindName :: String
  , kindFields :: List FieldRepr ktps
    -- ^ This is, effectively, the type of this kind.
  , kindConstraints :: [Expr ktps BoolType]
  }
  deriving Show

listEq :: forall k f sh . EqF (f :: k -> K.Type) => List f sh -> List f sh -> Bool
listEq Nil Nil = True
listEq (a :< as) (b :< bs) = a `eqF` b && as `listEq` bs

-- | Concrete instance of a 'Kind', but without the guarantee that this instance
-- satisfies any of the kind's constraints. That needs to be checked.
data Instance ktps = Instance
  { instanceValues :: List FieldValue ktps
  }
  deriving (Show)

instance Eq (Instance ktps) where
  i1 == i2 = instanceValues i1 `listEq` instanceValues i2

-- | A 'FieldRepr' is a key, type pair. Note that since both the key and the type
-- are tracked at the type level, this is a singleton type that lets you know
-- everything you could possibly need to know about the field at compile time.
data FieldRepr (pr :: (Symbol, Type)) where
  FieldRepr :: { fieldName :: SymbolRepr nm
               , fieldType :: TypeRepr tp } -> FieldRepr '(nm, tp)

deriving instance Show (FieldRepr pr)
instance ShowF FieldRepr

-- | The types of LOBOT.
data Type = BoolType
          | EnumType [Symbol]
          | SetType Type
          | KindType [(Symbol, Type)]

type BoolType = 'BoolType
type EnumType = 'EnumType
type SetType = 'SetType
type KindType = 'KindType

-- | Term-level representative of a type.
data TypeRepr tp where
  BoolRepr :: TypeRepr BoolType
  EnumRepr :: List SymbolRepr cs -> TypeRepr (EnumType cs)
  SetRepr  :: TypeRepr tp -> TypeRepr (SetType tp)
  KindRepr :: List FieldRepr ktps -> TypeRepr (KindType ktps)

deriving instance Show (TypeRepr tp)

-- | Field type literal.
data Literal tp where
  BoolLit     :: Bool -> Literal BoolType
  EnumLit     :: SymbolRepr c -> Index cs c -> Literal (EnumType cs)
  SetLit      :: [Literal tp] -> Literal (SetType tp)
  InstanceLit :: Instance ktps -> Literal (KindType ktps)

instance Eq (Literal tp) where
  BoolLit b1 == BoolLit b2 = b1 == b2
  EnumLit _ i1 == EnumLit _ i2 = isJust (i1 `testEquality` i2)
  SetLit ls1 == SetLit ls2 = ls1 == ls2
  InstanceLit i1 == InstanceLit i2 = i1 == i2
deriving instance Show (Literal tp)

eqLit :: Literal tp -> Literal tp -> Bool
eqLit (BoolLit b1) (BoolLit b2) = b1 == b2
eqLit (EnumLit _ i1) (EnumLit _ i2) | Just Refl <- i1 `testEquality` i2 = True
                                    | otherwise = False
eqLit (SetLit s1) (SetLit s2) = all (\e -> isJust (find (eqLit e) s2)) s1 &&
                                all (\e -> isJust (find (eqLit e) s1)) s2
eqLit (InstanceLit i1) (InstanceLit i2) = i1 == i2

-- | An instance of a particular field. This is just a field paired with a
-- concrete literal.
data FieldValue p where
  FieldValue :: { fieldValueType :: FieldRepr '(nm, tp)
                , fieldValueLit :: Literal tp
                } -> FieldValue '(nm, tp)

instance Eq (FieldValue p) where
  FieldValue _ l1 == FieldValue _ l2 = l1 == l2
instance EqF FieldValue where
  fv `eqF` fv' = fv == fv'

deriving instance Show (FieldValue p)
instance ShowF FieldValue

-- | A expression involving a particular kind.
data Expr (ktps :: [(Symbol, Type)]) (tp :: Type) where
  -- | An expression referring to an abstract instance of this kind.
  SelfExpr    :: Expr ktps (KindType ktps)
  -- | An expression referring to a field of an instance of some kind.
  FieldExpr   :: Expr ktps (KindType ktps') -> Index ktps' '(nm, tp) -> Expr ktps tp
  -- | An expression built from a literal value.
  LiteralExpr :: Literal tp -> Expr ktps tp
  -- | Equality of two expressions.
  EqExpr      :: Expr ktps tp -> Expr ktps tp -> Expr ktps BoolType
  -- | Set membership.
  MemberExpr  :: Expr ktps tp -> Expr ktps (SetType tp) -> Expr ktps BoolType
  -- | Logical implication.
  ImpliesExpr :: Expr ktps BoolType -> Expr ktps BoolType -> Expr ktps BoolType
  -- | Logical negation.
  NotExpr     :: Expr ktps BoolType -> Expr ktps BoolType

deriving instance Show (Expr ktps tp)

-- | Evaluate an expression given an instance of its kind.
evalExpr :: Instance ktps -> Expr ktps tp -> Literal tp
evalExpr inst e = case e of
  SelfExpr -> InstanceLit inst
  FieldExpr kd fld
    | InstanceLit (Instance kdLit) <- evalExpr inst kd -> fieldValueLit (kdLit !! fld)
  LiteralExpr l -> l
  EqExpr e1 e2
    | l1 <- evalExpr inst e1
    , l2 <- evalExpr inst e2 -> BoolLit (eqLit l1 l2)
  MemberExpr e1 e2
    | l1 <- evalExpr inst e1
    , SetLit s2 <- evalExpr inst e2 -> BoolLit (isJust (find (eqLit l1) s2))
  ImpliesExpr e1 e2
    | BoolLit b1 <- evalExpr inst e1
    , BoolLit b2 <- evalExpr inst e2 -> BoolLit (if b1 then b2 else True)
  NotExpr e' | BoolLit b <- evalExpr inst e' -> BoolLit (not b)

-- EXAMPLES

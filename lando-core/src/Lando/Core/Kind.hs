{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Lando.Core.Kind
Description : The core data type for representing feature models in Lando.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the core data type for representing a feature model in
Lando.
-}
module Lando.Core.Kind
  ( -- * Types
    Type(..), BoolType, IntType, EnumType, SetType, KindType
  , TypeRepr(..)
    -- * Kinds
  , Kind(..)
  , FieldRepr(..)
  , addConstraints
  , derivedKind
  , liftConstraints
    -- * Instances
  , Instance(..)
  , instanceType
  , FieldLiteral(..)
  , fieldLiteralType
  , Literal(..)
  , literalType
  , instanceOf
    -- * Expressions
  , Expr(..)
  , evalExpr
  , liftExpr
  ) where

import Data.Parameterized.List.Length

import Data.List (find)
import Data.List.NonEmpty hiding ((!!))
import Data.Parameterized.Some
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
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

-- | Augment a 'Kind' with some additional constraints.
addConstraints :: Kind ktps -> [Expr ktps BoolType] -> Kind ktps
addConstraints kd cs =
  kd { kindConstraints = kindConstraints kd ++ cs }

-- | Given a set of parent 'Kind's, create a derived 'Kind'.
derivedKind :: NonEmpty (Kind ktps) -> String -> [Expr ktps BoolType] -> Kind ktps
derivedKind kds@(kd :| _) nm cs =
  kd { kindName = nm
     , kindConstraints = concatMap kindConstraints kds ++ cs
     }

-- | Lift the constraints of a kind @K'@ into a parent kind @K@ that contains
-- @K'@. This should only be called once for a given child.
liftConstraints :: Kind ktps
                -> Kind ktps'
                -> Index ktps '(nm, KindType ktps')
                -> Kind ktps
liftConstraints k k' i = addConstraints k (liftExpr i <$> kindConstraints k')

-- | A 'FieldRepr' is a key, type pair. Note that since both the key and the type
-- are tracked at the type level, this is a singleton type that lets you know
-- everything you could possibly need to know about the field at compile time.
data FieldRepr (pr :: (Symbol, Type)) where
  FieldRepr :: { fieldName :: SymbolRepr nm
               , fieldType :: TypeRepr tp } -> FieldRepr '(nm, tp)

deriving instance Show (FieldRepr pr)
instance ShowF FieldRepr
instance (KnownSymbol nm, KnownRepr TypeRepr tp) => KnownRepr FieldRepr '(nm, tp) where
  knownRepr = FieldRepr knownRepr knownRepr

-- | The types of LOBOT.
data Type = BoolType
          | IntType
          | EnumType [Symbol]
          | SetType [Symbol]
          | KindType [(Symbol, Type)]

type BoolType = 'BoolType
type IntType = 'IntType
type EnumType = 'EnumType
type SetType = 'SetType
type KindType = 'KindType

-- | Term-level representative of a type.
data TypeRepr tp where
  BoolRepr :: TypeRepr BoolType
  IntRepr  :: TypeRepr IntType
  EnumRepr :: 1 <= (Length cs)
           => List SymbolRepr cs
           -> TypeRepr (EnumType cs)
  SetRepr  :: 1 <= (Length cs)
           => List SymbolRepr cs
           -> TypeRepr (SetType cs)
  KindRepr :: List FieldRepr ktps -> TypeRepr (KindType ktps)
deriving instance Show (TypeRepr tp)

instance KnownRepr TypeRepr BoolType
  where knownRepr = BoolRepr
instance KnownRepr TypeRepr IntType
  where knownRepr = IntRepr
instance (1 <= Length cs, KnownRepr (List SymbolRepr) cs) =>
  KnownRepr TypeRepr (EnumType cs)
  where knownRepr = EnumRepr knownRepr
instance (1 <= Length cs, KnownRepr (List SymbolRepr) cs) =>
  KnownRepr TypeRepr (SetType cs)
  where knownRepr = SetRepr knownRepr
instance KnownRepr (List FieldRepr) ktps => KnownRepr TypeRepr (KindType ktps)
  where knownRepr = KindRepr knownRepr

-- | Concrete value inhabiting a type.
data Literal tp where
  BoolLit :: Bool -> Literal BoolType
  IntLit  :: Integer -> Literal IntType
  EnumLit :: 1 <= Length cs
          => List SymbolRepr cs
          -> Index cs c
          -> Literal (EnumType cs)
  SetLit  :: 1 <= Length cs
          => List SymbolRepr cs
          -> [Some (Index cs)]
          -> Literal (SetType cs)
  KindLit :: Instance ktps -> Literal (KindType ktps)
deriving instance Show (Literal tp)

-- | Get the type of a literal.
literalType :: Literal tp -> TypeRepr tp
literalType (BoolLit _) = BoolRepr
literalType (IntLit _) = IntRepr
literalType (EnumLit cs _) = EnumRepr cs
literalType (SetLit cs _) = SetRepr cs
literalType (KindLit (Instance flds)) = KindRepr (fmapFC fieldLiteralType flds)

-- | An instance of a particular field. This is just the field name paired with
-- a concrete literal.
data FieldLiteral (p :: (Symbol, Type)) where
  FieldLiteral :: { fieldLiteralName :: SymbolRepr nm
                  , fieldLiteralValue :: Literal tp
                  } -> FieldLiteral '(nm, tp)

-- | Get the type of a 'FieldLiteral'.
fieldLiteralType :: FieldLiteral ftp -> FieldRepr ftp
fieldLiteralType FieldLiteral{..} =
  FieldRepr fieldLiteralName (literalType fieldLiteralValue)

deriving instance Show (FieldLiteral p)
instance ShowF FieldLiteral

-- | Concrete instance of a 'Kind', but without the guarantee that this instance
-- satisfies any of the kind's constraints. That needs to be checked.
data Instance ktps = Instance { instanceValues :: List FieldLiteral ktps }
  deriving (Show)

-- | Get the type of an 'Instance'.
instanceType :: Instance ktps -> List FieldRepr ktps
instanceType Instance{..} = fmapFC fieldLiteralType instanceValues

-- | A expression involving a particular kind.
data Expr (ktps :: [(Symbol, Type)]) (tp :: Type) where
  -- | An expression built from a literal value.
  LiteralExpr :: Literal tp -> Expr ktps tp
  -- | An expression referring to an abstract instance of this kind.
  SelfExpr    :: Expr ktps (KindType ktps)
  -- | An expression referring to a field of an instance of some kind.
  FieldExpr   :: Expr ktps (KindType ktps') -> Index ktps' '(nm, tp) -> Expr ktps tp
  -- | Equality of two expressions.
  EqExpr      :: Expr ktps tp -> Expr ktps tp -> Expr ktps BoolType
  -- | Less-than-or-equal for two integer expressions.
  LteExpr     :: Expr ktps IntType -> Expr ktps IntType -> Expr ktps BoolType
  -- | Set membership.
  MemberExpr  :: Expr ktps (EnumType cs)
              -> Expr ktps (SetType cs)
              -> Expr ktps BoolType
  -- | Logical implication.
  ImpliesExpr :: Expr ktps BoolType -> Expr ktps BoolType -> Expr ktps BoolType
  -- | Logical negation.
  NotExpr     :: Expr ktps BoolType -> Expr ktps BoolType

deriving instance Show (Expr ktps tp)

litEq :: Literal tp -> Literal tp -> Bool
litEq (BoolLit b1) (BoolLit b2) = b1 == b2
litEq (IntLit x1) (IntLit x2) = x1 == x2
litEq (EnumLit _ i1) (EnumLit _ i2) | Just Refl <- i1 `testEquality` i2 = True
                                    | otherwise = False
litEq (SetLit _ s1) (SetLit _ s2) = all (\i -> isJust (find (==i) s2)) s1 &&
                                    all (\i -> isJust (find (==i) s1)) s2
litEq (KindLit i1) (KindLit i2) = i1 `instanceEq` i2

fieldValueEq :: FieldLiteral ftp -> FieldLiteral ftp -> Bool
fieldValueEq fv1@(FieldLiteral _ _) fv2 =
  litEq (fieldLiteralValue fv1) (fieldLiteralValue fv2)

instanceEq :: forall (ktps :: [(Symbol, Type)]) . Instance ktps -> Instance ktps -> Bool
instanceEq i1 i2 = listEq (instanceValues i1) (instanceValues i2)
  where listEq :: forall (sh :: [(Symbol, Type)]) .
                  List FieldLiteral sh -> List FieldLiteral sh -> Bool
        listEq Nil Nil = True
        listEq (a :< as) (b :< bs) | FieldLiteral _ _ <- a
          = a `fieldValueEq` b && listEq as bs

-- | Determine whether an instance satisfies all the constraints of a kind.
instanceOf :: Instance ktps -> Kind ktps -> Bool
instanceOf inst (Kind{..}) = all constraintHolds kindConstraints
  where constraintHolds e | BoolLit True <- evalExpr inst e = True
                          | otherwise = False

-- | Evaluate an expression given an instance of its kind.
evalExpr :: Instance ktps -> Expr ktps tp -> Literal tp
evalExpr inst e = case e of
  SelfExpr -> KindLit inst
  FieldExpr kd i
    | KindLit (Instance fvs) <- evalExpr inst kd -> fieldLiteralValue (fvs !! i)
  LiteralExpr l -> l
  EqExpr e1 e2
    | l1 <- evalExpr inst e1
    , l2 <- evalExpr inst e2 -> BoolLit (litEq l1 l2)
  LteExpr e1 e2
    | IntLit x1 <- evalExpr inst e1
    , IntLit x2 <- evalExpr inst e2 -> BoolLit (x1 <= x2)
  MemberExpr e1 e2
    | EnumLit _ i <- evalExpr inst e1
    , SetLit _ s <- evalExpr inst e2 -> BoolLit (isJust (find (== Some i) s))
  ImpliesExpr e1 e2
    | BoolLit b1 <- evalExpr inst e1
    , BoolLit b2 <- evalExpr inst e2 -> BoolLit (not b1 || b2)
  NotExpr e' | BoolLit b <- evalExpr inst e' -> BoolLit (not b)

-- | Lift an expression about a kind @K'@ into an expression about a kind @K@ which
-- contains @K'@.
liftExpr :: Index ktps '(nm, KindType ktps')
         -> Expr ktps' tp
         -> Expr ktps tp
liftExpr i e = case e of
  LiteralExpr l -> LiteralExpr l
  SelfExpr -> FieldExpr SelfExpr i
  FieldExpr kd i' -> FieldExpr (liftExpr i kd) i'
  EqExpr e1 e2 -> EqExpr (liftExpr i e1) (liftExpr i e2)
  LteExpr e1 e2 -> LteExpr (liftExpr i e1) (liftExpr i e2)
  MemberExpr e1 e2 -> MemberExpr (liftExpr i e1) (liftExpr i e2)
  ImpliesExpr e1 e2 -> ImpliesExpr (liftExpr i e1) (liftExpr i e2)
  NotExpr e' -> NotExpr (liftExpr i e')

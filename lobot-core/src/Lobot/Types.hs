{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Lobot.Types
Description : The types of the Lobot language.
Copyright   : (c) Ben Selfridge, Matthew Yacavone, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the types of data and functions in Lobot, as well as some
type families and functions for ensuring types are non-abstract.
-}

module Lobot.Types
  ( -- * Types
    Type(..), BoolType, IntType, EnumType, SetType, StructType, AbsType
  , TypeRepr(..)
  , FieldRepr(..)
    -- * Functions
  , FunctionType(..), FunType
  , FunctionTypeRepr(..)
    -- * Abstract type utilities
  , HasAbstractTypes(..)
  , DecideNonAbstract(..)
  , nonAbstractIndex
  ) where

import Data.Parameterized.BoolRepr
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TH.GADT
import Data.Constraint (Dict(..))
import GHC.TypeLits
import GHC.Exts (Constraint)

-- | The types of LOBOT.
data Type = BoolType
          | IntType
          | EnumType (Ctx Symbol)
          | SetType (Ctx Symbol)
          | StructType (Ctx (Symbol, Type))
          | AbsType Symbol

type BoolType = 'BoolType
type IntType = 'IntType
type EnumType = 'EnumType
type SetType = 'SetType
type StructType = 'StructType
type AbsType = 'AbsType

-- | Term-level representative of a type.
data TypeRepr tp where
  BoolRepr   :: TypeRepr BoolType
  IntRepr    :: TypeRepr IntType
  EnumRepr   :: 1 <= CtxSize cs
             => Assignment SymbolRepr cs
             -> TypeRepr (EnumType cs)
  SetRepr    :: 1 <= CtxSize cs
             => Assignment SymbolRepr cs
             -> TypeRepr (SetType cs)
  StructRepr :: Assignment FieldRepr ftps
             -> TypeRepr (StructType ftps)
  AbsRepr    :: SymbolRepr s -> TypeRepr (AbsType s)
deriving instance Show (TypeRepr tp)
instance ShowF TypeRepr

instance TestEquality TypeRepr where
  testEquality BoolRepr BoolRepr = Just Refl
  testEquality IntRepr IntRepr = Just Refl
  testEquality (EnumRepr cs) (EnumRepr cs') = case testEquality cs cs' of
    Just Refl -> Just Refl
    Nothing -> Nothing
  testEquality (SetRepr cs) (SetRepr cs') = case testEquality cs cs' of
    Just Refl -> Just Refl
    Nothing -> Nothing
  testEquality (StructRepr flds) (StructRepr flds') = case testEquality flds flds' of
    Just Refl -> Just Refl
    Nothing -> Nothing
  testEquality (AbsRepr s) (AbsRepr s') = case testEquality s s' of
    Just Refl -> Just Refl
    Nothing -> Nothing
  testEquality _ _ = Nothing

instance KnownRepr TypeRepr BoolType
  where knownRepr = BoolRepr
instance KnownRepr TypeRepr IntType
  where knownRepr = IntRepr
instance (1 <= CtxSize cs, KnownRepr (Assignment SymbolRepr) cs) => KnownRepr TypeRepr (EnumType cs)
  where knownRepr = EnumRepr knownRepr
instance (1 <= CtxSize cs, KnownRepr (Assignment SymbolRepr) cs) => KnownRepr TypeRepr (SetType cs)
  where knownRepr = SetRepr knownRepr
instance KnownRepr (Assignment FieldRepr) ftps => KnownRepr TypeRepr (StructType ftps)
  where knownRepr = StructRepr knownRepr
instance KnownRepr SymbolRepr s => KnownRepr TypeRepr (AbsType s)
  where knownRepr = AbsRepr knownRepr

-- | A 'FieldRepr' is a key, type pair. A struct type is defined by a list of
-- 'FieldRepr's.
data FieldRepr (pr :: (Symbol, Type)) where
  FieldRepr :: { fieldName :: SymbolRepr nm
               , fieldType :: TypeRepr tp } -> FieldRepr '(nm, tp)

instance TestEquality FieldRepr where
  testEquality (FieldRepr nm tp) (FieldRepr nm' tp') =
    case (testEquality nm nm', testEquality tp tp') of
      (Just Refl, Just Refl) -> Just Refl
      _ -> Nothing

deriving instance Show (FieldRepr pr)
instance ShowF FieldRepr
instance (KnownSymbol nm, KnownRepr TypeRepr tp) => KnownRepr FieldRepr '(nm, tp) where
  knownRepr = FieldRepr knownRepr knownRepr

-- | Types for functions in Lobot.
data FunctionType = FunType Symbol (Ctx Type) Type

type FunType = 'FunType

-- | Term-level representative of a 'FunctionType'.
data FunctionTypeRepr fntp where
  FunctionTypeRepr :: { functionName :: SymbolRepr nm
                      , functionArgTypes :: Assignment TypeRepr args
                      , functionRetType :: TypeRepr ret
                      } -> FunctionTypeRepr (FunType nm args ret)

deriving instance Show (FunctionTypeRepr fntp)
instance ShowF FunctionTypeRepr
instance TestEquality FunctionTypeRepr where
  testEquality (FunctionTypeRepr nm args ret) (FunctionTypeRepr nm' args' ret')=
    case (testEquality nm nm', testEquality args args', testEquality ret ret') of
      (Just Refl, Just Refl, Just Refl) -> Just Refl
      _ -> Nothing
instance ( KnownSymbol nm
         , KnownRepr (Assignment TypeRepr) args
         , KnownRepr TypeRepr ret
         ) => KnownRepr FunctionTypeRepr (FunType nm args ret) where
  knownRepr = FunctionTypeRepr knownRepr knownRepr knownRepr


class HasAbstractTypes k where
  -- | A type family which indicates which @t :: k@ are abstract. Note that
  -- since associated type families are not closed, if @t :: k@ should be
  -- considered abstract then @NonAbstract t@ should be an absurd constraint,
  -- e.g. @'True ~ 'False@.
  type NonAbstract (t :: k) :: Constraint

instance HasAbstractTypes Type where
  type NonAbstract BoolType = ()
  type NonAbstract IntType = ()
  type NonAbstract (EnumType cs) = ()
  type NonAbstract (SetType cs) = ()
  type NonAbstract (StructType ftps) = NonAbstract ftps
  type NonAbstract (AbsType _) = 'True ~ 'False

instance HasAbstractTypes (Symbol, Type) where
  type NonAbstract '(_, tp) = NonAbstract tp

instance HasAbstractTypes k => HasAbstractTypes (Ctx k) where
  type NonAbstract EmptyCtx = ()
  type NonAbstract (ctx ::> t) = (NonAbstract ctx, NonAbstract t)

class HasAbstractTypes k => DecideNonAbstract (r :: k -> *) where
  -- | A function which, if it can, provides a proof that a @t :: k@ is
  -- abstract, given a term-level representation @r t@.
  isNonAbstract :: r t -> Maybe (Dict (NonAbstract t))

instance DecideNonAbstract TypeRepr where
  isNonAbstract BoolRepr = Just Dict
  isNonAbstract IntRepr = Just Dict
  isNonAbstract (EnumRepr _) = Just Dict
  isNonAbstract (SetRepr _) = Just Dict
  isNonAbstract (StructRepr flds) = do Dict <- isNonAbstract flds
                                       Just Dict
  isNonAbstract (AbsRepr _) = Nothing

instance DecideNonAbstract FieldRepr where
  isNonAbstract (FieldRepr _ tp) = isNonAbstract tp

instance DecideNonAbstract r => DecideNonAbstract (Assignment r) where
  isNonAbstract Empty = Just Dict
  isNonAbstract (ctx :> x) = do Dict <- isNonAbstract ctx
                                Dict <- isNonAbstract x
                                Just Dict

newtype NonAbstractDict t = PfNonAbs { pfNonAbs :: Dict (NonAbstract t) }

nonAbstractCtx :: forall k (r :: k -> *) (ctx :: Ctx k).
                  (HasAbstractTypes k, NonAbstract ctx)
               => Assignment r ctx
               -> Assignment NonAbstractDict ctx
nonAbstractCtx Empty = Empty
nonAbstractCtx (ctx :> _) = nonAbstractCtx ctx :> PfNonAbs Dict

-- | Given an index into a context of non-abstract things, provide a proof
-- that the thing at the given index is indeed non-abstract.
nonAbstractIndex :: forall k (r :: k -> *) (ctx :: Ctx k) (t :: k).
                    (HasAbstractTypes k, NonAbstract ctx)
                 => Assignment r ctx
                 -> Index ctx t
                 -> Dict (NonAbstract t)
nonAbstractIndex rs i = pfNonAbs $ (nonAbstractCtx rs) ! i


-- HashableF instances for the types in this file

$(return [])

instance HashableF TypeRepr where
  hashWithSaltF = $(structuralHashWithSalt [t|TypeRepr|]
    [ (TypeApp (ConType [t|FieldRepr|]) AnyType, [|hashWithSaltF|])
    , (TypeApp (TypeApp (ConType [t|Assignment|]) AnyType) AnyType, [|hashWithSaltF|])
    , (TypeApp (ConType [t|SymbolRepr|]) AnyType, [|hashWithSaltF|])
    ])

instance HashableF FieldRepr where
  hashWithSaltF = $(structuralHashWithSalt [t|FieldRepr|]
    [ (TypeApp (ConType [t|TypeRepr|]) AnyType, [|hashWithSaltF|])
    , (TypeApp (TypeApp (ConType [t|Assignment|]) AnyType) AnyType, [|hashWithSaltF|])
    , (TypeApp (ConType [t|SymbolRepr|]) AnyType, [|hashWithSaltF|])
    ])

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Lobot.Types
Description : The types of the Lobot language.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the types of data and functions in Lobot.
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
  , IsAbstract
  , AnyFieldsAbstract
  , isAbstract
  , lastNotAbstract
  ) where

import Data.Parameterized.BoolRepr
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.SymbolRepr
import GHC.TypeLits

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

type family IsAbstract (tp :: Type) :: Bool where
  IsAbstract (AbsType _) = 'True
  IsAbstract (StructType ftps) = AnyFieldsAbstract ftps
  IsAbstract _ = 'False

type family AnyFieldsAbstract (tps :: Ctx (Symbol, Type)) :: Bool where
  AnyFieldsAbstract EmptyCtx = 'False
  AnyFieldsAbstract (tps ::> '(nm, tp)) = IsAbstract tp || AnyFieldsAbstract tps

-- | Determine if a type is abstract.
isAbstract :: TypeRepr tp -> BoolRepr (IsAbstract tp)
isAbstract (AbsRepr _) = TrueRepr
isAbstract (StructRepr ftps) = anyFieldsAbstract ftps
isAbstract BoolRepr = FalseRepr
isAbstract IntRepr = FalseRepr
isAbstract (EnumRepr _) = FalseRepr
isAbstract (SetRepr _) = FalseRepr

anyFieldsAbstract :: Assignment FieldRepr ftps -> BoolRepr (AnyFieldsAbstract ftps)
anyFieldsAbstract Empty = FalseRepr
anyFieldsAbstract (ftps :> FieldRepr _ tp) = isAbstract tp %|| anyFieldsAbstract ftps

lastNotAbstract :: AnyFieldsAbstract (ftps ::> '(nm, tp)) ~ 'False
                           => Assignment FieldRepr (ftps ::> '(nm, tp))
                           -> IsAbstract tp :~: 'False
lastNotAbstract (_ :> FieldRepr _ tp) = case isAbstract tp of
  FalseRepr -> Refl

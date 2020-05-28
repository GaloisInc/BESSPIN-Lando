{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
    -- * Fields
  , Field(..)
  , FieldType(..)
  , FieldTypeRepr(..)
    -- * Expressions
  , Expr(..)
  , Literal(..)
    -- * Examples
  , riscv
  ) where

import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr

-- | Representation of a Lando feature model, which we term a 'Kind' for
-- brevity.
data Kind tps = Kind { kindName :: String
                     , kindFields :: List Field tps
                       -- ^ This is, effectively, the type of this kind.
                     , kindConstraints :: [Expr tps BoolType]
                     }
  deriving Show

-- | A 'Field' is a key, type pair. Note that since both the key and the type
-- are tracked at the type level, this is a singleton type that lets you know
-- everything you could possibly need to know about the field at compile time.
data Field (pr :: (Symbol, FieldType)) where
  Field :: { fieldName :: SymbolRepr nm
           , fieldType :: FieldTypeRepr tp } -> Field '(nm, tp)

deriving instance Show (Field pr)

instance ShowF Field

-- | Field type, a data kind used to track the value a field can take.
data FieldType = BoolType
               | EnumType [Symbol]
               | SetType FieldType

-- | Term-level representative of a field type.
data FieldTypeRepr tp where
  BoolRepr :: FieldTypeRepr BoolType
  EnumRepr :: List SymbolRepr cs -> FieldTypeRepr (EnumType cs)
  SetRepr  :: FieldTypeRepr tp -> FieldTypeRepr (SetType tp)

deriving instance Show (FieldTypeRepr tp)

-- | Field type literal.
data Literal tp where
  BoolLit :: Bool -> Literal BoolType
  EnumLit :: SymbolRepr c -> Index cs c -> Literal (EnumType cs)
  SetLit :: [Literal tp] -> Literal (SetType tp)

deriving instance Show (Literal tp)

-- | An expression built up from literals and fields of a particular kind.
data Expr (ktps :: [(Symbol, FieldType)]) tp where
  FieldExpr   :: SymbolRepr nm -> Index ktps '(nm, tp) -> Expr ktps tp
  LiteralExpr :: Literal tp -> Expr ktps tp
  EqExpr      :: Expr ktps tp -> Expr ktps tp -> Expr ktps BoolType
  MemberExpr  :: Expr ktps tp -> Expr ktps (SetType tp) -> Expr ktps BoolType
  ImpliesExpr :: Expr ktps BoolType -> Expr ktps BoolType -> Expr ktps BoolType

deriving instance Show (Expr ktps tp)

constrainKind :: [Expr ktps BoolType] -> Kind ktps -> Kind ktps
constrainKind constraints k = k { kindConstraints = kindConstraints k ++ constraints}

-- | Feature model for RISC-V ISA (example).
riscv = Kind { kindName = "riscv"
             , kindFields = flds
             , kindConstraints = [ has_d_implies_has_f ]
             }
  where flds = reg_width :< exts :< Nil
        reg_width = Field { fieldName = knownSymbol @"reg_width"
                          , fieldType = reg_width_type
                          }
        reg_width_list = knownSymbol @"RV32" :< knownSymbol @"RV64" :< Nil
        reg_width_type = EnumRepr reg_width_list
        exts = Field { fieldName = knownSymbol @"exts"
                     , fieldType = exts_type
                     }
        exts_list = knownSymbol @"M" :<
                    knownSymbol @"A" :<
                    knownSymbol @"F" :<
                    knownSymbol @"D" :<
                    knownSymbol @"C" :< Nil
        exts_type = SetRepr $ EnumRepr exts_list

        d = EnumLit (knownSymbol @"D") index3
        f = EnumLit (knownSymbol @"F") index2

        has_d = MemberExpr (LiteralExpr d) (FieldExpr (knownSymbol @"exts") index1)
        has_f = MemberExpr (LiteralExpr f) (FieldExpr (knownSymbol @"exts") index1)
        has_d_implies_has_f = ImpliesExpr has_d has_f

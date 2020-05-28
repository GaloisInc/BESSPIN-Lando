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
  , Instance(..)
  , constrainKind
  , validateInstance
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

import Data.List (find)
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.SymbolRepr

import Prelude hiding ((!!))

-- | Representation of a Lando feature model, which we term a 'Kind' for
-- brevity.
data Kind tps = Kind { kindName :: String
                     , kindFields :: List Field tps
                       -- ^ This is, effectively, the type of this kind.
                     , kindConstraints :: [Expr tps BoolType]
                     }
  deriving Show

-- | Concrete instance of a 'Kind'.
data Instance tps = Instance { instanceKind :: Kind tps
                             , instanceValues :: List FieldValue tps
                             }
  deriving Show

-- | Create an instance of a kind, checking that all the constraints hold.
validateInstance :: Kind tps
                 -> List FieldValue tps
                 -> Either (Expr tps BoolType) (Instance tps)
validateInstance kd vals = case find constraintFails (kindConstraints kd) of
  Just failingConstraint -> Left failingConstraint
  Nothing -> Right $ Instance kd vals
  where constraintFails e | BoolLit False <- evalExpr vals e = True
                          | otherwise = False

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

type BoolType = 'BoolType
type EnumType = 'EnumType
type SetType = 'SetType

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

eqLit :: Literal tp -> Literal tp -> Bool
eqLit (BoolLit b1) (BoolLit b2) = b1 == b2
eqLit (EnumLit _ i1) (EnumLit _ i2) | Just Refl <- i1 `testEquality` i2 = True
                                    | otherwise = False
eqLit (SetLit s1) (SetLit s2) = all (\e -> isJust (find (eqLit e) s2)) s1 &&
                                all (\e -> isJust (find (eqLit e) s1)) s2

-- | Field paired with a literal value of the same type.
data FieldValue p where
  FieldValue :: Field '(nm, tp) -> Literal tp -> FieldValue '(nm, tp)

deriving instance Show (FieldValue p)
instance ShowF FieldValue

-- | An expression built up from literals and fields of a particular kind.
data Expr (ktps :: [(Symbol, FieldType)]) tp where
  FieldExpr   :: SymbolRepr nm -> Index ktps '(nm, tp) -> Expr ktps tp
  LiteralExpr :: Literal tp -> Expr ktps tp
  EqExpr      :: Expr ktps tp -> Expr ktps tp -> Expr ktps BoolType
  MemberExpr  :: Expr ktps tp -> Expr ktps (SetType tp) -> Expr ktps BoolType
  ImpliesExpr :: Expr ktps BoolType -> Expr ktps BoolType -> Expr ktps BoolType

deriving instance Show (Expr ktps tp)

-- | Evaluate an expression given an assignment of fields to values.
evalExpr :: List FieldValue ktps -> Expr ktps tp -> Literal tp
evalExpr vs (FieldExpr _ ix) = let FieldValue _ l = vs !! ix
                               in l
evalExpr _ (LiteralExpr l) = l
evalExpr vs (EqExpr e1 e2) = let l1 = evalExpr vs e1
                                 l2 = evalExpr vs e2
                             in BoolLit (eqLit l1 l2)
evalExpr vs (MemberExpr e1 e2) = let l1 = evalExpr vs e1
                                     SetLit s2 = evalExpr vs e2
                                 in BoolLit (isJust (find (eqLit l1) s2))
evalExpr vs (ImpliesExpr e1 e2) = let BoolLit b1 = evalExpr vs e1
                                      BoolLit b2 = evalExpr vs e2
                                  in BoolLit (if b1 then b2 else True)

-- | Add constraints to an existing 'Kind'.
constrainKind :: [Expr ktps BoolType] -> Kind ktps -> Kind ktps
constrainKind constraints k = k { kindConstraints = kindConstraints k ++ constraints}






--------------
-- EXAMPLES --
--------------

reg_width = Field { fieldName = knownSymbol @"reg_width"
                  , fieldType = reg_width_type
                  }
reg_width_list = knownSymbol @"RV32" :< knownSymbol @"RV64" :< Nil
reg_width_type = EnumRepr reg_width_list

rv32_val = FieldValue reg_width (EnumLit (knownSymbol @"RV32") index0)
rv64_val = FieldValue reg_width (EnumLit (knownSymbol @"RV64") index1)

exts = Field { fieldName = knownSymbol @"exts"
             , fieldType = exts_type
             }
exts_list = knownSymbol @"M" :<
            knownSymbol @"A" :<
            knownSymbol @"F" :<
            knownSymbol @"D" :<
            knownSymbol @"C" :< Nil
exts_type = SetRepr $ EnumRepr exts_list

m_lit = EnumLit (knownSymbol @"M") index0
a_lit = EnumLit (knownSymbol @"A") index1
f_lit = EnumLit (knownSymbol @"F") index2
d_lit = EnumLit (knownSymbol @"D") index3
c_lit = EnumLit (knownSymbol @"C") (IndexThere index3)

-- | Feature model for RISC-V ISA (example).
riscv = Kind { kindName = "riscv"
             , kindFields = flds
             , kindConstraints = [ has_d_implies_has_f ]
             }
  where flds = reg_width :< exts :< Nil

        d = EnumLit (knownSymbol @"D") index3
        f = EnumLit (knownSymbol @"F") index2

        has_d = MemberExpr (LiteralExpr d) (FieldExpr (knownSymbol @"exts") index1)
        has_f = MemberExpr (LiteralExpr f) (FieldExpr (knownSymbol @"exts") index1)
        has_d_implies_has_f = ImpliesExpr has_d has_f

-- Returns a Right
my_riscv = validateInstance riscv (rv32_val :< ma :< Nil)
  where ma = FieldValue exts (SetLit [m_lit, a_lit])

-- Returns a Left
my_bad_riscv = validateInstance riscv (rv64_val :< md :< Nil)
  where md = FieldValue exts (SetLit [m_lit, d_lit])

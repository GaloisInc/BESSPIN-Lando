{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Lobot.Expr
Description : Expression data type
Copyright   : (c) Matthew Yacavone, Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This modules defines the core expression data type we use for constraints.
-}
module Lobot.Expr
  ( -- * Expressions
    Expr(..)
  , structExpr
  , FieldInst(..)
  , fieldInstFieldType
  , FunctionCall(..)
  , getCalls
  , getFieldCalls
  , subst
    -- * Literals
  , Literal(..)
  , literalType
  , litEq
  ) where

import Lobot.Types

import qualified Data.HashSet as HS

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Data.Parameterized.TH.GADT
import GHC.TypeLits
import Prelude hiding (lookup)

-- | A expression involving a particular variable context, given a particular
-- function environment.
data Expr (env :: Ctx FunctionType) (ctx :: Ctx Type) (tp :: Type) where
  -- | An expression built from a literal value.
  LiteralExpr :: NonAbstract tp => Literal tp -> Expr env ctx tp
  -- | Constructing an expression of struct type. Note that this overlaps with
  -- the @'LiteralExpr' ('StructLit' _)@ case, so use 'structExpr' instead of
  -- this constructor in most cases.
  StructExpr  :: Assignment (FieldInst (Expr env ctx)) ftps
              -> Expr env ctx (StructType ftps)
  -- | An expression referring to a particular value in the current context.
  VarExpr     :: Index ctx tp -> Expr env ctx tp
  -- | An expression referring to a field of an instance of some kind.
  FieldExpr   :: Expr env ctx (StructType ftps) -> Index ftps '(nm, tp) -> Expr env ctx tp
  -- | Function application.
  ApplyExpr   :: Index env (FunType nm args ret)
              -> Assignment (Expr env ctx) args
              -> Expr env ctx ret
  -- | Equality of two expressions.
  EqExpr      :: NonAbstract tp
              => Expr env ctx tp
              -> Expr env ctx tp
              -> Expr env ctx BoolType
  -- | Inequality of two expressions.
  NeqExpr     :: NonAbstract tp
              => Expr env ctx tp
              -> Expr env ctx tp
              -> Expr env ctx BoolType
  -- | Less-than-or-equal for two integer expressions.
  LteExpr     :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx BoolType
  -- | Less-than for two integer expressions.
  LtExpr      :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx BoolType
  -- | Less-than-or-equal for two integer expressions.
  GteExpr     :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx BoolType
  -- | Less-than-or-equal for two integer expressions.
  GtExpr      :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx BoolType
  -- | Add two integer expressions.
  PlusExpr    :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx IntType
  -- | Subtract two integer expressions.
  MinusExpr   :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx IntType
  -- | Multiply two integer expressions.
  TimesExpr   :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx IntType
  -- | Divide two integer expressions and get the remainder.
  ModExpr     :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx IntType
  -- | Divide two integer expressions.
  DivExpr     :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx IntType
  -- | Take the absolute value of an integer.
  AbsExpr     :: Expr env ctx IntType -> Expr env ctx IntType
  -- | Negate an integer.
  NegExpr     :: Expr env ctx IntType -> Expr env ctx IntType
  -- | Set membership.
  MemberExpr  :: Expr env ctx (EnumType cs)
              -> Expr env ctx (SetType cs)
              -> Expr env ctx BoolType
  -- | Set non-membership.
  NotMemberExpr  :: Expr env ctx (EnumType cs)
                 -> Expr env ctx (SetType cs)
                 -> Expr env ctx BoolType
  -- | Subset relation.
  SubsetExpr  :: Expr env ctx (SetType cs)
              -> Expr env ctx (SetType cs)
              -> Expr env ctx BoolType
  -- | Test whether a set is nonempty.
  NonEmptyExpr :: Expr env ctx (SetType cs) -> Expr env ctx BoolType
  -- | Get the cardinality (size) of a set.
  SizeExpr     :: Expr env ctx (SetType cs) -> Expr env ctx IntType
  -- | Set intersection.
  IntersectExpr  :: Expr env ctx (SetType cs) -> Expr env ctx (SetType cs) -> Expr env ctx (SetType cs)
  -- | Set union.
  UnionExpr      :: Expr env ctx (SetType cs) -> Expr env ctx (SetType cs) -> Expr env ctx (SetType cs)
  -- | Set symmetric difference.
  SymDiffExpr    :: Expr env ctx (SetType cs) -> Expr env ctx (SetType cs) -> Expr env ctx (SetType cs)
  -- | Set difference.
  DiffExpr       :: Expr env ctx (SetType cs) -> Expr env ctx (SetType cs) -> Expr env ctx (SetType cs)
  -- | Set complement.
  ComplementExpr :: Expr env ctx (SetType cs) -> Expr env ctx (SetType cs)
  -- | Logical and.
  AndExpr     :: Expr env ctx BoolType -> Expr env ctx BoolType -> Expr env ctx BoolType
  -- | Logical or.
  OrExpr      :: Expr env ctx BoolType -> Expr env ctx BoolType -> Expr env ctx BoolType
  -- | Logical xor.
  XorExpr     :: Expr env ctx BoolType -> Expr env ctx BoolType -> Expr env ctx BoolType
  -- | Logical implication.
  ImpliesExpr :: Expr env ctx BoolType -> Expr env ctx BoolType -> Expr env ctx BoolType
  -- | Logical bi-implication.
  IffExpr :: Expr env ctx BoolType -> Expr env ctx BoolType -> Expr env ctx BoolType
  -- | Logical negation.
  NotExpr     :: Expr env ctx BoolType -> Expr env ctx BoolType

deriving instance Show (Expr env ctx tp)
instance ShowF (Expr env ctx)

-- | Like 'StructExpr', but if all the given expressions are 'LiteralExpr's,
-- return a 'LiteralExpr' of a 'StructLit'.
structExpr :: Assignment (FieldInst (Expr env ctx)) ftps
           -> Expr env ctx (StructType ftps)
structExpr fvs = case go fvs of
  Just (fvs', IsNonAbs) -> LiteralExpr (StructLit fvs')
  Nothing           -> StructExpr fvs
  where go :: Assignment (FieldInst (Expr env ctx)) ftps 
           -> Maybe (Assignment (FieldInst Literal) ftps, IsNonAbstract ftps)
        go Empty = Just (Empty, IsNonAbs)
        go (fvs' :> FieldInst nm tp (LiteralExpr l)) = do
          (fvs'', IsNonAbs) <- go fvs'
          Just (fvs'' :> FieldInst nm tp l, IsNonAbs)
        go _ = Nothing

-- | An instance of a particular field, along with term-level representives of
-- the field's name and type. Mostly used as  @'FieldInst' 'Literal' tp@ or
-- @'FieldInst' ('Expr' env ctx) tp@.
data FieldInst (f :: Type -> *) (p :: (Symbol, Type)) where
  FieldInst :: { fieldInstName :: SymbolRepr nm
               , fieldInstType :: TypeRepr tp
               , fieldInstValue :: f tp
               } -> FieldInst f '(nm, tp)

deriving instance Show (FieldInst (Expr env ctx) p)
deriving instance Show (FieldInst Literal p)
instance ShowF (FieldInst (Expr env ctx))
instance ShowF (FieldInst Literal)

instance FunctorFC FieldInst where
  fmapFC f (FieldInst nm tp x) = FieldInst nm tp $ f x

-- | Get the field type of a field instance.
fieldInstFieldType :: FieldInst f p -> FieldRepr p
fieldInstFieldType (FieldInst nm tp _) = FieldRepr nm tp

-- | The signature of a function call.
data FunctionCall env ctx ret where
  FunctionCall :: { funCallIdx  :: Index env (FunType nm args ret)
                  , funCallArgs :: Assignment (Expr env ctx) args
                  } -> FunctionCall env ctx ret

deriving instance Show (FunctionCall env ctx ret)
instance ShowF (FunctionCall env ctx)

-- | Return the set of all function calls which occur within the given
-- expression.
getCalls :: Expr env ctx tp -> HS.HashSet (Some (FunctionCall env ctx))
getCalls e = case e of
  LiteralExpr _ -> HS.empty
  StructExpr fls -> HS.unions (toListFC getFieldCalls fls)
  VarExpr _ -> HS.empty
  FieldExpr e' _ -> getCalls e'
  ApplyExpr fi es -> HS.insert (Some (FunctionCall fi es)) $
                      HS.unions (toListFC getCalls es)
  EqExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  NeqExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  LteExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  LtExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  GteExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  GtExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  PlusExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  MinusExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  TimesExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  DivExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  ModExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  AbsExpr e' -> getCalls e'
  NegExpr e' -> getCalls e'
  MemberExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  NotMemberExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  SubsetExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  NonEmptyExpr e' -> getCalls e'
  SizeExpr e' -> getCalls e'
  IntersectExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  UnionExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  SymDiffExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  DiffExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  ComplementExpr e' -> getCalls e'
  AndExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  OrExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  XorExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  ImpliesExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  IffExpr e1 e2 -> getCalls e1 `HS.union` getCalls e2
  NotExpr e' -> getCalls e'

-- | Return the set of all function calls which occur within the given
-- field expression.
getFieldCalls :: FieldInst (Expr env ctx) tp
              -> HS.HashSet (Some (FunctionCall env ctx))
getFieldCalls (FieldInst _ _ e) = getCalls e

-- | Substitute expressions for all variables in a context
subst :: Assignment (Expr env ctx') ctx -> Expr env ctx tp -> Expr env ctx' tp
subst s e = case e of
  LiteralExpr l -> LiteralExpr l
  StructExpr fvs -> structExpr (fmapFC (fmapFC (subst s)) fvs)
  VarExpr i -> s ! i
  FieldExpr kd i -> FieldExpr (subst s kd) i
  ApplyExpr fi es -> ApplyExpr fi (fmapFC (subst s) es)
  EqExpr e1 e2 -> EqExpr (subst s e1) (subst s e2)
  NeqExpr e1 e2 -> NeqExpr (subst s e1) (subst s e2)
  LteExpr e1 e2 -> LteExpr (subst s e1) (subst s e2)
  LtExpr e1 e2 -> LtExpr (subst s e1) (subst s e2)
  GteExpr e1 e2 -> GteExpr (subst s e1) (subst s e2)
  GtExpr e1 e2 -> GtExpr (subst s e1) (subst s e2)
  PlusExpr e1 e2 -> PlusExpr (subst s e1) (subst s e2)
  MinusExpr e1 e2 -> MinusExpr (subst s e1) (subst s e2)
  TimesExpr e1 e2 -> TimesExpr (subst s e1) (subst s e2)
  ModExpr e1 e2 -> ModExpr (subst s e1) (subst s e2)
  DivExpr e1 e2 -> DivExpr (subst s e1) (subst s e2)
  AbsExpr e' -> AbsExpr (subst s e')
  NegExpr e' -> NegExpr (subst s e')
  MemberExpr e1 e2 -> MemberExpr (subst s e1) (subst s e2)
  NotMemberExpr e1 e2 -> NotMemberExpr (subst s e1) (subst s e2)
  SubsetExpr e1 e2 -> SubsetExpr (subst s e1) (subst s e2)
  NonEmptyExpr e' -> NonEmptyExpr (subst s e')
  SizeExpr e' -> SizeExpr (subst s e')
  IntersectExpr e1 e2 -> IntersectExpr (subst s e1) (subst s e2)
  UnionExpr e1 e2 -> UnionExpr (subst s e1) (subst s e2)
  SymDiffExpr e1 e2 -> SymDiffExpr (subst s e1) (subst s e2)
  DiffExpr e1 e2 -> DiffExpr (subst s e1) (subst s e2)
  ComplementExpr e' -> ComplementExpr (subst s e')
  AndExpr e1 e2 -> AndExpr (subst s e1) (subst s e2)
  OrExpr e1 e2 -> OrExpr (subst s e1) (subst s e2)
  XorExpr e1 e2 -> XorExpr (subst s e1) (subst s e2)
  ImpliesExpr e1 e2 -> ImpliesExpr (subst s e1) (subst s e2)
  IffExpr e1 e2 -> IffExpr (subst s e1) (subst s e2)
  NotExpr e' -> NotExpr (subst s e')

-- | Concrete value inhabiting a type.
data Literal tp where
  BoolLit   :: Bool -> Literal BoolType
  IntLit    :: Integer -> Literal IntType
  EnumLit   :: 1 <= CtxSize cs
            => Assignment SymbolRepr cs -> Index cs c -> Literal (EnumType cs)
  SetLit    :: 1 <= CtxSize cs
            => Assignment SymbolRepr cs -> [Some (Index cs)] -> Literal (SetType cs)
  StructLit :: Assignment (FieldInst Literal) ftps -> Literal (StructType ftps)
  AbsLit    :: SymbolRepr s -> ByteString -> Literal (AbsType s)

deriving instance Show (Literal tp)
instance ShowF Literal

-- | Get the type of a literal.
literalType :: Literal tp -> TypeRepr tp
literalType (BoolLit _) = BoolRepr
literalType (IntLit _) = IntRepr
literalType (EnumLit cs _) = EnumRepr cs
literalType (SetLit cs _) = SetRepr cs
literalType (StructLit fls) = StructRepr (fmapFC fieldInstFieldType fls)
literalType (AbsLit s _) = AbsRepr s

-- | Equality of abstract literals is equality of the underlying bytestrings.
litEq :: Literal tp -> Literal tp -> Bool
litEq (BoolLit b1) (BoolLit b2) = b1 == b2
litEq (IntLit x1) (IntLit x2) = x1 == x2
litEq (EnumLit _ i1) (EnumLit _ i2) | Just Refl <- i1 `testEquality` i2 = True
                                    | otherwise = False
litEq (SetLit _ s1) (SetLit _ s2) = all (\i -> isJust (find (==i) s2)) s1 &&
                                    all (\i -> isJust (find (==i) s1)) s2
litEq (StructLit fls1) (StructLit fls2) = fls1 `flsEq` fls2
  where flsEq :: forall (ftps :: Ctx (Symbol, Type)) .
                 Assignment (FieldInst Literal) ftps ->
                 Assignment (FieldInst Literal) ftps -> Bool
        flsEq Empty Empty = True
        flsEq (as :> a) (bs :> b) | FieldInst _ _ _ <- a
          = a `fieldValueEq` b && flsEq as bs
litEq (AbsLit _ a1) (AbsLit _ a2) = a1 == a2

fieldValueEq :: FieldInst Literal ftp -> FieldInst Literal ftp -> Bool
fieldValueEq fv1@(FieldInst _ _ _) fv2 =
  litEq (fieldInstValue fv1) (fieldInstValue fv2)


-- TestEquality and HashableF instances for the types in this file

$(return [])

instance TestEquality Literal where
  testEquality = $(structuralTypeEquality [t|Literal|]
    [ (TypeApp (TypeApp (ConType [t|Index|]) AnyType) AnyType, [|testEquality|])
    , (TypeApp (TypeApp (ConType [t|Assignment|]) AnyType) AnyType, [|testEquality|])
    , (TypeApp (ConType [t|SymbolRepr|]) AnyType, [|testEquality|])
    ])

instance HashableF Literal where
  hashWithSaltF = $(structuralHashWithSalt [t|Literal|]
    [ (TypeApp (TypeApp (ConType [t|Index|]) AnyType) AnyType, [|hashWithSaltF|])
    , (TypeApp (TypeApp (ConType [t|Assignment|]) AnyType) AnyType, [|hashWithSaltF|])
    , (TypeApp (ConType [t|SymbolRepr|]) AnyType, [|hashWithSaltF|])
    ])

instance TestEquality (Expr env ctx) where
  testEquality = $(structuralTypeEquality [t|Expr|]
    [ (TypeApp (TypeApp (TypeApp (ConType [t|Expr|]) AnyType) AnyType) AnyType, [|testEquality|])
    , (TypeApp (ConType [t|Literal|]) AnyType, [|testEquality|])
    , (TypeApp (TypeApp (ConType [t|Index|]) AnyType) AnyType, [|testEquality|])
    , (TypeApp (TypeApp (ConType [t|Assignment|]) AnyType) AnyType, [|testEquality|])
    ])

instance HashableF (Expr env ctx) where
  hashWithSaltF = $(structuralHashWithSalt [t|Expr|]
    [ (TypeApp (TypeApp (TypeApp (ConType [t|Expr|]) AnyType) AnyType) AnyType, [|hashWithSaltF|])
    , (TypeApp (ConType [t|Literal|]) AnyType, [|hashWithSaltF|])
    , (TypeApp (TypeApp (ConType [t|Index|]) AnyType) AnyType, [|hashWithSaltF|])
    , (TypeApp (TypeApp (ConType [t|Assignment|]) AnyType) AnyType, [|hashWithSaltF|])
    ])

instance TestEquality f => TestEquality (FieldInst f) where
  testEquality (FieldInst nm tp fl) (FieldInst nm' tp' fl')
    | Just Refl <- testEquality nm nm'
    , Just Refl <- testEquality tp tp'
    , Just Refl <- testEquality fl fl' = Just Refl
    | otherwise = Nothing

instance HashableF f => HashableF (FieldInst f) where
  s `hashWithSaltF` (FieldInst nm tp fl) =
    s `hashWithSaltF` nm `hashWithSaltF` tp `hashWithSaltF` fl

instance TestEquality (FunctionCall env ctx) where
  testEquality (FunctionCall fi args) (FunctionCall fi' args')
    | Just Refl <- testEquality fi fi'
    , Just Refl <- testEquality args args' = Just Refl
    | otherwise = Nothing

instance HashableF (FunctionCall env ctx) where
  s `hashWithSaltF` (FunctionCall fi args) =
    s `hashWithSaltF` fi `hashWithSaltF` args

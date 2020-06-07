{-# LANGUAGE ApplicativeDo #-}
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
Module      : Lobot.Core.Kind
Description : The core data type for representing feature models in Lobot.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the core data type for representing a feature model in
Lobot.
-}
module Lobot.Core.Kind
  ( -- * Types
    Type(..), BoolType, IntType, EnumType, SetType, StructType
  , TypeRepr(..)
    -- * Functions
  , FunctionType(..), FunType
  , FunctionTypeRepr(..)
    -- * Kinds
  , Kind(..)
  , FieldRepr(..)
  , addConstraints
  , derivedKind
  , liftConstraints
    -- * Instances
  , FieldLiteral(..)
  , fieldLiteralType
  , Literal(..)
  , literalType
  , instanceOf
    -- * Function implementation
  , FunctionImpl(..)
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

-- | Representation of a Lobot feature model, which we term a 'Kind' for
-- brevity. A kind consists of a name, a type, a function environment, and a
-- list of constraints that must hold for all instances of this kind.
data Kind (env :: [FunctionType]) (tp :: Type) = Kind
  { kindName :: String
  , kindType :: TypeRepr tp
  , kindFunctionEnv :: List FunctionTypeRepr env
  , kindConstraints :: [Expr env tp BoolType]
  }
  deriving Show

-- | Augment a 'Kind' with some additional constraints.
addConstraints :: Kind env tp -> [Expr env tp BoolType] -> Kind env tp
addConstraints kd cs =
  kd { kindConstraints = kindConstraints kd ++ cs }

-- | Given a set of parent 'Kind's of the same type, create a derived 'Kind'.
derivedKind :: NonEmpty (Kind env tp) -> String -> [Expr env tp BoolType] -> Kind env tp
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

-- | A 'FieldRepr' is a key, type pair. Note that since both the key and the type
-- are tracked at the type level, this is a singleton type that lets you know
-- everything you could possibly need to know about the field at compile time.
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

-- | The types of LOBOT.
data Type = BoolType
          | IntType
          | EnumType [Symbol]
          | SetType [Symbol]
          | StructType [(Symbol, Type)]

type BoolType = 'BoolType
type IntType = 'IntType
type EnumType = 'EnumType
type SetType = 'SetType
type StructType = 'StructType

-- | Types for functions in Lobot.
data FunctionType = FunType Symbol [Type] Type

type FunType = 'FunType

data FunctionTypeRepr fntp where
  FunctionTypeRepr :: SymbolRepr nm
                   -> List TypeRepr args
                   -> TypeRepr ret
                   -> FunctionTypeRepr (FunType nm args ret)

deriving instance Show (FunctionTypeRepr fntp)
instance ShowF FunctionTypeRepr
instance TestEquality FunctionTypeRepr where
  testEquality (FunctionTypeRepr nm args ret) (FunctionTypeRepr nm' args' ret')=
    case (testEquality nm nm', testEquality args args', testEquality ret ret') of
      (Just Refl, Just Refl, Just Refl) -> Just Refl
      _ -> Nothing
instance ( KnownSymbol nm
         , KnownRepr (List TypeRepr) args
         , KnownRepr TypeRepr ret
         ) => KnownRepr FunctionTypeRepr (FunType nm args ret) where
  knownRepr = FunctionTypeRepr knownRepr knownRepr knownRepr

-- | Term-level representative of a type.
data TypeRepr tp where
  BoolRepr   :: TypeRepr BoolType
  IntRepr    :: TypeRepr IntType
  EnumRepr   :: 1 <= (Length cs)
             => List SymbolRepr cs
             -> TypeRepr (EnumType cs)
  SetRepr    :: 1 <= (Length cs)
             => List SymbolRepr cs
             -> TypeRepr (SetType cs)
  StructRepr :: List FieldRepr ftps
             -> TypeRepr (StructType ftps)
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
  testEquality _ _ = Nothing

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
instance KnownRepr (List FieldRepr) ftps => KnownRepr TypeRepr (StructType ftps)
  where knownRepr = StructRepr knownRepr

-- | Concrete value inhabiting a type.
data Literal tp where
  BoolLit   :: Bool -> Literal BoolType
  IntLit    :: Integer -> Literal IntType
  EnumLit   :: 1 <= Length cs
            => List SymbolRepr cs
            -> Index cs c
            -> Literal (EnumType cs)
  SetLit    :: 1 <= Length cs
            => List SymbolRepr cs
            -> [Some (Index cs)]
            -> Literal (SetType cs)
  StructLit :: List FieldLiteral ftps -> Literal (StructType ftps)
deriving instance Show (Literal tp)

-- | Get the type of a literal.
literalType :: Literal tp -> TypeRepr tp
literalType (BoolLit _) = BoolRepr
literalType (IntLit _) = IntRepr
literalType (EnumLit cs _) = EnumRepr cs
literalType (SetLit cs _) = SetRepr cs
literalType (StructLit fls) = StructRepr (fmapFC fieldLiteralType fls)

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

data FunctionImpl m fntp where
  FunctionImpl :: { fnImplType :: FunctionTypeRepr (FunType nm args ret)
                  , fnImplRun :: List Literal args -> m (Literal ret)
                  } -> FunctionImpl m (FunType nm args ret)

-- | A expression involving a particular kind, given a particular function
-- environment.
data Expr (env :: [FunctionType]) (ctx :: Type) (tp :: Type) where
  -- | An expression built from a literal value.
  LiteralExpr :: Literal tp -> Expr env ctx tp
  -- | An expression referring to an abstract instance of this kind.
  SelfExpr    :: Expr env ctx ctx
  -- | An expression referring to a field of an instance of some kind.
  FieldExpr   :: Expr env ctx (StructType ftps) -> Index ftps '(nm, tp) -> Expr env ctx tp
  -- | Function application.
  ApplyExpr   :: Index env (FunType nm args ret)
              -> List (Expr env ctx) args
              -> Expr env ctx ret
  -- | Equality of two expressions.
  EqExpr      :: Expr env ctx tp -> Expr env ctx tp -> Expr env ctx BoolType
  -- | Less-than-or-equal for two integer expressions.
  LteExpr     :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx BoolType
  -- | Set membership.
  MemberExpr  :: Expr env ctx (EnumType cs)
              -> Expr env ctx (SetType cs)
              -> Expr env ctx BoolType
  -- | Logical implication.
  ImpliesExpr :: Expr env ctx BoolType -> Expr env ctx BoolType -> Expr env ctx BoolType
  -- | Logical negation.
  NotExpr     :: Expr env ctx BoolType -> Expr env ctx BoolType

deriving instance Show (Expr env ctx tp)
instance ShowF (Expr env ctx)

litEq :: Literal tp -> Literal tp -> Bool
litEq (BoolLit b1) (BoolLit b2) = b1 == b2
litEq (IntLit x1) (IntLit x2) = x1 == x2
litEq (EnumLit _ i1) (EnumLit _ i2) | Just Refl <- i1 `testEquality` i2 = True
                                    | otherwise = False
litEq (SetLit _ s1) (SetLit _ s2) = all (\i -> isJust (find (==i) s2)) s1 &&
                                    all (\i -> isJust (find (==i) s1)) s2
litEq (StructLit fls1) (StructLit fls2) = fls1 `flsEq` fls2
  where flsEq :: forall (ftps :: [(Symbol, Type)]) .
                  List FieldLiteral ftps -> List FieldLiteral ftps -> Bool
        flsEq Nil Nil = True
        flsEq (a :< as) (b :< bs) | FieldLiteral _ _ <- a
          = a `fieldValueEq` b && flsEq as bs

fieldValueEq :: FieldLiteral ftp -> FieldLiteral ftp -> Bool
fieldValueEq fv1@(FieldLiteral _ _) fv2 =
  litEq (fieldLiteralValue fv1) (fieldLiteralValue fv2)

-- | Determine whether a literal satisfies all the constraints of a kind.
instanceOf :: MonadFail m
           => List (FunctionImpl m) env
           -> Literal tp
           -> Kind env tp
           -> m Bool
instanceOf env inst (Kind{..}) = and <$> traverse constraintHolds kindConstraints
  where constraintHolds e = do
          BoolLit b <- evalExpr env inst e
          return b

-- | Evaluate an expression given an instance of its function and type contexts.
evalExpr :: MonadFail m
         => List (FunctionImpl m) env
         -> Literal ctx
         -> Expr env ctx tp
         -> m (Literal tp)
evalExpr fns inst e = case e of
  LiteralExpr l -> pure l
  SelfExpr -> pure inst
  FieldExpr kd i -> do
    StructLit fls <- evalExpr fns inst kd
    pure $ fieldLiteralValue (fls !! i)
  ApplyExpr fi es -> do
    ls <- traverseFC (evalExpr fns inst) es
    fnImplRun (fns !! fi) ls
  EqExpr e1 e2 -> do
    l1 <- evalExpr fns inst e1
    l2 <- evalExpr fns inst e2
    pure $ BoolLit (litEq l1 l2)
  LteExpr e1 e2 -> do
    IntLit x1 <- evalExpr fns inst e1
    IntLit x2 <- evalExpr fns inst e2
    pure $ BoolLit (x1 <= x2)
  MemberExpr e1 e2 -> do
    EnumLit _ i <- evalExpr fns inst e1
    SetLit _ s <- evalExpr fns inst e2
    pure $ BoolLit (isJust (find (== Some i) s))
  ImpliesExpr e1 e2 -> do
    BoolLit b1 <- evalExpr fns inst e1
    BoolLit b2 <- evalExpr fns inst e2
    pure $ BoolLit (not b1 || b2)
  NotExpr e' -> do
    BoolLit b <- evalExpr fns inst e'
    pure $ BoolLit (not b)

-- | Lift an expression about a kind @K'@ into an expression about a kind @K@ which
-- contains @K'@.
liftExpr :: Index ftps '(nm, tp)
         -> Expr env tp tp'
         -> Expr env (StructType ftps) tp'
liftExpr i e = case e of
  LiteralExpr l -> LiteralExpr l
  SelfExpr -> FieldExpr SelfExpr i
  FieldExpr kd i' -> FieldExpr (liftExpr i kd) i'
  ApplyExpr fi es -> ApplyExpr fi (fmapFC (liftExpr i) es)
  EqExpr e1 e2 -> EqExpr (liftExpr i e1) (liftExpr i e2)
  LteExpr e1 e2 -> LteExpr (liftExpr i e1) (liftExpr i e2)
  MemberExpr e1 e2 -> MemberExpr (liftExpr i e1) (liftExpr i e2)
  ImpliesExpr e1 e2 -> ImpliesExpr (liftExpr i e1) (liftExpr i e2)
  NotExpr e' -> NotExpr (liftExpr i e')

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
  , giveSelf
  , liftExpr
  ) where

import Lobot.Types

import qualified Data.ByteString as BS

import Data.List (find)
import Data.List.NonEmpty hiding ((!!))
import Data.Text (Text)
import Data.Parameterized.Some
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Prelude hiding ((!!))

-- | Representation of a Lobot feature model, which we term a 'Kind' for
-- brevity. A kind consists of a name, a type, a function environment, and a
-- list of constraints that must hold for all instances of this kind.
data Kind (env :: Ctx FunctionType) (tp :: Type) = Kind
  { kindName :: Text
  , kindType :: TypeRepr tp
  , kindFunctionEnv :: Assignment FunctionTypeRepr env
  , kindConstraints :: [Expr env tp BoolType]
  }
  deriving Show
instance ShowF (Kind env)

-- | Augment a 'Kind' with some additional constraints.
addConstraints :: Kind env tp -> [Expr env tp BoolType] -> Kind env tp
addConstraints kd cs =
  kd { kindConstraints = kindConstraints kd ++ cs }

-- | Given a set of parent 'Kind's of the same type, create a derived 'Kind'.
derivedKind :: NonEmpty (Kind env tp) -> Text -> [Expr env tp BoolType] -> Kind env tp
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

-- | Concrete value inhabiting a type.
data Literal tp where
  BoolLit   :: Bool -> Literal BoolType
  IntLit    :: Integer -> Literal IntType
  EnumLit   :: 1 <= CtxSize cs
            => Assignment SymbolRepr cs -> Index cs c -> Literal (EnumType cs)
  SetLit    :: 1 <= CtxSize cs
            => Assignment SymbolRepr cs -> [Some (Index cs)] -> Literal (SetType cs)
  StructLit :: Assignment FieldLiteral ftps -> Literal (StructType ftps)
  AbsLit    :: SymbolRepr s -> BS.ByteString -> Literal (AbsType s)

deriving instance Show (Literal tp)
instance ShowF Literal

-- | Get the type of a literal.
literalType :: Literal tp -> TypeRepr tp
literalType (BoolLit _) = BoolRepr
literalType (IntLit _) = IntRepr
literalType (EnumLit cs _) = EnumRepr cs
literalType (SetLit cs _) = SetRepr cs
literalType (StructLit fls) = StructRepr (fmapFC fieldLiteralType fls)
literalType (AbsLit s _) = AbsRepr s

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
                  , fnImplRun :: Assignment Literal args -> m (Literal ret)
                  } -> FunctionImpl m (FunType nm args ret)

-- | A expression involving a particular kind, given a particular function
-- environment.
data Expr (env :: Ctx FunctionType) (ctx :: Type) (tp :: Type) where
  -- | An expression built from a literal value.
  LiteralExpr :: Literal tp -> Expr env ctx tp
  -- | An expression referring to an abstract instance of this kind.
  SelfExpr    :: Expr env ctx ctx
  -- | An expression referring to a field of an instance of some kind.
  FieldExpr   :: Expr env ctx (StructType ftps) -> Index ftps '(nm, tp) -> Expr env ctx tp
  -- | Function application.
  ApplyExpr   :: Index env (FunType nm args ret)
              -> Assignment (Expr env ctx) args
              -> Expr env ctx ret
  -- | Equality of two expressions.
  EqExpr      :: Expr env ctx tp -> Expr env ctx tp -> Expr env ctx BoolType
  -- | Less-than-or-equal for two integer expressions.
  LteExpr     :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx BoolType
  -- | Add two integer expression.
  PlusExpr     :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx IntType
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
                 Assignment FieldLiteral ftps -> Assignment FieldLiteral ftps -> Bool
        flsEq Empty Empty = True
        flsEq (as :> a) (bs :> b) | FieldLiteral _ _ <- a
          = a `fieldValueEq` b && flsEq as bs
litEq (AbsLit _ a1) (AbsLit _ a2) = a1 == a2

fieldValueEq :: FieldLiteral ftp -> FieldLiteral ftp -> Bool
fieldValueEq fv1@(FieldLiteral _ _) fv2 =
  litEq (fieldLiteralValue fv1) (fieldLiteralValue fv2)

-- | Determine whether a literal satisfies all the constraints of a kind.
instanceOf :: MonadFail m
           => Assignment (FunctionImpl m) env
           -> Literal tp
           -> Kind env tp
           -> m Bool
instanceOf env inst (Kind{..}) = and <$> traverse constraintHolds kindConstraints
  where constraintHolds e = do
          BoolLit b <- evalExpr env inst e
          return b

-- | Evaluate an expression given an instance of its function and type contexts.
evalExpr :: MonadFail m
         => Assignment (FunctionImpl m) env
         -> Literal ctx
         -> Expr env ctx tp
         -> m (Literal tp)
evalExpr fns inst e = case e of
  LiteralExpr l -> pure l
  SelfExpr -> pure inst
  FieldExpr kd i -> do
    StructLit fls <- evalExpr fns inst kd
    pure $ fieldLiteralValue (fls ! i)
  ApplyExpr fi es -> do
    ls <- traverseFC (evalExpr fns inst) es
    fnImplRun (fns ! fi) ls
  EqExpr e1 e2 -> do
    l1 <- evalExpr fns inst e1
    l2 <- evalExpr fns inst e2
    pure $ BoolLit (litEq l1 l2)
  LteExpr e1 e2 -> do
    IntLit x1 <- evalExpr fns inst e1
    IntLit x2 <- evalExpr fns inst e2
    pure $ BoolLit (x1 <= x2)
  PlusExpr e1 e2 -> do
    IntLit x1 <- evalExpr fns inst e1
    IntLit x2 <- evalExpr fns inst e2
    pure $ IntLit (x1 + x2)
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

-- | Substitute a value for 'SelfExpr' in an expression. (This is composition
-- of 'Expr's!)
giveSelf :: Expr env a b -> Expr env b c -> Expr env a c
giveSelf s e = case e of
  LiteralExpr l -> LiteralExpr l
  SelfExpr -> s
  FieldExpr kd i' -> FieldExpr (giveSelf s kd) i'
  ApplyExpr fi es -> ApplyExpr fi (fmapFC (giveSelf s) es)
  EqExpr e1 e2 -> EqExpr (giveSelf s e1) (giveSelf s e2)
  LteExpr e1 e2 -> LteExpr (giveSelf s e1) (giveSelf s e2)
  PlusExpr e1 e2 -> PlusExpr (giveSelf s e1) (giveSelf s e2)
  MemberExpr e1 e2 -> MemberExpr (giveSelf s e1) (giveSelf s e2)
  ImpliesExpr e1 e2 -> ImpliesExpr (giveSelf s e1) (giveSelf s e2)
  NotExpr e' -> NotExpr (giveSelf s e')

-- | Lift an expression about a kind @K'@ into an expression about a kind @K@ which
-- contains @K'@.
liftExpr :: Index ftps '(nm, tp)
         -> Expr env tp tp'
         -> Expr env (StructType ftps) tp'
liftExpr i = giveSelf (FieldExpr SelfExpr i)

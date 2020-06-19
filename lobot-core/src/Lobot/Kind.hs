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
  , instanceOf
  , addConstraints
  , derivedKind
  , liftConstraints
  , giveSelf
  , liftExpr
  ) where

import Lobot.Expr
import Lobot.Types

import Data.List.NonEmpty hiding ((!!))
import Data.Text (Text)
import Data.Parameterized.Classes
import Data.Parameterized.Context
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

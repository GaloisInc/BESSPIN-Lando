{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
  , KindExpr
  , pattern SelfExpr
  , instanceOf
  , getFailingConstraints
  , addConstraints
  , derivedKind
  , liftConstraints
  , giveSelf
  , liftExpr
    -- * Checks
  , Check(..)
  , NamedType(..)
    -- * Constrained functions
  , ConstrainedFunction(..)
  , cFunType'
  ) where

import Lobot.Expr
import Lobot.Types

import Data.Functor.Const
import Data.Maybe (catMaybes)
import Data.List.NonEmpty hiding ((!!))
import Data.Text (Text)
import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.TraversableFC
import UnliftIO (MonadUnliftIO(..))
import Prelude hiding ((!!))

type KindExpr env tp = Expr env (EmptyCtx ::> tp)

-- | Representation of a constrained Lobot function, which consists of a
-- function name, argument types, a return type, argument names, constraints
-- on those arguments which must be satisfied every time the function is
-- called, and constraints on the return type which are assumed to be
-- satisfied for every call of the function.
data ConstrainedFunction (env :: Ctx FunctionType) (fntp :: FunctionType) where
  CFun :: { cfunType :: FunctionTypeRepr (FunType nm args ret)
          , cfunArgNames :: Assignment (Const Text) args
          , cfunArgConstraints :: [Expr     env args BoolType]
          , cfunRetConstraints :: [KindExpr env ret  BoolType]
          } -> ConstrainedFunction env (FunType nm args ret)

cFunType' :: ConstrainedFunction env fntp -> FunctionTypeRepr fntp
cFunType' (CFun ftp _ _ _) = ftp

deriving instance Show (ConstrainedFunction env fntp)
instance ShowF (ConstrainedFunction env)

-- | Representation of a Lobot feature model, which we term a 'Kind' for
-- brevity. A kind consists of a name, a type, a function environment, and a
-- list of constraints that must hold for all instances of this kind.
data Kind (env :: Ctx FunctionType) (tp :: Type) = Kind
  { kindName :: Text
  , kindType :: TypeRepr tp
  , kindFunctionEnv :: Assignment FunctionTypeRepr env
  , kindConstraints :: [KindExpr env tp BoolType]
  }
  deriving Show
instance ShowF (Kind env)

data NamedType tp = NamedType { namedTypeName :: Text
                              , namedTypeType :: TypeRepr tp
                              }

data Check (env :: Ctx FunctionType) (tps :: Ctx Type) = Check
  { checkName :: Text
  , checkFields :: Assignment NamedType tps
  , checkFunctionEnv :: Assignment FunctionTypeRepr env
  , checkConstraints :: [Expr env tps BoolType]
  , checkRequirements :: [Expr env tps BoolType]
  }

-- | Augment a 'Kind' with some additional constraints.
addConstraints :: Kind env tp -> [KindExpr env tp BoolType] -> Kind env tp
addConstraints kd cs =
  kd { kindConstraints = kindConstraints kd ++ cs }

-- | Given a set of parent 'Kind's of the same type, create a derived 'Kind'.
derivedKind :: NonEmpty (Kind env tp)
            -> Text
            -> [KindExpr env tp BoolType]
            -> Kind env tp
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

-- | Determine whether a set of literals satisfies a constraint set.
instanceOf :: (MonadUnliftIO m, MonadFail m)
           => Assignment (FunctionImpl m) env
           -> FunctionCallCache env ctx
           -> Assignment Literal ctx
           -> [Expr env ctx BoolType]
           -> m Bool
instanceOf env cache ls constraints =
  null . fst <$> getFailingConstraints env cache ls constraints

-- | Like 'instanceOf', but returns the list of all constraints that failed
-- instead of just a 'Bool', as well as the list of all new function calls
-- added to the cache.
getFailingConstraints :: forall env ctx m. (MonadUnliftIO m, MonadFail m)
                      => Assignment (FunctionImpl m) env
                      -> FunctionCallCache env ctx
                      -> Assignment Literal ctx
                      -> [Expr env ctx BoolType]
                      -> m ([Expr env ctx BoolType], [FunctionCallResult env ctx])
getFailingConstraints env cache ls constraints =
  runEvalM cache (catMaybes <$> traverse failingConstraint constraints)
  where failingConstraint :: Expr env ctx BoolType
                          -> EvalM env ctx m (Maybe (Expr env ctx BoolType))
        failingConstraint e = do
          EvalResult (BoolLit b) _ <- evalExpr env ls e
          pure (if b then Nothing else Just e)

-- | Substitute a value for 'self' in a kind expression.
-- giveSelf :: Expr env ctx a -> KindExpr env a b -> Expr env ctx b
giveSelf :: Expr env ctx a -> Expr env (EmptyCtx ::> a) b -> Expr env ctx b
giveSelf s e = case e of
  LiteralExpr l -> LiteralExpr l
  StructExpr fvs -> structExpr (fmapFC (fmapFC (giveSelf s)) fvs)
  SelfExpr -> s
  FieldExpr kd i' -> FieldExpr (giveSelf s kd) i'
  ApplyExpr fi es -> ApplyExpr fi (fmapFC (giveSelf s) es)
  EqExpr e1 e2 -> EqExpr (giveSelf s e1) (giveSelf s e2)
  NeqExpr e1 e2 -> NeqExpr (giveSelf s e1) (giveSelf s e2)
  LteExpr e1 e2 -> LteExpr (giveSelf s e1) (giveSelf s e2)
  LtExpr e1 e2 -> LtExpr (giveSelf s e1) (giveSelf s e2)
  GteExpr e1 e2 -> GteExpr (giveSelf s e1) (giveSelf s e2)
  GtExpr e1 e2 -> GtExpr (giveSelf s e1) (giveSelf s e2)
  PlusExpr e1 e2 -> PlusExpr (giveSelf s e1) (giveSelf s e2)
  MinusExpr e1 e2 -> MinusExpr (giveSelf s e1) (giveSelf s e2)
  TimesExpr e1 e2 -> TimesExpr (giveSelf s e1) (giveSelf s e2)
  ModExpr e1 e2 -> ModExpr (giveSelf s e1) (giveSelf s e2)
  DivExpr e1 e2 -> DivExpr (giveSelf s e1) (giveSelf s e2)
  AbsExpr e' -> AbsExpr (giveSelf s e')
  NegExpr e' -> NegExpr (giveSelf s e')
  MemberExpr e1 e2 -> MemberExpr (giveSelf s e1) (giveSelf s e2)
  NotMemberExpr e1 e2 -> NotMemberExpr (giveSelf s e1) (giveSelf s e2)
  SubsetExpr e1 e2 -> SubsetExpr (giveSelf s e1) (giveSelf s e2)
  NonEmptyExpr e' -> NonEmptyExpr (giveSelf s e')
  SizeExpr e' -> SizeExpr (giveSelf s e')
  IntersectExpr e1 e2 -> IntersectExpr (giveSelf s e1) (giveSelf s e2)
  UnionExpr e1 e2 -> UnionExpr (giveSelf s e1) (giveSelf s e2)
  SymDiffExpr e1 e2 -> SymDiffExpr (giveSelf s e1) (giveSelf s e2)
  DiffExpr e1 e2 -> DiffExpr (giveSelf s e1) (giveSelf s e2)
  ComplementExpr e' -> ComplementExpr (giveSelf s e')
  AndExpr e1 e2 -> AndExpr (giveSelf s e1) (giveSelf s e2)
  OrExpr e1 e2 -> OrExpr (giveSelf s e1) (giveSelf s e2)
  XorExpr e1 e2 -> XorExpr (giveSelf s e1) (giveSelf s e2)
  ImpliesExpr e1 e2 -> ImpliesExpr (giveSelf s e1) (giveSelf s e2)
  IffExpr e1 e2 -> IffExpr (giveSelf s e1) (giveSelf s e2)
  NotExpr e' -> NotExpr (giveSelf s e')
{-# COMPLETE
    LiteralExpr
  , StructExpr
  , SelfExpr
  , FieldExpr
  , ApplyExpr
  , EqExpr
  , LteExpr
  , LtExpr
  , GteExpr
  , GtExpr
  , PlusExpr
  , MinusExpr
  , TimesExpr
  , ModExpr
  , DivExpr
  , AbsExpr
  , NegExpr
  , MemberExpr
  , NotMemberExpr
  , SubsetExpr
  , NonEmptyExpr
  , SizeExpr
  , IntersectExpr
  , UnionExpr
  , SymDiffExpr
  , DiffExpr
  , ComplementExpr
  , AndExpr
  , OrExpr
  , XorExpr
  , ImpliesExpr
  , IffExpr
  , NotExpr #-}

singletonIndexRefl :: forall tp tp' . Index (EmptyCtx ::> tp') tp -> tp :~: tp'
singletonIndexRefl i = case viewIndex knownSize i of
  IndexViewLast _ -> Refl
  IndexViewInit i' -> case viewIndex knownSize i' of { }

-- | Convenient pattern for matching on 'KindExpr' variables.
pattern SelfExpr :: () => (tp' ~ tp) => KindExpr env tp' tp
pattern SelfExpr <- VarExpr (singletonIndexRefl -> Refl)
  where SelfExpr = VarExpr baseIndex
-- | Lift an expression about a kind @K'@ into an expression about a kind @K@ which
-- contains @K'@.
liftExpr :: Index ftps '(nm, tp)
         -> KindExpr env tp tp'
         -> KindExpr env (StructType ftps) tp'
liftExpr i = giveSelf (FieldExpr (VarExpr baseIndex) i)

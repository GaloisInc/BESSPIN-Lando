{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Lobot.TypeCheck.ISyntax
Description : An intermediate untyped syntax used during typechecking.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galoicom
Stability   : experimental
Portability : POSIX

This module defines an intermediate version of the untyped syntax for use
during during typechecking. In particular, this type is used between the first
and second passes, where all types have been checked, all kind names have been
resolved, all identifiers have been scope checked, but no terms have been type
checked.
-}
module Lobot.TypeCheck.ISyntax
  ( Kind(..)
  , Check(..)
  , CheckField(..)
  , FunctionType(..)
  , DerivedConstraint(..)
  , Type
  , unIType
  , Expr(..)
  , unIExpr
  , LExpr
  , unILExpr
  , Literal(..)
  , unILit
  , LLiteral
  , unILLit
  ) where

import Data.Parameterized.Classes
import Data.Parameterized.Some
import Data.Parameterized.Context

import Lobot.Lexer
import qualified Lobot.Syntax as S
import qualified Lobot.Types as T

data Kind where
  Kind :: { kindName :: LText
          , kindType ::  T.TypeRepr tp
          , kindConstraints :: [LExpr (EmptyCtx ::> tp)]
          , kindDerivedConstraints :: [DerivedConstraint]
          } -> Kind

deriving instance Show Kind

data CheckField tp = CheckField { checkFieldName :: LText
                                , checkFieldType :: T.TypeRepr tp
                                , checkFieldDerivedConstraints :: [DerivedConstraint]
                                }
  deriving (Show)

instance ShowF CheckField

data Check where
  Check :: { checkName :: LText
           , checkFields :: Assignment CheckField tps
           , checkConstraints :: [LExpr tps]
           , checkRequirements :: [LExpr tps]
           } -> Check

deriving instance Show Check

-- | Unlike 'S.FunctionType', this saves the 'DerivedConstraint's on the
-- function's argument and return types, though they are currently not
-- used for anything.
data FunctionType = FunType { funType :: Some T.FunctionTypeRepr
                            , funArgConstraints :: [[DerivedConstraint]]
                            , funRetConstraints :: [DerivedConstraint]
                            } deriving Show

-- | After the first pass of typechecking, all kind names have been resolved
-- in types, but no expressions (and thus no kind constraints) have been
-- checked yet. Instead of copying over the untyped constraints, which could
-- create strange error reporting w.r.t. source locations, we save
-- 'DerivedConstraint's, which are essentially pointers to where to get
-- additional constraints in the second pass, once they're typechecked.
data DerivedConstraint = FromKind LText
                       | FromField LText [DerivedConstraint]
                       deriving Show

type Type = (S.LType, Some T.TypeRepr, [DerivedConstraint])

unIType :: Type -> S.LType
unIType (tp, _, _) = tp

data Expr (ctx :: Ctx T.Type) where
  LiteralExpr    :: LLiteral -> Expr ctx
  -- | this case differs from 'S.VarExpr'
  VarExpr        :: LText -> Index ctx tp -> Expr ctx
  -- | this case differs from 'S.SelfFieldExpr'
  SelfFieldExpr  :: LText -> Index ftps tp -> Expr (EmptyCtx ::> T.StructType ftps)
  FieldExpr      :: LExpr ctx -> LText -> Expr ctx
  ApplyExpr      :: LText -> [LExpr ctx] -> Expr ctx
  EqExpr         :: LExpr ctx -> LExpr ctx -> Expr ctx
  LteExpr        :: LExpr ctx -> LExpr ctx -> Expr ctx
  PlusExpr       :: LExpr ctx -> LExpr ctx -> Expr ctx
  MemberExpr     :: LExpr ctx -> LExpr ctx -> Expr ctx
  ImpliesExpr    :: LExpr ctx -> LExpr ctx -> Expr ctx
  NotExpr        :: LExpr ctx -> Expr ctx
  -- | this case differs from 'S.IsInstanceExpr'
  IsInstanceExpr :: LExpr ctx -> Type -> Expr ctx

deriving instance Show (Expr ctx)

unIExpr :: Expr ctx -> S.Expr
unIExpr (LiteralExpr l) = S.LiteralExpr (unILLit l)
unIExpr (VarExpr s _) = S.VarExpr s
unIExpr (SelfFieldExpr f _) = S.VarExpr f
unIExpr (FieldExpr x f) = S.FieldExpr (unILExpr x) f
unIExpr (ApplyExpr fn args) = S.ApplyExpr fn (unILExpr <$> args)
unIExpr (EqExpr x y) = S.EqExpr (unILExpr x) (unILExpr y)
unIExpr (LteExpr x y) = S.LteExpr (unILExpr x) (unILExpr y)
unIExpr (PlusExpr x y) = S.PlusExpr (unILExpr x) (unILExpr y)
unIExpr (MemberExpr x y) = S.MemberExpr (unILExpr x) (unILExpr y)
unIExpr (ImpliesExpr x y) = S.ImpliesExpr (unILExpr x) (unILExpr y)
unIExpr (NotExpr x) = S.NotExpr (unILExpr x)
unIExpr (IsInstanceExpr x tp) = S.IsInstanceExpr (unILExpr x) (unIType tp)

type LExpr ctx = Loc (Expr ctx)

unILExpr :: LExpr ctx -> S.LExpr
unILExpr (L p x) = L p (unIExpr x)

data Literal = BoolLit Bool
             | IntLit Integer
             | EnumLit LText
             | SetLit [LText]
             | StructLit (Maybe Type) [(LText, LLiteral)]
             -- ^ this case differs from 'S.StructLit'
             deriving Show

type LLiteral = Loc Literal

unILLit :: LLiteral -> S.LLiteral
unILLit (L p l) = L p (unILit l)

unILit :: Literal -> S.Literal
unILit (BoolLit b) = S.BoolLit b
unILit (IntLit z) = S.IntLit z
unILit (EnumLit e) = S.EnumLit e
unILit (SetLit es) = S.SetLit es
unILit (StructLit mb_tp fls) =
  S.StructLit (unIType <$> mb_tp) (fmap (\(t,l) -> (t, unILLit l)) fls)

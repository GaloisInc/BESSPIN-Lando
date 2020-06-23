{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Lobot.Expr
Description : Expression data type
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This modules defines the core expression data type we use for constraints.
-}
module Lobot.Expr
  ( -- * Expressions
    Expr(..)
    -- * Literals
  , FieldLiteral(..)
  , fieldLiteralType
  , Literal(..)
  , literalType
  , litEq
    -- * Function implementation
  , FunctionImpl(..)
    -- * Evaluation
  , evalExpr
  , runEvalM
  , EvalM
  , EvalResult(..)
  , FunctionCallResult(..)
  ) where

import Lobot.Types

import qualified Control.Monad.State as S
import qualified Data.ByteString as BS

import Data.List (find)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import GHC.TypeLits

-- | A expression involving a particular variable context, given a particular
-- function environment.
data Expr (env :: Ctx FunctionType) (ctx :: Ctx Type) (tp :: Type) where
  -- | An expression built from a literal value.
  LiteralExpr :: IsAbstractType tp ~ 'False => Literal tp -> Expr env ctx tp
  -- | An expression referring to a particular value in the current context.
  VarExpr     :: Index ctx tp -> Expr env ctx tp
  -- | An expression referring to a field of an instance of some kind.
  FieldExpr   :: Expr env ctx (StructType ftps) -> Index ftps '(nm, tp) -> Expr env ctx tp
  -- | Function application.
  ApplyExpr   :: Index env (FunType nm args ret)
              -> Assignment (Expr env ctx) args
              -> Expr env ctx ret
  -- | Equality of two expressions.
  EqExpr      :: IsAbstractType tp ~ 'False
              => Expr env ctx tp
              -> Expr env ctx tp
              -> Expr env ctx BoolType
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

-- | The result of a function call on a particular set of argument expressions,
-- whose return type is not abstract. This is used for refining the solver's
-- model of the function to improve its search during instance generation.
data FunctionCallResult env ctx where
  FunctionCallResult :: IsAbstractType ret ~ 'False
                     => Index env (FunType nm args ret)
                     -- ^ Index of the called function
                     -> Assignment (Expr env ctx) args
                     -- ^ Argument expressions
                     -> Literal ret
                     -- ^ Literal value of result
                     -> FunctionCallResult env ctx

-- | Expression evaluation monad. Every time we evaluate a function, we record
-- the call as a 'FunctionCallResult' that can be collected after evaluation.
newtype EvalM env ctx m a =
  EvalM { unEvalM :: S.StateT [FunctionCallResult env ctx] m a }
  deriving ( Functor
           , Applicative
           , Monad
           , S.MonadState [FunctionCallResult env ctx]
           , MonadFail
           )

-- | Unwrap an 'EvalM' computation, collecting both the result of the evaluation
-- and a list of all of the 'FunctionCallResult's computed as the result of
-- calling concrete functions.
runEvalM :: EvalM env ctx m a -> m (a, [FunctionCallResult env ctx])
runEvalM k = S.runStateT (unEvalM k) []

-- | Lift a value from the underlying monad into 'EvalM'.
lift :: Monad m => m a -> EvalM env ctx m a
lift = EvalM . S.lift

addCall :: (IsAbstractType ret ~ 'False, Monad m)
        => Index env (FunType nm args ret)
        -> Assignment (Expr env ctx) args
        -> Literal ret
        -> EvalM env ctx m ()
addCall fi args ret = S.modify (call:)
  where call = FunctionCallResult fi args ret

data EvalResult env ctx tp =
  EvalResult { evalResultLit :: Literal tp
             , evalResultExpr :: Expr env ctx tp
             }

litEvalResult :: IsAbstractType tp ~ 'False
              => Literal tp
              -> EvalResult env ctx tp
litEvalResult l = EvalResult l (LiteralExpr l)

-- | Evaluate an expression.
--
-- When we evaluate an expression, there are two things we might want back. The
-- first and most obvious is a 'Literal', i.e. the literal result of evaluating
-- the expression. The second is another 'Expr', consisting of what the
-- expression becomes after all non-abstract subexpressions have been evaluated.
--
-- If the expression's type is non-abstract, we simply return the literal as a
-- 'LiteralExpr'. If it is abstract, we reconstruct the original expression
-- after all sub-expressions have been simplified in this manner.
evalExpr :: MonadFail m
         => Assignment (FunctionImpl m) env
         -> Assignment Literal ctx
         -> Expr env ctx tp
         -> EvalM env ctx m (EvalResult env ctx tp)
evalExpr fns ls e = case e of
  LiteralExpr l -> pure $ litEvalResult l
  VarExpr i -> do
    let l = ls ! i
        e' = case isAbstractType (literalType l) of
               FalseRepr -> LiteralExpr l
               TrueRepr -> VarExpr i
    pure $ EvalResult l e'
  FieldExpr se i -> do
    EvalResult (StructLit fls) se' <- evalExpr fns ls se
    let l = fieldLiteralValue (fls ! i)
        e' = case isAbstractType (literalType l) of
               FalseRepr -> LiteralExpr l
               TrueRepr -> FieldExpr se' i
    pure $ EvalResult l e'
  ApplyExpr fi es -> do
    let fn = fns ! fi
    evalArgs <- traverseFC (evalExpr fns ls) es
    let argLits = fmapFC evalResultLit evalArgs
        argEs = fmapFC evalResultExpr evalArgs
    l <- lift $ fnImplRun fn argLits
    let e' = case isAbstractType (literalType l) of
               FalseRepr -> LiteralExpr l
               TrueRepr -> ApplyExpr fi argEs
    -- TODO: Is there a way to clean this up?
    () <- case isAbstractType (functionRetType (fnImplType fn)) of
      TrueRepr -> return ()
      FalseRepr -> addCall fi argEs l
    return $ EvalResult l e'
  EqExpr e1 e2 -> do
    EvalResult l1 _ <- evalExpr fns ls e1
    EvalResult l2 _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (litEq l1 l2))
  LteExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (x1 <= x2))
  PlusExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (IntLit (x1 + x2))
  MemberExpr e1 e2 -> do
    EvalResult (EnumLit _ i) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (isJust (find (== Some i) s)))
  ImpliesExpr e1 e2 -> do
    EvalResult (BoolLit b1) _ <- evalExpr fns ls e1
    EvalResult (BoolLit b2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (not b1 || b2))
  NotExpr e' -> do
    EvalResult (BoolLit b) _ <- evalExpr fns ls e'
    pure $ litEvalResult (BoolLit (not b))

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

deriving instance Show (FieldLiteral p)
instance ShowF FieldLiteral

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

-- | Get the type of a 'FieldLiteral'.
fieldLiteralType :: FieldLiteral ftp -> FieldRepr ftp
fieldLiteralType FieldLiteral{..} =
  FieldRepr fieldLiteralName (literalType fieldLiteralValue)

-- | Implementation of a function.
data FunctionImpl m fntp where
  FunctionImpl :: { fnImplType :: FunctionTypeRepr (FunType nm args ret)
                  , fnImplRun :: Assignment Literal args -> m (Literal ret)
                  } -> FunctionImpl m (FunType nm args ret)

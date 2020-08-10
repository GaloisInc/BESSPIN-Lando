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
  , structExpr
  , FieldInst(..)
  , fieldInstFieldType
    -- * Literals
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

import Data.Bits (xor)
import Data.List (find)
import Data.Maybe (isNothing)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Data.Parameterized.TH.GADT
import Data.Constraint (Dict(..))
import GHC.TypeLits

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
  DivExpr   :: Expr env ctx IntType -> Expr env ctx IntType -> Expr env ctx IntType
  -- | Negate an integer.
  NegExpr   :: Expr env ctx IntType -> Expr env ctx IntType
  -- | Set membership.
  MemberExpr  :: Expr env ctx (EnumType cs)
              -> Expr env ctx (SetType cs)
              -> Expr env ctx BoolType
  -- | Set non-membership.
  NotMemberExpr  :: Expr env ctx (EnumType cs)
                 -> Expr env ctx (SetType cs)
                 -> Expr env ctx BoolType
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
  Just (fvs', Dict) -> LiteralExpr (StructLit fvs')
  Nothing           -> StructExpr fvs
  where go :: Assignment (FieldInst (Expr env ctx)) ftps 
           -> Maybe (Assignment (FieldInst Literal) ftps, Dict (NonAbstract ftps))
        go Empty = Just (Empty, Dict)
        go (fvs' :> FieldInst nm tp (LiteralExpr l)) = do
          (fvs'', Dict) <- go fvs'
          Just (fvs'' :> FieldInst nm tp l, Dict)
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

-- | The result of a function call on a particular set of argument expressions,
-- whose return type is not abstract. This is used for refining the solver's
-- model of the function to improve its search during instance generation.
data FunctionCallResult env ctx where
  FunctionCallResult :: NonAbstract ret
                     => Index env (FunType nm args ret)
                     -- ^ Index of the called function
                     -> Assignment (Expr env ctx) args
                     -- ^ Argument expressions
                     -> Literal ret
                     -- ^ Literal value of result
                     -> String
                     -- ^ Any console output generated by the function
                     -> FunctionCallResult env ctx

deriving instance Show (FunctionCallResult env ctx)

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

addCall :: (NonAbstract ret, Monad m)
        => Index env (FunType nm args ret)
        -> Assignment (Expr env ctx) args
        -> Literal ret
        -> String
        -> EvalM env ctx m ()
addCall fi args ret st = S.modify (call:)
  where call = FunctionCallResult fi args ret st

data EvalResult env ctx tp =
  EvalResult { evalResultLit :: Literal tp
             , evalResultExpr :: Expr env ctx tp
             }

litEvalResult :: NonAbstract tp
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
  StructExpr fls -> do
    evalFls <- traverseFC (evalField fns ls) fls
    let flLits = fmapFC evalResultFieldLit evalFls
        flEs = fmapFC evalResultFieldExpr evalFls
    pure $ EvalResult (StructLit flLits) (structExpr flEs)
  VarExpr i -> do
    let l = ls ! i
        e' = case isNonAbstract (literalType l) of
               Just Dict -> LiteralExpr l
               Nothing -> VarExpr i
    pure $ EvalResult l e'
  FieldExpr se i -> do
    EvalResult (StructLit fls) se' <- evalExpr fns ls se
    let l = fieldInstValue (fls ! i)
        e' = case isNonAbstract (literalType l) of
               Just Dict -> LiteralExpr l
               Nothing -> FieldExpr se' i
    pure $ EvalResult l e'
  ApplyExpr fi es -> do
    let fn = fns ! fi
    evalArgs <- traverseFC (evalExpr fns ls) es
    let argLits = fmapFC evalResultLit evalArgs
        argEs = fmapFC evalResultExpr evalArgs
    (l,st) <- lift $ fnImplRun fn argLits
    let e' = case isNonAbstract (literalType l) of
               Just Dict -> LiteralExpr l
               Nothing -> ApplyExpr fi argEs
    -- TODO: Is there a way to clean this up?
    () <- case isNonAbstract (functionRetType (fnImplType fn)) of
      Nothing -> return ()
      Just Dict -> addCall fi argEs l st
    return $ EvalResult l e'
  EqExpr e1 e2 -> do
    EvalResult l1 _ <- evalExpr fns ls e1
    EvalResult l2 _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (litEq l1 l2))
  NeqExpr e1 e2 -> do
    EvalResult l1 _ <- evalExpr fns ls e1
    EvalResult l2 _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (not $ litEq l1 l2))
  LteExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (x1 <= x2))
  LtExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (x1 < x2))
  GteExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (x1 >= x2))
  GtExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (x1 > x2))
  PlusExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (IntLit (x1 + x2))
  MinusExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (IntLit (x1 - x2))
  TimesExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (IntLit (x1 * x2))
  DivExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (IntLit (x1 `div` x2))
  ModExpr e1 e2 -> do
    EvalResult (IntLit x1) _ <- evalExpr fns ls e1
    EvalResult (IntLit x2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (IntLit (x1 `mod` x2))
  NegExpr e' -> do
    EvalResult (IntLit x) _ <- evalExpr fns ls e'
    pure $ litEvalResult (IntLit (- x))
  MemberExpr e1 e2 -> do
    EvalResult (EnumLit _ i) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (isJust (find (== Some i) s)))
  NotMemberExpr e1 e2 -> do
    EvalResult (EnumLit _ i) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (isNothing (find (== Some i) s)))
  AndExpr e1 e2 -> do
    EvalResult (BoolLit b1) _ <- evalExpr fns ls e1
    EvalResult (BoolLit b2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (b1 && b2))
  OrExpr e1 e2 -> do
    EvalResult (BoolLit b1) _ <- evalExpr fns ls e1
    EvalResult (BoolLit b2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (b1 || b2))
  XorExpr e1 e2 -> do
    EvalResult (BoolLit b1) _ <- evalExpr fns ls e1
    EvalResult (BoolLit b2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (b1 `xor` b2))
  ImpliesExpr e1 e2 -> do
    EvalResult (BoolLit b1) _ <- evalExpr fns ls e1
    EvalResult (BoolLit b2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (not b1 || b2))
  IffExpr e1 e2 -> do
    EvalResult (BoolLit b1) _ <- evalExpr fns ls e1
    EvalResult (BoolLit b2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (b1 == b2))
  NotExpr e' -> do
    EvalResult (BoolLit b) _ <- evalExpr fns ls e'
    pure $ litEvalResult (BoolLit (not b))

data EvalFieldResult env ctx p =
  EvalFieldResult { evalResultFieldLit :: FieldInst Literal p
                  , evalResultFieldExpr :: FieldInst (Expr env ctx) p
                  }

evalField :: MonadFail m
          => Assignment (FunctionImpl m) env
          -> Assignment Literal ctx
          -> FieldInst (Expr env ctx) p
          -> EvalM env ctx m (EvalFieldResult env ctx p)
evalField fns ls (FieldInst nm tp e) = do
  EvalResult l e' <- evalExpr fns ls e
  pure $ EvalFieldResult (FieldInst nm (literalType l) l)
                         (FieldInst nm tp e')

-- | Concrete value inhabiting a type.
data Literal tp where
  BoolLit   :: Bool -> Literal BoolType
  IntLit    :: Integer -> Literal IntType
  EnumLit   :: 1 <= CtxSize cs
            => Assignment SymbolRepr cs -> Index cs c -> Literal (EnumType cs)
  SetLit    :: 1 <= CtxSize cs
            => Assignment SymbolRepr cs -> [Some (Index cs)] -> Literal (SetType cs)
  StructLit :: Assignment (FieldInst Literal) ftps -> Literal (StructType ftps)
  AbsLit    :: SymbolRepr s -> BS.ByteString -> Literal (AbsType s)

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

-- | Implementation of a function.
data FunctionImpl m fntp where
  FunctionImpl :: { fnImplType :: FunctionTypeRepr (FunType nm args ret)
                  , fnImplRun :: Assignment Literal args -> m (Literal ret, String)
                  } -> FunctionImpl m (FunType nm args ret)


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

instance Eq (FunctionCallResult env ctx) where
  (FunctionCallResult fi args ret st) == (FunctionCallResult fi' args' ret' st')
    | Just Refl <- testEquality args args'
    , Just Refl <- testEquality ret ret'
    , Just Refl <- testEquality fi fi' = st == st'
    | otherwise = False

instance Hashable (FunctionCallResult env ctx) where
  s `hashWithSalt` (FunctionCallResult fi args ret st) =
    s `hashWithSaltF` fi `hashWithSaltF` args `hashWithSaltF` ret `hashWithSalt` st

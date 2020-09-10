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
    -- * Literals
  , Literal(..)
  , literalType
  , litEq
    -- * Function implementation
  , FunctionImpl(..)
  , fnImplRunLazy
    -- * Evaluation
  , evalExpr
  , runEvalM
  , EvalM
  , EvalResult(..)
  , FunctionCallCache
  , lookupCall
  , FunctionCallResult(..)
  , liftST
  ) where

import Lobot.Types
import Lobot.Utils

import Data.ByteString (ByteString)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.ST
import Data.Parameterized.HashTable
import System.IO.Unsafe (unsafeInterleaveIO)
import UnliftIO (MonadUnliftIO(..), withRunInIO)

import Data.Bits (xor)
import Data.List (find, intersect, union, (\\))
import Data.Maybe (isNothing)
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

-- | The signature of a function call to be cached, assuming that the given
-- expressions are all the result of 'evalExpr'.
data FunctionCall env ctx ret where
  FunctionCall :: NonAbstract ret
               => Index env (FunType nm args ret)
               -- ^ Index of the called function
               -> Assignment (Expr env ctx) args
               -- ^ Argument expressions
               -> FunctionCall env ctx ret

deriving instance Show (FunctionCall env ctx ret)

-- | An 'STRef' which stores function calls with non-abstract return types
-- and their literal results.
type FunctionCallCache env ctx =
  HashTable RealWorld (FunctionCall env ctx) Literal

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

-- | Expression evaluation monad - a monad transformer which has a
-- 'FunctionCallCache' reference and accumulates a list of
-- 'FunctionCallResult's, the later of which is indended to include all
-- function calls which were added to the cache during evaluation.
newtype EvalM env ctx m a =
  EvalM { unEvalM :: WriterT [FunctionCallResult env ctx]
                             (ReaderT (FunctionCallCache env ctx) m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (FunctionCallCache env ctx)
           , MonadWriter [FunctionCallResult env ctx]
           , MonadIO
           , MonadFail
           )

instance MonadTrans (EvalM env ctx) where
  lift = EvalM . lift . lift

liftST :: MonadIO m => ST RealWorld a -> m a
liftST = liftIO . stToIO

-- | Unwrap an 'EvalM' computation, collecting both the result of the evaluation
-- and a list of all of the 'FunctionCallResult's computed as the result of
-- calling concrete functions.
runEvalM :: FunctionCallCache env ctx
         -> EvalM env ctx m a
         -> m (a, [FunctionCallResult env ctx])
runEvalM cache k = runReaderT (runWriterT (unEvalM k)) cache

data EvalResult env ctx tp =
  EvalResult { evalResultLit :: Literal tp
             , evalResultExpr :: Expr env ctx tp
             }

litEvalResult :: NonAbstract tp
              => Literal tp
              -> EvalResult env ctx tp
litEvalResult l = EvalResult l (LiteralExpr l)

-- | Within an 'EvalM' computation, try to lookup in the current cache the
-- function call defined by the given index and arguments, and return the
-- result if there is a hit. If there is no hit, evaluate the this function
-- on its arguments using the given environment of function implementations,
-- add the result to the cache and the current list of function call results,
-- and return this result.
lookupCall :: (NonAbstract ret, MonadIO m)
           => Assignment (FunctionImpl m) env
           -> Index env (FunType nm args ret)
           -> Assignment (EvalResult env ctx) args
           -> EvalM env ctx m (Literal ret)
lookupCall fns fi evalArgs = do
  -- We must be careful to keep everything lazy here! In particular, we don't
  -- want to evaluate `fmapFC evalResultLit evalArgs` if we get a cache hit.
  let fn = fns ! fi
      argEs = fmapFC evalResultExpr evalArgs
  cache <- ask
  mb_res <- liftST $ lookup cache (FunctionCall fi argEs)
  case mb_res of
    Just l -> pure l
    Nothing -> do
      let argLits = fmapFC evalResultLit evalArgs
      (l, outp) <- lift $ fnImplRun fn argLits
      liftST $ insert cache (FunctionCall fi argEs) l
      tell [FunctionCallResult fi argEs l outp]
      pure l

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
evalExpr :: (MonadUnliftIO m, MonadFail m)
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
               Just IsNonAbs -> LiteralExpr l
               Nothing -> VarExpr i
    pure $ EvalResult l e'
  FieldExpr se i -> do
    EvalResult (StructLit fls) se' <- evalExpr fns ls se
    let l = fieldInstValue (fls ! i)
        e' = case isNonAbstract (literalType l) of
               Just IsNonAbs -> LiteralExpr l
               Nothing -> FieldExpr se' i
    pure $ EvalResult l e'
  ApplyExpr fi es -> do
    -- We must be careful to keep everything lazy here! In particular, we only
    -- want to evaluate `fmapFC evalResultLit evalArgs` if we get a cache miss
    -- in `lookupCall` (see the comment below).
    let fn = fns ! fi
    evalArgs <- traverseFC (evalExpr fns ls) es
    case isNonAbstract (functionRetType (fnImplType fn)) of
      Just IsNonAbs -> litEvalResult <$> lookupCall fns fi evalArgs
      -- We don't cache functions with non-abstract return types, but they may
      -- still appear in the arguments of some other cached call. To ensure
      -- a concrete function is not evaluated if it is later found to be part
      -- of a cached call, we make sure to call 'fnImplRunLazy' instead of
      -- 'fnImplRun' in the following case.
      Nothing -> let argLits = fmapFC evalResultLit  evalArgs
                     argEs   = fmapFC evalResultExpr evalArgs
                  in do l <- fst <$> lift (fnImplRunLazy fn argLits)
                        pure $ EvalResult l (ApplyExpr fi argEs)
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
  AbsExpr e' -> do
    EvalResult (IntLit x) _ <- evalExpr fns ls e'
    pure $ litEvalResult (IntLit (abs x))
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
  SubsetExpr e1 e2 -> do
    EvalResult (SetLit _ s1) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (BoolLit (s1 `intersect` s2 == s1))
  NonEmptyExpr e' -> do
    EvalResult (SetLit _ s) _ <- evalExpr fns ls e'
    pure $ litEvalResult (BoolLit (not (null s)))
  SizeExpr e' -> do
    EvalResult (SetLit _ s) _ <- evalExpr fns ls e'
    pure $ litEvalResult (IntLit (fromIntegral (length s)))
  IntersectExpr e1 e2 -> do
    EvalResult (SetLit cs s1) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (SetLit cs (s1 `intersect` s2))
  UnionExpr e1 e2 -> do
    EvalResult (SetLit cs s1) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (SetLit cs (s1 `union` s2))
  SymDiffExpr e1 e2 -> do
    EvalResult (SetLit cs s1) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (SetLit cs ((s1 \\ s2) `union` (s2 \\ s1)))
  DiffExpr e1 e2 -> do
    EvalResult (SetLit cs s1) _ <- evalExpr fns ls e1
    EvalResult (SetLit _ s2) _ <- evalExpr fns ls e2
    pure $ litEvalResult (SetLit cs (s1 \\ s2))
  ComplementExpr e' -> do
    EvalResult (SetLit cs s) _ <- evalExpr fns ls e'
    pure $ litEvalResult (SetLit cs (toListWithIndex (\i _ -> Some i) cs \\ s))
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

evalField :: (MonadFail m, MonadUnliftIO m)
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

-- | Implementation of a function.
data FunctionImpl m fntp where
  FunctionImpl :: { fnImplType :: FunctionTypeRepr (FunType nm args ret)
                  , fnImplRun :: Assignment Literal args -> m (Literal ret, String)
                  } -> FunctionImpl m (FunType nm args ret)

-- | Using some lazy IO magic, a version of 'fnImplRun' which only performs
-- any inner IO actions when the its resulting value is demanded.
fnImplRunLazy :: MonadUnliftIO m => FunctionImpl m (FunType nm args ret)
              -> Assignment Literal args -> m (Literal ret, String)
fnImplRunLazy fn args =
  withRunInIO $ \run -> unsafeInterleaveIO $ run (fnImplRun fn args)


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

instance Eq (FunctionCallResult env ctx) where
  (FunctionCallResult fi args ret st) == (FunctionCallResult fi' args' ret' st')
    | Just Refl <- testEquality args args'
    , Just Refl <- testEquality ret ret'
    , Just Refl <- testEquality fi fi' = st == st'
    | otherwise = False

instance Hashable (FunctionCallResult env ctx) where
  s `hashWithSalt` (FunctionCallResult fi args ret st) =
    s `hashWithSaltF` fi `hashWithSaltF` args `hashWithSaltF` ret `hashWithSalt` st

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Lobot.TypeCheck.SecondPass
Description : The second pass of typechecking the Lobot AST.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module does the second pass of typechecking the Lobot AST.
-}

module Lobot.TypeCheck.SecondPass
  ( secondPass
  ) where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Data.Text (Text, append)
import Data.List (union)
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import Control.Monad (unless, forM_)
import Control.Monad.State (get, modify)
import Control.Monad.Except (throwError, catchError)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Data.Constraint (Dict(..))

import Lobot.Utils hiding (unzip)
import Lobot.Expr as E
import Lobot.Kind as K
import Lobot.Syntax as S
import Lobot.Types as T

import Lobot.TypeCheck.Monad
import Lobot.TypeCheck.IDecls as I
import Lobot.TypeCheck.FirstPass (tcType)


secondPass :: Assignment FunctionTypeRepr env
           -> [I.Decl]
           -> WithWarnings (Either TypeError)
                           ([Some (K.Kind env)], [Some (K.Check env)])
secondPass env ds = evalTCM $ tcDecls env ds


newtype P2Cns env ctx = P2Cns [E.Expr env ctx BoolType]

type TCM2 env = TCM (P2Cns env) TypeError


addKind :: Text -> K.Kind env tp -> EnumNameSet -> TCM2 env ()
addKind nm (K.Kind _ tp _ cns) enms =
  modify (H.insert nm (NamedKind tp (P2Cns cns) (not (null cns)) enms))

addFunction :: Text -> I.FunctionType -> TCM2 env ()
addFunction nm (I.FunType arg_tps ret_tp _ _ arg_enms ret_enms) = do
  modify (H.insert nm (NamedFunction arg_tps ret_tp (P2Cns []) (P2Cns [])
                                                    arg_enms ret_enms))

lookupKind :: Assignment T.FunctionTypeRepr env
           -> LText -> TCM2 env (Some (K.Kind env))
lookupKind env (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just (NamedKind tp (P2Cns cns) _ _) -> pure (Some (K.Kind k tp env cns))
    _ -> throwError (KindNameNotInScope (L p k))

lookupFunction :: Assignment T.FunctionTypeRepr env
               -> LText -> TCM2 env (Some (Index env), [EnumNameSet])
lookupFunction env (L p fn) = do
  mb_fn' <- H.lookup fn <$> get
  let mb_idx = findIndex (\FunctionTypeRepr{..} -> fn == symbolRepr functionName) env
  case (mb_fn', mb_idx) of
    (Just (NamedFunction _ _ _ _ arg_enms _), Just idx) -> pure (idx, arg_enms)
    _ -> throwError (FunctionNameNotInScope (L p fn))


tcDecls :: Assignment FunctionTypeRepr env
        -> [I.Decl]
        -> TCM2 env ([Some (K.Kind env)], [Some (K.Check env)])
tcDecls env ds = partitionEithers . catMaybes <$> mapM (tcDecl env) ds

tcDecl :: Assignment FunctionTypeRepr env
       -> I.Decl
       -> TCM2 env (Maybe (Either (Some (K.Kind env))
                                  (Some (K.Check env))))
tcDecl env (I.KindDecl ik) =
  Just . Left <$> tcKind env ik
tcDecl env (I.CheckDecl ick) =
  Just . Right <$> tcCheck env ick
tcDecl env (I.TypeSynDecl nm (Some tp) enms) = do
  tcDecl env (I.KindDecl (I.Kind nm tp [] [] enms))
tcDecl _env (I.FunctionDecl (L _ nm) ftp) = do
  addFunction nm ftp
  pure Nothing

tcKind :: Assignment FunctionTypeRepr env
       -> I.Kind
       -> TCM2 env (Some (K.Kind env))
tcKind env (I.Kind (L _ nm) tp cns dcns enms) = do
  cns'  <- mapM (tcExpr enms env (singleton $ SelfElem tp) T.BoolRepr) cns
  dcns' <- concat <$> mapM (resolveDerivedConstraint env tp) dcns
  let k' = K.Kind nm tp env (cns' ++ dcns')
  addKind nm k' enms
  pure $ Some k'

tcCheck :: forall env .
           Assignment FunctionTypeRepr env
        -> I.Check
        -> TCM2 env (Some (K.Check env))
tcCheck env (I.Check (L _ nm) flds cns reqs enms) = do
  let tps = fmapFC (\(I.CheckField (S.L _ nm') tp _) -> VarElem nm' tp) flds
  cns'  <- mapM (tcExpr enms env tps T.BoolRepr) cns
  let collectDCs :: Index tps tp -> CheckField tp
                 -> TCM2 env [E.Expr env tps 'T.BoolType]
      collectDCs i (CheckField _ tp dcns) = do
        kes <- concat <$> mapM (resolveDerivedConstraint env tp) dcns
        return $ giveSelf (E.VarExpr i) <$> kes
  dcns' <- traverseAndCollect collectDCs flds
  reqs' <- mapM (tcExpr enms env tps T.BoolRepr) reqs
  let namedTypes = fmapFC (\(CheckField (S.L _ fnm) tp _) -> NamedType fnm tp) flds
  let ck' = K.Check nm namedTypes env (cns' ++ dcns') reqs'
  pure $ Some ck'

-- | Using the given context of fully checked kinds, turns a
-- 'DerivedConstraint' into a list of type-checked expressions in the
-- appropriate scope.
-- NOTE: Maybe we want to do something more type-directed here in the future...
resolveDerivedConstraint :: Assignment T.FunctionTypeRepr env
                         -> T.TypeRepr tp
                         -> DerivedConstraint
                         -> TCM2 env [K.KindExpr env tp T.BoolType]

resolveDerivedConstraint env tp (FromKind (L p nm)) = do
  Some k <- lookupKind env (L p nm)
  case testEquality tp (K.kindType k) of
    Just Refl -> pure $ K.kindConstraints k
    _ -> throwError . InternalError p $
           "Malformed derived constraint: FromKind " `append` nm

resolveDerivedConstraint env tp (FromField (L p f) ds)
  | T.StructRepr ftps <- tp
  , Just (Some idx) <- findIndex (\(FieldRepr f' _) -> f == symbolRepr f') ftps
  , FieldRepr _ ftp <- ftps ! idx
    = do ds' <- concat <$> mapM (resolveDerivedConstraint env ftp) ds
         pure $ fmap (giveSelf (E.FieldExpr K.SelfExpr idx)) ds'
  | otherwise
    = throwError . InternalError p $
        "Malformed derived constraint: FromField " `append` f


-- Type inference and checking for expressions

data ContextElem (tp :: T.Type) where
  SelfElem :: T.TypeRepr tp -> ContextElem tp
  VarElem  :: Text -> T.TypeRepr tp -> ContextElem tp

ifVarElem :: (Text -> T.TypeRepr tp -> Bool) -> ContextElem tp -> Bool
ifVarElem f (VarElem nm tp) = f nm tp
ifVarElem _ _ = False

-- | ...
addlEnms :: S.LExpr -> TCM2 env EnumNameSet
addlEnms (L _ (S.ApplyExpr (L p fn) _)) = do
  mb_fn' <- H.lookup fn <$> get
  case mb_fn' of
    Just (NamedFunction _ _ _ _ _ ret_enms) -> pure ret_enms
    _ -> throwError (FunctionNameNotInScope (L p fn))
addlEnms (L _ (S.LiteralExpr (L _ (S.StructLit (Just tp) _)))) =
  (\(_,_,enms) -> enms) <$> tcType tp
addlEnms (L _ (S.FieldExpr x _)) = addlEnms x
addlEnms _ = pure HS.empty

-- | Guess, or infer, the type of an expression without any knowledge of what
-- its type should be. The 'Bool' returned is true if and only if the type
-- returned is a guess rather than an inference.
tcInferExpr :: EnumNameSet
            -> Assignment T.FunctionTypeRepr env
            -> Assignment ContextElem ctx
            -> S.LExpr
            -> TCM2 env (Bool, Pair T.TypeRepr (E.Expr env ctx))

tcInferExpr enms env _ (L _ (S.LiteralExpr l)) = do
  (isGuess, InferredLit tp l') <- tcInferLit enms env l
  pure (isGuess, Pair tp (E.LiteralExpr l'))

tcInferExpr enms env ctx (L p (S.VarExpr t))
  -- if t is an enum:
  | unLoc t `HS.member` enms
    = tcInferExpr enms env ctx (L p (S.LiteralExpr (L p (S.EnumLit t))))
  -- if we're in a kind context and t is a field name:
  | (Empty :> SelfElem (T.StructRepr ftps)) <- ctx
  , Just (Some i) <- findIndex (\(FieldRepr f _) -> unLoc t == symbolRepr f) ftps
  , FieldRepr _ tp <- ftps ! i
    = pure (False, Pair tp (E.FieldExpr (E.VarExpr baseIndex) i))
  -- if t is a variable name from the current context:
  | Just (Some i) <- findIndex (ifVarElem (\nm _ -> unLoc t == nm)) ctx
  , VarElem _ tp <- ctx ! i
    = pure (False, Pair tp (E.VarExpr i))
  -- otherwise, we error
  | otherwise
    = throwError (OtherNameNotInScope t)

tcInferExpr _ _ ctx (L p S.SelfExpr)
  | (Empty :> SelfElem tp) <- ctx = pure (False, Pair tp K.SelfExpr)
  | otherwise                     = throwError (UnexpectedSelfError p)

tcInferExpr enms env ctx (L _ (S.FieldExpr x (L p f))) = do
  (_, Pair xtp x') <- tcInferExpr enms env ctx x
  Some f' <- pure $ someSymbol f
  case xtp of
    StructRepr ftps -> case fieldIndex f' ftps of
      Just (SomeField tp i) -> pure (False, Pair tp (E.FieldExpr x' i))
      Nothing -> throwError (NoSuchFieldError (L p f) x (Some xtp))
    _ -> throwError (TypeMismatchError x (TypeString "a struct")
                                                    (Just $ SomeType xtp))

tcInferExpr enms env ctx (L _ (S.ApplyExpr fn args)) = do
  (Some fi, arg_enms) <- lookupFunction env fn
  fntp@FunctionTypeRepr{..} <- pure $ env ! fi
  mb_args' <-
    tcExprs enms env ctx functionArgTypes (reverse (zip args arg_enms))
  case mb_args' of
    Just args' -> pure (False, Pair functionRetType (E.ApplyExpr fi args'))
    Nothing -> throwError (FunctionArgLengthError fn (Some fntp) args)

tcInferExpr enms env ctx (L _ (S.EqExpr x y)) = do
  x_enms <- HS.union enms <$> addlEnms y
  y_enms <- HS.union enms <$> addlEnms x
  (xGuess, Pair xtp x') <- tcInferExpr x_enms env ctx x
  (yGuess, Pair ytp y') <- tcInferExpr y_enms env ctx y
  let uni_err = TypeUnificationError x (SomeType xtp)
                                     y (SomeType ytp)
  case (isNonAbstract xtp, isNonAbstract ytp) of
    (Just Dict, Just Dict) -> do
      case (testEquality xtp ytp, xGuess, yGuess, unifyTypes xtp ytp) of
        (Just Refl, _, _, _) ->
          pure (False, Pair T.BoolRepr (E.EqExpr x' y'))
        (Nothing, False, True, _) -> do
          y'' <- tcExpr y_enms env ctx xtp y
          pure (False, Pair T.BoolRepr (E.EqExpr x' y''))
        (Nothing, True, False, _) -> do
          x'' <- tcExpr x_enms env ctx ytp x
          pure (False, Pair T.BoolRepr (E.EqExpr x'' y'))
        (Nothing, True, True, Just (SomeNonAbsTp uni_tp)) ->
          do { x'' <- tcExpr x_enms env ctx uni_tp x
             ; y'' <- tcExpr y_enms env ctx uni_tp y
             ; pure (False, Pair T.BoolRepr (E.EqExpr x'' y''))
             } `catchError` (const $ throwError uni_err)
        _ -> throwError uni_err
    (Nothing, _) -> throwError (AbstractEqualityError x (SomeType xtp))
    (_, Nothing) -> throwError (AbstractEqualityError y (SomeType ytp))
-- we entirely leverage the previous case here
tcInferExpr enms env ctx (L p (S.NeqExpr x y)) =
  tcInferExpr enms env ctx (L p (S.EqExpr x y)) >>= \case
    (False, Pair T.BoolRepr (E.EqExpr x' y')) ->
      pure (False, Pair T.BoolRepr (E.NeqExpr x' y'))
    _ -> throwError $ InternalError p "NeqExpr!"

tcInferExpr enms env ctx (L _ (S.LteExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.LteExpr x' y'))
tcInferExpr enms env ctx (L _ (S.LtExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.LtExpr x' y'))
tcInferExpr enms env ctx (L _ (S.GteExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.GteExpr x' y'))
tcInferExpr enms env ctx (L _ (S.GtExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.GtExpr x' y'))
tcInferExpr enms env ctx (L _ (S.PlusExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.PlusExpr x' y'))
tcInferExpr enms env ctx (L _ (S.MinusExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.MinusExpr x' y'))
tcInferExpr enms env ctx (L _ (S.TimesExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.TimesExpr x' y'))
tcInferExpr enms env ctx (L _ (S.ModExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.ModExpr x' y'))
tcInferExpr enms env ctx (L _ (S.DivExpr x y)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  y' <- tcExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.DivExpr x' y'))
tcInferExpr enms env ctx (L _ (S.NegExpr x)) = do
  x' <- tcExpr enms env ctx T.IntRepr x
  pure (False, Pair T.IntRepr (E.NegExpr x'))

tcInferExpr enms env ctx (L _ (S.MemberExpr x y)) = do
  x_enms <- HS.union enms <$> addlEnms y
  y_enms <- HS.union enms <$> addlEnms x
  (xGuess, Pair xtp x') <- tcInferExpr x_enms env ctx x
  (yGuess, Pair ytp y') <- tcInferExpr y_enms env ctx y
  let uni_err = EnumSetUnificationError x (SomeType xtp)
                                        y (SomeType ytp)
  case (xtp, ytp) of
    (T.EnumRepr xcs, T.SetRepr ycs) -> do
      case (testEquality xcs ycs, xGuess, yGuess, unifyEnumNames xcs ycs) of
        (Just Refl, _, _, _) ->
          pure (False, Pair T.BoolRepr (E.MemberExpr x' y'))
        (Nothing, False, True, _) -> do
          y'' <- tcExpr y_enms env ctx (T.SetRepr xcs) y
          pure (False, Pair T.BoolRepr (E.MemberExpr x' y''))
        (Nothing, True, False, _) -> do
          x'' <- tcExpr x_enms env ctx (T.EnumRepr ycs) x
          pure (False, Pair T.BoolRepr (E.MemberExpr x'' y'))
        (Nothing, True, True, SomeNonEmptySyms uni_cs) ->
          do { x'' <- tcExpr x_enms env ctx (T.EnumRepr uni_cs) x
             ; y'' <- tcExpr y_enms env ctx (T.SetRepr  uni_cs) y
             ; pure (False, Pair T.BoolRepr (E.MemberExpr x'' y''))
             } `catchError` (const $ throwError uni_err)
        _ -> throwError uni_err
    (T.EnumRepr _, _) -> throwError (TypeMismatchError y
                                                       (TypeString "a set")
                                                       (Just $ SomeType ytp))
    (_,_) -> throwError (TypeMismatchError x
                                           (TypeString "an enum")
                                           (Just $ SomeType xtp))
-- we entirely leverage the previous case here
tcInferExpr enms env ctx (L p (S.NotMemberExpr x y)) =
  tcInferExpr enms env ctx (L p (S.MemberExpr x y)) >>= \case
    (False, Pair T.BoolRepr (E.MemberExpr x' y')) ->
      pure (False, Pair T.BoolRepr (E.NotMemberExpr x' y'))
    _ -> throwError $ InternalError p "NonMemberExpr!"

tcInferExpr enms env ctx (L _ (S.AndExpr x y)) = do
  x' <- tcExpr enms env ctx T.BoolRepr x
  y' <- tcExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.AndExpr x' y'))
tcInferExpr enms env ctx (L _ (S.OrExpr x y)) = do
  x' <- tcExpr enms env ctx T.BoolRepr x
  y' <- tcExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.OrExpr x' y'))
tcInferExpr enms env ctx (L _ (S.XorExpr x y)) = do
  x' <- tcExpr enms env ctx T.BoolRepr x
  y' <- tcExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.XorExpr x' y'))
tcInferExpr enms env ctx (L _ (S.ImpliesExpr x y)) = do
  x' <- tcExpr enms env ctx T.BoolRepr x
  y' <- tcExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.ImpliesExpr x' y'))
tcInferExpr enms env ctx (L _ (S.IffExpr x y)) = do
  x' <- tcExpr enms env ctx T.BoolRepr x
  y' <- tcExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.IffExpr x' y'))
tcInferExpr enms env ctx (L _ (S.NotExpr x)) = do
  x' <- tcExpr enms env ctx T.BoolRepr x
  pure (False, Pair T.BoolRepr (E.NotExpr x'))

tcInferExpr enms env ctx (L _ (S.IsInstanceExpr x tp)) = do
  (Some tp', dcns, enms') <- tcType tp
  x' <- tcExpr (enms `HS.union` enms') env ctx tp' x
  dcns' <- concat <$> mapM (resolveDerivedConstraint env tp') dcns
  let res = if null dcns' then E.LiteralExpr (E.BoolLit True)
                          else foldr1 E.AndExpr (giveSelf x' <$> dcns')
  pure (False, Pair T.BoolRepr res)
  
data SomeField (ftps :: Ctx (Symbol, T.Type)) (nm :: Symbol) :: * where
  SomeField :: T.TypeRepr tp -> Index ftps '(nm, tp) -> SomeField ftps nm

-- adapted from 'elemIndex'
fieldIndex :: SymbolRepr nm -> Assignment FieldRepr ftps
           -> Maybe (SomeField ftps nm)
fieldIndex nm ftps = case traverseAndCollect (go nm) ftps of
                       Left x  -> Just x
                       Right _ -> Nothing
  where go :: SymbolRepr nm -> Index ftps pr -> FieldRepr pr
           -> Either (SomeField ftps nm) ()
        go nm' i (FieldRepr nm'' tp)
          | Just Refl <- testEquality nm' nm'' = Left (SomeField tp i)
          | otherwise = Right ()


-- | Check that the type of an expression is equal to some known type.
tcExpr :: EnumNameSet
       -> Assignment T.FunctionTypeRepr env
       -> Assignment ContextElem ctx
       -> T.TypeRepr tp
       -> S.LExpr
       -> TCM2 env (E.Expr env ctx tp)
tcExpr enms env _ tp (L _ (S.LiteralExpr l)) = do
  CheckedLit l' <- tcLit enms env tp l
  return $ E.LiteralExpr l'
tcExpr enms env ctx tp x = do
  (isGuess, Pair tp' x') <- tcInferExpr enms env ctx x
  case (isGuess, testEquality tp tp') of
    -- NOTE: currently the below case never occurs
    (True, _) -> throwError (TypeInferenceError x)
    (False, Nothing) -> throwError (TypeMismatchError x (SomeType tp)
                                                      (Just $ SomeType tp'))
    (False, Just Refl) -> pure x'

-- | Check that a list of expressions have the respective types of a list of
-- types. Returns 'Nothing' if the function is given a different number of
-- terms and types.
tcExprs :: EnumNameSet
        -> Assignment T.FunctionTypeRepr env
        -> Assignment ContextElem ctx
        -> Assignment T.TypeRepr tps
        -> [(S.LExpr, EnumNameSet)]
        -> TCM2 env (Maybe (Assignment (E.Expr env ctx) tps))
tcExprs _ _ _ Empty [] = pure $ Just Empty
tcExprs enms env ctx (tps :> tp) ((x,enms'):xs) = do
  x' <- tcExpr (enms `HS.union` enms') env ctx tp x
  mxs' <- tcExprs enms env ctx tps xs
  case mxs' of
    Just xs' -> pure $ Just (xs' :> x')
    Nothing -> pure Nothing
tcExprs _ _ _ _ _ = pure Nothing


-- Type inference and checking for literals

data InferredLit where
  InferredLit :: NonAbstract tp
              => T.TypeRepr tp -> E.Literal tp -> InferredLit

-- | Guess, or infer, the type of a literal without any knowledge of what its
-- type should be. The 'Bool' returned is true if and only if the type
-- returned is a guess rather than an inference.
tcInferLit :: EnumNameSet
           -> Assignment T.FunctionTypeRepr env 
           -> S.LLiteral -> TCM2 env (Bool, InferredLit)

tcInferLit _ _ (L _ (S.BoolLit b)) = pure (False, InferredLit T.BoolRepr (E.BoolLit b))
tcInferLit _ _ (L _ (S.IntLit z))  = pure (False, InferredLit T.IntRepr  (E.IntLit z))

tcInferLit enms _ (L _ (S.EnumLit (L p e))) | Some e' <- someSymbol e = do
  unless (e `HS.member` enms) $ emitWarning (EnumNameNotInScope (L p e))
  pure (True , InferredLit (T.EnumRepr (Empty :> e'))
                          (E.EnumLit  (Empty :> e') baseIndex))

tcInferLit enms _ (L _ (S.SetLit es)) | Some es' <- someSymbols (unLoc <$> es) = do
  forM_ es $ \(L p e) ->
    unless (e `HS.member` enms) $ emitWarning (EnumNameNotInScope (L p e))
  let idxs = toListWithIndex (\i _ -> Some i) es'
  case decideLeq (knownNat @1) (ctxSizeNat (size es')) of
        Left LeqProof -> pure (True, InferredLit (T.SetRepr es')
                                                (E.SetLit  es' idxs))
                   -- we want to be able to give a guess for the type of
                   --  an empty set, but the type doesn't allow it! so we
                   --  do this awful hack...
        Right _ -> let emptySet = (Empty :> knownSymbol @"")
                    in pure (True, InferredLit (T.SetRepr emptySet)
                                              (E.SetLit emptySet []))

tcInferLit enms env (L _ (S.StructLit Nothing fvs)) = do
  (areGuesses, fvs') <- unzip <$> mapM (tcInferFieldLit enms env) fvs
  InferredFieldLits fvs'' <- pure $ toInferredFieldLits (reverse fvs')
  pure (or areGuesses, InferredLit (E.literalType (E.StructLit fvs''))
                                   (E.StructLit fvs''))
tcInferLit enms env (L p (S.StructLit (Just tp) fvs)) = do
  tcType tp >>= \case
    (Some (T.StructRepr ftps), [], enms')
     -> do
      mfvs' <- tcFieldLits (enms `HS.union` enms') env ftps (reverse fvs)
      case mfvs' of
         Just (CheckedFieldLits fvs') ->
           pure (False, InferredLit (T.StructRepr ftps) (E.StructLit fvs'))
         Nothing ->
           throwError (StructLiteralLengthError p (Some ftps) (fst <$> fvs))
    _ -> throwError (StructLiteralTypeError tp)

data CheckedLit tp where
  CheckedLit :: NonAbstract tp => E.Literal tp -> CheckedLit tp

-- | Check that the type of a literal is equal to some known type.
tcLit :: EnumNameSet
      -> Assignment T.FunctionTypeRepr env
      -> T.TypeRepr tp -> S.LLiteral -> TCM2 env (CheckedLit tp)

tcLit enms _ (T.EnumRepr cs) (L _ (S.EnumLit (L p e))) = do
  unless (e `HS.member` enms) $ emitWarning (EnumNameNotInScope (L p e))
  Some i <- enumElemIndex cs (L p e)
  pure $ CheckedLit (E.EnumLit cs i)
tcLit _ _ tp l@(L p (S.EnumLit _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr l)) (SomeType tp)
                                (Just $ TypeString "an enum"))

tcLit enms _ (T.SetRepr cs) (L _ (S.SetLit es)) = do
  forM_ es $ \(L p e) ->
    unless (e `HS.member` enms) $ emitWarning (EnumNameNotInScope (L p e))
  es' <- mapM (enumElemIndex cs) es
  pure $ CheckedLit (E.SetLit cs es')
tcLit _ _ tp l@(L p (S.SetLit _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr l)) (SomeType tp)
                                (Just $ TypeString "a set"))

tcLit enms env (T.StructRepr ftps) (L p (S.StructLit Nothing fvs)) = do
  mfvs' <- tcFieldLits enms env ftps (reverse fvs)
  case mfvs' of
   Just (CheckedFieldLits fvs') -> pure $ CheckedLit (E.StructLit fvs')
   Nothing -> throwError (StructLiteralLengthError p (Some ftps) (fst <$> fvs))
tcLit enms env tp@(T.StructRepr _) l@(L p (S.StructLit _ _)) = do
  (_, InferredLit tp' l') <- tcInferLit enms env l
  case testEquality tp tp' of
    Nothing ->
      throwError (TypeMismatchError (L p (S.LiteralExpr l))
                                    (SomeType tp)
                                    (Just $ SomeType tp'))
    Just Refl ->
      pure $ CheckedLit l'
tcLit _ _ tp l@(L p (S.StructLit _ _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr l)) (SomeType tp)
                                (Just $ TypeString "a struct"))

tcLit enms env tp l@(L p _) = do
  (isGuess, InferredLit tp' l') <- tcInferLit enms env l
  case (isGuess, testEquality tp tp') of
    (True, _) ->
      throwError (TypeInferenceError (L p (S.LiteralExpr l)))
    (False, Nothing) ->
      throwError (TypeMismatchError (L p (S.LiteralExpr l))
                                    (SomeType tp)
                                    (Just $ SomeType tp'))
    (False, Just Refl) ->
      pure (CheckedLit l')

enumElemIndex :: 1 <= CtxSize cs => Assignment SymbolRepr cs -> LText
              -> TCM2 env (Some (Index cs))
enumElemIndex cs (L p s)
  | Some s' <- someSymbol s, Just i <- elemIndex s' cs = pure (Some i)
  | otherwise = throwError $
      TypeMismatchError (L p (S.LiteralExpr (L p (S.EnumLit (L p s)))))
                        (SomeType (T.SetRepr cs)) Nothing


-- Type inference and checking for field literals

data InferredFieldLit where
  InferredFieldLit :: NonAbstract tp
                   => FieldLiteral '(nm, tp) -> InferredFieldLit

tcInferFieldLit :: EnumNameSet
              -> Assignment T.FunctionTypeRepr env 
              -> (LText, S.LLiteral) -> TCM2 env (Bool, InferredFieldLit)
tcInferFieldLit enms env (L _ s, l) = do
  (isGuess, InferredLit tp l') <- tcInferLit enms env l
  Some s' <- pure $ someSymbol s
  pure $ (isGuess, InferredFieldLit (E.FieldLiteral (FieldRepr s' tp) l'))

data InferredFieldLits where
  InferredFieldLits :: NonAbstract ftps
                   => Assignment FieldLiteral ftps
                   -> InferredFieldLits

toInferredFieldLits :: [InferredFieldLit] -> InferredFieldLits
toInferredFieldLits [] = InferredFieldLits Empty
toInferredFieldLits (InferredFieldLit fl : ifls)
  | InferredFieldLits fls <- toInferredFieldLits ifls
  = InferredFieldLits (fls :> fl)

data CheckedFieldLits ftps where
  CheckedFieldLits :: NonAbstract ftps
                 => Assignment FieldLiteral ftps
                 -> CheckedFieldLits ftps

tcFieldLits :: EnumNameSet
            -> Assignment T.FunctionTypeRepr env 
            -> Assignment T.FieldRepr ftps
            -> [(LText, S.LLiteral)]
            -> TCM2 env (Maybe (CheckedFieldLits ftps))
tcFieldLits _ _ Empty [] = pure $ Just (CheckedFieldLits Empty)
tcFieldLits enms env (ftps :> ftp@(FieldRepr s1 tp)) ((L p s2, l):fvs) = do
  unless (symbolRepr s1 == s2) $
    throwError (StructLiteralNameMismatchError (L p s2) (symbolRepr s1))
  CheckedLit l' <- tcLit enms env tp l
  let fv' = E.FieldLiteral ftp l'
  mfvs' <- tcFieldLits enms env ftps fvs
  case mfvs' of
    Just (CheckedFieldLits fvs') -> pure $ Just (CheckedFieldLits (fvs' :> fv'))
    Nothing -> pure Nothing
tcFieldLits _ _ _ _ = pure Nothing


-- Unification of type guesses

data SomeNonAbstractType where
  SomeNonAbsTp :: NonAbstract tp
               => TypeRepr tp -> SomeNonAbstractType

-- | Given guesses for the types of two terms, try to produce a single type
-- which is a valid guess for the type of both terms.
unifyTypes :: (NonAbstract tp1, NonAbstract tp2)
           => TypeRepr tp1 -> TypeRepr tp2 -> Maybe SomeNonAbstractType

unifyTypes T.BoolRepr T.BoolRepr = Just $ SomeNonAbsTp T.BoolRepr
unifyTypes T.IntRepr  T.IntRepr  = Just $ SomeNonAbsTp T.IntRepr 

unifyTypes (T.EnumRepr cs1) (T.EnumRepr cs2)
  | SomeNonEmptySyms uni_cs <- unifyEnumNames cs1 cs2
  = Just $ SomeNonAbsTp (T.EnumRepr uni_cs)
unifyTypes (T.SetRepr  cs1) (T.SetRepr  cs2)
  | SomeNonEmptySyms uni_cs <- unifyEnumNames cs1 cs2
  = Just $ SomeNonAbsTp (T.SetRepr  uni_cs)

unifyTypes (T.StructRepr ftps1) (T.StructRepr ftps2) = do
  let uni_ftps_map = H.unionWith (bind2 unifyFields) (toFieldMap ftps1)
                                                     (toFieldMap ftps2)
  SomeNonAbsFlds uni_ftps <- toNonAbstractFields <$> sequence (H.elems uni_ftps_map)
  Just $ SomeNonAbsTp (T.StructRepr uni_ftps)
  where toFieldMap :: NonAbstract ftps
                   => Assignment FieldRepr ftps
                   -> H.HashMap Text (Maybe SomeNonAbstractField)
        toFieldMap Empty = H.empty
        toFieldMap (ftps :> ftp@(FieldRepr f _))
          = H.insert (symbolRepr f) (Just (SomeNonAbsFld ftp)) (toFieldMap ftps)

unifyTypes _ _ = Nothing


data SomeNonAbstractField where
  SomeNonAbsFld :: NonAbstract tp => FieldRepr '(nm, tp)
                -> SomeNonAbstractField

-- | Given guesses for the types of two fields with the same name, try to
-- produce a single type which is a valid guess for the type of both fields.
unifyFields :: SomeNonAbstractField -> SomeNonAbstractField
            -> Maybe SomeNonAbstractField
unifyFields (SomeNonAbsFld (FieldRepr f tp1)) (SomeNonAbsFld (FieldRepr _ tp2))
  = (\(SomeNonAbsTp uni_tp) -> SomeNonAbsFld (FieldRepr f uni_tp)) <$> unifyTypes tp1 tp2

data SomeNonAbstractFields where
  SomeNonAbsFlds :: NonAbstract ftps
                 => Assignment FieldRepr ftps
                 -> SomeNonAbstractFields

toNonAbstractFields :: [SomeNonAbstractField] -> SomeNonAbstractFields
toNonAbstractFields [] = SomeNonAbsFlds Empty
toNonAbstractFields (SomeNonAbsFld ftp : ftps)
  | SomeNonAbsFlds ftps' <- toNonAbstractFields ftps
  = SomeNonAbsFlds (ftps' :> ftp)


data SomeNonEmptySymbols where
  SomeNonEmptySyms :: 1 <= CtxSize cs
                   => Assignment SymbolRepr cs -> SomeNonEmptySymbols

-- | The union of two nonempty 'Assignment SymbolRepr', ignoring empty
-- symbols (`knownSymbol @""`) unless both given assignments consist only of
-- empty symbols, in which case the resulting assignment consists only of
-- empty symbols.
unifyEnumNames :: (1 <= CtxSize cs1, 1 <= CtxSize cs2)
               => Assignment SymbolRepr cs1 -> Assignment SymbolRepr cs2
               -> SomeNonEmptySymbols
unifyEnumNames cs1 cs2 =
  let uni_cs_list = filter (\s -> s /= Some (knownSymbol @""))
                           (union (toListFC Some cs1) (toListFC Some cs2))
   in case fromList uni_cs_list of
        Some uni_cs ->
          case decideLeq (knownNat @1) (ctxSizeNat (size uni_cs)) of
            Left LeqProof -> SomeNonEmptySyms uni_cs
            Right _ -> SomeNonEmptySyms (Empty :> knownSymbol @"")

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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

import qualified Data.HashMap as H
import qualified Data.HashSet as HS

import Data.Text (Text, append)
import Data.List (union)
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import Control.Monad (when, forM_)
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.Except (throwError, catchError)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC

import Lobot.Utils hiding (unzip)
import Lobot.Expr as E
import Lobot.Kind as K
import Lobot.Syntax as S
import Lobot.Types as T

import Lobot.TypeCheck.Monad
import Lobot.TypeCheck.ISyntax as I
import Lobot.TypeCheck.FirstPass (checkType)

import Debug.Trace

secondPass :: Assignment FunctionTypeRepr env
           -> [I.Decl]
           -> WithWarnings (Either TypeError)
                           ([Some (K.Kind env)], [Some (K.Check env)])
secondPass env ds =
  evalStateT (partitionEithers . catMaybes <$> mapM (checkIDecl env) ds)
             H.empty


newtype P2Cns env ctx = P2Cns [E.Expr env ctx BoolType]

type CtxM2 env = CtxM (P2Cns env) TypeError


addKind :: Text -> K.Kind env tp -> EnumNameSet -> CtxM2 env ()
addKind nm (K.Kind _ tp _ cns) enms =
  modify (H.insert nm (NamedKind tp (P2Cns cns) (not (null cns)) enms))

addFunction :: Text -> I.FunctionType -> CtxM2 env ()
addFunction nm (I.FunType arg_tps ret_tp _ _ arg_enms ret_enms) = do
  modify (H.insert nm (NamedFunction arg_tps ret_tp (P2Cns []) (P2Cns [])
                                                    arg_enms ret_enms))

lookupKind :: Assignment T.FunctionTypeRepr env
           -> LText -> CtxM2 env (Some (K.Kind env))
lookupKind env (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just (NamedKind tp (P2Cns cns) _ _) -> pure (Some (K.Kind k tp env cns))
    _ -> throwError (KindNameNotInScope (L p k))

lookupFunction :: Assignment T.FunctionTypeRepr env
               -> LText -> CtxM2 env (Some (Index env), [EnumNameSet])
lookupFunction env (L p fn) = do
  mb_fn' <- H.lookup fn <$> get
  let mb_idx = findIndex (\FunctionTypeRepr{..} -> fn == symbolRepr functionName) env
  case (mb_fn', mb_idx) of
    (Just (NamedFunction _ _ _ _ arg_enms _), Just idx) -> pure (idx, arg_enms)
    _ -> throwError (FunctionNameNotInScope $ trace "here" (L p fn))


checkIDecl :: Assignment FunctionTypeRepr env
           -> I.Decl
           -> CtxM2 env (Maybe (Either (Some (K.Kind env))
                                       (Some (K.Check env))))
checkIDecl env (I.KindDecl ik) =
  Just . Left <$> checkIKind env ik
checkIDecl env (I.CheckDecl ick) =
  Just . Right <$> checkICheck env ick
checkIDecl env (I.TypeSynDecl (L _ nm) (Some tp) enms) = do
  addKind nm (K.Kind nm tp env []) enms
  pure Nothing
checkIDecl _env (I.FunctionDecl (L _ nm) ftp) = do
  addFunction nm ftp
  pure Nothing

checkIKind :: Assignment FunctionTypeRepr env
           -> I.Kind
           -> CtxM2 env (Some (K.Kind env))
checkIKind env (I.Kind (L _ nm) tp cns dcns enms) = do
  cns'  <- mapM (checkExpr enms env (singleton $ SelfElem tp) T.BoolRepr) cns
  dcns' <- concat <$> mapM (resolveDerivedConstraint env tp) dcns
  let k' = K.Kind nm tp env (cns' ++ dcns')
  addKind nm k' enms
  pure $ Some k'

checkICheck :: forall env .
               Assignment FunctionTypeRepr env
            -> I.Check
            -> CtxM2 env (Some (K.Check env))
checkICheck env (I.Check (L _ nm) flds cns reqs enms) = do
  let tps = fmapFC (\(I.CheckField (S.L _ nm') tp _) -> VarElem nm' tp) flds
  cns'  <- mapM (checkExpr enms env tps T.BoolRepr) cns
  let collectDCs :: Index tps tp -> CheckField tp -> CtxM2 env [E.Expr env tps 'T.BoolType]
      collectDCs i (CheckField _ tp dcns) = do
        kes <- concat <$> mapM (resolveDerivedConstraint env tp) dcns
        return $ giveSelf (E.VarExpr i) <$> kes
  dcns' <- traverseAndCollect collectDCs flds
  reqs' <- mapM (checkExpr enms env tps T.BoolRepr) reqs
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
                         -> CtxM2 env [K.KindExpr env tp T.BoolType]

resolveDerivedConstraint env tp (FromKind (L p nm)) = do
  Some k <- lookupKind env (L p nm)
  case testEquality tp (K.kindType k) of
    Just Refl -> pure $ K.kindConstraints k
    _ -> throwError (InternalError p $ "Malformed derived constraint: FromKind " `append` nm)

resolveDerivedConstraint env tp (FromField (L p f) ds)
  | T.StructRepr ftps <- tp
  , Just (Some idx) <- findIndex (\(FieldRepr f' _) -> f == symbolRepr f') ftps
  , FieldRepr _ ftp <- ftps ! idx
    = do ds' <- concat <$> mapM (resolveDerivedConstraint env ftp) ds
         pure $ fmap (giveSelf (E.FieldExpr K.SelfExpr idx)) ds'
  | otherwise
    = throwError (InternalError p $ "Malformed derived constraint: FromField " `append` f)


-- Type inference and checking for expressions

data ContextElem (tp :: T.Type) where
  SelfElem :: T.TypeRepr tp -> ContextElem tp
  VarElem  :: Text -> T.TypeRepr tp -> ContextElem tp

ifVarElem :: (Text -> T.TypeRepr tp -> Bool) -> ContextElem tp -> Bool
ifVarElem f (VarElem nm tp) = f nm tp
ifVarElem _ _ = False

-- | ...
addlEnms :: S.LExpr -> CtxM2 env EnumNameSet
addlEnms (L _ (S.ApplyExpr (L p fn) _)) = do
  mb_fn' <- H.lookup fn <$> get
  case mb_fn' of
    Just (NamedFunction _ _ _ _ _ ret_enms) -> pure ret_enms
    _ -> throwError (FunctionNameNotInScope $ trace "there" (L p fn))
addlEnms (L _ (S.LiteralExpr (L _ (S.StructLit (Just tp) _)))) =
  (\(_,_,enms) -> enms) <$> checkType tp
addlEnms (L _ (S.FieldExpr x _)) = addlEnms x
addlEnms _ = pure HS.empty

-- | Guess, or infer, the type of an expression without any knowledge of what
-- its type should be. The 'Bool' returned is true if and only if the type
-- returned is a guess rather than an inference.
guessExpr :: EnumNameSet
          -> Assignment T.FunctionTypeRepr env
          -> Assignment ContextElem ctx
          -> S.LExpr
          -> CtxM2 env (Bool, Pair T.TypeRepr (E.Expr env ctx))

guessExpr enms env _ (L _ (S.LiteralExpr l)) = do
  (isGuess, GuessedLit tp l') <- guessLit enms env l
  pure (isGuess, Pair tp (E.LiteralExpr l'))

guessExpr enms env ctx (L p (S.VarExpr t))
  -- if t is an enum:
  | unLoc t `HS.member` enms
    = guessExpr enms env ctx (L p (S.LiteralExpr (L p (S.EnumLit t))))
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

guessExpr _ _ ctx (L p S.SelfExpr)
  | (Empty :> SelfElem tp) <- ctx = pure (False, Pair tp K.SelfExpr)
  | otherwise                     = throwError (UnexpectedSelfError p)

guessExpr enms env ctx (L _ (S.FieldExpr x (L p f))) = do
  (_, Pair xtp x') <- guessExpr enms env ctx x
  Some f' <- pure $ someSymbol f
  case xtp of
    StructRepr ftps -> case fieldIndex f' ftps of
      Just (SomeField tp i) -> pure (False, Pair tp (E.FieldExpr x' i))
      Nothing -> throwError (NoSuchFieldError (L p f) x (Some xtp))
    _ -> throwError (TypeMismatchError x (TypeString "a struct")
                                                    (Just $ SomeType xtp))

guessExpr enms env ctx (L _ (S.ApplyExpr fn args)) = do
  (Some fi, arg_enms) <- lookupFunction env fn
  fntp@FunctionTypeRepr{..} <- pure $ env ! fi
  mb_args' <-
    checkExprs enms env ctx functionArgTypes (reverse (zip args arg_enms))
  case mb_args' of
    Just args' -> pure (False, Pair functionRetType (E.ApplyExpr fi args'))
    Nothing -> throwError (FunctionArgLengthError fn (Some fntp) args)

guessExpr enms env ctx (L _ (S.EqExpr x y)) = do
  x_enms <- HS.union enms <$> addlEnms y
  y_enms <- HS.union enms <$> addlEnms x
  (xGuess, Pair xtp x') <- guessExpr x_enms env ctx x
  (yGuess, Pair ytp y') <- guessExpr y_enms env ctx y
  let uni_err = TypeUnificationError x (SomeType xtp)
                                     y (SomeType ytp)
  case (isAbstractType xtp, isAbstractType ytp) of
    (FalseRepr, FalseRepr) -> do
      case (testEquality xtp ytp, xGuess, yGuess, unifyTypes xtp ytp) of
        (Just Refl, _, _, _) ->
          pure (False, Pair T.BoolRepr (E.EqExpr x' y'))
        (Nothing, False, True, _) -> do
          y'' <- checkExpr y_enms env ctx xtp y
          pure (False, Pair T.BoolRepr (E.EqExpr x' y''))
        (Nothing, True, False, _) -> do
          x'' <- checkExpr x_enms env ctx ytp x
          pure (False, Pair T.BoolRepr (E.EqExpr x'' y'))
        (Nothing, True, True, Just (SomeNonAbsTp uni_tp)) ->
          do { x'' <- checkExpr x_enms env ctx uni_tp x
             ; y'' <- checkExpr y_enms env ctx uni_tp y
             ; pure (False, Pair T.BoolRepr (E.EqExpr x'' y''))
             } -- `catchError` (const . trace "hi" $ throwError uni_err)
        _ -> throwError uni_err
    (TrueRepr, _) -> throwError (AbstractEqualityError x (SomeType xtp))
    (_, TrueRepr) -> throwError (AbstractEqualityError y (SomeType ytp))

guessExpr enms env ctx (L _ (S.LteExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.LteExpr x' y'))
guessExpr enms env ctx (L _ (S.LtExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.LtExpr x' y'))
guessExpr enms env ctx (L _ (S.GteExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.GteExpr x' y'))
guessExpr enms env ctx (L _ (S.GtExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.GtExpr x' y'))
guessExpr enms env ctx (L _ (S.PlusExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.PlusExpr x' y'))
guessExpr enms env ctx (L _ (S.MinusExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.MinusExpr x' y'))
guessExpr enms env ctx (L _ (S.TimesExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.TimesExpr x' y'))
guessExpr enms env ctx (L _ (S.ModExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.ModExpr x' y'))
guessExpr enms env ctx (L _ (S.DivExpr x y)) = do
  x' <- checkExpr enms env ctx T.IntRepr x
  y' <- checkExpr enms env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.DivExpr x' y'))

guessExpr enms env ctx (L _ (S.MemberExpr x y)) = do
  x_enms <- HS.union enms <$> addlEnms y
  y_enms <- HS.union enms <$> addlEnms x
  (xGuess, Pair xtp x') <- guessExpr x_enms env ctx x
  (yGuess, Pair ytp y') <- guessExpr y_enms env ctx y
  let uni_err = EnumSetUnificationError x (SomeType xtp)
                                        y (SomeType ytp)
  case (xtp, ytp) of
    (T.EnumRepr xcs, T.SetRepr ycs) -> do
      case (testEquality xcs ycs, xGuess, yGuess, unifyEnumNames xcs ycs) of
        (Just Refl, _, _, _) ->
          pure (False, Pair T.BoolRepr (E.MemberExpr x' y'))
        (Nothing, False, True, _) -> do
          y'' <- checkExpr y_enms env ctx (T.SetRepr xcs) y
          pure (False, Pair T.BoolRepr (E.MemberExpr x' y''))
        (Nothing, True, False, _) -> do
          x'' <- checkExpr x_enms env ctx (T.EnumRepr ycs) x
          pure (False, Pair T.BoolRepr (E.MemberExpr x'' y'))
        (Nothing, True, True, SomeNonEmptySyms uni_cs) ->
          do { x'' <- checkExpr x_enms env ctx (T.EnumRepr uni_cs) x
             ; y'' <- checkExpr y_enms env ctx (T.SetRepr  uni_cs) y
             ; pure (False, Pair T.BoolRepr (E.MemberExpr x'' y''))
             } `catchError` (const $ throwError uni_err)
        _ -> throwError uni_err
    (T.EnumRepr _, _) -> throwError (TypeMismatchError y
                                                       (TypeString "a set")
                                                       (Just $ SomeType ytp))
    (_,_) -> throwError (TypeMismatchError x
                                           (TypeString "an enum")
                                           (Just $ SomeType xtp))

guessExpr enms env ctx (L _ (S.AndExpr x y)) = do
  x' <- checkExpr enms env ctx T.BoolRepr x
  y' <- checkExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.AndExpr x' y'))
guessExpr enms env ctx (L _ (S.OrExpr x y)) = do
  x' <- checkExpr enms env ctx T.BoolRepr x
  y' <- checkExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.OrExpr x' y'))
guessExpr enms env ctx (L _ (S.XorExpr x y)) = do
  x' <- checkExpr enms env ctx T.BoolRepr x
  y' <- checkExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.XorExpr x' y'))
guessExpr enms env ctx (L _ (S.ImpliesExpr x y)) = do
  x' <- checkExpr enms env ctx T.BoolRepr x
  y' <- checkExpr enms env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.ImpliesExpr x' y'))
guessExpr enms env ctx (L _ (S.NotExpr x)) = do
  x' <- checkExpr enms env ctx T.BoolRepr x
  pure (False, Pair T.BoolRepr (E.NotExpr x'))

guessExpr enms env ctx (L _ (S.IsInstanceExpr x tp)) = do
  (Some tp', dcns, enms') <- checkType tp
  x' <- checkExpr (enms `HS.union` enms') env ctx tp' x
  dcns' <- concat <$> mapM (resolveDerivedConstraint env tp') dcns
  let res = if null dcns' then E.LiteralExpr (E.BoolLit True)
                          else foldr1 E.AndExpr (giveSelf x' <$> dcns')
  pure (False, Pair T.BoolRepr res)
  
data SomeField (ftps :: Ctx (Symbol, T.Type)) (nm :: Symbol) :: * where
  SomeField :: T.TypeRepr tp -> Index ftps '(nm, tp) -> SomeField ftps nm

-- adapted from 'elemIndex'
fieldIndex :: SymbolRepr nm -> Assignment FieldRepr ftps -> Maybe (SomeField ftps nm)
fieldIndex nm ftps = case traverseAndCollect (go nm) ftps of
                       Left x  -> Just x
                       Right _ -> Nothing
  where go :: SymbolRepr nm -> Index ftps pr -> FieldRepr pr -> Either (SomeField ftps nm) ()
        go nm' i (FieldRepr nm'' tp)
          | Just Refl <- testEquality nm' nm'' = Left (SomeField tp i)
          | otherwise = Right ()


-- | Check that the type of an expression is equal to some known type.
checkExpr :: EnumNameSet
          -> Assignment T.FunctionTypeRepr env
          -> Assignment ContextElem ctx
          -> T.TypeRepr tp
          -> S.LExpr
          -> CtxM2 env (E.Expr env ctx tp)
checkExpr enms env _ tp (L _ (S.LiteralExpr l)) = do
  CheckedLit l' <- checkLit enms env tp l
  return $ E.LiteralExpr l'
checkExpr enms env ctx tp x = do
  (isGuess, Pair tp' x') <- guessExpr enms env ctx x
  case (isGuess, testEquality tp tp') of
    -- NOTE: currently the below case never occurs
    (True, _) -> throwError (TypeInferenceError x)
    (False, Nothing) -> throwError (TypeMismatchError x (SomeType tp)
                                                      (Just $ SomeType tp'))
    (False, Just Refl) -> pure x'

-- | Check that a list of expressions have the respective types of a list of
-- types. Returns 'Nothing' if the function is given a different number of
-- terms and types.
checkExprs :: EnumNameSet
           -> Assignment T.FunctionTypeRepr env
           -> Assignment ContextElem ctx
           -> Assignment T.TypeRepr tps
           -> [(S.LExpr, EnumNameSet)]
           -> CtxM2 env (Maybe (Assignment (E.Expr env ctx) tps))
checkExprs _ _ _ Empty [] = pure $ Just Empty
checkExprs enms env ctx (tps :> tp) ((x,enms'):xs) = do
  x' <- checkExpr (enms `HS.union` enms') env ctx tp x
  mxs' <- checkExprs enms env ctx tps xs
  case mxs' of
    Just xs' -> pure $ Just (xs' :> x')
    Nothing -> pure Nothing
checkExprs _ _ _ _ _ = pure Nothing


-- Type inference and checking for literals

data GuessedLit where
  GuessedLit :: IsAbstractType tp ~ 'False
             => T.TypeRepr tp -> E.Literal tp -> GuessedLit

-- | Guess, or infer, the type of a literal without any knowledge of what its
-- type should be. The 'Bool' returned is true if and only if the type
-- returned is a guess rather than an inference.
guessLit :: EnumNameSet
         -> Assignment T.FunctionTypeRepr env 
         -> S.LLiteral -> CtxM2 env (Bool, GuessedLit)

guessLit _ _ (L _ (S.BoolLit b)) = pure (False, GuessedLit T.BoolRepr (E.BoolLit b))
guessLit _ _ (L _ (S.IntLit z))  = pure (False, GuessedLit T.IntRepr  (E.IntLit z))

guessLit enms _ (L _ (S.EnumLit (L p e))) | Some e' <- someSymbol e = do
  when (e `HS.notMember` enms) $ emitWarning (EnumNameNotInScope (L p e))
  pure (True , GuessedLit (T.EnumRepr (Empty :> e'))
                          (E.EnumLit  (Empty :> e') baseIndex))
guessLit enms _ (L _ (S.SetLit es)) | Some es' <- someSymbols (unLoc <$> es) = do
  forM_ es $ \(L p e) ->
    when (e `HS.notMember` enms) $ emitWarning (EnumNameNotInScope (L p e))
  let idxs = toListWithIndex (\i _ -> Some i) es'
  case decideLeq (knownNat @1) (ctxSizeNat (size es')) of
        Left LeqProof -> pure (True, GuessedLit (T.SetRepr es')
                                                (E.SetLit  es' idxs))
                   -- we want to be able to give a guess for the type of
                   --  an empty set, but the type doesn't allow it! so we
                   --  do this awful hack...
        Right _ -> let emptySet = (Empty :> knownSymbol @"")
                    in pure (True, GuessedLit (T.SetRepr emptySet)
                                              (E.SetLit emptySet []))
  
guessLit enms env (L _ (S.StructLit Nothing fvs)) = do
  (areGuesses, fvs') <- unzip <$> mapM (guessFieldLit enms env) fvs
  GuessedFieldLits fvs'' <- pure $ toGuessedFieldLits (reverse fvs')
  pure (or areGuesses, GuessedLit (E.literalType (E.StructLit fvs'')) (E.StructLit fvs''))
guessLit enms env (L p (S.StructLit (Just tp) fvs)) = do
  (Some tp', _dcns, enms') <- checkType tp
  -- TODO: Check that this is a valid instance given dcns?
  case tp' of
    T.StructRepr ftps -> do
      mfvs' <- checkFieldLits (enms `HS.union` enms') env ftps (reverse fvs)
      case mfvs' of
         Just (CheckedFieldLits fvs') ->
           pure (False, GuessedLit (T.StructRepr ftps) (E.StructLit fvs'))
         Nothing ->
           throwError (StructLiteralLengthError p (Some ftps) (fst <$> fvs))
    _ -> throwError (StructLiteralTypeError tp)

data CheckedLit tp where
  CheckedLit :: IsAbstractType tp ~ 'False => E.Literal tp -> CheckedLit tp

-- | Check that the type of a literal is equal to some known type.
checkLit :: EnumNameSet
         -> Assignment T.FunctionTypeRepr env
         -> T.TypeRepr tp -> S.LLiteral -> CtxM2 env (CheckedLit tp)

checkLit enms _ (T.EnumRepr cs) (L _ (S.EnumLit (L p e))) = do
  when (e `HS.notMember` enms) $ emitWarning (EnumNameNotInScope (L p e))
  Some i <- enumElemIndex cs (L p e)
  pure $ CheckedLit (E.EnumLit cs i)
checkLit _ _ tp l@(L p (S.EnumLit _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr l)) (SomeType tp)
                                (Just $ TypeString "an enum"))
checkLit enms _ (T.SetRepr cs) (L _ (S.SetLit es)) = do
  forM_ es $ \(L p e) ->
    when (e `HS.notMember` enms) $ emitWarning (EnumNameNotInScope (L p e))
  es' <- mapM (enumElemIndex cs) es
  pure $ CheckedLit (E.SetLit cs es')
checkLit _ _ tp l@(L p (S.SetLit _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr l)) (SomeType tp)
                                (Just $ TypeString "a set"))

checkLit enms env (T.StructRepr ftps) (L p (S.StructLit Nothing fvs)) = do
  mfvs' <- checkFieldLits enms env ftps (reverse fvs)
  case mfvs' of
   Just (CheckedFieldLits fvs') -> pure $ CheckedLit (E.StructLit fvs')
   Nothing -> throwError (StructLiteralLengthError p (Some ftps) (fst <$> fvs))
checkLit enms env tp@(T.StructRepr _) l@(L p (S.StructLit _ _)) = do
  (_, GuessedLit tp' l') <- guessLit enms env l
  case testEquality tp tp' of
    Nothing ->
      throwError (TypeMismatchError (L p (S.LiteralExpr l))
                                    (SomeType tp)
                                    (Just $ SomeType tp'))
    Just Refl ->
      pure $ CheckedLit l'
checkLit _ _ tp l@(L p (S.StructLit _ _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr l)) (SomeType tp)
                                (Just $ TypeString "a struct"))

checkLit enms env tp l@(L p _) = do
  (isGuess, GuessedLit tp' l') <- guessLit enms env l
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
              -> CtxM2 env (Some (Index cs))
enumElemIndex cs (L p s)
  | Some s' <- someSymbol s, Just i <- elemIndex s' cs = pure (Some i)
  | otherwise = throwError (TypeMismatchError (L p (S.LiteralExpr (L p (S.EnumLit (L p s)))))
                                              (SomeType (T.SetRepr cs)) Nothing)


-- Type inference and checking for field literals

data GuessedFieldLit where
  GuessedFieldLit :: IsAbstractType tp ~ 'False
                  => FieldLiteral '(nm, tp) -> GuessedFieldLit

guessFieldLit :: EnumNameSet
              -> Assignment T.FunctionTypeRepr env 
              -> (LText, S.LLiteral) -> CtxM2 env (Bool, GuessedFieldLit)
guessFieldLit enms env (L _ s, l) = do
  (isGuess, GuessedLit _ l') <- guessLit enms env l
  Some s' <- pure $ someSymbol s
  pure $ (isGuess, GuessedFieldLit (E.FieldLiteral s' l'))

data GuessedFieldLits where
  GuessedFieldLits :: AnyAbstractFields ftps ~ 'False
                   => Assignment FieldLiteral ftps
                   -> GuessedFieldLits

toGuessedFieldLits :: [GuessedFieldLit] -> GuessedFieldLits
toGuessedFieldLits [] = GuessedFieldLits Empty
toGuessedFieldLits (GuessedFieldLit fl : ifls)
  | GuessedFieldLits fls <- toGuessedFieldLits ifls
  = GuessedFieldLits (fls :> fl)

data CheckedFieldLits ftps where
  CheckedFieldLits :: AnyAbstractFields ftps ~ 'False
                 => Assignment FieldLiteral ftps
                 -> CheckedFieldLits ftps

checkFieldLits :: EnumNameSet
               -> Assignment T.FunctionTypeRepr env 
               -> Assignment T.FieldRepr ftps
               -> [(LText, S.LLiteral)]
               -> CtxM2 env (Maybe (CheckedFieldLits ftps))
checkFieldLits _ _ Empty [] = pure $ Just (CheckedFieldLits Empty)
checkFieldLits enms env (ftps :> FieldRepr s1 ftp) ((L p s2, l):fvs) = do
  when (symbolRepr s1 /= s2) $
    throwError (StructLiteralNameMismatchError (L p s2) (symbolRepr s1))
  CheckedLit l' <- checkLit enms env ftp l
  let fv' = E.FieldLiteral s1 l'
  mfvs' <- checkFieldLits enms env ftps fvs
  case mfvs' of
    Just (CheckedFieldLits fvs') -> pure $ Just (CheckedFieldLits (fvs' :> fv'))
    Nothing -> pure Nothing
checkFieldLits _ _ _ _ = pure Nothing


-- Unification of type guesses

data SomeNonAbstractType where
  SomeNonAbsTp :: IsAbstractType tp ~ 'False
               => TypeRepr tp -> SomeNonAbstractType

-- | Given guesses for the types of two terms, try to produce a single type
-- which is a valid guess for the type of both terms.
unifyTypes :: (IsAbstractType tp1 ~ 'False, IsAbstractType tp2 ~ 'False)
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
  where toFieldMap :: AnyAbstractFields ftps ~ 'False
                   => Assignment FieldRepr ftps
                   -> H.Map Text (Maybe SomeNonAbstractField)
        toFieldMap Empty = H.empty
        toFieldMap (ftps :> ftp@(FieldRepr f tp))
          | FalseRepr <- isAbstractType tp
          = H.insert (symbolRepr f) (Just (SomeNonAbsFld ftp)) (toFieldMap ftps)
          | otherwise = error "FIXME: Why can't GHC figure out this is impossible?"

unifyTypes _ _ = Nothing


data SomeNonAbstractField where
  SomeNonAbsFld :: IsAbstractType tp ~ 'False => FieldRepr '(nm, tp)
                -> SomeNonAbstractField

-- | Given guesses for the types of two fields with the same name, try to
-- produce a single type which is a valid guess for the type of both fields.
unifyFields :: SomeNonAbstractField -> SomeNonAbstractField
            -> Maybe SomeNonAbstractField
unifyFields (SomeNonAbsFld (FieldRepr f tp1)) (SomeNonAbsFld (FieldRepr _ tp2))
  = (\(SomeNonAbsTp uni_tp) -> SomeNonAbsFld (FieldRepr f uni_tp)) <$> unifyTypes tp1 tp2

data SomeNonAbstractFields where
  SomeNonAbsFlds :: AnyAbstractFields ftps ~ 'False
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

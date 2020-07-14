{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lobot.TypeCheck.FirstPass
Description : The first pass of typechecking the Lobot AST.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module does the first pass of typechecking the Lobot AST, which involves
checking all types (even those in expressions), checking all function types,
and scope checking enum names. The result is the function type envionment
of the file, and a list of 'I.Kinds'. An 'I.Kind' is a `TypeRepr`, a list of
untyped constraints ('[S.LExpr]') and a list derived constraints
('[DerivedConstraint]'). The derived constraints will be unfolded in the next
pass.
-}

module Lobot.TypeCheck.FirstPass
  ( firstPass
  , FirstPassResult(..)
  ) where

import qualified Data.HashMap as H
import qualified Data.HashSet as HS

import Data.Text (Text)
import Data.Traversable (forM)
import Control.Monad (when, forM_)
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.Except (throwError)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC

import Lobot.Utils hiding (unzip)
import Lobot.Kind as K
import Lobot.Syntax as S
import Lobot.Types as T

import Lobot.TypeCheck.Monad
import qualified Lobot.TypeCheck.ISyntax as I


data FirstPassResult where
  FirstPassResult :: Assignment FunctionTypeRepr env
                  -> [I.Kind]
                  -> [I.Check]
                  -> H.Map Text (Some (K.Kind env))
                  -> FirstPassResult

firstPass :: [S.Decl]
          -> WithWarnings (Either TypeError) FirstPassResult
firstPass ds = do
  (Some env, ks, cks, tpsyns) <- evalStateT (checkDecls ds) H.empty
  let tpsyns' = H.mapWithKey (\nm (Some tp) -> Some (K.Kind nm tp env [])) tpsyns
  pure (FirstPassResult env ks cks tpsyns')


type CtxM1 = CtxM NamedThing TypeError

data NamedThing = NamedKind I.Kind EnumNameSet
                | NamedFunction I.FunctionType [EnumNameSet]
                deriving Show

type EnumNameSet = HS.Set Text

addKind :: I.Kind -> EnumNameSet -> CtxM1 ()
addKind ik@I.Kind{ kindName = (L p nm) } enms = do
  is_in <- H.member nm <$> get
  if is_in then throwError (KindNameAlreadyDefined (L p nm))
           else modify (H.insert nm (NamedKind ik enms))

addAbsType :: LText -> Some TypeRepr -> CtxM1 ()
addAbsType nm (Some tp) = addKind (I.Kind nm tp [] []) HS.empty

lookupKind :: LText -> CtxM1 (I.Kind, EnumNameSet)
lookupKind (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just (NamedKind k' enms) -> pure (k', enms)
    _ -> throwError (KindNameNotInScope (L p k))

addFunction :: LText -> I.FunctionType -> [EnumNameSet] -> CtxM1 ()
addFunction (L p nm) ftp enms = do
  is_in <- H.member nm <$> get
  if is_in then throwError (KindNameAlreadyDefined (L p nm))
           else modify (H.insert nm (NamedFunction ftp enms))

lookupFunction :: LText -> CtxM1 (I.FunctionType, [EnumNameSet])
lookupFunction (L p f) = do
  mb_f' <- H.lookup f <$> get
  case mb_f' of
    Just (NamedFunction f' enms) -> pure (f', enms)
    _ -> throwError (FunctionNameNotInScope (L p f))


checkDecls :: [S.Decl]
           -> CtxM1 ( Some (Assignment FunctionTypeRepr)
                    , [I.Kind]
                    , [I.Check]
                    , H.Map Text (Some TypeRepr) )
checkDecls [] = pure (Some Empty, [], [], H.empty)
checkDecls (d:ds) = do
  to_add <- checkDecl d
  (Some ftps, ks, cks, tpsyns) <- checkDecls ds
  case to_add of
    AddFunType (Some ftp) -> pure (Some $ ftps :> ftp, ks, cks, tpsyns)
    AddIKind k'           -> pure (Some ftps, k':ks, cks, tpsyns)
    AddICheck ck'         -> pure (Some ftps, ks, ck':cks, tpsyns)
    AddTypeSyn nm tp      -> pure (Some ftps, ks, cks, H.insert nm tp tpsyns)
    AddNothing            -> pure (Some ftps, ks, cks, tpsyns)

data ToAdd = AddFunType (Some FunctionTypeRepr)
           | AddIKind I.Kind
           | AddICheck I.Check
           | AddTypeSyn Text (Some TypeRepr)
           | AddNothing

checkDecl :: S.Decl -> CtxM1 ToAdd

checkDecl (S.KindDecl k) = do
  (Some tp, dcns, enms) <- checkType (S.kindType k)
  cns <- mapM (checkExpr enms (Empty :> SelfElem tp)) (S.kindConstraints k)
  let k' = I.Kind (S.kindName k) tp cns dcns
  addKind k' enms
  pure $ AddIKind k'

checkDecl (S.CheckDecl ck) = do
  -- [(Some TypeRepr, [DerivedConstraint], EnumNameSet)]
  -- First, compute the types
  checkTypeResults <- traverse checkType (snd <$> S.checkFields ck)
  namedTypes' <- forM (zip (S.checkFields ck) checkTypeResults) $ \((nm, _), (Some tp, dcns, _)) -> do
    return $ Some $ I.CheckField nm tp dcns
  Some namedTypes <- return $ fromList namedTypes'
  -- let tps = fmapFC namedTypeType cftps
  -- Next, compute the constraints
  let enms = mconcat (thd3 <$> checkTypeResults)
  cns <- mapM (checkExpr enms (fmapFC (\(I.CheckField (S.L _ nm) tp _) -> VarElem nm tp) namedTypes)) (S.checkConstraints ck)
  reqs <- mapM (checkExpr enms (fmapFC (\(I.CheckField (S.L _ nm) tp _) -> VarElem nm tp) namedTypes)) (S.checkRequirements ck)
  let ck' = I.Check (S.checkName ck) namedTypes cns reqs
  pure $ AddICheck ck'

checkDecl (S.TypeSynDecl nm tp) = do
  (Some tp', dcns, enms) <- checkType tp
  let k' = I.Kind nm tp' [] dcns
  addKind k' enms
  pure $ AddIKind k'

checkDecl (S.AbsTypeDecl nm) | Some nmSymb <- someSymbol (unLoc nm) = do
  let tp = T.AbsRepr nmSymb
  addAbsType nm (Some tp)
  pure $ AddTypeSyn (unLoc nm) (Some tp)

checkDecl (S.AbsFunctionDecl nm ftp) = do
  (ftp', enms) <- checkFunctionType ftp
  addFunction nm ftp' enms
  pure $ AddFunType (I.funType ftp')


checkType :: S.LType
          -> CtxM1 (Some TypeRepr, [I.DerivedConstraint], EnumNameSet)

checkType (L _ S.BoolType) = pure (Some T.BoolRepr, [], HS.empty)
checkType (L _ S.IntType)  = pure (Some T.IntRepr , [], HS.empty)

checkType tp@(L _ (S.EnumType cs)) | Some cs' <- someSymbols cs = do
  ensureUnique id cs (DuplicateEnumNameError tp)
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure (Some (T.EnumRepr cs'), [], HS.fromList cs)
    Right _ -> throwError (EmptyEnumOrSetError tp)
checkType tp@((L _ (S.SetType cs))) | Some cs' <- someSymbols cs = do
  ensureUnique id cs (DuplicateEnumNameError tp)
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure (Some (T.SetRepr cs'), [], HS.fromList cs)
    Right _ -> throwError (EmptyEnumOrSetError tp)

checkType (L _ (S.StructType fls)) = do
  (Some fls', dcns, enms) <- mapFst3 fromList . unzip3 <$> mapM checkFieldType fls
  ensureUnique unLoc (fmap fst fls) FieldNameAlreadyDefined
  pure (Some (T.StructRepr fls'), dcns, HS.unions enms)

checkType (L p (S.KindNames [])) = throwError (InternalError p "empty kind union")
checkType (L _ (S.KindNames [k])) = do
  (I.Kind _ tp _ _, enms) <- lookupKind k
  pure (Some tp, [I.FromKind k], enms)
checkType (L p (S.KindNames (k:ks))) = do
  (I.Kind _ k_tp _ _, k_enms) <- lookupKind k
  (Some ks_tp, dcns, ks_enms) <- checkType (L p (S.KindNames ks))
  case testEquality k_tp ks_tp of
    Just Refl -> pure (Some ks_tp, (I.FromKind k):dcns, k_enms `HS.union` ks_enms)
    Nothing -> throwError (KindUnionMismatchError k (Some ks_tp) (Some k_tp))

checkFieldType :: (LText, S.LType)
               -> CtxM1 (Some FieldRepr, I.DerivedConstraint, EnumNameSet)
checkFieldType (f, tp) = do
  Some f' <- pure $ someSymbol (unLoc f)
  (Some tp', dcns, enms) <- checkType tp
  pure (Some (FieldRepr f' tp'), I.FromField f dcns, enms)


checkFunctionType :: S.FunctionType -> CtxM1 (I.FunctionType, [EnumNameSet])
checkFunctionType (S.FunType (L _ fn) arg_tps ret_tp) = do
  Some fn' <- pure $ someSymbol fn
  (Some arg_tps', arg_dcns, arg_enms) <- mapFst3 fromList . unzip3 <$>
                                           mapM checkType (reverse arg_tps)
  (Some ret_tp', ret_dcns, _) <- checkType ret_tp
  pure (I.FunType (Some $ FunctionTypeRepr fn' arg_tps' ret_tp') arg_dcns ret_dcns, arg_enms)


data ContextElem (tp :: T.Type) where
  SelfElem :: T.TypeRepr tp -> ContextElem tp
  VarElem  :: Text -> T.TypeRepr tp -> ContextElem tp

ifVarElem :: (Text -> T.TypeRepr tp -> Bool) -> ContextElem tp -> Bool
ifVarElem f (VarElem nm tp) = f nm tp
ifVarElem _ _ = False

checkExpr :: EnumNameSet -> Assignment ContextElem ctx
          -> S.LExpr -> CtxM1 (I.LExpr ctx)

checkExpr enms ctx (L p (S.IsInstanceExpr x tp)) = do
  (tp', dcns, enms') <- checkType tp
  x' <- checkExpr (enms `HS.union` enms') ctx x
  pure $ L p (I.IsInstanceExpr x' (tp, tp', dcns))

checkExpr enms ctx (L p (S.VarExpr t))
  | unLoc t `HS.member` enms
    = pure $ L p (I.LiteralExpr (L p (I.EnumLit t)))
  | (Empty :> SelfElem (T.StructRepr ftps)) <- ctx
  , Just (Some idx) <- findIndex (\(FieldRepr f _) -> unLoc t == symbolRepr f) ftps
    = pure $ L p (I.SelfFieldExpr t idx)
  | Just (Some idx) <- findIndex (ifVarElem (\nm _ -> unLoc t == nm)) ctx
    = pure $ L p (I.VarExpr t idx)
  | otherwise
    = throwError (OtherNameNotInScope t)

checkExpr _ ctx (L p S.SelfExpr)
  | (Empty :> SelfElem _) <- ctx = pure $ L p (I.VarExpr (L p "self") baseIndex)
  | otherwise                    = throwError (UnexpectedSelfError p)

checkExpr enms ctx (L p (S.ApplyExpr fn args)) = do
  (_, arg_enms) <- lookupFunction fn
  args' <- mapM (\(a, enms') -> checkExpr (enms `HS.union` enms') ctx a)
                (zip args arg_enms)
  pure $ L p (I.ApplyExpr fn args')

-- the remaining cases are mechanical
checkExpr enms _ (L p (S.LiteralExpr l)) = do
  l' <- checkLiteral (Just enms) l
  pure $ L p (I.LiteralExpr l')
checkExpr enms ctx (L p (S.FieldExpr x f)) = do
  x' <- checkExpr enms ctx x
  pure $ L p (I.FieldExpr x' f)
checkExpr enms ctx (L p (S.EqExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.EqExpr x' y')
checkExpr enms ctx (L p (S.LteExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.LteExpr x' y')
checkExpr enms ctx (L p (S.LtExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.LtExpr x' y')
checkExpr enms ctx (L p (S.GteExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.GteExpr x' y')
checkExpr enms ctx (L p (S.GtExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.GtExpr x' y')
checkExpr enms ctx (L p (S.PlusExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.PlusExpr x' y')
checkExpr enms ctx (L p (S.MinusExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.MinusExpr x' y')
checkExpr enms ctx (L p (S.TimesExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.TimesExpr x' y')
checkExpr enms ctx (L p (S.MemberExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.MemberExpr x' y')
checkExpr enms ctx (L p (S.AndExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.AndExpr x' y')
checkExpr enms ctx (L p (S.OrExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.OrExpr x' y')
checkExpr enms ctx (L p (S.XorExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.XorExpr x' y')
checkExpr enms ctx (L p (S.ImpliesExpr x y)) = do
  x' <- checkExpr enms ctx x
  y' <- checkExpr enms ctx y
  pure $ L p (I.ImpliesExpr x' y')
checkExpr enms ctx (L p (S.NotExpr x)) = do
  x' <- checkExpr enms ctx x
  pure $ L p (I.NotExpr x')

checkLiteral :: Maybe EnumNameSet -> S.LLiteral -> CtxM1 I.LLiteral

checkLiteral _ (L p (S.BoolLit b)) = pure $ L p (I.BoolLit b)
checkLiteral _ (L p (S.IntLit  i)) = pure $ L p (I.IntLit  i)

checkLiteral mb_enms (L p (S.EnumLit e)) = do
  when (maybe False (\enms -> unLoc e `HS.notMember` enms) mb_enms) $
    emitWarning (EnumNameNotInScope e)
  pure $ L p (I.EnumLit e)
checkLiteral mb_enms (L p (S.SetLit es)) = do
  forM_ es $ \e ->
    when (maybe False (\enms -> unLoc e `HS.notMember` enms) mb_enms) $
      emitWarning (EnumNameNotInScope e)
  pure $ L p (I.SetLit es)

checkLiteral mb_enms (L p (S.StructLit (Just tp) fvs)) = do
  (tp', dcns, enms') <- checkType tp
  let enms'' = fmap (\enms -> enms `HS.union` enms') mb_enms
  fvs' <- mapM (mapM (checkLiteral enms'')) fvs
  pure $ L p (I.StructLit (Just (tp, tp', dcns)) fvs')
-- if there's no explict type given for the literal we can't do any sensible
--  enum scope checking yet, so we leave everything to the second pass
checkLiteral _ (L p (S.StructLit Nothing fvs)) = do
  fvs' <- mapM (mapM (checkLiteral Nothing)) fvs
  pure $ L p (I.StructLit Nothing fvs')

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
  , checkType
  ) where

import qualified Data.HashMap as H
import qualified Data.HashSet as HS

import Data.Maybe (catMaybes)
import Data.List.NonEmpty (nonEmpty)
import Data.Traversable (forM)
import Control.Monad (when)
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.Except (throwError)
import Data.Functor.Const
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr

import Lobot.Utils hiding (unzip)
import qualified Lobot.Utils as U
import Lobot.Syntax as S
import Lobot.Types as T

import Lobot.TypeCheck.Monad
import Lobot.TypeCheck.ISyntax (EnumNameSet, DerivedConstraint)
import qualified Lobot.TypeCheck.ISyntax as I


firstPass :: [S.Decl]
          -> WithWarnings (Either TypeError)
                          (Some (Assignment FunctionTypeRepr), [I.Decl])
firstPass ds = evalStateT (checkDecls ds) H.empty


data P1Cns (ctx :: Ctx T.Type) =
  P1Cns [S.LExpr] (Assignment (Const [DerivedConstraint]) ctx)

type CtxM1 = CtxM P1Cns TypeError


addKind :: I.Kind -> CtxM1 ()
addKind (I.Kind (L p nm) tp cns dcns enms) = do
  is_in <- H.member nm <$> get
  let hasCns = not (null cns) || not (null dcns)
  if is_in then throwError (KindNameAlreadyDefined (L p nm))
           else modify (H.insert nm (NamedKind tp (P1Cns cns (Empty :> Const dcns)) hasCns enms))

addAbsType :: LText -> Some TypeRepr -> CtxM1 ()
addAbsType nm (Some tp) = addKind (I.Kind nm tp [] [] HS.empty)

lookupKindType :: LText -> CtxM cns TypeError (Some T.TypeRepr, Bool, EnumNameSet)
lookupKindType (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just (NamedKind ktp _ hasCns enms) -> pure (Some ktp, hasCns, enms)
    _ -> throwError (KindNameNotInScope (L p k))

addFunction :: LText -> I.FunctionType -> CtxM1 ()
addFunction (L p nm) (I.FunType arg_tps ret_tp arg_dcns ret_dcns arg_enms ret_enms) = do
  is_in <- H.member nm <$> get
  let (arg_dcns', ret_dcns') = (P1Cns [] arg_dcns, P1Cns [] (Empty :> Const ret_dcns))
  if is_in then throwError (KindNameAlreadyDefined (L p nm))
           else modify (H.insert nm (NamedFunction arg_tps ret_tp
                                                   arg_dcns' ret_dcns'
                                                   arg_enms ret_enms))


checkDecls :: [S.Decl]
           -> CtxM1 (Some (Assignment FunctionTypeRepr), [I.Decl])
checkDecls [] = pure (Some Empty, [])
checkDecls (d:ds) = do
  (d', mb_ftp) <- checkDecl d
  (Some ftps, ds') <- checkDecls ds
  case mb_ftp of
    Nothing         -> pure (Some ftps,          d':ds')
    Just (Some ftp) -> pure (Some (ftps :> ftp), d':ds')

checkDecl :: S.Decl -> CtxM1 (I.Decl, Maybe (Some FunctionTypeRepr))

checkDecl (S.KindDecl k) = do
  (Some tp, dcns, enms) <- checkType (S.kindType k)
  let cns = S.kindConstraints k
  let k' = I.Kind (S.kindName k) tp cns dcns enms
  when (null cns && null dcns) $ emitWarning (KindHasNoConstraints (S.kindName k))
  addKind k'
  pure (I.KindDecl k', Nothing)

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
  let ck' = I.Check (S.checkName ck) namedTypes (S.checkConstraints ck)
                    (S.checkRequirements ck) enms
  pure (I.CheckDecl ck', Nothing)

checkDecl (S.TypeSynDecl nm tp) = do
  (Some tp', dcns, enms) <- checkType tp
  case dcns of
    (dcn:_) -> throwError $ TypeSynonymConstrainedError nm dcn
    [] -> do let k' = I.Kind nm tp' [] [] enms
             addKind k'
             pure (I.TypeSynDecl nm (Some tp') enms, Nothing)

checkDecl (S.AbsTypeDecl nm) | Some nmSymb <- someSymbol (unLoc nm) = do
  let tp = T.AbsRepr nmSymb
  addAbsType nm (Some tp)
  pure (I.TypeSynDecl nm (Some tp) HS.empty, Nothing)

checkDecl (S.AbsFunctionDecl nm ftp) = do
  f'@(I.FunType arg_tps ret_tp _ _ _ _) <- checkFunctionType ftp
  Some nm' <- pure $ someSymbol (unLoc nm)
  addFunction nm f'
  pure (I.FunctionDecl nm f', Just $ Some (FunctionTypeRepr nm' arg_tps ret_tp))


checkType :: S.LType
          -> CtxM cns TypeError (Some TypeRepr, [I.DerivedConstraint], EnumNameSet)

checkType (L _ S.BoolType) = pure (Some T.BoolRepr, [], HS.empty)
checkType (L _ S.IntType)  = pure (Some T.IntRepr , [], HS.empty)

checkType tp@(L _ (S.EnumType cs)) | Some cs' <- someSymbols cs = do
  ensureUnique id cs (DuplicateEnumNameError tp)
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure (Some (T.EnumRepr cs'), [], HS.fromList cs)
    Right _ -> throwError (EmptyEnumOrSetError tp)

checkType ((L _ (S.SetType tp))) =
  checkType tp >>= \case
    (Some (T.EnumRepr cs), [], enms) -> pure (Some (T.SetRepr cs), [], enms)
    _ -> throwError (SubsetTypeError tp)

checkType (L _ (S.StructType fls)) = do
  (Some fls', mb_dcns, enms) <- mapFst3 fromList . unzip3 <$> mapM checkFieldType fls
  ensureUnique unLoc (fmap fst fls) FieldNameAlreadyDefined
  pure (Some (T.StructRepr fls'), catMaybes mb_dcns, HS.unions enms)

checkType (L p (S.KindNames [])) = throwError (InternalError p "empty kind union")
checkType (L _ (S.KindNames [k])) = do
  (Some tp, hasCns, enms) <- lookupKindType k
  let dcns = if hasCns then [I.FromKind k] else []
  pure (Some tp, dcns, enms)
checkType (L p (S.KindNames (k:ks))) = do
  (Some k_tp, k_hasCns, k_enms) <- lookupKindType k
  let k_dcns = if k_hasCns then [I.FromKind k] else []
  (Some ks_tp, ks_dcns, ks_enms) <- checkType (L p (S.KindNames ks))
  case testEquality k_tp ks_tp of
    Just Refl -> pure (Some ks_tp, k_dcns ++ ks_dcns, k_enms `HS.union` ks_enms)
    Nothing -> throwError (KindUnionMismatchError k (Some ks_tp) (Some k_tp))

checkFieldType :: (LText, S.LType)
               -> CtxM cns TypeError (Some FieldRepr, Maybe I.DerivedConstraint, EnumNameSet)
checkFieldType (f, tp) = do
  Some f' <- pure $ someSymbol (unLoc f)
  (Some tp', dcns, enms) <- checkType tp
  pure (Some (FieldRepr f' tp'), I.FromField f <$> nonEmpty dcns, enms)

checkFunctionType :: S.FunctionType -> CtxM1 I.FunctionType
checkFunctionType (S.FunType _ arg_tps ret_tp) = do
  (Pair arg_tps' arg_dcns, arg_enms) <- unzipHelper <$> mapM checkType arg_tps
  (Some ret_tp', ret_dcns, ret_enms) <- checkType ret_tp
  pure (I.FunType arg_tps' ret_tp' arg_dcns ret_dcns arg_enms ret_enms)
  where unzipHelper :: [(Some TypeRepr, [I.DerivedConstraint], EnumNameSet)]
                    -> ( Pair (Assignment TypeRepr)
                              (Assignment (Const [I.DerivedConstraint]))
                       , [EnumNameSet] ) 
        unzipHelper ts = mapFst U.unzip . unzip $
          (\(Some tp, ds, es) -> (Pair tp (Const ds), es)) <$> ts

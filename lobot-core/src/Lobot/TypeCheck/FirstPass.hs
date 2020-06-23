{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lobot.TypeCheck.FirstPass
Description : The first pass of typechecking the Lobot AST.
Copyright   : (c) Matt Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module does the first pass of typechecking the Lobot AST, which involves
checking all types (even those in expressions), checking all function types,
and scope checking enum names. The result is the function type envionment
of the file, and a list of 'IKinds'. An 'IKind' is a `TypeRepr`, a list of
untyped constraints ('[S.LExpr]') and a list derived constraints
('[DerivedConstraint]'). The derived constraints will be unfolded in the next
pass.
-}

module Lobot.TypeCheck.FirstPass ( firstPass ) where

import qualified Data.HashMap as H
import qualified Data.HashSet as HS

import Data.Text (Text)
import Control.Monad (when, forM_)
import Control.Monad.State (get, modify)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr

import Lobot.Utils hiding (unzip)
import Lobot.Syntax as S
import Lobot.Types as T

import Lobot.TypeCheck.Types


firstPass :: [S.Decl]
          -> Either TypeError ([IKind], Some (Assignment FunctionTypeRepr))
firstPass = evalCtxM . checkDecls


type CtxM1 = CtxM NamedThing TypeError

data NamedThing = NamedKind IKind EnumNameSet
                | NamedFunction IFunctionType [EnumNameSet]

type EnumNameSet = HS.Set Text

addKind :: LText -> IKind -> EnumNameSet -> CtxM1 ()
addKind (L p nm) ik enms = do
  is_in <- H.member nm <$> get
  if is_in then typeError (KindNameAlreadyDefined (L p nm))
           else modify (H.insert nm (NamedKind ik enms))

addAbsType :: LText -> Some TypeRepr -> CtxM1 ()
addAbsType nm tp = addKind nm (IKind tp [] []) HS.empty

lookupKind :: LText -> CtxM1 (IKind, EnumNameSet)
lookupKind (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just (NamedKind k' enms) -> pure (k', enms)
    _ -> typeError (KindNameNotInScope (L p k))

addFunction :: LText -> IFunctionType -> [EnumNameSet] -> CtxM1 ()
addFunction (L p nm) ftp enms = do
  is_in <- H.member nm <$> get
  if is_in then typeError (KindNameAlreadyDefined (L p nm))
           else modify (H.insert nm (NamedFunction ftp enms))

lookupFunction :: LText -> CtxM1 (IFunctionType, [EnumNameSet])
lookupFunction (L p f) = do
  mb_f' <- H.lookup f <$> get
  case mb_f' of
    Just (NamedFunction f' enms) -> pure (f', enms)
    _ -> typeError (FunctionNameNotInScope (L p f))


checkDecls :: [S.Decl]
           -> CtxM1 ([IKind], Some (Assignment FunctionTypeRepr))
checkDecls [] = pure ([], Some Empty)
checkDecls (d:ds) = do
  to_add <- checkDecl d
  (ks, Some ftps) <- checkDecls ds
  case to_add of
    AddIKind k'           -> pure (k':ks, Some ftps)
    AddFunType (Some ftp) -> pure (ks, Some $ ftps :> ftp)
    AddNothing            -> pure (ks, Some ftps)

data ToAdd = AddIKind IKind
           | AddFunType (Some FunctionTypeRepr)
           | AddNothing

checkDecl :: S.Decl -> CtxM1 ToAdd

checkDecl (S.KindDecl k) = do
  (tp, dcns, enms) <- checkType (S.kindType k)
  cns <- mapM (checkExpr enms) (S.kindConstraints k)
  let k' = IKind tp cns dcns
  addKind (S.kindName k) k' enms
  pure $ AddIKind k'

checkDecl (S.TypeSynDecl nm tp) =
  -- type synonyms are just handled as kinds with no constraints
  checkDecl (S.KindDecl (S.Kind nm tp []))

checkDecl (S.AbsTypeDecl nm) | Some nmSymb <- someSymbol (unLoc nm) = do
  addAbsType nm (Some (T.AbsRepr nmSymb))
  pure $ AddNothing

checkDecl (S.AbsFunctionDecl nm ftp) = do
  (ftp', enms) <- checkFunctionType ftp
  addFunction nm ftp' enms
  pure $ AddFunType (ifunType ftp')


checkType :: S.LType
          -> CtxM1 (Some TypeRepr, [DerivedConstraint], EnumNameSet)

checkType (L _ S.BoolType) = pure (Some T.BoolRepr, [], HS.empty)
checkType (L _ S.IntType)  = pure (Some T.IntRepr , [], HS.empty)

checkType tp@(L _ (S.EnumType cs)) | Some cs' <- someSymbols cs = do
  ensureUnique id cs (DuplicateEnumNameError tp)
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure (Some (T.EnumRepr cs'), [], HS.fromList cs)
    Right _ -> typeError (EmptyEnumOrSetError tp)
checkType tp@((L _ (S.SetType cs))) | Some cs' <- someSymbols cs = do
  ensureUnique id cs (DuplicateEnumNameError tp)
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure (Some (T.SetRepr cs'), [], HS.fromList cs)
    Right _ -> typeError (EmptyEnumOrSetError tp)

checkType (L _ (S.StructType fls)) = do
  (Some fls', dcns, enms) <- mapFst3 fromList . unzip3 <$> mapM checkFieldType fls
  ensureUnique unLoc (fmap fst fls) FieldNameAlreadyDefined
  pure (Some (T.StructRepr fls'), dcns, HS.unions enms)

checkType (L p (S.KindNames [])) = typeError (InternalError p "empty kind union")
checkType (L _ (S.KindNames [k])) = do
  (IKind tp _ _, enms) <- lookupKind k
  pure (tp, [FromKind k], enms)
checkType (L p (S.KindNames (k:ks))) = do
  (IKind (Some k_tp) _ _, k_enms) <- lookupKind k
  (Some ks_tp, dcns, ks_enms) <- checkType (L p (S.KindNames ks))
  case testEquality k_tp ks_tp of
    Just Refl -> pure (Some ks_tp, (FromKind k):dcns, k_enms `HS.union` ks_enms)
    Nothing -> typeError (KindUnionMismatchError k (Some ks_tp) (Some k_tp))

checkFieldType :: (LText, S.LType)
               -> CtxM1 (Some FieldRepr, DerivedConstraint, EnumNameSet)
checkFieldType (f, tp) = do
  Some f' <- pure $ someSymbol (unLoc f)
  (Some tp', dcns, enms) <- checkType tp
  pure (Some (FieldRepr f' tp'), FromField f dcns, enms)


checkFunctionType :: S.FunctionType -> CtxM1 (IFunctionType, [EnumNameSet])
checkFunctionType (S.FunType (L _ fn) arg_tps ret_tp) = do
  Some fn' <- pure $ someSymbol fn
  (Some arg_tps', arg_dcns, arg_enms) <- mapFst3 fromList . unzip3 <$>
                                      mapM checkType arg_tps
  (Some ret_tp', ret_dcns, _) <- checkType ret_tp
  pure (IFunType (Some $ FunctionTypeRepr fn' arg_tps' ret_tp') arg_dcns ret_dcns, arg_enms)


checkExpr :: EnumNameSet -> S.LExpr -> CtxM1 ILExpr

checkExpr enms (L p (IsInstanceExpr x tp)) = do
  (tp', dcns, enms') <- checkType tp
  x' <- checkExpr (enms `HS.union` enms') x
  pure $ L p (IsInstanceExpr x' (tp', dcns))

checkExpr enms (L p (VarExpr t))
  | unLoc t `HS.member` enms = pure $ L p (LiteralExpr (L p (EnumLit t)))
  | otherwise                = pure $ L p (VarExpr t)

checkExpr enms (L p (ApplyExpr fn args)) = do
  (_, arg_enms) <- lookupFunction fn
  args' <- mapM (\(a, enms') -> checkExpr (enms `HS.union` enms') a)
                (zip args arg_enms)
  pure $ L p (ApplyExpr fn args')

-- the remaining cases are mechanical
checkExpr enms (L p (LiteralExpr l)) = do
  l' <- checkLiteral (Just enms) l
  pure $ L p (LiteralExpr l')
checkExpr _ (L p SelfExpr) = do
  pure $ L p SelfExpr
checkExpr enms (L p (FieldExpr x f)) = do
  x' <- checkExpr enms x
  pure $ L p (FieldExpr x' f)
checkExpr enms (L p (EqExpr x y)) = do
  x' <- checkExpr enms x
  y' <- checkExpr enms y
  pure $ L p (EqExpr x' y')
checkExpr enms (L p (LteExpr x y)) = do
  x' <- checkExpr enms x
  y' <- checkExpr enms y
  pure $ L p (LteExpr x' y')
checkExpr enms (L p (PlusExpr x y)) = do
  x' <- checkExpr enms x
  y' <- checkExpr enms y
  pure $ L p (PlusExpr x' y')
checkExpr enms (L p (MemberExpr x y)) = do
  x' <- checkExpr enms x
  y' <- checkExpr enms y
  pure $ L p (MemberExpr x' y')
checkExpr enms (L p (ImpliesExpr x y)) = do
  x' <- checkExpr enms x
  y' <- checkExpr enms y
  pure $ L p (ImpliesExpr x' y')
checkExpr enms (L p (NotExpr x)) = do
  x' <- checkExpr enms x
  pure $ L p (NotExpr x')

checkLiteral :: Maybe EnumNameSet -> S.LLiteral -> CtxM1 ILLiteral

checkLiteral _ (L p (BoolLit b)) = pure $ L p (BoolLit b)
checkLiteral _ (L p (IntLit  i)) = pure $ L p (IntLit  i)

checkLiteral mb_enms (L p (EnumLit e)) = do
  when (maybe False (\enms -> unLoc e `HS.member` enms) mb_enms) $
    typeError (EnumNameNotInScope e)
  pure $ L p (EnumLit e)
checkLiteral mb_enms (L p (SetLit es)) = do
  forM_ es $ \e ->
    when (maybe False (\enms -> unLoc e `HS.member` enms) mb_enms) $
      typeError (EnumNameNotInScope e)
  pure $ L p (SetLit es)

checkLiteral mb_enms (L p (StructLit (Just tp) fvs)) = do
  (tp', dcns, enms') <- checkType tp
  let enms'' = fmap (\enms -> enms `HS.union` enms') mb_enms
  fvs' <- mapM (mapM (checkLiteral enms'')) fvs
  pure $ L p (StructLit (Just (tp', dcns)) fvs')
-- if there's no explict type given for the literal we can't do any sensible
--  enum scope checking yet, so we leave everything to the second pass
checkLiteral _ (L p (StructLit Nothing fvs)) = do
  fvs' <- mapM (mapM (checkLiteral Nothing)) fvs
  pure $ L p (StructLit Nothing fvs')
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

This module exposes the first pass of the type checking algorithm for the
Lobot AST. This consists of converting all 'Type's from "Lobot.Syntax" which
appear in the types of kinds, the fields of checks, and the types of functions
to 'TypeRepr's from "Lobot.Types". During this process, we also scope-check
all kind names which appear, accumulate 'DerivedConstraint's, and keep track
of the enum names each type should bring into scope. All of this information
is packaged into a list of intermediate declarations ('Decl' from
"Lobot.TypeCheck.IDecls"), and furthermore, we return the function environment
consisting of all functions that were just type checked.

-}

module Lobot.TypeCheck.FirstPass
  ( firstPass
  , tcType
  , lookupKindType
  ) where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Data.Maybe (catMaybes)
import Data.List.NonEmpty (nonEmpty)
import Data.Traversable (forM)
import Control.Monad (when)
import Control.Monad.State (get, modify)
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
import Lobot.TypeCheck.IDecls (EnumNameSet, DerivedConstraint)
import qualified Lobot.TypeCheck.IDecls as I


-- | Run the first pass of type checking on a list of untyped declarations,
-- producing a list of intermediate declarations.
firstPass :: [S.Decl]
          -> WithWarnings (Either TypeError)
                          (Some (Assignment FunctionTypeRepr), [I.Decl])
firstPass ds = evalTCM $ firstPassDecls ds


-- | For this pass, the type of kind/check constraints is simply a list of
-- untyped constraints from "Lobot.Syntax", but with an additional list of
-- 'DerivedConstraint's. The 'Assignment' ensures there is one set of derived
-- constraints for each type variable in scope. For kinds, there is only ever
-- one type variable in scope, but for checks this ensures we keep track of
-- the derived constraints for each field.
data P1Cns (ctx :: Ctx T.Type) =
  P1Cns [S.LExpr] (Assignment (Const [DerivedConstraint]) ctx)

-- | The 'TCM' used in the first pass.
type TCM1 = TCM P1Cns TypeError


addKind :: I.Kind -> TCM1 ()
addKind (I.Kind (L p nm) tp cns dcns enms) = do
  is_in <- H.member nm <$> get
  if is_in then throwError (KindNameAlreadyDefined (L p nm))
           else let cns' = P1Cns cns (Empty :> Const dcns)
                    hasCns = not (null cns) || not (null dcns)
                 in modify (H.insert nm (NamedKind tp cns' hasCns enms))

addAbsType :: LText -> Some TypeRepr -> TCM1 ()
addAbsType nm (Some tp) = addKind (I.Kind nm tp [] [] HS.empty)

addFunction :: LText -> I.FunctionType -> TCM1 ()
addFunction (L p nm) (I.FunType arg_tps ret_tp arg_dcns
                                ret_dcns arg_enms ret_enms) = do
  is_in <- H.member nm <$> get
  if is_in then throwError (FunctionNameAlreadyDefined (L p nm))
           else let arg_dcns' = P1Cns [] arg_dcns
                    ret_dcns' = P1Cns [] (Empty :> Const ret_dcns)
                 in modify (H.insert nm (NamedFunction arg_tps ret_tp
                                                       arg_dcns' ret_dcns'
                                                       arg_enms ret_enms))


-- | Look up the given kind name in the current context and return its type,
-- whether or not it has any constraints, and the set of enum names this type
-- should bring into scope.
lookupKindType :: LText
               -> TCM cns TypeError (Some T.TypeRepr, Bool, EnumNameSet)
lookupKindType (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just (NamedKind ktp _ hasCns enms) -> pure (Some ktp, hasCns, enms)
    _ -> throwError (KindNameNotInScope (L p k))


-- | Perform the first pass over a list of 'Decl's
firstPassDecls :: [S.Decl]
               -> TCM1 (Some (Assignment FunctionTypeRepr), [I.Decl])
firstPassDecls [] = pure (Some Empty, [])
firstPassDecls (d:ds) = do
  (d', mb_ftp) <- firstPassDecl d
  (Some ftps, ds') <- firstPassDecls ds
  case mb_ftp of
    Nothing         -> pure (Some ftps,          d':ds')
    Just (Some ftp) -> pure (Some (ftps :> ftp), d':ds')

-- | Perform the first pass on a 'Decl'
firstPassDecl :: S.Decl -> TCM1 (I.Decl, Maybe (Some FunctionTypeRepr))

firstPassDecl (S.KindDecl k) = do
  (Some tp, dcns, enms) <- tcType (S.kindType k)
  let cns = S.kindConstraints k
  let k' = I.Kind (S.kindName k) tp cns dcns enms
  when (null cns && null dcns) $ emitWarning (KindHasNoConstraints (S.kindName k))
  addKind k'
  pure (I.KindDecl k', Nothing)

firstPassDecl (S.CheckDecl ck) = do
  -- [(Some TypeRepr, [DerivedConstraint], EnumNameSet)]
  -- First, compute the types
  tcTypeResults <- traverse tcType (snd <$> S.checkFields ck)
  namedTypes' <- forM (zip (S.checkFields ck) tcTypeResults) $
    \((nm, _), (Some tp, dcns, _)) -> pure $ Some $ I.CheckField nm tp dcns
  Some namedTypes <- return $ fromList namedTypes'
  -- let tps = fmapFC namedTypeType cftps
  -- Next, compute the constraints
  let enms = mconcat (thd3 <$> tcTypeResults)
  let ck' = I.Check (S.checkName ck) namedTypes (S.checkConstraints ck)
                    (S.checkRequirements ck) enms
  pure (I.CheckDecl ck', Nothing)

firstPassDecl (S.TypeSynDecl nm tp) = do
  (Some tp', dcns, enms) <- tcType tp
  case dcns of
    (dcn:_) -> throwError $ TypeSynonymConstrainedError nm dcn
    [] -> do let k' = I.Kind nm tp' [] [] enms
             addKind k'
             pure (I.TypeSynDecl nm (Some tp') enms, Nothing)

firstPassDecl (S.AbsTypeDecl nm) | Some nmSymb <- someSymbol (unLoc nm) = do
  let tp = T.AbsRepr nmSymb
  addAbsType nm (Some tp)
  pure (I.TypeSynDecl nm (Some tp) HS.empty, Nothing)

firstPassDecl (S.AbsFunctionDecl nm ftp) = do
  f'@(I.FunType arg_tps ret_tp _ _ _ _) <- tcFunctionType ftp
  Some nm' <- pure $ someSymbol (unLoc nm)
  addFunction nm f'
  pure (I.FunctionDecl nm f', Just $ Some (FunctionTypeRepr nm' arg_tps ret_tp))


-- | Translate a 'Type' from "Lobot.Syntax" to a 'TypeRepr' from "Lobot.Types",
-- and keep track of the list of 'DerivedConstraint's generated by kind names
-- in the given type and the set of enum names this type should bring into
-- scope.
-- 
-- This function does not depend on the constraint type being used in 'TCM',
-- as it only needs to look up the type of a kind name (see 'lookupKindType'),
-- not its constraints. This function can therefore be used, and is used,
-- in both passes. In the second pass specifically, it's used in the
-- 'S.IsInstance' case.
tcType :: S.LType
       -> TCM cns TypeError (Some TypeRepr, [I.DerivedConstraint], EnumNameSet)

tcType (L _ S.BoolType) = pure (Some T.BoolRepr, [], HS.empty)
tcType (L _ S.IntType)  = pure (Some T.IntRepr , [], HS.empty)

tcType tp@(L _ (S.EnumType cs)) | Some cs' <- someSymbols cs = do
  ensureUnique id cs (DuplicateEnumNameError tp)
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure (Some (T.EnumRepr cs'), [], HS.fromList cs)
    Right _ -> throwError (EmptyEnumOrSetError tp)

tcType ((L _ (S.SetType tp))) =
  tcType tp >>= \case
    -- ensure that 'tp' is an unconstrained enum type
    (Some (T.EnumRepr cs), [], enms) -> pure (Some (T.SetRepr cs), [], enms)
    _ -> throwError (SubsetTypeError tp)

-- The 'S.StructType' case just calls 'tcFieldType' and concatenates/unions
-- the results as appropriate
tcType (L _ (S.StructType fls)) = do
  (Some fls', mb_dcns, enms) <- mapFst3 fromList . unzip3 <$> mapM tcFieldType fls
  ensureUnique unLoc (fmap fst fls) FieldNameAlreadyDefined
  pure (Some (T.StructRepr fls'), catMaybes mb_dcns, HS.unions enms)

-- In the @'KindNames' ks@ case, the list of derived constraints returned is
-- exactly @fmap 'I.FromKind' ks@, only with those entries for which
-- @snd3 ('lookupKindType' k)@ returns false omitted -- since these types are
-- known to have no constraints
tcType (L p (S.KindNames [])) = throwError (InternalError p "empty kind union")
tcType (L _ (S.KindNames [k])) = do
  (Some tp, hasCns, enms) <- lookupKindType k
  let dcns = if hasCns then [I.FromKind k] else []
  pure (Some tp, dcns, enms)
tcType (L p (S.KindNames (k:ks))) = do
  (Some k_tp, k_hasCns, k_enms) <- lookupKindType k
  let k_dcns = if k_hasCns then [I.FromKind k] else []
  (Some ks_tp, ks_dcns, ks_enms) <- tcType (L p (S.KindNames ks))
  case testEquality k_tp ks_tp of
    Just Refl -> pure (Some ks_tp, k_dcns ++ ks_dcns, k_enms `HS.union` ks_enms)
    Nothing -> throwError (KindUnionMismatchError k (Some ks_tp) (Some k_tp))

-- | Translate a field @('LText', 'Type')@ from "Lobot.Syntax" to a
-- 'FieldRepr' from "Lobot.Types", and keep track of the list of
-- 'DerivedConstraint's generated by kind names in the given type and the set
-- of enum names this type should bring into scope.
-- 
-- Note that every derived constraint returned by @tcFieldType (f,_)@ is
-- wrapped in @'I.FromField' f@.
tcFieldType :: (LText, S.LType)
            -> TCM cns TypeError
                       (Some FieldRepr, Maybe I.DerivedConstraint, EnumNameSet)
tcFieldType (f, tp) = do
  Some f' <- pure $ someSymbol (unLoc f)
  (Some tp', dcns, enms) <- tcType tp
  pure (Some (FieldRepr f' tp'), I.FromField f <$> nonEmpty dcns, enms)

-- | Translate a @'S.FunctionType'@ from "Lobot.Syntax" to a 'FunctionType'
-- from "Lobot.Types". This type includes the list of 'DerivedConstraint's
-- generated by kind names in each argument type and the return type, as well
-- as the set of enum names each argument type and return type should bring
-- into scope.
tcFunctionType :: S.FunctionType -> TCM1 I.FunctionType
tcFunctionType (S.FunType _ arg_tps ret_tp) = do
  (Pair arg_tps' arg_dcns, arg_enms) <- unzipHelper <$> mapM tcType arg_tps
  (Some ret_tp', ret_dcns, ret_enms) <- tcType ret_tp
  pure (I.FunType arg_tps' ret_tp' arg_dcns ret_dcns arg_enms ret_enms)
  where unzipHelper :: [(Some TypeRepr, [I.DerivedConstraint], EnumNameSet)]
                    -> ( Pair (Assignment TypeRepr)
                              (Assignment (Const [I.DerivedConstraint]))
                       , [EnumNameSet] ) 
        unzipHelper ts = mapFst U.unzip . unzip $
          (\(Some tp, ds, es) -> (Pair tp (Const ds), es)) <$> ts

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lobot.Core.TypeCheck
Description : The LOBOT type checker.
Copyright   : (c) Matt Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines the type checking algorithm for the Lobot AST.
-}

module Lobot.Core.TypeCheck
  (SomeTypeOrString(..), TypeError(..), checkKindDecls) where

import Data.Text (Text)
import Control.Monad (forM)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Data.Maybe (mapMaybe)
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableF
import Prelude hiding (zipWith)

import qualified Data.HashMap as H

import Lobot.Core.Utils
import Lobot.Core.Kind   as K
import Lobot.Core.Syntax as S


data TypeError = TypeMismatchError S.Expr SomeTypeOrString SomeTypeOrString
                 -- ^ argument order: expr, expected type, actual type
               | TypeInferenceError S.Expr
               | EnumNameError Text (Some (Assignment SymbolRepr))
               | FieldNameError Text (Some (Assignment FieldRepr))
               | EmptyEnumOrSetError
               | KindUnionMismatchError (Some K.TypeRepr) (Some K.TypeRepr)
               | KindNameNotInScope Text
               | InternalError Text

data SomeTypeOrString :: * where
  SomeType   :: K.TypeRepr tp -> SomeTypeOrString
  TypeString :: Text -> SomeTypeOrString

-- | Given a list of kind declarations, produce a list of typed kinds.
checkKindDecls :: Assignment K.FunctionTypeRepr env
               -> [S.KindDecl] -> Either TypeError [Some (K.Kind env)]
checkKindDecls env decls = evalStateT (mapM (checkKindDecl env) decls) H.empty


-- Validating a kind declaration

checkKindDecl :: Assignment K.FunctionTypeRepr env -> S.KindDecl
              -> StateT DeclCtx (Either TypeError) (Some (K.Kind env))
checkKindDecl env KindDecl{..} = do
  Some ctx <- resolveType kindDeclType
  cns <- lift $ mapM (checkExpr env ctx K.BoolRepr) kindDeclConstraints
  modify (H.insert kindDeclName (Some ctx))
  pure $ Some (Kind kindDeclName ctx env cns)


type DeclCtx = H.Map Text (Some K.TypeRepr)

lookupType :: Text -> StateT DeclCtx (Either TypeError) (Some K.TypeRepr)
lookupType k = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just k' -> pure $ k'
    Nothing -> lift $ Left (KindNameNotInScope k)


-- Resolving all kind names in a `S.Type` to get a `K.Type`

resolveType :: S.Type -> StateT DeclCtx (Either TypeError) (Some K.TypeRepr)

resolveType S.BoolType = pure $ Some K.BoolRepr
resolveType S.IntType  = pure $ Some K.IntRepr

resolveType (S.EnumType cs) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (sizeToCtxSize (size cs')) of
    Left LeqProof -> pure $ Some (K.EnumRepr cs')
    Right _ -> lift $ Left EmptyEnumOrSetError
resolveType (S.SetType cs) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (sizeToCtxSize (size cs')) of
    Left LeqProof -> pure $ Some (K.SetRepr cs')
    Right _ -> lift $ Left EmptyEnumOrSetError
  
resolveType (S.StructType fls) = do
  Some fls' <- fromList <$> mapM resolveFieldType fls
  pure $ Some (K.StructRepr fls')
  
resolveType (S.KindNames []) = lift $ Left (InternalError "empty kind union")
resolveType (S.KindNames (k:ks)) = do
  ktp  <- lookupType k
  ktps <- mapM lookupType ks
  case mapMaybe (\ktp' -> if ktp == ktp' then Nothing else Just ktp') ktps of
    [] -> pure $ ktp
    (ktp':_) -> lift $ Left (KindUnionMismatchError ktp ktp')

resolveFieldType :: (Text, S.Type) -> StateT DeclCtx (Either TypeError) (Some FieldRepr)
resolveFieldType (nm, ty) = do
  Some nm' <- pure $ someSymbol nm
  Some ty' <- resolveType ty
  pure $ Some (FieldRepr nm' ty')


-- Type inference and checking for expressions

inferExpr :: Assignment K.FunctionTypeRepr env -> K.TypeRepr ctx
          -> S.Expr -> Either TypeError (Pair K.TypeRepr (K.Expr env ctx))

inferExpr _ _ (S.LiteralExpr l) = fmapF K.LiteralExpr <$> inferLit l
inferExpr _ ctx S.SelfExpr = pure $ Pair ctx K.SelfExpr

inferExpr env ctx (S.FieldExpr x f) = do
  Pair xtp x' <- inferExpr env ctx x
  case (xtp, someSymbol f) of
    (StructRepr ftps, Some f') -> case fieldIndex f' ftps of
      Just (SomeField tp i) -> pure $ Pair tp (K.FieldExpr x' i)
      Nothing -> Left (FieldNameError f (Some ftps))
    _ -> Left (TypeMismatchError x (TypeString "a struct") (SomeType xtp))
  
inferExpr env ctx (S.EqExpr x y) =
  case (inferExpr env ctx x, inferExpr env ctx y) of
    (Left _, Left _) -> Left (TypeInferenceError x)
    (Right (Pair xtp x'), Left _) -> do y' <- checkExpr env ctx xtp y
                                        pure $ Pair K.BoolRepr (K.EqExpr x' y')
    (Left _, Right (Pair ytp y')) -> do x' <- checkExpr env ctx ytp x
                                        pure $ Pair K.BoolRepr (K.EqExpr x' y')
    (Right (Pair xtp x'), Right (Pair ytp y')) -> case testEquality xtp ytp of
      Just Refl -> pure $ Pair K.BoolRepr (K.EqExpr x' y')
      _ -> Left (TypeMismatchError y (SomeType xtp) (SomeType ytp))

inferExpr env ctx (S.LteExpr x y) = do
  x' <- checkExpr env ctx K.IntRepr x
  y' <- checkExpr env ctx K.IntRepr y
  pure $ Pair K.BoolRepr (K.LteExpr x' y')

inferExpr env ctx (S.MemberExpr x y) = do
  case (inferExpr env ctx x, inferExpr env ctx y) of
    (Left _, Left _) -> Left (TypeInferenceError x)
    (Right (Pair (EnumRepr cs) x'), Left _) -> do
      y' <- checkExpr env ctx (SetRepr cs) y
      pure $ Pair K.BoolRepr (K.MemberExpr x' y')
    (Left _, Right (Pair (SetRepr cs) y')) -> do
      x' <- checkExpr env ctx (EnumRepr cs) x
      pure $ Pair K.BoolRepr (K.MemberExpr x' y')
    (Right (Pair (EnumRepr xcs) x'), Right (Pair (SetRepr ycs) y')) ->
      case testEquality xcs ycs of
        Just Refl -> pure $ Pair K.BoolRepr (K.MemberExpr x' y')
        _ -> Left (TypeMismatchError y (SomeType (SetRepr xcs)) (SomeType (SetRepr ycs)))
    (Right (Pair xtp _), _) ->
      Left (TypeMismatchError x (TypeString "an enum") (SomeType xtp))
    (_, Right (Pair ytp _)) ->
      Left (TypeMismatchError y (TypeString "a set") (SomeType ytp))
  
inferExpr env ctx (S.ImpliesExpr x y) = do
  x' <- checkExpr env ctx K.BoolRepr x
  y' <- checkExpr env ctx K.BoolRepr y
  pure $ Pair K.BoolRepr (K.ImpliesExpr x' y')
  
inferExpr env ctx (S.NotExpr x) = do
  x' <- checkExpr env ctx K.BoolRepr x
  pure $ Pair K.BoolRepr (K.NotExpr x')

inferExpr _ _ (S.IsInstance _ _) =
  Left (InternalError "an `IsInstance` expression made it to typechecking")

checkExpr :: Assignment K.FunctionTypeRepr env -> K.TypeRepr ctx
          -> K.TypeRepr tp -> S.Expr -> Either TypeError (K.Expr env ctx tp)

checkExpr _ _ tp (S.LiteralExpr l) = K.LiteralExpr <$> checkLit tp l

checkExpr env ctx tp x = do
  Pair tp' x' <- inferExpr env ctx x
  case testEquality tp tp' of
    Just Refl -> pure x'
    _ -> Left (TypeMismatchError x (SomeType tp) (SomeType tp'))


data SomeField (ftps :: Ctx (Symbol, K.Type)) (nm :: Symbol) :: * where
  SomeField :: K.TypeRepr tp -> Index ftps '(nm, tp) -> SomeField ftps nm

-- adapted from 'elemIndex'
fieldIndex :: SymbolRepr nm -> Assignment FieldRepr ftps -> Maybe (SomeField ftps nm)
fieldIndex nm ftps = case traverseAndCollect (go nm) ftps of
                       Left x  -> Just x
                       Right _ -> Nothing
  where go :: SymbolRepr nm -> Index ftps pr -> FieldRepr pr -> Either (SomeField ftps nm) ()
        go nm' i (FieldRepr nm'' tp)
          | Just Refl <- testEquality nm' nm'' = Left (SomeField tp i)
          | otherwise = Right ()


-- Type inference and checking for literals

inferLit :: S.Literal -> Either TypeError (Pair K.TypeRepr K.Literal)

inferLit (S.BoolLit b) = pure $ Pair K.BoolRepr (K.BoolLit b)
inferLit (S.IntLit z)  = pure $ Pair K.IntRepr  (K.IntLit z)

inferLit (S.StructLit ls) = do
  Some fls <- fromList <$> mapM inferFieldLit ls
  pure $ Pair (K.literalType (K.StructLit fls)) (K.StructLit fls)

inferLit lit = Left (TypeInferenceError (S.LiteralExpr lit))

checkLit :: K.TypeRepr tp -> S.Literal -> Either TypeError (K.Literal tp)

checkLit (K.EnumRepr cs) (S.EnumLit e) = do
  Some i <- enumElemIndex cs e
  pure $ K.EnumLit cs i
checkLit tp (S.EnumLit e) =
  Left (TypeMismatchError (S.LiteralExpr (S.EnumLit e))
                          (SomeType tp) (TypeString "an enum"))
  
checkLit (K.SetRepr cs) (S.SetLit es) = do
  es' <- forM es (enumElemIndex cs)
  pure $ K.SetLit cs es'
checkLit tp (S.SetLit es) =
  Left (TypeMismatchError (S.LiteralExpr (S.SetLit es))
                          (SomeType tp) (TypeString "a set"))

checkLit tp lit = do
  Pair tp' lit' <- inferLit lit
  case testEquality tp tp' of
    Just Refl -> pure lit'
    _ -> Left (TypeMismatchError (S.LiteralExpr lit)
                                 (SomeType tp) (SomeType tp'))

inferFieldLit :: (Text, S.Literal) -> Either TypeError (Some FieldLiteral)
inferFieldLit (s,l) = do
  Pair _ l' <- inferLit l
  Some s' <- pure $ someSymbol s
  pure $ Some (K.FieldLiteral s' l')


enumElemIndex :: 1 <= CtxSize cs => Assignment SymbolRepr cs -> Text
              -> Either TypeError (Some (Index cs))
enumElemIndex cs s
  | Some s' <- someSymbol s, Just i <- elemIndex s' cs = pure (Some i)
  | otherwise = Left (EnumNameError s (Some cs))

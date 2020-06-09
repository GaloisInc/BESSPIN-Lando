{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  ( SomeTypeOrString(..), TypeError(..)
  , typeCheck, checkKindDecls, resolveType
  , inferExpr, checkExpr, inferLit, checkLit ) where

import Data.Text (Text)
import Control.Monad (forM)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableF
import Prelude hiding (zipWith, unzip)

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
               | KindUnionMismatchError Text (Some K.TypeRepr) (Some K.TypeRepr)
               | KindNameNotInScope Text
               | InternalError Text
               deriving Show

data SomeTypeOrString :: * where
  SomeType   :: K.TypeRepr tp -> SomeTypeOrString
  TypeString :: Text -> SomeTypeOrString
deriving instance Show SomeTypeOrString

-- | Given a list of kind declarations, produce a list of typed kinds. 
typeCheck :: Assignment K.FunctionTypeRepr env
          -> [S.KindDecl] -> Either TypeError [Some (K.Kind env)]
typeCheck env decls = evalStateT (checkKindDecls env decls) H.empty


-- Validating kind declarations

checkKindDecls :: Assignment K.FunctionTypeRepr env -> [S.KindDecl]
               -> StateT (DeclCtx env) (Either TypeError) [Some (K.Kind env)]
checkKindDecls env decls = mapM (checkKindDecl env) decls

checkKindDecl :: Assignment K.FunctionTypeRepr env -> S.KindDecl
              -> StateT (DeclCtx env) (Either TypeError) (Some (K.Kind env))
checkKindDecl env KindDecl{..} = do
  Pair ctx (Cns ctx_cns) <- resolveType env kindDeclType
  cns <- lift $ mapM (checkExpr env ctx K.BoolRepr) kindDeclConstraints
  let decl = Some (Kind kindDeclName ctx env (cns ++ ctx_cns))
  modify (H.insert kindDeclName decl)
  pure $ decl

type DeclCtx env = H.Map Text (Some (Kind env))

lookupKind :: Assignment K.FunctionTypeRepr env -> Text
           -> StateT (DeclCtx env) (Either TypeError) (Some (K.Kind env))
lookupKind _ k = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just k' -> pure $ k'
    Nothing -> lift $ Left (KindNameNotInScope k)


-- Resolving all kind names in a `S.Type` to get a `K.Type` and a list of
-- additional constraints.

data Constraints env tp = Cns [K.Expr env tp K.BoolType]

resolveType :: Assignment K.FunctionTypeRepr env -> S.Type
            -> StateT (DeclCtx env) (Either TypeError)
                      (Pair TypeRepr (Constraints env))

resolveType _ S.BoolType = pure $ Pair K.BoolRepr (Cns [])
resolveType _ S.IntType  = pure $ Pair K.IntRepr  (Cns [])

resolveType _ (S.EnumType cs) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (sizeToCtxSize (size cs')) of
    Left LeqProof -> pure $ Pair (K.EnumRepr cs') (Cns [])
    Right _ -> lift $ Left EmptyEnumOrSetError
resolveType _ (S.SetType cs) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (sizeToCtxSize (size cs')) of
    Left LeqProof -> pure $ Pair (K.SetRepr cs') (Cns [])
    Right _ -> lift $ Left EmptyEnumOrSetError

resolveType env (S.StructType fls) = do
  Pair fls' cnss <- unzip <$> mapM (resolveFieldType env) fls
  let cns = concat $ toListWithIndex (\i (Cns' cns') -> liftExpr i <$> cns') cnss
  pure $ Pair (K.StructRepr fls') (Cns cns)
  
resolveType _ (S.KindNames []) =
  lift $ Left (InternalError "empty kind union")
resolveType env (S.KindNames [k]) = do
  Some k' <- lookupKind env k
  pure $ Pair (kindType k') (Cns (kindConstraints k'))
resolveType env (S.KindNames (k:ks)) = do
  Some k' <- lookupKind env k
  Pair tp (Cns cns) <- resolveType env (S.KindNames ks)
  case testEquality (kindType k') tp of
    Just Refl -> pure $ Pair (kindType k') (Cns (kindConstraints k' ++ cns))
    Nothing -> lift $ Left (KindUnionMismatchError (kindName k') (Some (kindType k')) (Some tp))

data Constraints' env (pr :: (Symbol, K.Type)) where
  Cns' :: [K.Expr env tp K.BoolType] -> Constraints' env '(nm, tp)

resolveFieldType :: Assignment K.FunctionTypeRepr env -> (Text, S.Type)
                 -> StateT (DeclCtx env) (Either TypeError)
                           (Pair FieldRepr (Constraints' env))
resolveFieldType env (nm,tp) = do
  Some nm' <- pure $ someSymbol nm
  Pair tp' (Cns cns) <- resolveType env tp
  pure $ Pair (FieldRepr nm' tp') (Cns' cns)


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

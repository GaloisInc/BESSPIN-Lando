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
  ( TypeError(..)
  , typeCheck
  , SomeTypeOrString(..)
  , ppTypeError
    -- * Internals
  , CtxM
  , Constraints(..)
  , checkKindDecls
  , resolveType
  , inferExpr
  , checkExpr
  , inferLit
  , checkLit
  ) where

import qualified Data.HashMap as H
import qualified Text.PrettyPrint as PP

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

import Lobot.Core.Utils
import Lobot.Core.Lexer (AlexPosn, errorPrefix)
import Lobot.Core.Kind   as K
import Lobot.Core.Syntax as S
import Lobot.Core.Kind.Pretty   as K
import Lobot.Core.Syntax.Pretty as S

-- | State/error monad for type checking. We maintain a hashmap from names to
-- 'K.Kind's as we iterate through a @[S.Kind]@.
type CtxM env = StateT (H.Map Text (Some (Kind env))) (Either TypeError)

data TypeError = TypeMismatchError S.LExpr SomeTypeOrString (Maybe SomeTypeOrString)
                 -- ^ argument order: expr, expected type, actual type
               | TypeInferenceError S.LExpr
               | FieldNameError LText (Some (Assignment FieldRepr))
               | EmptyEnumOrSetError S.LType
               | KindUnionMismatchError LText (Some K.TypeRepr) (Some K.TypeRepr)
               -- ^ argument order: kind name, expected type, actual type
               | KindNameNotInScope LText
               | UnknownFunctionError LText
               | FunctionArgLengthError LText (Some FunctionTypeRepr) [S.LExpr]
               | InternalError AlexPosn Text
               deriving Show

ppTypeError :: FilePath -> TypeError -> PP.Doc
ppTypeError fp (TypeMismatchError (L p x) exp_tp Nothing) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Type mismatch on expression:" PP.<+> S.ppExpr x
  PP.$$ PP.nest 2 (PP.text "Expected type:") PP.<+> PP.nest 6 (ppSomeTypeOrString exp_tp)
ppTypeError fp (TypeMismatchError x exp_tp (Just act_tp)) =
  ppTypeError fp (TypeMismatchError x exp_tp Nothing)
  PP.$$ PP.nest 2 (PP.text "  Actual type:") PP.<+> PP.nest 6 (ppSomeTypeOrString act_tp)
ppTypeError fp (TypeInferenceError (L p x)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Could not infer the type of expression:" PP.<+> S.ppExpr x
ppTypeError fp (FieldNameError (L p f) (Some _)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Field name" PP.<+> S.ppText f PP.<+> "not in scope."
ppTypeError fp (EmptyEnumOrSetError (L p tp)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Empty enum or set:" PP.<+> S.ppType tp
ppTypeError fp (KindUnionMismatchError (L p k) (Some exp_tp) (Some act_tp)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "In a kind union, type mismatch on kind name:" PP.<+> S.ppText k
  PP.$$ PP.nest 2 (PP.text "Expected type:") PP.<+> PP.nest 6 (ppTypeRepr exp_tp)
  PP.$$ PP.nest 2 (PP.text "  Actual type:") PP.<+> PP.nest 6 (ppTypeRepr act_tp)
ppTypeError fp (KindNameNotInScope (L p k)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Kind name" PP.<+> S.ppText k PP.<+> "not in scope."
ppTypeError fp (UnknownFunctionError (L p fn)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Function name" PP.<+> S.ppText fn PP.<+> "not in scope."
ppTypeError fp (FunctionArgLengthError (L p fn) (Some (FunctionTypeRepr{..})) args) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Function" PP.<+> S.ppText fn
  PP.<+> "expects" PP.<+> PP.int (sizeInt . size $ functionArgTypes)
  PP.<+> "arguments, but was given" PP.<+> PP.int (length args)
ppTypeError fp (InternalError p str) =
  PP.text (errorPrefix fp p)
  PP.<+> "Internal error!" PP.$$ PP.nest 2 (S.ppText str)

data SomeTypeOrString :: * where
  SomeType   :: K.TypeRepr tp -> SomeTypeOrString
  TypeString :: Text -> SomeTypeOrString
deriving instance Show SomeTypeOrString

ppSomeTypeOrString :: SomeTypeOrString -> PP.Doc
ppSomeTypeOrString (SomeType tp) = ppTypeRepr tp
ppSomeTypeOrString (TypeString s) = S.ppText s

lookupKind :: Assignment K.FunctionTypeRepr env
           -> LText
           -> CtxM env (Some (K.Kind env))
lookupKind _ (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just k' -> pure $ k'
    Nothing -> typeError (KindNameNotInScope (L p k))

typeError :: TypeError -> CtxM env a
typeError = lift . Left

try :: CtxM env a -> CtxM env (Either TypeError a)
try x = evalStateT x <$> get


-- | Given a list of kind declarations, produce a list of typed kinds.
typeCheck :: Assignment K.FunctionTypeRepr env
          -> [S.KindDecl]
          -> Either TypeError [Some (K.Kind env)]
typeCheck env decls = evalStateT (checkKindDecls env decls) H.empty


-- Validating kind declarations

checkKindDecls :: Assignment K.FunctionTypeRepr env
               -> [S.KindDecl]
               -> CtxM env [Some (K.Kind env)]
checkKindDecls env decls = mapM (checkKindDecl env) decls

checkKindDecl :: Assignment K.FunctionTypeRepr env
              -> S.KindDecl
              -> CtxM env (Some (K.Kind env))
checkKindDecl env KindDecl{..} = do
  Pair ctx (Cns ctx_cns) <- resolveType env kindDeclType
  cns' <- mapM (checkExpr env ctx K.BoolRepr) kindDeclConstraints
  let decl = Some (Kind kindDeclName ctx env (cns' ++ ctx_cns))
  modify (H.insert kindDeclName decl)
  pure $ decl


-- Resolving all kind names in a `S.Type` to get a `K.Type` and a list of
-- additional constraints.

data Constraints env tp = Cns [K.Expr env tp K.BoolType]

resolveType :: Assignment K.FunctionTypeRepr env
            -> S.LType
            -> CtxM env (Pair TypeRepr (Constraints env))

resolveType _ (L _ S.BoolType) = pure $ Pair K.BoolRepr (Cns [])
resolveType _ (L _ S.IntType)  = pure $ Pair K.IntRepr  (Cns [])

resolveType _ tp@(L _ (S.EnumType cs)) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure $ Pair (K.EnumRepr cs') (Cns [])
    Right _ -> typeError (EmptyEnumOrSetError tp)
resolveType _ tp@((L _ (S.SetType cs))) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure $ Pair (K.SetRepr cs') (Cns [])
    Right _ -> typeError (EmptyEnumOrSetError tp)

resolveType env (L _ (S.StructType fls)) = do
  Pair fls' cnss <- unzip <$> mapM (resolveFieldType env) fls
  let cns = concat $ toListWithIndex (\i (Cns' cns') -> liftExpr i <$> cns') cnss
  pure $ Pair (K.StructRepr fls') (Cns cns)

resolveType _ (L p (S.KindNames [])) =
  typeError (InternalError p "empty kind union")
resolveType env (L _ (S.KindNames [k])) = do
  Some k' <- lookupKind env k
  pure $ Pair (kindType k') (Cns (kindConstraints k'))
resolveType env (L p (S.KindNames (k:ks))) = do
  Some k' <- lookupKind env k
  Pair tp (Cns cns) <- resolveType env (L p (S.KindNames ks))
  case testEquality (kindType k') tp of
    Just Refl -> pure $ Pair (kindType k') (Cns (kindConstraints k' ++ cns))
    Nothing -> typeError (KindUnionMismatchError k (Some tp) (Some (kindType k')))

data Constraints' env (pr :: (Symbol, K.Type)) where
  Cns' :: [K.Expr env tp K.BoolType] -> Constraints' env '(nm, tp)

resolveFieldType :: Assignment K.FunctionTypeRepr env -> (LText, S.LType)
                 -> CtxM env (Pair FieldRepr (Constraints' env))
resolveFieldType env (L _ nm,tp) = do
  Some nm' <- pure $ someSymbol nm
  Pair tp' (Cns cns) <- resolveType env tp
  pure $ Pair (FieldRepr nm' tp') (Cns' cns)


-- Type inference and checking for expressions

inferExpr :: Assignment K.FunctionTypeRepr env -> K.TypeRepr ctx
          -> S.LExpr -> CtxM env (Pair K.TypeRepr (K.Expr env ctx))

inferExpr _ _ (L _ (S.LiteralExpr l)) = fmapF K.LiteralExpr <$> inferLit l
inferExpr _ ctx (L _ S.SelfExpr) = pure $ Pair ctx K.SelfExpr

inferExpr _ (K.StructRepr ftps) (L _ (S.SelfFieldExpr (L p f)))
  | Some f' <- someSymbol f =
      case fieldIndex f' ftps of
        Just (SomeField tp i) -> pure $ Pair tp (K.FieldExpr K.SelfExpr i)
        Nothing -> typeError (FieldNameError (L p f) (Some ftps))
inferExpr _ _ (L _ (S.SelfFieldExpr f)) =
  typeError (FieldNameError f (Some Empty))

inferExpr env ctx (L _ (S.FieldExpr x (L p f))) = do
  Pair xtp x' <- inferExpr env ctx x
  case (xtp, someSymbol f) of
    (StructRepr ftps, Some f') -> case fieldIndex f' ftps of
      Just (SomeField tp i) -> pure $ Pair tp (K.FieldExpr x' i)
      Nothing -> typeError (FieldNameError (L p f) (Some ftps))
    _ -> typeError (TypeMismatchError x (TypeString "a struct")
                                        (Just $ SomeType xtp))

inferExpr env ctx (L _ (S.ApplyExpr (L p fn) xs)) = do
  case functionIndex fn env of
    Just (Some fi) -> do
      fntp@FunctionTypeRepr{..} <- return $ env ! fi
      mxs' <- checkExprs env ctx functionArgTypes xs
      case mxs' of
        Just xs' -> pure $ Pair functionRetType (K.ApplyExpr fi xs')
        Nothing -> typeError (FunctionArgLengthError (L p fn) (Some fntp) xs)
    Nothing -> typeError (UnknownFunctionError (L p fn))

inferExpr env ctx (L _ (S.EqExpr x y)) = do
  mb_x' <- try $ inferExpr env ctx x
  mb_y' <- try $ inferExpr env ctx y
  case (mb_x', mb_y') of
    (Left _, Left _) -> typeError (TypeInferenceError x)
    (Right (Pair xtp x'), Left _) -> do y' <- checkExpr env ctx xtp y
                                        pure $ Pair K.BoolRepr (K.EqExpr x' y')
    (Left _, Right (Pair ytp y')) -> do x' <- checkExpr env ctx ytp x
                                        pure $ Pair K.BoolRepr (K.EqExpr x' y')
    (Right (Pair xtp x'), Right (Pair ytp y')) -> case testEquality xtp ytp of
      Just Refl -> pure $ Pair K.BoolRepr (K.EqExpr x' y')
      _ -> typeError (TypeMismatchError y (SomeType xtp) (Just $ SomeType ytp))

inferExpr env ctx (L _ (S.LteExpr x y)) = do
  x' <- checkExpr env ctx K.IntRepr x
  y' <- checkExpr env ctx K.IntRepr y
  pure $ Pair K.BoolRepr (K.LteExpr x' y')

inferExpr env ctx (L _ (S.PlusExpr x y)) = do
  x' <- checkExpr env ctx K.IntRepr x
  y' <- checkExpr env ctx K.IntRepr y
  pure $ Pair K.IntRepr (K.PlusExpr x' y')

inferExpr env ctx (L _ (S.MemberExpr x y)) = do
  mb_x' <- try $ inferExpr env ctx x
  mb_y' <- try $ inferExpr env ctx y
  case (mb_x', mb_y') of
    (Left _, Left _) -> typeError (TypeInferenceError x)
    (Right (Pair (EnumRepr cs) x'), Left _) -> do
      y' <- checkExpr env ctx (SetRepr cs) y
      pure $ Pair K.BoolRepr (K.MemberExpr x' y')
    (Left _, Right (Pair (SetRepr cs) y')) -> do
      x' <- checkExpr env ctx (EnumRepr cs) x
      pure $ Pair K.BoolRepr (K.MemberExpr x' y')
    (Right (Pair (EnumRepr xcs) x'), Right (Pair (SetRepr ycs) y')) ->
      case testEquality xcs ycs of
        Just Refl -> pure $ Pair K.BoolRepr (K.MemberExpr x' y')
        _ -> typeError (TypeMismatchError y (SomeType (SetRepr xcs))
                                            (Just $ SomeType (SetRepr ycs)))
    (Right (Pair xtp _), _) ->
      typeError (TypeMismatchError x (TypeString "an enum")
                                     (Just $ SomeType xtp))
    (_, Right (Pair ytp _)) ->
      typeError (TypeMismatchError y (TypeString "a set")
                                     (Just $ SomeType ytp))

inferExpr env ctx (L _ (S.ImpliesExpr x y)) = do
  x' <- checkExpr env ctx K.BoolRepr x
  y' <- checkExpr env ctx K.BoolRepr y
  pure $ Pair K.BoolRepr (K.ImpliesExpr x' y')

inferExpr env ctx (L _ (S.NotExpr x)) = do
  x' <- checkExpr env ctx K.BoolRepr x
  pure $ Pair K.BoolRepr (K.NotExpr x')

inferExpr _env _ctx (L p (S.IsInstanceExpr _x _t)) = do
  typeError (InternalError p "IsInstance (:) expressions are not yet supported")
  -- Pair t' (Cns cns) <- resolveType env t
  -- x' <- checkExpr env ctx t' x
  -- pure $ Pair K.BoolRepr (foldr1 K.AndExpr (giveSelf x' <$> cns))

checkExpr :: Assignment K.FunctionTypeRepr env -> K.TypeRepr ctx
          -> K.TypeRepr tp -> S.LExpr
          -> CtxM env (K.Expr env ctx tp)

checkExpr _ _ tp (L _ (S.LiteralExpr l)) = K.LiteralExpr <$> checkLit tp l

checkExpr env ctx tp x = do
  Pair tp' x' <- inferExpr env ctx x
  case testEquality tp tp' of
    Just Refl -> pure x'
    _ -> typeError (TypeMismatchError x (SomeType tp) (Just $ SomeType tp'))

checkExprs :: Assignment K.FunctionTypeRepr env
           -> K.TypeRepr ctx
           -> Assignment K.TypeRepr tps
           -> [S.LExpr]
           -> CtxM env (Maybe (Assignment (K.Expr env ctx) tps))
checkExprs _ _ Empty [] = pure $ Just Empty
checkExprs env ctx (tps :> tp) (x:xs) = do
  x' <- checkExpr env ctx tp x
  mxs' <- checkExprs env ctx tps xs
  case mxs' of
    Just xs' -> pure $ Just (xs' :> x')
    Nothing -> pure Nothing
checkExprs _ _ _ _ = pure Nothing

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

functionIndex :: Text
              -> Assignment FunctionTypeRepr fntps
              -> Maybe (Some (Index fntps))
functionIndex nm =
  findIndex $ \FunctionTypeRepr{..} -> nm == symbolRepr functionName

-- Type inference and checking for literals

inferLit :: S.LLiteral -> CtxM env (Pair K.TypeRepr K.Literal)

inferLit (L _ (S.BoolLit b)) = pure $ Pair K.BoolRepr (K.BoolLit b)
inferLit (L _ (S.IntLit z))  = pure $ Pair K.IntRepr  (K.IntLit z)

inferLit (L _ (S.StructLit ls)) = do
  Some fls <- fromList <$> mapM inferFieldLit ls
  pure $ Pair (K.literalType (K.StructLit fls)) (K.StructLit fls)

inferLit lit@(L p _) = typeError (TypeInferenceError (L p (S.LiteralExpr lit)))

checkLit :: K.TypeRepr tp -> S.LLiteral -> CtxM env (K.Literal tp)

checkLit (K.EnumRepr cs) (L _ (S.EnumLit e)) = do
  Some i <- enumElemIndex cs e
  pure $ K.EnumLit cs i
checkLit tp lit@(L p (S.EnumLit _)) =
  typeError (TypeMismatchError (L p (S.LiteralExpr lit)) (SomeType tp)
                               (Just $ TypeString "an enum"))

checkLit (K.SetRepr cs) (L _ (S.SetLit es)) = do
  es' <- forM es (enumElemIndex cs)
  pure $ K.SetLit cs es'
checkLit tp lit@(L p (S.SetLit _)) =
  typeError (TypeMismatchError (L p (S.LiteralExpr lit)) (SomeType tp)
                               (Just $ TypeString "a set"))

checkLit tp lit@(L p _) = do
  Pair tp' lit' <- inferLit lit
  case testEquality tp tp' of
    Just Refl -> pure lit'
    _ -> typeError (TypeMismatchError (L p (S.LiteralExpr lit)) (SomeType tp)
                                      (Just $ SomeType tp'))

inferFieldLit :: (LText, S.LLiteral) -> CtxM env (Some FieldLiteral)
inferFieldLit (L _ s, l) = do
  Pair _ l' <- inferLit l
  Some s' <- pure $ someSymbol s
  pure $ Some (K.FieldLiteral s' l')


enumElemIndex :: 1 <= CtxSize cs => Assignment SymbolRepr cs -> LText
              -> CtxM env (Some (Index cs))
enumElemIndex cs (L p s)
  | Some s' <- someSymbol s, Just i <- elemIndex s' cs = pure (Some i)
  | otherwise = typeError (TypeMismatchError (L p (S.LiteralExpr (L p (S.EnumLit (L p s)))))
                                             (SomeType (K.SetRepr cs)) Nothing)

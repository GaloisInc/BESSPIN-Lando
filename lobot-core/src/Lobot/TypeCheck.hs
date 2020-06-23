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
Module      : Lobot.TypeCheck
Description : The LOBOT type checker.
Copyright   : (c) Matt Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines the type checking algorithm for the Lobot AST.
-}

module Lobot.TypeCheck
  ( TypeError(..)
  , typeCheck
  , SomeTypeOrString(..)
  , ppTypeError
    -- * Internals
  , CtxM
  , Constraints(..)
  , checkDecls
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
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Prelude hiding (zipWith, unzip)

import Lobot.Utils
import Lobot.Lexer (AlexPosn, errorPrefix)
import Lobot.Expr as E
import Lobot.Kind as K
import Lobot.Syntax as S
import Lobot.Types as T
import Lobot.Pretty as P
import Lobot.Syntax.Pretty as S

-- | State/error monad for type checking. We maintain a hashmap from names to
-- 'K.Kind's as we iterate through a @[S.Kind]@.
type CtxM env = StateT (H.Map Text (Some (K.Kind env))) (Either TypeError)

data TypeError = TypeMismatchError S.LExpr SomeTypeOrString (Maybe SomeTypeOrString)
                 -- ^ argument order: expr, expected type, actual type
               | AbstractEqualityError S.LExpr SomeTypeOrString
               | TypeInferenceError S.LExpr
               | FieldNameError LText (Some (Assignment FieldRepr))
               | EmptyEnumOrSetError S.LType
               | KindUnionMismatchError LText (Some T.TypeRepr) (Some T.TypeRepr)
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
ppTypeError fp (AbstractEqualityError (L p x) tp) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Equality comparison on abstract type:" PP.<+> ppSomeTypeOrString tp
  PP.$$ PP.nest 2 (PP.text "  In expression:" PP.<+> S.ppExpr x)
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
  SomeType   :: T.TypeRepr tp -> SomeTypeOrString
  TypeString :: Text -> SomeTypeOrString
deriving instance Show SomeTypeOrString

ppSomeTypeOrString :: SomeTypeOrString -> PP.Doc
ppSomeTypeOrString (SomeType tp) = ppTypeRepr tp
ppSomeTypeOrString (TypeString s) = S.ppText s

lookupKind :: Assignment T.FunctionTypeRepr env
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


-- | Given a list of declarations, produce a list of typed kinds.
typeCheck :: Assignment T.FunctionTypeRepr env
          -> [S.Decl]
          -> Either TypeError [Some (K.Kind env)]
typeCheck env decls = evalStateT (checkDecls env decls) H.empty


-- Validating declarations

checkDecls :: Assignment T.FunctionTypeRepr env
           -> [S.Decl]
           -> CtxM env [Some (K.Kind env)]
checkDecls env decls = mapM (checkDecl env) decls

checkDecl :: Assignment T.FunctionTypeRepr env
          -> S.Decl
          -> CtxM env (Some (K.Kind env))
checkDecl env (S.KindDecl S.Kind{..}) = do
  Pair ctx (Cns ctx_cns) <- resolveType env kindType
  cns' <- mapM (checkExpr env ctx T.BoolRepr) kindConstraints
  let decl = Some (K.Kind (unLoc kindName) ctx env (cns' ++ ctx_cns))
  modify (H.insert (unLoc kindName) decl)
  pure $ decl
checkDecl env (S.TypeSynDecl nm tp) = do
  -- for now, we just handle type synonyms as kinds with no constraints
  checkDecl env (S.KindDecl (S.Kind nm tp []))
checkDecl env (S.AbsTypeDecl (L _ nm)) | Some nmSymb <- someSymbol nm = do
  -- for now, we also handle abstract types as kinds
  let decl = Some (K.Kind nm (T.AbsRepr nmSymb) env [])
  modify (H.insert nm decl)
  pure $ decl
  


-- Resolving all kind names in a `S.Type` to get a `T.Type` and a list of
-- additional constraints.

data Constraints env tp = Cns [K.KindExpr env tp T.BoolType]

resolveType :: Assignment T.FunctionTypeRepr env
            -> S.LType
            -> CtxM env (Pair TypeRepr (Constraints env))

resolveType _ (L _ S.BoolType) = pure $ Pair T.BoolRepr (Cns [])
resolveType _ (L _ S.IntType)  = pure $ Pair T.IntRepr  (Cns [])

resolveType _ tp@(L _ (S.EnumType cs)) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure $ Pair (T.EnumRepr cs') (Cns [])
    Right _ -> typeError (EmptyEnumOrSetError tp)
resolveType _ tp@((L _ (S.SetType cs))) | Some cs' <- someSymbols cs =
  case decideLeq (knownNat @1) (ctxSizeNat (size cs')) of
    Left LeqProof -> pure $ Pair (T.SetRepr cs') (Cns [])
    Right _ -> typeError (EmptyEnumOrSetError tp)

resolveType env (L _ (S.StructType fls)) = do
  Pair fls' cnss <- unzip <$> mapM (resolveFieldType env) fls
  let cns = concat $ toListWithIndex (\i (Cns' cns') -> liftExpr i <$> cns') cnss
  pure $ Pair (T.StructRepr fls') (Cns cns)

resolveType _ (L p (S.KindNames [])) =
  typeError (InternalError p "empty kind union")
resolveType env (L _ (S.KindNames [k])) = do
  Some k' <- lookupKind env k
  pure $ Pair (K.kindType k') (Cns (K.kindConstraints k'))
resolveType env (L p (S.KindNames (k:ks))) = do
  Some k' <- lookupKind env k
  Pair tp (Cns cns) <- resolveType env (L p (S.KindNames ks))
  case testEquality (K.kindType k') tp of
    Just Refl -> pure $ Pair (K.kindType k') (Cns (K.kindConstraints k' ++ cns))
    Nothing -> typeError (KindUnionMismatchError k (Some tp) (Some (K.kindType k')))

data Constraints' env (pr :: (Symbol, T.Type)) where
  Cns' :: [K.KindExpr env tp T.BoolType] -> Constraints' env '(nm, tp)

resolveFieldType :: Assignment T.FunctionTypeRepr env -> (LText, S.LType)
                 -> CtxM env (Pair FieldRepr (Constraints' env))
resolveFieldType env (L _ nm,tp) = do
  Some nm' <- pure $ someSymbol nm
  Pair tp' (Cns cns) <- resolveType env tp
  pure $ Pair (FieldRepr nm' tp') (Cns' cns)


-- Type inference and checking for expressions

inferExpr :: Assignment T.FunctionTypeRepr env
          -> T.TypeRepr ctx
          -> S.LExpr
          -> CtxM env (Pair T.TypeRepr (K.KindExpr env ctx))
inferExpr env _ (L _ (S.LiteralExpr l)) = do
  InferLit tp l' <- inferLit env l
  return $ Pair tp (E.LiteralExpr l')
inferExpr _ ctx (L _ S.SelfExpr) = pure $ Pair ctx K.SelfExpr
inferExpr _ (T.StructRepr ftps) (L _ (S.VarExpr (L p f)))
  | Some f' <- someSymbol f =
      case fieldIndex f' ftps of
        Just (SomeField tp i) -> pure $ Pair tp (E.FieldExpr K.SelfExpr i)
        Nothing -> typeError (FieldNameError (L p f) (Some ftps))
inferExpr _ _ (L _ (S.VarExpr f)) =
  typeError (FieldNameError f (Some Empty))
inferExpr env ctx (L _ (S.FieldExpr x (L p f))) = do
  Pair xtp x' <- inferExpr env ctx x
  case (xtp, someSymbol f) of
    (StructRepr ftps, Some f') -> case fieldIndex f' ftps of
      Just (SomeField tp i) -> pure $ Pair tp (E.FieldExpr x' i)
      Nothing -> typeError (FieldNameError (L p f) (Some ftps))
    _ -> typeError (TypeMismatchError x (TypeString "a struct")
                                        (Just $ SomeType xtp))
inferExpr env ctx (L _ (S.ApplyExpr (L p fn) xs)) = do
  case functionIndex fn env of
    Just (Some fi) -> do
      fntp@FunctionTypeRepr{..} <- return $ env ! fi
      mxs' <- checkExprs env ctx functionArgTypes xs
      case mxs' of
        Just xs' -> pure $ Pair functionRetType (E.ApplyExpr fi xs')
        Nothing -> typeError (FunctionArgLengthError (L p fn) (Some fntp) xs)
    Nothing -> typeError (UnknownFunctionError (L p fn))
inferExpr env ctx (L _ (S.EqExpr x y)) = do
  mb_x' <- try $ inferExpr env ctx x
  mb_y' <- try $ inferExpr env ctx y
  case (mb_x', mb_y') of
    (Left _, Left _) -> typeError (TypeInferenceError x)
    (Right (Pair xtp x'), Left _)
      | FalseRepr <- isAbstractType xtp -> do y' <- checkExpr env ctx xtp y
                                              pure $ Pair T.BoolRepr (E.EqExpr x' y')
      | otherwise -> typeError (AbstractEqualityError x (SomeType xtp))
    (Left _, Right (Pair ytp y'))
      | FalseRepr <- isAbstractType  ytp -> do x' <- checkExpr env ctx ytp x
                                               pure $ Pair T.BoolRepr (E.EqExpr x' y')
      | otherwise -> typeError (AbstractEqualityError y (SomeType ytp))
    (Right (Pair xtp x'), Right (Pair ytp y'))
      | FalseRepr <- isAbstractType xtp -> case testEquality xtp ytp of
          Just Refl -> pure $ Pair T.BoolRepr (E.EqExpr x' y')
          _ -> typeError (TypeMismatchError y (SomeType xtp) (Just $ SomeType ytp))
      | otherwise -> typeError (AbstractEqualityError x (SomeType xtp))
inferExpr env ctx (L _ (S.LteExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure $ Pair T.BoolRepr (E.LteExpr x' y')
inferExpr env ctx (L _ (S.PlusExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure $ Pair T.IntRepr (E.PlusExpr x' y')
inferExpr env ctx (L _ (S.MemberExpr x y)) = do
  mb_x' <- try $ inferExpr env ctx x
  mb_y' <- try $ inferExpr env ctx y
  case (mb_x', mb_y') of
    (Left _, Left _) -> typeError (TypeInferenceError x)
    (Right (Pair (EnumRepr cs) x'), Left _) -> do
      y' <- checkExpr env ctx (SetRepr cs) y
      pure $ Pair T.BoolRepr (E.MemberExpr x' y')
    (Left _, Right (Pair (SetRepr cs) y')) -> do
      x' <- checkExpr env ctx (EnumRepr cs) x
      pure $ Pair T.BoolRepr (E.MemberExpr x' y')
    (Right (Pair (EnumRepr xcs) x'), Right (Pair (SetRepr ycs) y')) ->
      case testEquality xcs ycs of
        Just Refl -> pure $ Pair T.BoolRepr (E.MemberExpr x' y')
        _ -> typeError (TypeMismatchError y (SomeType (SetRepr xcs))
                                            (Just $ SomeType (SetRepr ycs)))
    (Right (Pair xtp _), _) ->
      typeError (TypeMismatchError x (TypeString "an enum")
                                     (Just $ SomeType xtp))
    (_, Right (Pair ytp _)) ->
      typeError (TypeMismatchError y (TypeString "a set")
                                     (Just $ SomeType ytp))
inferExpr env ctx (L _ (S.ImpliesExpr x y)) = do
  x' <- checkExpr env ctx T.BoolRepr x
  y' <- checkExpr env ctx T.BoolRepr y
  pure $ Pair T.BoolRepr (E.ImpliesExpr x' y')
inferExpr env ctx (L _ (S.NotExpr x)) = do
  x' <- checkExpr env ctx T.BoolRepr x
  pure $ Pair T.BoolRepr (E.NotExpr x')
inferExpr _env _ctx (L p (S.IsInstanceExpr _x _t)) = do
  typeError (InternalError p "IsInstance (:) expressions are not yet supported")
  -- Pair t' (Cns cns) <- resolveType env t
  -- x' <- checkExpr env ctx t' x
  -- pure $ Pair T.BoolRepr (foldr1 K.AndExpr (giveSelf x' <$> cns))

checkExpr :: Assignment T.FunctionTypeRepr env
          -> T.TypeRepr ctx
          -> T.TypeRepr tp
          -> S.LExpr
          -> CtxM env (K.KindExpr env ctx tp)
checkExpr env _ tp (L _ (S.LiteralExpr l)) = do
  CheckLit l' <- checkLit env tp l
  return $ E.LiteralExpr l'
checkExpr env ctx tp x = do
  Pair tp' x' <- inferExpr env ctx x
  case testEquality tp tp' of
    Just Refl -> pure x'
    _ -> typeError (TypeMismatchError x (SomeType tp) (Just $ SomeType tp'))

checkExprs :: Assignment T.FunctionTypeRepr env
           -> T.TypeRepr ctx
           -> Assignment T.TypeRepr tps
           -> [S.LExpr]
           -> CtxM env (Maybe (Assignment (K.KindExpr env ctx) tps))
checkExprs _ _ Empty [] = pure $ Just Empty
checkExprs env ctx (tps :> tp) (x:xs) = do
  x' <- checkExpr env ctx tp x
  mxs' <- checkExprs env ctx tps xs
  case mxs' of
    Just xs' -> pure $ Just (xs' :> x')
    Nothing -> pure Nothing
checkExprs _ _ _ _ = pure Nothing

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

functionIndex :: Text
              -> Assignment FunctionTypeRepr fntps
              -> Maybe (Some (Index fntps))
functionIndex nm =
  findIndex $ \FunctionTypeRepr{..} -> nm == symbolRepr functionName

-- Type inference and checking for literals

data InferLit where
  InferLit :: IsAbstractType tp ~ 'False => T.TypeRepr tp -> E.Literal tp -> InferLit

-- | Infer the type of a literal without any knowledge of what its type should be.
inferLit :: Assignment T.FunctionTypeRepr env 
         -> S.LLiteral -> CtxM env InferLit
inferLit _ (L _ (S.BoolLit b)) = pure $ InferLit T.BoolRepr (E.BoolLit b)
inferLit _ (L _ (S.IntLit z))  = pure $ InferLit T.IntRepr  (E.IntLit z)
inferLit env (L _ (S.StructLit _tp ls)) = do
  -- Pair tp' _ <- resolveType env tp
  InferFieldLits fls <- toInferFieldLits <$> mapM (inferFieldLit env) ls
  pure $ InferLit (E.literalType (E.StructLit fls)) (E.StructLit fls)
inferLit _ lit@(L p _) = typeError (TypeInferenceError (L p (S.LiteralExpr lit)))

data CheckLit tp where
  CheckLit :: IsAbstractType tp ~ 'False => E.Literal tp -> CheckLit tp

-- | Check that the type of a literal is equal to some known type.
checkLit :: Assignment T.FunctionTypeRepr env
         -> T.TypeRepr tp -> S.LLiteral -> CtxM env (CheckLit tp)
checkLit _ (T.EnumRepr cs) (L _ (S.EnumLit e)) = do
  Some i <- enumElemIndex cs e
  pure $ CheckLit (E.EnumLit cs i)
checkLit _ tp lit@(L p (S.EnumLit _)) =
  typeError (TypeMismatchError (L p (S.LiteralExpr lit)) (SomeType tp)
                               (Just $ TypeString "an enum"))
checkLit _ (T.SetRepr cs) (L _ (S.SetLit es)) = do
  es' <- forM es (enumElemIndex cs)
  pure $ CheckLit (E.SetLit cs es')
checkLit _ tp lit@(L p (S.SetLit _)) =
  typeError (TypeMismatchError (L p (S.LiteralExpr lit)) (SomeType tp)
                               (Just $ TypeString "a set"))
checkLit env tp lit@(L p _) = do
  InferLit tp' lit' <- inferLit env lit
  case testEquality tp tp' of
    Just Refl -> pure $ CheckLit lit'
    _ -> typeError (TypeMismatchError (L p (S.LiteralExpr lit)) (SomeType tp)
                                      (Just $ SomeType tp'))

data InferFieldLit where
  InferFieldLit :: IsAbstractType tp ~ 'False => FieldLiteral '(nm, tp) -> InferFieldLit

inferFieldLit :: Assignment T.FunctionTypeRepr env 
              -> (LText, S.LLiteral) -> CtxM env InferFieldLit
inferFieldLit env (L _ s, l) = do
  InferLit _ l' <- inferLit env l
  Some s' <- pure $ someSymbol s
  pure $ InferFieldLit (E.FieldLiteral s' l')

data InferFieldLits where
  InferFieldLits :: AnyAbstractFields ftps ~ 'False
                 => Assignment FieldLiteral ftps
                 -> InferFieldLits

toInferFieldLits :: [InferFieldLit] -> InferFieldLits
toInferFieldLits [] = InferFieldLits Empty
toInferFieldLits (InferFieldLit fl : ifls)
  | InferFieldLits fls <- toInferFieldLits ifls = InferFieldLits (fls :> fl)

enumElemIndex :: 1 <= CtxSize cs => Assignment SymbolRepr cs -> LText
              -> CtxM env (Some (Index cs))
enumElemIndex cs (L p s)
  | Some s' <- someSymbol s, Just i <- elemIndex s' cs = pure (Some i)
  | otherwise = typeError (TypeMismatchError (L p (S.LiteralExpr (L p (S.EnumLit (L p s)))))
                                             (SomeType (T.SetRepr cs)) Nothing)

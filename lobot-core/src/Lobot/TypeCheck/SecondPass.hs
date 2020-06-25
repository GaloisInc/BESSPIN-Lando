{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lobot.TypeCheck.SecondPass
Description : The second pass of typechecking the Lobot AST.
Copyright   : (c) Matt Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module does the second pass of typechecking the Lobot AST.
-}

module Lobot.TypeCheck.SecondPass ( secondPass ) where

import qualified Data.HashMap as H

import Data.Text (Text, append)
import Control.Monad (when, forM_)
import Control.Monad.State (get, modify)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Prelude hiding ((!))

import Lobot.Utils hiding (unzip)
import Lobot.Expr as E
import Lobot.Kind as K
import Lobot.Syntax as S
import Lobot.Types as T

import Lobot.TypeCheck.Monad
import Lobot.TypeCheck.ISyntax as I


secondPass :: Assignment FunctionTypeRepr env
           -> [I.Kind]
           -> Either TypeError [Some (K.Kind env)]
secondPass env ks = evalCtxM (checkIKinds env ks)


type CtxM2 env = CtxM (Some (K.Kind env)) TypeError

addKind :: Text -> K.Kind env tp -> CtxM2 env ()
addKind nm k = modify (H.insert nm (Some k))

lookupKind :: LText -> CtxM2 env (Some (K.Kind env))
lookupKind (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just k' -> pure k'
    Nothing -> typeError (KindNameNotInScope (L p k))

lookupFunction :: Assignment T.FunctionTypeRepr env
               -> LText -> CtxM2 env (Some (Index env))
lookupFunction env (L p fn) =
  case findIndex (\FunctionTypeRepr{..} -> fn == symbolRepr functionName) env of
    Just idx -> pure idx
    Nothing -> typeError (FunctionNameNotInScope (L p fn))


checkIKinds :: Assignment FunctionTypeRepr env
            -> [I.Kind]
            -> CtxM2 env [Some (K.Kind env)]
checkIKinds env decls = mapM (checkIKind env) decls

checkIKind :: Assignment FunctionTypeRepr env
           -> I.Kind
           -> CtxM2 env (Some (K.Kind env))
checkIKind env (I.Kind (L _ nm) tp cns dcns) = do
  cns'  <- mapM (checkExpr env (singleton tp) T.BoolRepr) cns
  dcns' <- concat <$> mapM (resolveDerivedConstraint env tp) dcns
  let k' = K.Kind nm tp env (cns' ++ dcns')
  addKind nm k'
  pure $ Some k'


resolveDerivedConstraint :: Assignment T.FunctionTypeRepr env
                         -> T.TypeRepr tp
                         -> DerivedConstraint
                         -> CtxM2 env [K.KindExpr env tp T.BoolType]

resolveDerivedConstraint env tp (FromKind (L p nm)) = do
  Some k <- lookupKind (L p nm)
  case testEquality tp (K.kindType k) of
    Just Refl -> pure $ K.kindConstraints k
    _ -> typeError (InternalError p $ "Malformed derived constraint: FromKind " `append` nm)

resolveDerivedConstraint env tp (FromField (L p f) ds)
  | T.StructRepr ftps <- tp
  , Just (Some idx) <- findIndex (\(FieldRepr f' _) -> f == symbolRepr f') ftps
  , FieldRepr _ ftp <- ftps ! idx
    = do ds' <- concat <$> mapM (resolveDerivedConstraint env ftp) ds
         pure $ fmap (giveSelf (E.FieldExpr K.SelfExpr idx)) ds'
  | otherwise
    = typeError (InternalError p $ "Malformed derived constraint: FromField " `append` f)


-- Type inference and checking for expressions

-- | ...
guessExpr :: Assignment T.FunctionTypeRepr env
          -> Assignment T.TypeRepr ctx
          -> I.LExpr ctx
          -> CtxM2 env (Bool, Pair T.TypeRepr (E.Expr env ctx))

guessExpr env _ (L _ (I.LiteralExpr l)) = do
  (isGuess, InferLit tp l') <- guessLit env l
  pure (isGuess, Pair tp (E.LiteralExpr l'))

guessExpr _ ctx (L _ (I.VarExpr _ i)) = 
  pure (False, Pair (ctx ! i) (E.VarExpr i))
guessExpr _ (Empty :> T.StructRepr ftps) (L _ (I.SelfFieldExpr _ fi))
  | FieldRepr _ tp <- ftps ! fi
  = pure (False, Pair tp (E.FieldExpr (E.VarExpr baseIndex) fi))

guessExpr env ctx (L _ (I.FieldExpr x (L p f))) = do
  (isGuess, Pair xtp x') <- guessExpr env ctx x
  Some f' <- pure $ someSymbol f
  case xtp of
    StructRepr ftps -> case fieldIndex f' ftps of
      Just (SomeField tp i) -> pure (isGuess, Pair tp (E.FieldExpr x' i))
      Nothing -> typeError (FieldNameNotInScope (L p f))
    _ -> typeError (TypeMismatchError (unILExpr x) (TypeString "a struct")
                                                   (Just $ SomeType xtp))
  
guessExpr env ctx (L _ (I.ApplyExpr fn xs)) = do
  Some fi <- lookupFunction env fn
  fntp@FunctionTypeRepr{..} <- pure $ env ! fi
  mxs' <- checkExprs env ctx functionArgTypes xs
  case mxs' of
    Just xs' -> pure (False, Pair functionRetType (E.ApplyExpr fi xs'))
    Nothing -> typeError (FunctionArgLengthError fn (Some fntp) (unILExpr <$> xs))

guessExpr _env _ctx (L _ (I.EqExpr _x _y)) = undefined

guessExpr env ctx (L _ (I.LteExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.LteExpr x' y'))
guessExpr env ctx (L _ (I.PlusExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.PlusExpr x' y'))

guessExpr _env _ctx (L _ (I.MemberExpr _x _y)) = undefined

guessExpr env ctx (L _ (I.ImpliesExpr x y)) = do
  x' <- checkExpr env ctx T.BoolRepr x
  y' <- checkExpr env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.ImpliesExpr x' y'))
guessExpr env ctx (L _ (I.NotExpr x)) = do
  x' <- checkExpr env ctx T.BoolRepr x
  pure (False, Pair T.BoolRepr (E.NotExpr x'))

guessExpr env ctx (L _ (I.IsInstanceExpr x (_, Some tp, dcns))) = do
  x' <- checkExpr env ctx tp x
  dcns' <- concat <$> mapM (resolveDerivedConstraint env tp) dcns
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

-- | ...
inferExpr :: Assignment T.FunctionTypeRepr env
          -> Assignment T.TypeRepr ctx
          -> I.LExpr ctx
          -> CtxM2 env (Pair T.TypeRepr (E.Expr env ctx))
inferExpr env ctx x = do
  (isGuess, pr) <- guessExpr env ctx x
  if isGuess then typeError (TypeInferenceError (unILExpr x))
             else pure pr

-- | ...
checkExpr :: Assignment T.FunctionTypeRepr env
          -> Assignment T.TypeRepr ctx
          -> T.TypeRepr tp
          -> I.LExpr ctx
          -> CtxM2 env (E.Expr env ctx tp)
checkExpr env _ tp (L _ (I.LiteralExpr l)) = do
  CheckLit l' <- checkLit env tp l
  return $ E.LiteralExpr l'
checkExpr env ctx tp x = do
  Pair tp' x' <- inferExpr env ctx x
  case testEquality tp tp' of
    Just Refl -> pure x'
    _ -> typeError (TypeMismatchError (unILExpr x) (SomeType tp)
                                                   (Just $ SomeType tp'))

-- | ...
checkExprs :: Assignment T.FunctionTypeRepr env
           -> Assignment T.TypeRepr ctx
           -> Assignment T.TypeRepr tps
           -> [I.LExpr ctx]
           -> CtxM2 env (Maybe (Assignment (E.Expr env ctx) tps))
checkExprs _ _ Empty [] = pure $ Just Empty
checkExprs env ctx (tps :> tp) (x:xs) = do
  x' <- checkExpr env ctx tp x
  mxs' <- checkExprs env ctx tps xs
  case mxs' of
    Just xs' -> pure $ Just (xs' :> x')
    Nothing -> pure Nothing
checkExprs _ _ _ _ = pure Nothing


-- Type inference and checking for literals

data InferLit where
  InferLit :: IsAbstractType tp ~ 'False => T.TypeRepr tp -> E.Literal tp -> InferLit

-- | Guess, or infer, the type of a literal without any knowledge of what its
-- type should be. The 'Bool' returned is true if and only if the type
-- returned is a guess rather than an inference.
guessLit :: Assignment T.FunctionTypeRepr env 
         -> I.LLiteral -> CtxM2 env (Bool, InferLit)

guessLit env (L _ (I.BoolLit b)) = pure (False, InferLit T.BoolRepr (E.BoolLit b))
guessLit env (L _ (I.IntLit z))  = pure (False, InferLit T.IntRepr  (E.IntLit z))

guessLit env (L _ (I.EnumLit (L _ e))) | Some e' <- someSymbol e =
  pure (True , InferLit (T.EnumRepr (Empty :> e'))
                        (E.EnumLit  (Empty :> e') baseIndex))
guessLit env (L p (I.SetLit es)) | Some es' <- someSymbols (fmap unLoc es) =
  let idxs = toListWithIndex (\i _ -> Some i) es'
   in case decideLeq (knownNat @1) (ctxSizeNat (size es')) of
        Left LeqProof -> pure (True , InferLit (T.SetRepr es')
                                               (E.SetLit  es' idxs))
        Right _ -> typeError (TypeInferenceError (L p (S.LiteralExpr (L p (S.SetLit es)))))
  
guessLit env (L _ (I.StructLit Nothing fvs)) = do
  (areGuesses, fvs') <- unzip <$> mapM (guessFieldLit env) fvs
  InferFieldLits fvs'' <- pure $ toInferFieldLits fvs'
  pure (or areGuesses, InferLit (E.literalType (E.StructLit fvs'')) (E.StructLit fvs''))
guessLit env (L p (I.StructLit (Just (_, Some (T.StructRepr ftps), _dcns)) fvs))
  | FalseRepr <- anyAbstractFields ftps
  = do mfvs' <- checkFieldLits env ftps fvs
       -- TODO: Check that this is a valid instance given dcns?
       case mfvs' of
         Just fvs' -> pure (False, InferLit (T.StructRepr ftps) (E.StructLit fvs'))
         Nothing -> typeError (StructLiteralLengthError p (Some ftps) (fst <$> fvs))
guessLit env (L _ (I.StructLit (Just (tp, _, _)) fvs)) =
  typeError (StructLiteralTypeError tp)

data CheckLit tp where
  CheckLit :: IsAbstractType tp ~ 'False => E.Literal tp -> CheckLit tp

-- | Check that the type of a literal is equal to some known type.
checkLit :: Assignment T.FunctionTypeRepr env
         -> T.TypeRepr tp -> I.LLiteral -> CtxM2 env (CheckLit tp)
checkLit = undefined


-- Type inference and checking for field literals

data InferFieldLit where
  InferFieldLit :: IsAbstractType tp ~ 'False => FieldLiteral '(nm, tp) -> InferFieldLit

guessFieldLit :: Assignment T.FunctionTypeRepr env 
              -> (LText, I.LLiteral) -> CtxM2 env (Bool, InferFieldLit)
guessFieldLit env (L _ s, l) = do
  (isGuess, InferLit _ l') <- guessLit env l
  Some s' <- pure $ someSymbol s
  pure $ (isGuess, InferFieldLit (E.FieldLiteral s' l'))

data InferFieldLits where
  InferFieldLits :: AnyAbstractFields ftps ~ 'False
                 => Assignment FieldLiteral ftps
                 -> InferFieldLits

toInferFieldLits :: [InferFieldLit] -> InferFieldLits
toInferFieldLits [] = InferFieldLits Empty
toInferFieldLits (InferFieldLit fl : ifls)
  | InferFieldLits fls <- toInferFieldLits ifls = InferFieldLits (fls :> fl)

checkFieldLits :: AnyAbstractFields ftps ~ 'False
               => Assignment T.FunctionTypeRepr env 
               -> Assignment T.FieldRepr ftps
               -> [(LText, I.LLiteral)]
               -> CtxM2 env (Maybe (Assignment FieldLiteral ftps))
checkFieldLits _ Empty [] = pure $ Just Empty
checkFieldLits env (ftps :> FieldRepr s1 ftp) ((L p s2, l):fvs) = do
  when (symbolRepr s1 /= s2) $
    typeError (StructLiteralNameMismatchError (L p s2) (symbolRepr s1))
  CheckLit l' <- checkLit env ftp l
  let fv' = E.FieldLiteral s1 l'
  mfvs' <- checkFieldLits env ftps fvs
  case mfvs' of
    Just fvs' -> pure $ Just (fvs' :> fv')
    Nothing -> pure Nothing
checkFieldLits _ _ _ = pure Nothing

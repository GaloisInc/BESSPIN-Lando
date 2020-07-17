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
  , SecondPassResult(..)
  ) where

import qualified Data.HashMap as H

import Data.Text (Text, append)
import Data.List (union)
import Control.Monad (when, forM)
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

data SecondPassResult env = SecondPassResult [Some (K.Kind env)] [Some (K.Check env)]

secondPass :: Assignment FunctionTypeRepr env
           -> [I.Kind]
           -> [I.Check]
           -> H.Map Text (Some (K.Kind env))
           -> WithWarnings (Either TypeError) (SecondPassResult env)
secondPass env ks cks = evalStateT $ do
  ks' <- checkIKinds env ks
  cks' <- checkIChecks env cks
  return $ SecondPassResult ks' cks'

type CtxM2 env = CtxM (Some (K.Kind env)) TypeError

addKind :: Text -> K.Kind env tp -> CtxM2 env ()
addKind nm k = modify (H.insert nm (Some k))

lookupKind :: LText -> CtxM2 env (Some (K.Kind env))
lookupKind (L p k) = do
  mb_k' <- H.lookup k <$> get
  case mb_k' of
    Just k' -> pure k'
    Nothing -> throwError (KindNameNotInScope (L p k))

lookupFunction :: Assignment T.FunctionTypeRepr env
               -> LText -> CtxM2 env (Some (Index env))
lookupFunction env (L p fn) =
  case findIndex (\FunctionTypeRepr{..} -> fn == symbolRepr functionName) env of
    Just idx -> pure idx
    Nothing -> throwError (FunctionNameNotInScope (L p fn))


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

checkIChecks :: Assignment FunctionTypeRepr env
             -> [I.Check]
             -> CtxM2 env [Some (K.Check env)]
checkIChecks env decls = mapM (checkICheck env) decls

checkICheck :: forall env .
               Assignment FunctionTypeRepr env
            -> I.Check
            -> CtxM2 env (Some (K.Check env))
checkICheck env (I.Check (L _ nm) flds cns reqs) = do
  let tps = fmapFC checkFieldType flds
  cns'  <- mapM (checkExpr env tps T.BoolRepr) cns
  let collectDCs :: Index tps tp -> CheckField tp -> CtxM2 env [E.Expr env tps 'T.BoolType]
      collectDCs i (CheckField _ tp dcns) = do
        kes <- concat <$> mapM (resolveDerivedConstraint env tp) dcns
        return $ giveSelf (E.VarExpr i) <$> kes
  dcns' <- traverseAndCollect collectDCs flds
  reqs' <- mapM (checkExpr env tps T.BoolRepr) reqs
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

resolveDerivedConstraint _ tp (FromKind (L p nm)) = do
  Some k <- lookupKind (L p nm)
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

-- | Guess, or infer, the type of an expression without any knowledge of what
-- its type should be. The 'Bool' returned is true if and only if the type
-- returned is a guess rather than an inference.
guessExpr :: Assignment T.FunctionTypeRepr env
          -> Assignment T.TypeRepr ctx
          -> I.LExpr ctx
          -> CtxM2 env (Bool, Pair T.TypeRepr (E.Expr env ctx))

guessExpr env _ (L _ (I.LiteralExpr l)) = do
  (isGuess, GuessedLit tp l') <- guessLit env l
  pure (isGuess, Pair tp (E.LiteralExpr l'))

guessExpr _ ctx (L _ (I.VarExpr _ i)) = 
  pure (False, Pair (ctx ! i) (E.VarExpr i))
guessExpr _ (Empty :> T.StructRepr ftps) (L _ (I.SelfFieldExpr _ fi))
  | FieldRepr _ tp <- ftps ! fi
  = pure (False, Pair tp (E.FieldExpr (E.VarExpr baseIndex) fi))

guessExpr env ctx (L _ (I.FieldExpr x (L p f))) = do
  (_, Pair xtp x') <- guessExpr env ctx x
  Some f' <- pure $ someSymbol f
  case xtp of
    StructRepr ftps -> case fieldIndex f' ftps of
      Just (SomeField tp i) -> pure (False, Pair tp (E.FieldExpr x' i))
      Nothing -> throwError (NoSuchFieldError (L p f) (unILExpr x) (Some xtp))
    _ -> throwError (TypeMismatchError (unILExpr x) (TypeString "a struct")
                                                    (Just $ SomeType xtp))

guessExpr env ctx (L _ (I.ApplyExpr fn xs)) = do
  Some fi <- lookupFunction env fn
  fntp@FunctionTypeRepr{..} <- pure $ env ! fi
  mxs' <- checkExprs env ctx functionArgTypes (reverse xs)
  case mxs' of
    Just xs' -> pure (False, Pair functionRetType (E.ApplyExpr fi xs'))
    Nothing -> throwError (FunctionArgLengthError fn (Some fntp) (unILExpr <$> xs))

guessExpr env ctx (L _ (I.EqExpr x y)) = do
  (xGuess, Pair xtp x') <- guessExpr env ctx x
  (yGuess, Pair ytp y') <- guessExpr env ctx y
  let uni_err = TypeUnificationError (unILExpr x) (SomeType xtp)
                                     (unILExpr y) (SomeType ytp)
  case (isAbstractType xtp, isAbstractType ytp) of
    (FalseRepr, FalseRepr) -> do
      case (testEquality xtp ytp, xGuess, yGuess, unifyTypes xtp ytp) of
        (Just Refl, _, _, _) ->
          pure (False, Pair T.BoolRepr (E.EqExpr x' y'))
        (Nothing, False, True, _) -> do
          y'' <- checkExpr env ctx xtp y
          pure (False, Pair T.BoolRepr (E.EqExpr x' y''))
        (Nothing, True, False, _) -> do
          x'' <- checkExpr env ctx ytp x
          pure (False, Pair T.BoolRepr (E.EqExpr x'' y'))
        (Nothing, True, True, Just (SomeNonAbsTp uni_tp)) ->
          do { x'' <- checkExpr env ctx uni_tp x
             ; y'' <- checkExpr env ctx uni_tp y
             ; pure (False, Pair T.BoolRepr (E.EqExpr x'' y''))
             } -- `catchError` (const . trace "hi" $ throwError uni_err)
        _ -> throwError uni_err
    (TrueRepr, _) -> throwError (AbstractEqualityError (unILExpr x) (SomeType xtp))
    (_, TrueRepr) -> throwError (AbstractEqualityError (unILExpr y) (SomeType ytp))

guessExpr env ctx (L _ (I.LteExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.LteExpr x' y'))
guessExpr env ctx (L _ (I.LtExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.LtExpr x' y'))
guessExpr env ctx (L _ (I.GteExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.GteExpr x' y'))
guessExpr env ctx (L _ (I.GtExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.BoolRepr (E.GtExpr x' y'))
guessExpr env ctx (L _ (I.PlusExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.PlusExpr x' y'))
guessExpr env ctx (L _ (I.MinusExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.MinusExpr x' y'))
guessExpr env ctx (L _ (I.TimesExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.TimesExpr x' y'))
guessExpr env ctx (L _ (I.ModExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.ModExpr x' y'))
guessExpr env ctx (L _ (I.DivExpr x y)) = do
  x' <- checkExpr env ctx T.IntRepr x
  y' <- checkExpr env ctx T.IntRepr y
  pure (False, Pair T.IntRepr (E.DivExpr x' y'))

guessExpr env ctx (L _ (I.MemberExpr x y)) = do
  (xGuess, Pair xtp x') <- guessExpr env ctx x
  (yGuess, Pair ytp y') <- guessExpr env ctx y
  let uni_err = EnumSetUnificationError (unILExpr x) (SomeType xtp)
                                        (unILExpr y) (SomeType ytp)
  case (xtp, ytp) of
    (T.EnumRepr xcs, T.SetRepr ycs) -> do
      case (testEquality xcs ycs, xGuess, yGuess, unifyEnumNames xcs ycs) of
        (Just Refl, _, _, _) ->
          pure (False, Pair T.BoolRepr (E.MemberExpr x' y'))
        (Nothing, False, True, _) -> do
          y'' <- checkExpr env ctx (T.SetRepr xcs) y
          pure (False, Pair T.BoolRepr (E.MemberExpr x' y''))
        (Nothing, True, False, _) -> do
          x'' <- checkExpr env ctx (T.EnumRepr ycs) x
          pure (False, Pair T.BoolRepr (E.MemberExpr x'' y'))
        (Nothing, True, True, SomeNonEmptySyms uni_cs) ->
          do { x'' <- checkExpr env ctx (T.EnumRepr uni_cs) x
             ; y'' <- checkExpr env ctx (T.SetRepr  uni_cs) y
             ; pure (False, Pair T.BoolRepr (E.MemberExpr x'' y''))
             } `catchError` (const $ throwError uni_err)
        _ -> throwError uni_err
    (T.EnumRepr _, _) -> throwError (TypeMismatchError (unILExpr y)
                                                       (TypeString "a set")
                                                       (Just $ SomeType ytp))
    (_,_) -> throwError (TypeMismatchError (unILExpr x)
                                           (TypeString "an enum")
                                           (Just $ SomeType xtp))

guessExpr env ctx (L _ (I.AndExpr x y)) = do
  x' <- checkExpr env ctx T.BoolRepr x
  y' <- checkExpr env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.AndExpr x' y'))
guessExpr env ctx (L _ (I.OrExpr x y)) = do
  x' <- checkExpr env ctx T.BoolRepr x
  y' <- checkExpr env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.OrExpr x' y'))
guessExpr env ctx (L _ (I.XorExpr x y)) = do
  x' <- checkExpr env ctx T.BoolRepr x
  y' <- checkExpr env ctx T.BoolRepr y
  pure (False, Pair T.BoolRepr (E.XorExpr x' y'))
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


-- | Check that the type of an expression is equal to some known type.
checkExpr :: Assignment T.FunctionTypeRepr env
          -> Assignment T.TypeRepr ctx
          -> T.TypeRepr tp
          -> I.LExpr ctx
          -> CtxM2 env (E.Expr env ctx tp)
checkExpr env _ tp (L _ (I.LiteralExpr l)) = do
  CheckedLit l' <- checkLit env tp l
  return $ E.LiteralExpr l'
checkExpr env ctx tp x = do
  (isGuess, Pair tp' x') <- guessExpr env ctx x
  case (isGuess, testEquality tp tp') of
    -- NOTE: currently the below case never occurs
    (True, _) -> throwError (TypeInferenceError (unILExpr x))
    (False, Nothing) -> throwError (TypeMismatchError (unILExpr x) (SomeType tp)
                                                      (Just $ SomeType tp'))
    (False, Just Refl) -> pure x'

-- | Check that a list of expressions have the respective types of a list of
-- types. Returns 'Nothing' if the function is given a different number of
-- terms and types.
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

data GuessedLit where
  GuessedLit :: IsAbstractType tp ~ 'False
             => T.TypeRepr tp -> E.Literal tp -> GuessedLit

-- | Guess, or infer, the type of a literal without any knowledge of what its
-- type should be. The 'Bool' returned is true if and only if the type
-- returned is a guess rather than an inference.
guessLit :: Assignment T.FunctionTypeRepr env 
         -> I.LLiteral -> CtxM2 env (Bool, GuessedLit)

guessLit _ (L _ (I.BoolLit b)) = pure (False, GuessedLit T.BoolRepr (E.BoolLit b))
guessLit _ (L _ (I.IntLit z))  = pure (False, GuessedLit T.IntRepr  (E.IntLit z))

guessLit _ (L _ (I.EnumLit (L _ e))) | Some e' <- someSymbol e =
  pure (True , GuessedLit (T.EnumRepr (Empty :> e'))
                          (E.EnumLit  (Empty :> e') baseIndex))
guessLit _ (L _ (I.SetLit es)) | Some es' <- someSymbols (fmap unLoc es) =
  let idxs = toListWithIndex (\i _ -> Some i) es'
   in case decideLeq (knownNat @1) (ctxSizeNat (size es')) of
        Left LeqProof -> pure (True, GuessedLit (T.SetRepr es')
                                                (E.SetLit  es' idxs))
                   -- we want to be able to give a guess for the type of
                   --  an empty set, but the type doesn't allow it! so we
                   --  do this awful hack...
        Right _ -> let emptySet = (Empty :> knownSymbol @"")
                    in pure (True, GuessedLit (T.SetRepr emptySet)
                                              (E.SetLit emptySet []))
  
guessLit env (L _ (I.StructLit Nothing fvs)) = do
  (areGuesses, fvs') <- unzip <$> mapM (guessFieldLit env) fvs
  GuessedFieldLits fvs'' <- pure $ toGuessedFieldLits (reverse fvs')
  pure (or areGuesses, GuessedLit (E.literalType (E.StructLit fvs'')) (E.StructLit fvs''))
guessLit env (L p (I.StructLit (Just (_, Some (T.StructRepr ftps), _dcns)) fvs)) = do
  mfvs' <- checkFieldLits env ftps (reverse fvs)
  -- TODO: Check that this is a valid instance given dcns?
  case mfvs' of
     Just (CheckedFieldLits fvs') -> pure (False, GuessedLit (T.StructRepr ftps) (E.StructLit fvs'))
     Nothing -> throwError (StructLiteralLengthError p (Some ftps) (fst <$> fvs))
guessLit _ (L _ (I.StructLit (Just (tp, _, _)) _)) =
  throwError (StructLiteralTypeError tp)

data CheckedLit tp where
  CheckedLit :: IsAbstractType tp ~ 'False => E.Literal tp -> CheckedLit tp

-- | Check that the type of a literal is equal to some known type.
checkLit :: Assignment T.FunctionTypeRepr env
         -> T.TypeRepr tp -> I.LLiteral -> CtxM2 env (CheckedLit tp)

checkLit _ (T.EnumRepr cs) (L _ (I.EnumLit e)) = do
  Some i <- enumElemIndex cs e
  pure $ CheckedLit (E.EnumLit cs i)
checkLit _ tp l@(L p (I.EnumLit _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr (unILLit l))) (SomeType tp)
                                (Just $ TypeString "an enum"))
checkLit _ (T.SetRepr cs) (L _ (I.SetLit es)) = do
  es' <- forM es (enumElemIndex cs)
  pure $ CheckedLit (E.SetLit cs es')
checkLit _ tp l@(L p (I.SetLit _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr (unILLit l))) (SomeType tp)
                                (Just $ TypeString "a set"))

checkLit env (T.StructRepr ftps) (L p (I.StructLit Nothing fvs)) = do
  mfvs' <- checkFieldLits env ftps (reverse fvs)
  case mfvs' of
   Just (CheckedFieldLits fvs') -> pure $ CheckedLit (E.StructLit fvs')
   Nothing -> throwError (StructLiteralLengthError p (Some ftps) (fst <$> fvs))
checkLit env tp@(T.StructRepr _) l@(L p (I.StructLit _ _)) = do
  (_, GuessedLit tp' l') <- guessLit env l
  case testEquality tp tp' of
    Nothing ->
      throwError (TypeMismatchError (L p (S.LiteralExpr (unILLit l)))
                                    (SomeType tp)
                                    (Just $ SomeType tp'))
    Just Refl ->
      pure $ CheckedLit l'
checkLit _ tp l@(L p (I.StructLit _ _)) =
  throwError (TypeMismatchError (L p (S.LiteralExpr (unILLit l))) (SomeType tp)
                                (Just $ TypeString "a struct"))

checkLit env tp l@(L p _) = do
  (isGuess, GuessedLit tp' l') <- guessLit env l
  case (isGuess, testEquality tp tp') of
    (True, _) ->
      throwError (TypeInferenceError (L p (S.LiteralExpr (unILLit l))))
    (False, Nothing) ->
      throwError (TypeMismatchError (L p (S.LiteralExpr (unILLit l)))
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

guessFieldLit :: Assignment T.FunctionTypeRepr env 
              -> (LText, I.LLiteral) -> CtxM2 env (Bool, GuessedFieldLit)
guessFieldLit env (L _ s, l) = do
  (isGuess, GuessedLit _ l') <- guessLit env l
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

checkFieldLits :: Assignment T.FunctionTypeRepr env 
               -> Assignment T.FieldRepr ftps
               -> [(LText, I.LLiteral)]
               -> CtxM2 env (Maybe (CheckedFieldLits ftps))
checkFieldLits _ Empty [] = pure $ Just (CheckedFieldLits Empty)
checkFieldLits env (ftps :> FieldRepr s1 ftp) ((L p s2, l):fvs) = do
  when (symbolRepr s1 /= s2) $
    throwError (StructLiteralNameMismatchError (L p s2) (symbolRepr s1))
  CheckedLit l' <- checkLit env ftp l
  let fv' = E.FieldLiteral s1 l'
  mfvs' <- checkFieldLits env ftps fvs
  case mfvs' of
    Just (CheckedFieldLits fvs') -> pure $ Just (CheckedFieldLits (fvs' :> fv'))
    Nothing -> pure Nothing
checkFieldLits _ _ _ = pure Nothing


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

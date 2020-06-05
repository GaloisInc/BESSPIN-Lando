{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

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

module Lobot.Core.TypeCheck where

import Data.Parameterized.List.Length

import Data.Text (Text)
import Control.Monad (forM)
import Data.Parameterized.Some
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr

import Lobot.Core.Kind   as K
import Lobot.Core.Syntax as S


-- type Ctx       = [(String, CtxElem)]
-- data CtxElem   = KindElem (Some K.Kind) | TypeElem K.Type
-- type CtxM      = ReaderT Ctx (Either TypeError)
data TypeError = TypeMismatch S.Expr (Some K.TypeRepr)
               | EnumNameError Text (Some (List SymbolRepr))

-- getKind :: CtxElem -> Maybe (Some K.Kind)
-- getKind (KindElem k) = Just k
-- getKind (TypeElem _) = Nothing

elemIndex :: forall (f :: k -> *) l x. TestEquality f
          => f x -> List f l -> Maybe (Index l x)
elemIndex x Nil = Nothing
elemIndex x (y :< ys) = case testEquality x y of
  Nothing -> IndexThere <$> elemIndex x ys
  Just Refl -> pure IndexHere

someSymbols :: [Text] -> Some (List SymbolRepr)
someSymbols = undefined

-- -- | Given a list of kind declarations, produce a list of typed kinds.
-- typeCheck :: [S.Decl] -> Either TypeError [Some K.Kind]
-- typeCheck decls = mapMaybe (getKind . snd) $ runReaderT (tcDecls decls) []
-- 
-- tcDecls :: [S.Decl] -> CtxM Ctx
-- tcDecls []     = ask
-- tcDecls (d:ds) = do
--   d' <- tcDecl d
--   local (++ [d']) (tcDecls ds)
-- 
-- tcDecl :: S.Decl -> CtxM (String, CtxElem)
-- tcDecl = undefined


-- checkType :: 

tcExpr :: List K.FieldRepr ktps
       -> S.Expr -> K.TypeRepr tp -> Either TypeError (K.Expr ktps tp)
tcExpr = undefined


thing :: 1 <= (Length cs) => List SymbolRepr cs -> Text -> Either TypeError (Some (Index cs))
thing cs s | Some s' <- someSymbol s, Just i <- elemIndex s' cs = pure (Some i)
           | otherwise = Left (EnumNameError s (Some cs))


tcLit :: List K.FieldRepr ktps
      -> S.Literal -> K.TypeRepr tp -> Either TypeError (K.Literal tp)

tcLit _ (S.BoolLit b) K.BoolRepr = pure $ K.BoolLit b
tcLit _ (S.IntLit z)  K.IntRepr  = pure $ K.IntLit z

tcLit _ (S.EnumLit s) (K.EnumRepr cs) = do
  Some i <- thing cs s
  pure $ K.EnumLit cs i

tcLit _ (S.SetLit ls) (K.SetRepr cs)  = K.SetLit cs <$> forM ls (thing cs)
  
tcLit _ (S.KindLit is) (K.KindRepr fs) = undefined -- K.KindLit <$> forM is ()

tcLit _ lit typ = Left (TypeMismatch (S.LiteralExpr lit) (Some typ))
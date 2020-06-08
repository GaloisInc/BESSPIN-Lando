{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lobot.Core.Utils
Description : The LOBOT type checker.
Copyright   : (c) Matt Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines the type checking algorithm for the Lobot AST.
-}

module Lobot.Core.Utils where

import Data.Text (Text)
import Data.Parameterized.Some
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr

-- | Returns the index of the first element in the given assignment which is
-- equal by 'testEquality' to the query element, or Nothing if there is no
-- such element.
elemIndex :: forall (f :: k -> *) ctx x. TestEquality f
          => f x -> Assignment f ctx -> Maybe (Index ctx x)
elemIndex x ys = case traverseAndCollect (go x) ys of
                   Left i  -> Just i
                   Right _ -> Nothing
  where go :: forall (f :: k -> *) ctx x y. TestEquality f
           => f x -> Index ctx y -> f y -> Either (Index ctx x) ()
        go a i b | Just Refl <- testEquality a b = Left i
                 | otherwise = Right ()

-- | Generates an assignment of symbol representitves from a list.
someSymbols :: [Text] -> Some (Assignment SymbolRepr)
someSymbols = fromList . fmap someSymbol

-- | Converts a proof of 'Size' to a runtime representitive of 'CtxSize'.
sizeToCtxSize :: Size ctx -> NatRepr (CtxSize ctx)
sizeToCtxSize sz = case viewSize sz of
  ZeroSize -> knownNat @0
  IncSize sz' -> addNat (knownNat @1) (sizeToCtxSize sz')

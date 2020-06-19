{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lobot.Utils
Description : Useful functions.
Copyright   : (c) Matt Yacavone, Ben Selfridge, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines some functions used elsewhere in Lobot Core.
-}

module Lobot.Utils where

import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC

-- | Returns the index of the first element in the given assignment which is
-- equal by 'testEquality' to the query element, or Nothing if there is no
-- such element.
elemIndex :: TestEquality f => f x -> Assignment f ctx -> Maybe (Index ctx x)
elemIndex x = listToMaybe . elemIndices x

-- | Returns a list of indices that are equal by 'testEquality' to the query
-- element.
elemIndices :: TestEquality f => f x -> Assignment f ctx -> [Index ctx x]
elemIndices x ys = runIdentity (traverseAndCollect (go x) ys)
  where go :: forall k (f :: k -> *) x ctx' y . TestEquality f
           => f x -> Index ctx' y -> f y -> Identity [Index ctx' x]
        go a i b | Just Refl <- testEquality a b = Identity [i]
                 | otherwise = Identity []

-- | Returns the index of the first element in an assignment that satisfies the
-- given predicate.
findIndex :: (forall x . f x -> Bool)
          -> Assignment f ctx -> Maybe (Some (Index ctx))
findIndex p = listToMaybe . findIndices p

-- | Returns a list of indices that satisfy a given predicate.
findIndices :: (forall x . f x -> Bool) -> Assignment f ctx -> [Some (Index ctx)]
findIndices p xs = runIdentity (traverseAndCollect (go p) xs)
  where go :: forall k (f :: k -> *) ctx' y .
              (forall x . f x -> Bool) -> Index ctx' y -> f y -> Identity [Some (Index ctx')]
        go p' i x | p' x = Identity [Some i]
                  | otherwise = Identity []

-- | Generates an assignment of symbol representatives from a list.
someSymbols :: [Text] -> Some (Assignment SymbolRepr)
someSymbols = fromList . fmap someSymbol

-- | Converts a proof of 'Size' to a runtime representative of 'CtxSize'.
ctxSizeNat :: Size ctx -> NatRepr (CtxSize ctx)
ctxSizeNat sz = case viewSize sz of
  ZeroSize -> knownNat @0
  IncSize sz' -> addNat (knownNat @1) (ctxSizeNat sz')

-- | Create a pair of assignments from a list of pairs of values.
-- Implementation is based on 'fromList'.
unzip :: [Pair f g] -> Pair (Assignment f) (Assignment g)
unzip = go (empty, empty)
  where go :: (Assignment f ctx, Assignment g ctx) -> [Pair f g]
           -> Pair (Assignment f) (Assignment g)
        go (prevl, prevr) [] = Pair prevl prevr
        go (prevl, prevr) (Pair x y:next) =
          (go $! (prevl `extend` x, prevr `extend` y)) next

-- | A non-monadic version of 'traverseWithIndex'.
mapWithIndex :: (forall tp. Index ctx tp -> f tp -> g tp) -> Assignment f ctx -> Assignment g ctx
mapWithIndex f a = generate (size a) $ \i -> f i (a ! i)

-- | A version of 'toListFC' which includes indices.
toListWithIndex :: (forall tp. Index ctx tp -> f tp -> a) -> Assignment f ctx -> [a] 
toListWithIndex f xs = toListFC (\i -> f i (xs ! i)) (generate (size xs) id)

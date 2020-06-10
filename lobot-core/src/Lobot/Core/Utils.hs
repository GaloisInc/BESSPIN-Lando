{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lobot.Core.Utils
Description : Useful functions.
Copyright   : (c) Matt Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines some functions used elsewhere in Lobot Core.
-}

module Lobot.Core.Utils where

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
elemIndex :: forall k (f :: k -> *) ctx x. TestEquality f
          => f x -> Assignment f ctx -> Maybe (Index ctx x)
elemIndex x ys = case traverseAndCollect (go x) ys of
                   Left i  -> Just i
                   Right _ -> Nothing
  where go :: forall k (f :: k -> *) ctx x y. TestEquality f
           => f x -> Index ctx y -> f y -> Either (Index ctx x) ()
        go a i b | Just Refl <- testEquality a b = Left i
                 | otherwise = Right ()

-- | Generates an assignment of symbol representitves from a list.
someSymbols :: [Text] -> Some (Assignment SymbolRepr)
someSymbols = fromList . fmap someSymbol

-- | Converts a proof of 'Size' to a runtime representitive of 'CtxSize'.
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

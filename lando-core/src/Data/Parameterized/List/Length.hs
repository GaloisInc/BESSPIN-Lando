{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Parameterized.List.Length
  ( Length
  , ilength
  , natReprToIndex
  ) where

import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import GHC.TypeLits

-- | Length of a type-level list.
type family Length (l :: [k]) :: Nat where
  Length '[] = 0
  Length (c ': cs) = Length cs + 1

-- | Compute the length of a 'List'.
ilength :: List f sh -> NatRepr (Length sh)
ilength Nil = knownNat @0
ilength (_ :< as) = incNat (ilength as)

-- | Given a 'List', convert a 'NatRepr' into an 'Index' into that list. If the
-- 'NatRepr' exceeds the length of the list, return 'Nothing'.
natReprToIndex :: List f sh -> NatRepr n -> Maybe (Some (Index sh))
natReprToIndex Nil _ = Nothing
natReprToIndex (_ :< as) n = case isZeroOrGT1 n of
  Left Refl -> Just $ Some (IndexHere)
  Right LeqProof
    | Just (Some ix) <- natReprToIndex as (decNat n) -> Just (Some (IndexThere ix))
  _ -> Nothing

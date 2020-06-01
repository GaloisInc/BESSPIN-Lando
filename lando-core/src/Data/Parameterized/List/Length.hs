{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Parameterized.List.Length
  ( Length
  , ilength
  ) where

import Data.Parameterized.List
import Data.Parameterized.NatRepr
import GHC.TypeLits

type family Length (l :: [k]) :: Nat where
  Length '[] = 0
  Length (c ': cs) = Length cs + 1

ilength :: List f sh -> NatRepr (Length sh)
ilength Nil = knownNat @0
ilength (_ :< as) = incNat (ilength as)

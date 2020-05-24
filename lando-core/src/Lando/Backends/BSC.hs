module Lando.Backends.BSC
  ( RV(..)
  , RegWidth(..)
  , Extensions(..)
  , MExt(..)
  , AExt(..)
  , FExt(..)
  , DExt(..)
  , CExt(..)
  ) where

data RV = RV RegWidth Extensions

data RegWidth = RV32 | RV64

data Extensions = Extensions { mExt :: MExt
                             , aExt :: AExt
                             , fExt :: FExt
                             , dExt :: DExt
                             , cExt :: CExt
                             }

data MExt = MYes | MNo
data AExt = AYes | ANo
data FExt = FYes | FNo
data DExt = DYes | DNo
data CExt = CYes | CNo


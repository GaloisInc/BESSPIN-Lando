-- | This module defines the feature model for the Bluespec compiler Piccolo
-- backend.
module Lando.RISCV
  ( RV(..)
  , RegWidth(..)
  , Extensions(..)
  , MExt(..)
  , AExt(..)
  , FExt(..)
  , DExt(..)
  , CExt(..)
  ) where

-- | The RISC-V feature model.
data RV = RV RegWidth Extensions

-- | Register widths supported by RISC-V
data RegWidth = RV32 | RV64

-- | ISA extensions (on top of the base ISA)
data Extensions = Extensions { mExt :: MExt
                             , aExt :: AExt
                             , fExt :: FExt
                             , dExt :: DExt
                             , cExt :: CExt
                             }

-- | The M (multiply) extension
data MExt = MYes | MNo
-- | The A (memory atomics) extension
data AExt = AYes | ANo
-- | The F (single-precision floating point) extension
data FExt = FYes | FNo
-- | The D (double-precision floating point) extension
data DExt = DYes | DNo
-- | The C (compressed instructions) extension
data CExt = CYes | CNo

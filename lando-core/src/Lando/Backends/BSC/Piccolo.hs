-- | This module defines the feature model for the Bluespec compiler Piccolo
-- backend.
module Lando.Backends.BSC.Piccolo
  ( PiccoloOptions(..)
  ) where

import Lando.RISCV

data PiccoloOptions = PiccoloOptions { piccoloRV :: RV }

-- | Build a new configuration of the Piccolo processor.
buildPiccolo :: FilePath -- ^ Path to Piccolo directory
             -> PiccoloOptions
             -> IO ()
buildPiccolo = undefined

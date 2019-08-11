module Compat
  ( Monad(..)
  , fromIntegral
  , mapM
  , mapM_
  ) where

import           Control.Monad (mapM, mapM_)
import           Kernel        (Monad (..), fromIntegral)

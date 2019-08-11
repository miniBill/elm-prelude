module Compat
  ( Monad(..)
  , fromInteger
  , fromIntegral
  , mapM
  , mapM_
  ) where

import           Control.Monad (mapM, mapM_)
import           Kernel        (Monad (..), fromInteger, fromIntegral)

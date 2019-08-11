module Compat
  ( Either(..)
  , Monad(..)
  , fromIntegral
  , mapM
  , mapM_
  ) where

import           Control.Monad (mapM, mapM_)
import           Kernel        (Either (..), Monad (..), fromIntegral)

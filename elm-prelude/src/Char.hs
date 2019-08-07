module Char
  ( toUpper
  ) where

import qualified Data.Char
import           Protolude

toUpper :: Char -> Char
toUpper = Data.Char.toUpper

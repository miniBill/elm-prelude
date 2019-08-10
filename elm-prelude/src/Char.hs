module Char
  ( Char
  , toLower
  , toUpper
  ) where

import qualified Data.Char
import           Kernel    (Char)

{-| Convert to lower case. -}
toLower :: Char -> Char
toLower = Data.Char.toLower

{-| Convert to upper case. -}
toUpper :: Char -> Char
toUpper = Data.Char.toUpper

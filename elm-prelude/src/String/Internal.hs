{-# LANGUAGE PackageImports #-}

module String.Internal
  ( String(..)
  , raw
  , unpack
  ) where

import qualified Data.String
import qualified Data.Text      as T
import           Kernel         (Appendable (..), Char, Equatable (..), List)
import qualified "base" Prelude as P

newtype String =
  String
    { runString :: T.Text
    }

instance Data.String.IsString String where
  fromString s = String (T.pack s)

instance Appendable String where
  append (String l) (String r) = String (T.append l r)

instance Equatable String where
  equal (String l) (String r) = (P.==) l r

unpack :: String -> List Char
unpack (String s) = T.unpack s

raw :: String -> T.Text
raw (String s) = s

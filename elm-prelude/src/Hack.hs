{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Hack
  ( mul
  ) where

import qualified "base" Prelude as P

mul :: P.Num a => a -> a -> a
mul = (P.*)

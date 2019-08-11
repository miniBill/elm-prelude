module Main
  ( main
  ) where

import           Compat          (mapM_)
import qualified Test.Basics     as Basics
import           Test.QuickCheck (quickCheck)

main :: IO ()
main = mapM_ (\f -> quickCheck $ f 0) [Basics.tests]

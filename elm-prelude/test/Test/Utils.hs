module Test.Utils
  ( describe
  , test
  ) where

import           Char            (Char)
import qualified List
import qualified String
import           Test.QuickCheck (Property, Testable, conjoin, counterexample)

label :: Int -> String -> List Char
label indent msg = String.toList $ String.repeat indent "  " ++ msg ++ ":"

describe :: Testable prop => String -> [Int -> prop] -> Int -> Property
describe msg ts indent =
  counterexample (label indent msg) $
  conjoin $ List.map (\t -> t $ indent + 1) ts

test :: Testable prop => String -> prop -> Int -> Property
test msg prop indent = counterexample (label indent msg) prop

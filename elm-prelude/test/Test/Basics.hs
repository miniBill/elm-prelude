module Test.Basics
  ( tests
  ) where

--import           Test.Invariant  (commutative)
import           Test.QuickCheck (Property, (=/=), (===))

import qualified Dict
import           Test.Utils      (describe, test)

tests :: Int -> Property
tests =
  let comparison =
        describe
          "Comparison"
          [ test "max" $ 42 === max 32 (42 :: Int)
          , test "min" $ 42 === min 91 42
          , test "clamp low" $ 10 === clamp 10 20 5
          , test "clamp mid" $ 15 === clamp 10 20 15
          , test "clamp high" $ 20 === clamp 10 20 25
          , test "5 < 6" $ True === (5 < 6)
          , test "6 < 5" $ False === (6 < 5)
          , test "6 < 6" $ False === (6 < 6)
          , test "5 > 6" $ False === (5 > 6)
          , test "6 > 5" $ True === (6 > 5)
          , test "6 > 6" $ False === (6 > 6)
          , test "5 <= 6" $ True === (5 <= 6)
          , test "6 <= 5" $ False === (6 <= 5)
          , test "6 <= 6" $ True === (6 <= 6)
          , test "compare \"A\" \"B\"" $ LT === compare "A" "B"
          , test "compare 'f' 'f'" $ EQ === compare 'f' 'f'
          , test "compare (1, 2, 3, 4, 5, 6) (0, 1, 2, 3, 4, 5)" $
            GT === compare (1, 2, 3, 4, 5, 6) (0, 1, 2, 3, 4, 5)
          , test "compare ['a'] ['b']" $ LT === compare ['a'] ['b']
          --, test "array equality" $ Array.fromList [1, 1, 1, 1] === Array.repeat 4 1
          --, test "set equality" $ Set.fromList [1, 2] === Set.fromList [2, 1]
          , test "dict equality" $
            Dict.fromList [(1, 1), (2, 2)] === Dict.fromList [(2, 2), (1, 1)]
          , test "char equality" $ '0' =/= '饑'
          --, test "date equality" $ Date.fromString "2/7/1992" === Date.fromString "2/7/1992"
          --, test "date equality" $ Date.fromString "11/16/1995" =/= Date.fromString "2/7/1992"
          ]
   in describe "Basics" [comparison]

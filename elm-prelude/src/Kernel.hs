{-# LANGUAGE NoRebindableSyntax   #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Kernel
  ( Appendable(..)
  , Bool(..)
  , Char
  , Eq(..)
  , Float
  , Int
  , IO
  , List
  , Maybe(..)
  , Monad(..)
  , Number(..)
  , Ord(..)
  , Ordering(..)
  , String
  , (.)
  , (&)
  , (&&)
  , (||)
  , ($)
  , acos
  , asin
  , atan
  , atan2
  , ceiling
  , cos
  , e
  , fdiv
  , floor
  , fromIntegral
  , idiv
  , isInfinite
  , isNaN
  , log
  , logBase
  , modBy
  , not
  , pi
  , remainderBy
  , round
  , show
  , sin
  , sqrt
  , tan
  , toFloat
  , truncate
  , xor
  ) where

import           Data.Function  ((&))
import qualified Data.Text      as T
import qualified Hack
import           "base" Prelude (Bool (..), Char, Eq (..), Maybe (..),
                                 Monad (..), Ord (..), Ordering, Show (..),
                                 fromIntegral, not, ($), (&&), (.), (||))
import qualified "base" Prelude as P

type IO a = P.IO a

type Int = P.Integer

type Float = P.Float

type List a = [a]

type String = T.Text

infixl 4 ++

class Appendable a where
  (++) :: a -> a -> a

instance Appendable [a] where
  (++) = (P.++)

instance Appendable String where
  (++) = T.append

infixl 6 +

infixl 6 -

infixl 7 *

infixr 8 ^

class Ord a =>
      Number a
  where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  (^) :: a -> a -> a
  negate :: a -> a
  fromInteger :: Int -> a

instance Number Int where
  (+) = (P.+)
  (-) = (P.-)
  (*) = Hack.mul
  (^) = (P.^)
  negate = P.negate
  fromInteger i = i

instance Number Float where
  (+) = (P.+)
  (-) = (P.-)
  (*) = Hack.mul
  (^) = (P.**)
  negate = P.negate
  fromInteger = P.fromInteger

fdiv :: Float -> Float -> Float
fdiv = (P./)

idiv :: Int -> Int -> Int
idiv = P.div

isInfinite :: Float -> Bool
isInfinite = P.isInfinite

isNaN :: Float -> Bool
isNaN = P.isNaN

sin :: Float -> Float
sin = P.sin

cos :: Float -> Float
cos = P.cos

tan :: Float -> Float
tan = P.tan

asin :: Float -> Float
asin = P.asin

acos :: Float -> Float
acos = P.acos

atan :: Float -> Float
atan = P.atan

atan2 :: Float -> Float -> Float
atan2 = P.atan2

sqrt :: Float -> Float
sqrt = P.sqrt

log :: Float -> Float
log = P.log

logBase :: Float -> Float -> Float
logBase = P.logBase

e :: Float
e = P.exp 1

pi :: Float
pi = P.pi

xor :: Bool -> Bool -> Bool
xor a b = a /= b

toFloat :: Int -> Float
toFloat = P.fromInteger

round :: Float -> Int
round = P.round

floor :: Float -> Int
floor = P.floor

ceiling :: Float -> Int
ceiling = P.ceiling

truncate :: Float -> Int
truncate = P.truncate

modBy :: Int -> Int -> Int
modBy by val = P.mod val by

remainderBy :: Int -> Int -> Int
remainderBy by val = P.rem val by

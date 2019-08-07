{-# LANGUAGE NoRebindableSyntax   #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Kernel
  ( Appendable(..)
  , Bool(..)
  , Char
  , Comparable(..)
  , Equatable(..)
  , Float
  , Int
  , IO
  , List
  , Maybe(..)
  , Monad(..)
  , Number(..)
  , Order(..)
  , (.)
  , (&)
  , ($)
  , acos
  , and
  , asin
  , atan
  , atan2
  , ceiling
  , cos
  , e
  , fdiv
  , floor
  , fromIntegral
  , ge
  , gt
  , idiv
  , isInfinite
  , isNaN
  , le
  , log
  , logBase
  , lt
  , modBy
  , not
  , notEqual
  , or
  , orderToOrdering
  , pi
  , remainderBy
  , round
  , sin
  , sqrt
  , tan
  , toFloat
  , truncate
  , xor
  ) where

import           Data.Function  ((&))
import qualified Hack
import           "base" Prelude (Bool (..), Char, Maybe (..), Monad (..),
                                 fromIntegral, ($), (.))
import qualified "base" Prelude as P

type IO a = P.IO a

type Int = P.Integer

type Float = P.Float

type List a = [a]

class Appendable a where
  append :: a -> a -> a

instance Appendable [a] where
  append = (P.++)

class Equatable a where
  equal :: a -> a -> Bool

instance Equatable Float where
  equal l r = (P.==) l r

instance Equatable Int where
  equal l r = (P.==) l r

instance (Equatable a, Equatable b) => Equatable (a, b) where
  equal (a, b) (c, d) = and (equal a c) (equal b d)

instance (Equatable a, Equatable b, Equatable c) => Equatable (a, b, c) where
  equal (a, b, c) (d, f, g) = and (equal a d) $ and (equal b f) (equal c g)

class Equatable a =>
      Comparable a
  where
  compare :: a -> a -> Order

instance Comparable Int where
  compare l r =
    if (P.==) l r
      then EQ
      else if (P.<) l r
             then LT
             else GT

instance Comparable Float where
  compare l r =
    if (P.==) l r
      then EQ
      else if (P.<) l r
             then LT
             else GT

class Comparable a =>
      Number a
  where
  add :: a -> a -> a
  mul :: a -> a -> a
  sub :: a -> a -> a
  pow :: a -> a -> a
  negate :: a -> a
  fromInteger :: Int -> a

instance Number Int where
  add = (P.+)
  mul = Hack.mul
  sub = (P.-)
  pow = (P.^)
  negate = P.negate
  fromInteger i = i

instance Number Float where
  add = (P.+)
  mul = Hack.mul
  sub = (P.-)
  pow = (P.**)
  negate = P.negate
  fromInteger = P.fromInteger

data Order
  = LT
  | EQ
  | GT

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

lt :: Comparable a => a -> a -> Bool
lt l r =
  case compare l r of
    LT -> True
    _  -> False

le :: Comparable a => a -> a -> Bool
le l r =
  case compare l r of
    GT -> False
    _  -> True

notEqual :: Equatable a => a -> a -> Bool
notEqual l r = not (equal l r)

ge :: Comparable a => a -> a -> Bool
ge l r =
  case compare l r of
    LT -> False
    _  -> True

gt :: Comparable a => a -> a -> Bool
gt l r =
  case compare l r of
    GT -> True
    _  -> False

and :: Bool -> Bool -> Bool
and a b =
  case a of
    False -> False
    True  -> b

or :: Bool -> Bool -> Bool
or a b =
  case a of
    True  -> True
    False -> b

xor :: Bool -> Bool -> Bool
xor a b =
  case a of
    False -> b
    True ->
      case b of
        True  -> False
        False -> True

not :: Bool -> Bool
not a =
  case a of
    False -> True
    True  -> False

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

orderToOrdering :: Order -> P.Ordering
orderToOrdering value =
  case value of
    LT -> P.LT
    EQ -> P.EQ
    GT -> P.GT

{-# LANGUAGE NoImplicitPrelude #-}

{-| Tons of useful functions that get imported by default. -}
module Prelude
  ( -- * Math
    Int
  , Float
  , (+)
  , (-)
  , (*)
  , (/)
  , (//)
  , (^)
  -- * Int to Float / Float to Int
  , toFloat
  , round
  , floor
  , ceiling
  , truncate
  -- * Equality
  , (==)
  , (/=)
  -- * Comparison
  --
  -- $comparison
  , (<)
  , (>)
  , (<=)
  , (>=)
  , max
  , min
  , compare
  , Ordering
  -- * Booleans
  , Bool(..)
  , not
  , (&&)
  , (||)
  , xor
  -- * Append Strings and Lists
  , (++)
  -- * Fancier Math
  , modBy
  , remainderBy
  , negate
  , abs
  , clamp
  , sqrt
  , logBase
  , exp
  -- * Angles
  , degrees
  , radians
  , turns
  -- * Trigonometry
  , pi
  , cos
  , sin
  , tan
  , acos
  , asin
  , atan
  , atan2
  -- * Polar Coordinates
  , toPolar
  , fromPolar
  -- * Floating Point Checks
  , isNaN
  , isInfinite
  -- * Function Helpers
  , identity
  , always
  , ($)
  , (&)
  , (.)
  , Never
  , never
  -- * Other data structures
  , List
  , Maybe(..)
  , String
  , IO
  , Eq
  , Ord
  , Number
  ) where

import           Kernel (Appendable, Bool (..), Eq, IO, List, Maybe (..),
                         Number (..), Ord, Ordering (..), String, ($), (&), (.))
import qualified Kernel

infixl 6 +

infixl 6 -

infixl 7 *

infixr 8 ^

infixl 7 /

infixl 7 //

infix 4 ==

infix 4 /=

infix 4 <

infix 4 >

infix 4 <=

infix 4 >=

infixr 3 &&

infixr 3 ||

infixr 5 ++

-- MATHEMATICS
-- | An @Int@ is a whole number. Valid syntax for integers includes:
--
-- > 0
-- > 42
-- > 9000
-- > 0xFF   -- 255 in hexadecimal
-- > 0x000A --  10 in hexadecimal
--
-- __Historical Note:__ The name @Int@ comes from the term
-- [integer](https://en.wikipedia.org/wiki/Integer).
-- It appears that the @int@ abbreviation was introduced in
-- [ALGOL 68](https://en.wikipedia.org/wiki/ALGOL_68), shortening it
-- from @integer@ in [ALGOL 60](https://en.wikipedia.org/wiki/ALGOL_60).
-- Today, almost all programming languages use this abbreviation.
--
-- __@elm-prelude@ Note:__ This is an alias for Haskell's arbitrary precision
-- @Integer@ type, so it doesn't suffer from any precision problem.
type Int = Kernel.Int

-- | A @Float@ is a
-- [floating-point number](https://en.wikipedia.org/wiki/Floating-point_arithmetic).
-- Valid syntax for floats includes:
--
-- > 0
-- > 42
-- > 3.14
-- > 0.1234
-- > 6.022e23   -- == (6.022 * 10^23)
-- > 6.022e+23  -- == (6.022 * 10^23)
-- > 1.602e−19  -- == (1.602 * 10^-19)
-- > 1e3        -- == (1 * 10^3) == 1000
--
-- __Historical Note:__ The particular details of floats (e.g. @NaN@) are
-- specified by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754)
-- which is literally hard-coded into almost all CPUs in the world.
-- That means if you think @NaN@ is weird, you must successfully overtake
-- Intel and AMD with a chip that is not backwards compatible with any
-- widely-used assembly language.
type Float = Kernel.Float

-- | Add two numbers. The @number@ type variable means this operation can be
-- specialized to @Int -> Int -> Int@ or to @Float -> Float -> Float@. So you
-- can do things like this:
--
-- > 3002 + 4004 == 7006  -- all ints
-- > 3.14 + 3.14 == 6.28  -- all floats
--
-- You /cannot/ add an @Int@ and a @Float@ directly though. Use functions like
-- 'toFloat' or 'round' to convert both values to the same type.
-- So if you needed to add a list length to a @Float@ for some reason, you
-- could say one of these:
--
-- >>> 3.14 + toFloat (List.length [1,2,3])
-- 6.14
-- >>> round 3.14 + List.length [1,2,3]
-- 6
--
-- __Note:__ Languages like Java and JavaScript automatically convert @Int@ values
-- to @Float@ values when you mix and match. This can make it difficult to be sure
-- exactly what type of number you are dealing with. When you try to /infer/ these
-- conversions (as Scala does) it can be even more confusing. Elm has opted for a
-- design that makes all conversions explicit, and @elm-prelude@ follows
-- that convention.
--
-- __@elm-prelude@ Note:__ the @Number@ typeclass takes the place of Elm's
-- constrained type variable. In general, you should not need to worry about it.
(+) :: Number number => number -> number -> number
(+) = Kernel.add

-- | Subtract numbers like @4 - 3 == 1@.
--
-- See '+' for docs on the @Number@ constraint.
(-) :: Number number => number -> number -> number
(-) = Kernel.sub

-- | Multiply numbers like @2 * 3 == 6@.
--
-- See '+' for docs on the @Number@ constraint.
(*) :: Number number => number -> number -> number
(*) = Kernel.mul

-- | Floating-point division:
--
-- >>> 3.14 / 2
-- 1.57
(/) :: Float -> Float -> Float
(/) = Kernel.fdiv

-- | Integer division:
--
-- >>> 3 // 2
-- 1
--
-- Notice that the remainder is discarded.
(//) :: Int -> Int -> Int
(//) = Kernel.idiv

-- | Exponentiation
--
-- >>> 3^2
-- 9
-- >>> 3^3
-- 27
(^) :: Number number => number -> number -> number
(^) = Kernel.pow

-- INT TO FLOAT / FLOAT TO INT
-- | Convert an integer into a float. Useful when mixing @Int@ and @Float@
-- values like this:
--
-- > halfOf :: Int -> Float
-- > halfOf number =
-- >   toFloat number / 2
toFloat :: Int -> Float
toFloat = Kernel.toFloat

-- | Round a number to the nearest integer.
--
-- >>> round 1.0
-- 1
-- >>> round 1.2
-- 1
-- >>> round 1.5
-- 2
-- >>> round 1.8
-- 2
-- >>> round -1.2
-- -1
-- >>> round -1.5
-- -1
-- >>> round -1.8
-- -2
round :: Float -> Int
round = Kernel.round

-- | Floor function, rounding down.
--
-- >>> floor 1.0
-- 1
-- >>> floor 1.2
-- 1
-- >>> floor 1.5
-- 1
-- >>> floor 1.8
-- 1
-- >>> floor -1.2
-- -2
-- >>> floor -1.5
-- -2
-- >>> floor -1.8
-- -2
floor :: Float -> Int
floor = Kernel.floor

-- | Ceiling function, rounding up.
--
-- >>> ceiling 1.0
-- 1
-- >>> ceiling 1.2
-- 2
-- >>> ceiling 1.5
-- 2
-- >>> ceiling 1.8
-- 2
-- >>> ceiling -1.2
-- -1
-- >>> ceiling -1.5
-- -1
-- >>> ceiling -1.8
-- -1
ceiling :: Float -> Int
ceiling = Kernel.ceiling

-- | Truncate a number, rounding towards zero.
--
-- >>> truncate 1.0
-- 1
-- >>> truncate 1.2
-- 1
-- >>> truncate 1.5
-- 1
-- >>> truncate 1.8
-- 1
-- >>> truncate -1.2
-- -1
-- >>> truncate -1.5
-- -1
-- >>> truncate -1.8
-- -1
truncate :: Float -> Int
truncate = Kernel.truncate

-- EQUALITY
-- | Check if values are "the same".
--
-- __Note:__ Haskell uses structural equality on tuples, records, and user-defined
-- union types. This means the values @(3, 4)@ and @(3, 4)@ are definitely equal.
-- This is not true in languages like JavaScript that use reference equality on
-- objects.
--
-- __Note:__ Equality (in the Haskell sense) is not possible for certain types. For
-- example, the functions @(\\n -> n + 1)@ and @(\\n -> 1 + n)@ are "the
-- same" but detecting this in general is
-- [undecidable](https://en.wikipedia.org/wiki/Undecidable_problem).
-- Problematic types include functions and types containing functions.
(==) :: Eq equatable => equatable -> equatable -> Bool
(==) = (Kernel.==)

-- | Check if values are not "the same".
--
-- So @(a /= b)@ is the same as @(not (a == b))@.
(/=) :: Eq equatable => equatable -> equatable -> Bool
(/=) = (Kernel./=)

-- COMPARISON
-- $comparison
--
-- These functions only work on @comparable@ types. This includes numbers,
-- characters, strings, lists of comparable things, and tuples of comparable
-- things.
--
-- __@elm-prelude@ Note:__ These functions use Haskell's @Ord@ typeclass
-- instead of Elm's @comparable@ constraint.
(<) :: Ord comparable => comparable -> comparable -> Bool
(<) = (Kernel.<)

(>) :: Ord comparable => comparable -> comparable -> Bool
(>) = (Kernel.>)

(<=) :: Ord comparable => comparable -> comparable -> Bool
(<=) = (Kernel.<=)

(>=) :: Ord comparable => comparable -> comparable -> Bool
(>=) = (Kernel.>=)

-- | Find the smaller of two comparables.
--
-- >>> min 42 12345678
-- 42
-- >>> min "abc" "xyz"
-- "abc"
min :: Ord comparable => comparable -> comparable -> comparable
min x y =
  if x < y
    then x
    else y

-- | Find the larger of two comparables.
--
-- >>> max 42 12345678
-- 12345678
-- >>> max "abc" "xyz"
-- "xyz"
max :: Ord comparable => comparable -> comparable -> comparable
max x y =
  if x > y
    then x
    else y

-- | Compare any two comparable values. Comparable values include @String@,
-- @Char@, @Int@, @Float@, or a list or tuple containing comparable values. These
-- are also the only values that work as @Dict@ keys or @Set@ members.
--
-- >>> compare 3 4
-- LT
-- >>> compare 4 4
-- EQ
-- >>> compare 5 4
-- GT
compare :: Ord comparable => comparable -> comparable -> Ordering
compare = Kernel.compare

-- BOOLEANS
-- | Negate a boolean value.
--
-- >>> not True
-- False
-- >>> not False
-- True
not :: Bool -> Bool
not True  = False
not False = True

-- | The logical AND operator. @True@ if both inputs are @True@.
--
-- >>> True  && True
-- True
-- >>> True  && False
-- False
-- >>> False && True
-- False
-- >>> False && False
-- False
--
-- __Note:__ When used in the infix position, like @(left && right)@, the operator
-- short-circuits. This means if @left@ is @False@ we do not bother evaluating @right@
-- and just return @False@ overall.
(&&) :: Bool -> Bool -> Bool
False && ~_ = False
True && b = b

-- | The logical OR operator. @True@ if one or both inputs are @True@.
--
-- >>> True  || True
-- True
-- >>> True  || False
-- True
-- >>> False || True
-- True
-- >>> False || False
-- False
--
-- __Note:__ When used in the infix position, like @(left || right)@, the operator
-- short-circuits. This means if @left@ is @True@ we do not bother evaluating @right@
-- and just return @True@ overall.
(||) :: Bool -> Bool -> Bool
True || ~_ = True
False || b = b

-- | The exclusive-or operator. @True@ if exactly one input is @True@.
--
-- >>> xor True  True
-- False
-- >>> xor True  False
-- True
-- >>> xor False True
-- True
-- >>> xor False False
-- False
xor :: Bool -> Bool -> Bool
xor = (/=)

-- APPEND
-- | Put two appendable things together. This includes strings, lists, and text.
--
-- >>> "hello" ++ "world"
-- "helloworld"
-- >>> [1,1,2] ++ [3,5,8]
-- [1,1,2,3,5,8]
(++) :: Appendable appendable => appendable -> appendable -> appendable
(++) = (Kernel.++)

-- FANCIER MATH
-- | Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic).
-- A common trick is to use (n mod 2) to detect even and odd numbers:
--
-- >>> modBy 2 0
-- 0
-- >>> modBy 2 1
-- 1
-- >>> modBy 2 2
-- 0
-- >>> modBy 2 3
-- 1
--
-- Our @modBy@ function works in the typical mathematical way when you run into
-- negative numbers:
--
-- >>> List.map (modBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
-- [  3,  0,  1,  2,  3,  0,  1,  2,  3,  0,  1 ]
--
-- Use 'remainderBy' for a different treatment of negative numbers,
-- or read Daan Leijen’s [Division and Modulus for Computer Scientists](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf)
-- for more information.
modBy :: Int -> Int -> Int
modBy = Kernel.modBy

-- | Get the remainder after division. Here are bunch of examples of dividing by four:
--
-- >>> List.map (remainderBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
-- [ -1,  0, -3, -2, -1,  0,  1,  2,  3,  0,  1 ]
--
-- Use 'modBy' for a different treatment of negative numbers,
-- or read Daan Leijen’s [Division and Modulus for Computer Scientists](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf)
-- for more information.
remainderBy :: Int -> Int -> Int
remainderBy = Kernel.remainderBy

-- | Negate a number.
--
-- >>> negate 42
-- -42
-- >>> negate -42
-- 42
-- >>> negate 0
-- 0
negate :: Number number => number -> number
negate = Kernel.neg

-- | Get the [absolute value](https://en.wikipedia.org/wiki/Absolute_value)
-- of a number.
--
-- >>> abs 16
-- 16
-- >>> abs -4
-- 4
-- >>> abs -8.5
-- 8.5
-- >>> abs 3.14
-- 3.14
abs :: Number number => number -> number
abs n =
  if n < fromInteger 0
    then Kernel.neg n
    else n

-- | Clamps a number within a given range. With the expression
-- @clamp 100 200 x@ the results are as follows:
--
-- > 100     if x < 100
-- >  x      if 100 <= x < 200
-- > 200     if 200 <= x
clamp :: Number number => number -> number -> number -> number
clamp low high number =
  if number < low
    then low
    else if number > high
           then high
           else number

-- | Take the square root of a number.
--
-- >>> sqrt  4
-- 2
-- >>> sqrt  9
-- 3
-- >>> sqrt 16
-- 4
-- >>> sqrt 25
-- 5
sqrt :: Float -> Float
sqrt = Kernel.sqrt

-- | Calculate the logarithm of a number with a given base.
--
-- >>> logBase 10 100
-- 2
-- >>> logBase 2 256
-- 8
logBase :: Float -> Float -> Float
logBase base number = Kernel.log number / Kernel.log base

-- | Exponentiation
exp :: Float -> Float
exp = Kernel.exp

-- ANGLES
-- | Convert radians to standard Elm angles (radians).
--
-- >>> radians pi
-- 3.141592653589793
radians :: Float -> Float
radians angleInRadians = angleInRadians

-- | Convert degrees to standard Elm angles (radians).
--
-- >>> degrees 180
-- 3.141592653589793
degrees :: Float -> Float
degrees angleInDegrees = angleInDegrees * pi / 180.0

-- | Convert turns to standard Elm angles (radians). One turn is equal to 360°.
--
-- >>> turns (1/2)
-- 3.141592653589793
turns :: Float -> Float
turns angleInTurns = 2 * pi * angleInTurns

-- TRIGONOMETRY
-- | An approximation of pi.
pi :: Float
pi = Kernel.pi

-- | Figure out the cosine given an angle in radians.
--
-- >>> cos (degrees 60)
-- 0.5000000000000001
-- >>> cos (turns (1/6))
-- 0.5000000000000001
-- >>> cos (radians (pi/3))
-- 0.5000000000000001
-- >>> cos (pi/3)
-- 0.5000000000000001
cos :: Float -> Float
cos = Kernel.cos

-- | Figure out the sine given an angle in radians.
--
-- >>> sin (degrees 30)
-- 0.49999999999999994
-- >>> sin (turns (1/12))
-- 0.49999999999999994
-- >>> sin (radians (pi/6))
-- 0.49999999999999994
-- >>> sin (pi/6)
-- 0.49999999999999994
sin :: Float -> Float
sin = Kernel.sin

-- | Figure out the tangent given an angle in radians.
--
-- >>> tan (degrees 45)
-- 0.9999999999999999
-- >>> tan (turns (1/8))
-- 0.9999999999999999
-- >>> tan (radians (pi/4))
-- 0.9999999999999999
-- >>> tan (pi/4)
-- 0.9999999999999999
tan :: Float -> Float
tan = Kernel.tan

-- | Figure out the arccosine for @adjacent / hypotenuse@ in radians:
--
-- >>> acos (1/2)
-- 1.0471975511965979 -- 60° or pi/3 radians
acos :: Float -> Float
acos = Kernel.acos

-- | Figure out the arcsine for @opposite / hypotenuse@ in radians:
--
-- >>> asin (1/2)
-- 0.5235987755982989 -- 30° or pi/6 radians
asin :: Float -> Float
asin = Kernel.asin

-- | This helps you find the angle (in radians) to an @(x,y)@ coordinate, but
-- in a way that is rarely useful in programming. __You probably want__
-- __'atan2' instead!__
--
-- This version takes @y/x@ as its argument, so there is no way to know whether
-- the negative signs comes from the @y@ or @x@ value. So as we go counter-clockwise
-- around the origin from point @(1,1)@ to @(1,-1)@ to @(-1,-1)@ to @(-1,1)@ we do
-- not get angles that go in the full circle:
--
-- >>> atan (  1 /  1 )
--  0.7853981633974483 --  45° or   pi/4 radians
-- >>> atan (  1 / -1 )
-- -0.7853981633974483 -- 315° or 7*pi/4 radians
-- >>> atan ( -1 / -1 )
--  0.7853981633974483 --  45° or   pi/4 radians
-- >>> atan ( -1 /  1 )
-- -0.7853981633974483 -- 315° or 7*pi/4 radians
--
-- Notice that everything is between @pi/2@ and @-pi/2@. That is pretty useless
-- for figuring out angles in any sort of visualization, so again, check out
-- 'atan2' instead!
atan :: Float -> Float
atan = Kernel.atan

-- | This helps you find the angle (in radians) to an @(x,y)@ coordinate.
-- So rather than saying @atan (y/x)@ you say @atan2 y x@ and you can get a full
-- range of angles:
--
-- >>> atan2  1  1
--  0.7853981633974483 --  45° or   pi/4 radians
-- >>> atan2  1 -1
--  2.356194490192345  -- 135° or 3*pi/4 radians
-- >>> atan2 -1 -1
-- -2.356194490192345  -- 225° or 5*pi/4 radians
-- >>> atan2 -1  1
-- -0.7853981633974483 -- 315° or 7*pi/4 radians
atan2 :: Float -> Float -> Float
atan2 = Kernel.atan2

-- POLAR COORDINATES
-- | Convert polar coordinates (r,&#x3b8;) to Cartesian coordinates (x,y).
--
-- >>> fromPolar (sqrt 2, degrees 45)
-- (1, 1)
fromPolar :: (Float, Float) -> (Float, Float)
fromPolar (radius, theta) = (radius * cos theta, radius * sin theta)

-- | Convert Cartesian coordinates (x,y) to polar coordinates (r,&#x3b8;).
--
-- >>> toPolar (3, 4)
-- ( 5, 0.9272952180016122)
-- >>> toPolar (5,12)
-- (13, 1.1760052070951352)
toPolar :: (Float, Float) -> (Float, Float)
toPolar (x, y) = (sqrt (x * x + y * y), atan2 y x)

-- CRAZY FLOATS
-- | Determine whether a float is an undefined or unrepresentable number.
-- NaN stands for /not a number/ and it is [a standardized part of floating point
-- numbers](https://en.wikipedia.org/wiki/NaN).
--
-- >>> isNaN (0/0)
-- True
-- >>> isNaN (sqrt -1)
-- True
-- >>> isNaN (1/0)
-- False  -- infinity is a number
-- >>> isNaN 1
-- False
isNaN :: Float -> Bool
isNaN = Kernel.isNaN

-- | Determine whether a float is positive or negative infinity.
--
-- >>> isInfinite (0/0)
-- False
-- >>> isInfinite (sqrt -1)
-- False
-- >>> isInfinite (1/0)
-- True
-- >>> isInfinite 1
-- False
--
-- Notice that NaN is not infinite! For float @n@ to be finite implies that
-- @not (isInfinite n || isNaN n)@ evaluates to @True@.
isInfinite :: Float -> Bool
isInfinite = Kernel.isInfinite

-- FUNCTION HELPERS
-- | Given a value, returns exactly the same value. This is called
-- [the identity function](https://en.wikipedia.org/wiki/Identity_function).
identity :: a -> a
identity x = x

-- | Create a function that /always/ returns the same value. Useful with
-- functions like @map@:
--
-- > List.map (always 0) [1,2,3,4,5] == [0,0,0,0,0]
-- > -- List.map (\_ -> 0) [1,2,3,4,5] == [0,0,0,0,0]
-- > -- always = (\x _ -> x)
always :: a -> b -> a
always a _ = a

-- | A value that can never happen! For context:
--
-- * The boolean type @Bool@ has two values: @True@ and @False@
-- * The unit type @()@ has one value: @()@
-- * The never type @Never@ has no values!
--
-- You may see it in the wild in @CLI Never@ which means this widget will never
-- produce any messages. You would need to write an event handler like
-- @onClick ??? :: Attribute Never@ but how can we fill in the question marks?!
-- So there cannot be any event handlers on that widget.
--
-- You may also see this used with tasks that never fail, like @Task Never ()@.
--
-- The @Never@ type is useful for restricting /arguments/ to a function. Maybe my
-- API can only accept widgets without event handlers, so I require @CLI Never@ and
-- users can give @CLI msg@ and everything will go fine. Generally speaking, you
-- do not want @Never@ in your return types though.
data Never =
  JustOneMore Never

-- | A function that can never be called. Seems extremely pointless, but it
-- /can/ come in handy. Imagine you have some widget that should never produce any
-- messages. And say you want to use it in some other widget that /does/ produce
-- messages. You could say:
--
-- > import CLI exposing (..)
-- > embedCLI :: CLI Never -> CLI msg
-- > embedCLI staticStuff =
-- >   column AlignCenter
-- >     [ text "hello"
-- >     , CLI.map never staticStuff
-- >     ]
--
-- So the @never@ function is basically telling the type system, make sure no one
-- ever calls me!
never :: Never -> a
never (JustOneMore nvr) = never nvr

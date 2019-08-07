module Protolude
  ( Bool(..)
  , Comparable
  , Char
  , Equatable
  , Float
  , Int
  , IO
  , List
  , Maybe(..)
  , Never
  , Number
  , Order(..)
  , (-)
  , (.)
  , (*)
  , (/)
  , (//)
  , (/=)
  , (&)
  , (&&)
  , (^)
  , (+)
  , (++)
  , (<)
  , (<=)
  , (==)
  , (>)
  , (>=)
  , (||)
  , ($)
  , abs
  , acos
  , always
  , asin
  , atan
  , atan2
  , ceiling
  , clamp
  , compare
  , cos
  , degrees
  , e
  , floor
  , fromInteger
  , fromPolar
  , identity
  , isInfinite
  , isNaN
  , logBase
  , max
  , min
  , modBy
  , negate
  , never
  , not
  , orderToOrdering
  , pi
  , radians
  , remainderBy
  , round
  , sin
  , sqrt
  , tan
  , toFloat
  , toPolar
  , truncate
  , turns
  , xor
  ) where

import           Kernel (Appendable, Bool (..), Char, Comparable, Equatable, IO,
                         List, Maybe (..), Number, Order (..), orderToOrdering,
                         ($), (&), (.))
import           Kernel (fromInteger)
import qualified Kernel

{-| Tons of useful functions that get imported by default.
# Math
@docs Int, Float, (+), (-), (*), (/), (//), (^)
# Int to Float / Float to Int
@docs toFloat, round, floor, ceiling, truncate
# Equality
@docs (==), (/=)
# Comparison
These functions only work on `comparable` types. This includes numbers,
characters, strings, lists of comparable things, and tuples of comparable
things.
@docs (<), (>), (<=), (>=), max, min, compare, Order
# Booleans
@docs Bool, not, (&&), (||), xor
# Append Strings and Lists
@docs (++)
# Fancier Math
@docs modBy, remainderBy, negate, abs, clamp, sqrt, logBase, e
# Angles
@docs degrees, radians, turns
# Trigonometry
@docs pi, cos, sin, tan, acos, asin, atan, atan2
# Polar Coordinates
@docs toPolar, fromPolar
# Floating Point Checks
@docs isNaN, isInfinite
# Function Helpers
@docs identity, always, ($), (&), (.), Never, never
-}
infixr 2 ||

(||) :: Bool -> Bool -> Bool
(||) = or

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
(&&) = and

infix 4 ==

(==) :: Equatable a => a -> a -> Bool
(==) = eq

infix 4 /=

(/=) :: Equatable a => a -> a -> Bool
(/=) = neq

infix 4 <

(<) :: Comparable a => a -> a -> Bool
(<) = lt

infix 4 >

(>) :: Comparable a => a -> a -> Bool
(>) = gt

infix 4 <=

(<=) :: Comparable a => a -> a -> Bool
(<=) = le

infix 4 >=

(>=) :: Comparable a => a -> a -> Bool
(>=) = ge

infixr 5 ++

(++) :: Appendable a => a -> a -> a
(++) = append

infixl 6 +

(+) :: Number a => a -> a -> a
(+) = add

infixl 6 -

(-) :: Number a => a -> a -> a
(-) = sub

infixl 7 *

(*) :: Number a => a -> a -> a
(*) = mul

infixl 7 /

(/) :: Float -> Float -> Float
(/) = fdiv

infixl 7 //

(//) :: Int -> Int -> Int
(//) = idiv

infixr 8 ^

(^) :: Number a => a -> a -> a
(^) = pow

-- MATHEMATICS
{-| An `Int` is a whole number. Valid syntax for integers includes:
    0
    42
    9000
    0xFF   -- 255 in hexadecimal
    0x000A --  10 in hexadecimal
**Note:** `Int` math is well-defined in the range `-2^31` to `2^31 - 1`. Outside
of that range, the behavior is determined by the compilation target. When
generating JavaScript, the safe range expands to `-2^53` to `2^53 - 1` for some
operations, but if we generate WebAssembly some day, we would do the traditional
[integer overflow][io]. This quirk is necessary to get good performance on
quirky compilation targets.
**Historical Note:** The name `Int` comes from the term [integer][]. It appears
that the `int` abbreviation was introduced in [ALGOL 68][68], shortening it
from `integer` in [ALGOL 60][60]. Today, almost all programming languages use
this abbreviation.
[io]: https://en.wikipedia.org/wiki/Integer_overflow
[integer]: https://en.wikipedia.org/wiki/Integer
[60]: https://en.wikipedia.org/wiki/ALGOL_60
[68]: https://en.wikipedia.org/wiki/ALGOL_68
-}
type Int = Kernel.Int

{-| A `Float` is a [floating-point number][fp]. Valid syntax for floats includes:
    0
    42
    3.14
    0.1234
    6.022e23   -- == (6.022 * 10^23)
    6.022e+23  -- == (6.022 * 10^23)
    1.602e−19  -- == (1.602 * 10^-19)
    1e3        -- == (1 * 10^3) == 1000
**Historical Note:** The particular details of floats (e.g. `NaN`) are
specified by [IEEE 754][ieee] which is literally hard-coded into almost all
CPUs in the world. That means if you think `NaN` is weird, you must
successfully overtake Intel and AMD with a chip that is not backwards
compatible with any widely-used assembly language.
[fp]: https://en.wikipedia.org/wiki/Floating-point_arithmetic
[ieee]: https://en.wikipedia.org/wiki/IEEE_754
-}
type Float = Kernel.Float

{-| Add two numbers. The `number` type variable means this operation can be
specialized to `Int -> Int -> Int` or to `Float -> Float -> Float`. So you
can do things like this:
    3002 + 4004 == 7006  -- all ints
    3.14 + 3.14 == 6.28  -- all floats
You _cannot_ add an `Int` and a `Float` directly though. Use functions like
[toFloat](#toFloat) or [round](#round) to convert both values to the same type.
So if you needed to add a list length to a `Float` for some reason, you
could say one of these:
    3.14 + toFloat (List.length [1,2,3]) == 6.14
    round 3.14 + List.length [1,2,3]     == 6
**Note:** Languages like Java and JavaScript automatically convert `Int` values
to `Float` values when you mix and match. This can make it difficult to be sure
exactly what type of number you are dealing with. When you try to _infer_ these
conversions (as Scala does) it can be even more confusing. Elm has opted for a
design that makes all conversions explicit.
-}
add :: Number number => number -> number -> number
add = Kernel.add

{-| Subtract numbers like `4 - 3 == 1`.
See [`(+)`](#+) for docs on the `number` type variable.
-}
sub :: Number number => number -> number -> number
sub = Kernel.sub

{-| Multiply numbers like `2 * 3 == 6`.
See [`(+)`](#+) for docs on the `number` type variable.
-}
mul :: Number number => number -> number -> number
mul = Kernel.mul

{-| Floating-point division:
    3.14 / 2 == 1.57
-}
fdiv :: Float -> Float -> Float
fdiv = Kernel.fdiv

{-| Integer division:
    3 // 2 == 1
Notice that the remainder is discarded.
-}
idiv :: Int -> Int -> Int
idiv = Kernel.idiv

{-| Exponentiation
    3^2 == 9
    3^3 == 27
-}
pow :: Number number => number -> number -> number
pow = Kernel.pow

-- INT TO FLOAT / FLOAT TO INT
{-| Convert an integer into a float. Useful when mixing `Int` and `Float`
values like this:
    halfOf :: Int -> Float
    halfOf number =
      toFloat number / 2
-}
toFloat :: Int -> Float
toFloat = Kernel.toFloat

{-| Round a number to the nearest integer.
    round 1.0 == 1
    round 1.2 == 1
    round 1.5 == 2
    round 1.8 == 2
    round -1.2 == -1
    round -1.5 == -1
    round -1.8 == -2
-}
round :: Float -> Int
round = Kernel.round

{-| Floor function, rounding down.
    floor 1.0 == 1
    floor 1.2 == 1
    floor 1.5 == 1
    floor 1.8 == 1
    floor -1.2 == -2
    floor -1.5 == -2
    floor -1.8 == -2
-}
floor :: Float -> Int
floor = Kernel.floor

{-| Ceiling function, rounding up.
    ceiling 1.0 == 1
    ceiling 1.2 == 2
    ceiling 1.5 == 2
    ceiling 1.8 == 2
    ceiling -1.2 == -1
    ceiling -1.5 == -1
    ceiling -1.8 == -1
-}
ceiling :: Float -> Int
ceiling = Kernel.ceiling

{-| Truncate a number, rounding towards zero.
    truncate 1.0 == 1
    truncate 1.2 == 1
    truncate 1.5 == 1
    truncate 1.8 == 1
    truncate -1.2 == -1
    truncate -1.5 == -1
    truncate -1.8 == -1
-}
truncate :: Float -> Int
truncate = Kernel.truncate

-- EQUALITY
{-| Check if values are &ldquo;the same&rdquo;.
**Note:** Elm uses structural equality on tuples, records, and user-defined
union types. This means the values `(3, 4)` and `(3, 4)` are definitely equal.
This is not true in languages like JavaScript that use reference equality on
objects.
**Note:** Equality (in the Elm sense) is not possible for certain types. For
example, the functions `(\n -> n + 1)` and `(\n -> 1 + n)` are &ldquo;the
same&rdquo; but detecting this in general is [undecidable][]. In a future
release, the compiler will detect when `(==)` is used with problematic
types and provide a helpful error message. This will require quite serious
infrastructure work that makes sense to batch with another big project, so the
stopgap is to crash as quickly as possible. Problematic types include functions
and JavaScript values like `Json.Encode.Value` which could contain functions
if passed through a port.
[undecidable]: https://en.wikipedia.org/wiki/Undecidable_problem
-}
eq :: Equatable a => a -> a -> Bool
eq = Kernel.equal

{-| Check if values are not &ldquo;the same&rdquo;.
So `(a /= b)` is the same as `(not (a == b))`.
-}
neq :: Equatable a => a -> a -> Bool
neq = Kernel.notEqual

-- COMPARISONS
{-|-}
lt :: Comparable comparable => comparable -> comparable -> Bool
lt = Kernel.lt

{-|-}
gt :: Comparable comparable => comparable -> comparable -> Bool
gt = Kernel.gt

{-|-}
le :: Comparable comparable => comparable -> comparable -> Bool
le = Kernel.le

{-|-}
ge :: Comparable comparable => comparable -> comparable -> Bool
ge = Kernel.ge

{-| Find the smaller of two comparables.
    min 42 12345678 == 42
    min "abc" "xyz" == "abc"
-}
min :: Comparable comparable => comparable -> comparable -> comparable
min x y =
  if lt x y
    then x
    else y

{-| Find the larger of two comparables.
    max 42 12345678 == 12345678
    max "abc" "xyz" == "xyz"
-}
max :: Comparable comparable => comparable -> comparable -> comparable
max x y =
  if gt x y
    then x
    else y

{-| Compare any two comparable values. Comparable values include `String`,
`Char`, `Int`, `Float`, or a list or tuple containing comparable values. These
are also the only values that work as `Dict` keys or `Set` members.
    compare 3 4 == LT
    compare 4 4 == EQ
    compare 5 4 == GT
-}
compare :: Comparable comparable => comparable -> comparable -> Order
compare = Kernel.compare

{-| Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.
-}
--type Order = Kernel.Order
-- BOOLEANS
{-| A “Boolean” value. It can either be `True` or `False`.
**Note:** Programmers coming from JavaScript, Java, etc. tend to reach for
boolean values way too often in Elm. Using a [union type][ut] is often clearer
and more reliable. You can learn more about this from Jeremy [here][jf] or
from Richard [here][rt].
[ut]: https://guide.elm-lang.org/types/union_types.html
[jf]: https://youtu.be/6TDKHGtAxeg?t=1m25s
[rt]: https://youtu.be/IcgmSRJHu_8?t=1m14s
-}
--type Bool = Kernel.Bool
{-| Negate a boolean value.
    not True == False
    not False == True
-}
not :: Bool -> Bool
not = Kernel.not

{-| The logical AND operator. `True` if both inputs are `True`.
    True  && True  == True
    True  && False == False
    False && True  == False
    False && False == False
**Note:** When used in the infix position, like `(left && right)`, the operator
short-circuits. This means if `left` is `False` we do not bother evaluating `right`
and just return `False` overall.
-}
and :: Bool -> Bool -> Bool
and = Kernel.and

{-| The logical OR operator. `True` if one or both inputs are `True`.
    True  || True  == True
    True  || False == True
    False || True  == True
    False || False == False
**Note:** When used in the infix position, like `(left || right)`, the operator
short-circuits. This means if `left` is `True` we do not bother evaluating `right`
and just return `True` overall.
-}
or :: Bool -> Bool -> Bool
or = Kernel.or

{-| The exclusive-or operator. `True` if exactly one input is `True`.
    xor True  True  == False
    xor True  False == True
    xor False True  == True
    xor False False == False
-}
xor :: Bool -> Bool -> Bool
xor = Kernel.xor

-- APPEND
{-| Put two appendable things together. This includes strings, lists, and text.
    "hello" ++ "world" == "helloworld"
    [1,1,2] ++ [3,5,8] == [1,1,2,3,5,8]
-}
append :: Appendable appendable => appendable -> appendable -> appendable
append = Kernel.append

-- FANCIER MATH
{-| Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic).
A common trick is to use (n mod 2) to detect even and odd numbers:
    modBy 2 0 == 0
    modBy 2 1 == 1
    modBy 2 2 == 0
    modBy 2 3 == 1
Our `modBy` function works in the typical mathematical way when you run into
negative numbers:
    List.map (modBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
    --                 [  3,  0,  1,  2,  3,  0,  1,  2,  3,  0,  1 ]
Use [`remainderBy`](#remainderBy) for a different treatment of negative numbers,
or read Daan Leijen’s [Division and Modulus for Computer Scientists][dm] for more
information.
[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
-}
modBy :: Int -> Int -> Int
modBy = Kernel.modBy

{-| Get the remainder after division. Here are bunch of examples of dividing by four:
    List.map (remainderBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
    --                       [ -1,  0, -3, -2, -1,  0,  1,  2,  3,  0,  1 ]
Use [`modBy`](#modBy) for a different treatment of negative numbers,
or read Daan Leijen’s [Division and Modulus for Computer Scientists][dm] for more
information.
[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
-}
remainderBy :: Int -> Int -> Int
remainderBy = Kernel.remainderBy

{-| Negate a number.
    negate 42 == -42
    negate -42 == 42
    negate 0 == 0
-}
negate :: Number number => number -> number
negate n = Kernel.negate n

{-| Get the [absolute value][abs] of a number.
    abs 16   == 16
    abs -4   == 4
    abs -8.5 == 8.5
    abs 3.14 == 3.14
[abs]: https://en.wikipedia.org/wiki/Absolute_value
-}
abs :: Number number => number -> number
abs n =
  if lt n (fromInteger 0)
    then (negate n)
    else n

{-| Clamps a number within a given range. With the expression
`clamp 100 200 x` the results are as follows:
    100     if x < 100
     x      if 100 <= x < 200
    200     if 200 <= x
-}
clamp :: Number number => number -> number -> number -> number
clamp low high number =
  if lt number low
    then low
    else if gt number high
           then high
           else number

{-| Take the square root of a number.
    sqrt  4 == 2
    sqrt  9 == 3
    sqrt 16 == 4
    sqrt 25 == 5
-}
sqrt :: Float -> Float
sqrt = Kernel.sqrt

{-| Calculate the logarithm of a number with a given base.
    logBase 10 100 == 2
    logBase 2 256 == 8
-}
logBase :: Float -> Float -> Float
logBase base number = fdiv (Kernel.log number) (Kernel.log base)

{-| An approximation of e.
-}
e :: Float
e = Kernel.e

-- ANGLES
{-| Convert radians to standard Elm angles (radians).
    radians pi == 3.141592653589793
-}
radians :: Float -> Float
radians angleInRadians = angleInRadians

{-| Convert degrees to standard Elm angles (radians).
    degrees 180 == 3.141592653589793
-}
degrees :: Float -> Float
degrees angleInDegrees = fdiv (mul angleInDegrees pi) 180

{-| Convert turns to standard Elm angles (radians). One turn is equal to 360°.
    turns (1/2) == 3.141592653589793
-}
turns :: Float -> Float
turns angleInTurns = mul (mul 2 pi) angleInTurns

-- TRIGONOMETRY
{-| An approximation of pi.
-}
pi :: Float
pi = Kernel.pi

{-| Figure out the cosine given an angle in radians.
    cos (degrees 60)     == 0.5000000000000001
    cos (turns (1/6))    == 0.5000000000000001
    cos (radians (pi/3)) == 0.5000000000000001
    cos (pi/3)           == 0.5000000000000001
-}
cos :: Float -> Float
cos = Kernel.cos

{-| Figure out the sine given an angle in radians.
    sin (degrees 30)     == 0.49999999999999994
    sin (turns (1/12))   == 0.49999999999999994
    sin (radians (pi/6)) == 0.49999999999999994
    sin (pi/6)           == 0.49999999999999994
-}
sin :: Float -> Float
sin = Kernel.sin

{-| Figure out the tangent given an angle in radians.
    tan (degrees 45)     == 0.9999999999999999
    tan (turns (1/8))    == 0.9999999999999999
    tan (radians (pi/4)) == 0.9999999999999999
    tan (pi/4)           == 0.9999999999999999
-}
tan :: Float -> Float
tan = Kernel.tan

{-| Figure out the arccosine for `adjacent / hypotenuse` in radians:
    acos (1/2) == 1.0471975511965979 -- 60° or pi/3 radians
-}
acos :: Float -> Float
acos = Kernel.acos

{-| Figure out the arcsine for `opposite / hypotenuse` in radians:
    asin (1/2) == 0.5235987755982989 -- 30° or pi/6 radians
-}
asin :: Float -> Float
asin = Kernel.asin

{-| This helps you find the angle (in radians) to an `(x,y)` coordinate, but
in a way that is rarely useful in programming. **You probably want
[`atan2`](#atan2) instead!**
This version takes `y/x` as its argument, so there is no way to know whether
the negative signs comes from the `y` or `x` value. So as we go counter-clockwise
around the origin from point `(1,1)` to `(1,-1)` to `(-1,-1)` to `(-1,1)` we do
not get angles that go in the full circle:
    atan (  1 /  1 ) ==  0.7853981633974483 --  45° or   pi/4 radians
    atan (  1 / -1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians
    atan ( -1 / -1 ) ==  0.7853981633974483 --  45° or   pi/4 radians
    atan ( -1 /  1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians
Notice that everything is between `pi/2` and `-pi/2`. That is pretty useless
for figuring out angles in any sort of visualization, so again, check out
[`atan2`](#atan2) instead!
-}
atan :: Float -> Float
atan = Kernel.atan

{-| This helps you find the angle (in radians) to an `(x,y)` coordinate.
So rather than saying `atan (y/x)` you say `atan2 y x` and you can get a full
range of angles:
    atan2  1  1 ==  0.7853981633974483 --  45° or   pi/4 radians
    atan2  1 -1 ==  2.356194490192345  -- 135° or 3*pi/4 radians
    atan2 -1 -1 == -2.356194490192345  -- 225° or 5*pi/4 radians
    atan2 -1  1 == -0.7853981633974483 -- 315° or 7*pi/4 radians
-}
atan2 :: Float -> Float -> Float
atan2 = Kernel.atan2

-- POLAR COORDINATES
{-| Convert polar coordinates (r,&theta;) to Cartesian coordinates (x,y).
    fromPolar (sqrt 2, degrees 45) == (1, 1)
-}
fromPolar :: (Float, Float) -> (Float, Float)
fromPolar (radius, theta) = (mul radius (cos theta), mul radius (sin theta))

{-| Convert Cartesian coordinates (x,y) to polar coordinates (r,&theta;).
    toPolar (3, 4) == ( 5, 0.9272952180016122)
    toPolar (5,12) == (13, 1.1760052070951352)
-}
toPolar :: (Float, Float) -> (Float, Float)
toPolar (x, y) = (sqrt (add (mul x x) (mul y y)), atan2 y x)

-- CRAZY FLOATS
{-| Determine whether a float is an undefined or unrepresentable number.
NaN stands for *not a number* and it is [a standardized part of floating point
numbers](https://en.wikipedia.org/wiki/NaN).
    isNaN (0/0)     == True
    isNaN (sqrt -1) == True
    isNaN (1/0)     == False  -- infinity is a number
    isNaN 1         == False
-}
isNaN :: Float -> Bool
isNaN = Kernel.isNaN

{-| Determine whether a float is positive or negative infinity.
    isInfinite (0/0)     == False
    isInfinite (sqrt -1) == False
    isInfinite (1/0)     == True
    isInfinite 1         == False
Notice that NaN is not infinite! For float `n` to be finite implies that
`not (isInfinite n || isNaN n)` evaluates to `True`.
-}
isInfinite :: Float -> Bool
isInfinite = Kernel.isInfinite

-- FUNCTION HELPERS
{-| Given a value, returns exactly the same value. This is called
[the identity function](https://en.wikipedia.org/wiki/Identity_function).
-}
identity :: a -> a
identity x = x

{-| Create a function that *always* returns the same value. Useful with
functions like `map`:
    List.map (always 0) [1,2,3,4,5] == [0,0,0,0,0]
    -- List.map (\_ -> 0) [1,2,3,4,5] == [0,0,0,0,0]
    -- always = (\x _ -> x)
-}
always :: a -> b -> a
always a _ = a

{-| A value that can never happen! For context:
  - The boolean type `Bool` has two values: `True` and `False`
  - The unit type `()` has one value: `()`
  - The never type `Never` has no values!
You may see it in the wild in `Html Never` which means this HTML will never
produce any messages. You would need to write an event handler like
`onClick ??? :: Attribute Never` but how can we fill in the question marks?!
So there cannot be any event handlers on that HTML.
You may also see this used with tasks that never fail, like `Task Never ()`.
The `Never` type is useful for restricting *arguments* to a function. Maybe my
API can only accept HTML without event handlers, so I require `Html Never` and
users can give `Html msg` and everything will go fine. Generally speaking, you
do not want `Never` in your return types though.
-}
data Never =
  JustOneMore Never

{-| A function that can never be called. Seems extremely pointless, but it
*can* come in handy. Imagine you have some HTML that should never produce any
messages. And say you want to use it in some other HTML that *does* produce
messages. You could say:
    import Html exposing (..)
    embedHtml :: Html Never -> Html msg
    embedHtml staticStuff =
      div []
        [ text "hello"
        , Html.map never staticStuff
        ]
So the `never` function is basically telling the type system, make sure no one
ever calls me!
-}
never :: Never -> a
never (JustOneMore nvr) = never nvr

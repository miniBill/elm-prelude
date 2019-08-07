module Color
  ( Color
  , black
  , green
  , red
  , rgb
  , rgba
  , toHex
  , toRgba
  , transparent
  , white
  ) where

import qualified List
import           Protolude
import qualified String

data Color =
  Color Float Float Float Float

rgb :: Float -> Float -> Float -> Color
rgb r g b = Color r g b 1

rgba :: Float -> Float -> Float -> Float -> Color
rgba = Color

toRgba :: Color -> (Float, Float, Float, Float)
toRgba (Color r g b a) = (r, g, b, a)

toHex :: Color -> String
toHex (Color r g b a) =
  (if a == 1
     then [r, g, b]
     else [a, r, g, b]) &
  List.map ((*) 255) &
  List.map round &
  List.map int255ToHex &
  String.concat &
  (++) "#"

int255ToHex :: Int -> String
int255ToHex n =
  if n < 0
    then "00"
    else if n > 255
           then "ff"
           else unsafeInt255Digits n &
                (\(a, b) ->
                   String.fromList [unsafeIntToChar a, unsafeIntToChar b])

unsafeInt255Digits :: Int -> (Int, Int)
unsafeInt255Digits n =
  let digit1 = n // 16
      digit0 =
        if digit1 == 0
          then n
          else modBy (digit1 * 16) n
   in (digit1, digit0)

unsafeIntToChar :: Int -> Char
unsafeIntToChar i =
  case i of
    0  -> '0'
    1  -> '1'
    2  -> '2'
    3  -> '3'
    4  -> '4'
    5  -> '5'
    6  -> '6'
    7  -> '7'
    8  -> '8'
    9  -> '9'
    10 -> 'a'
    11 -> 'b'
    12 -> 'c'
    13 -> 'd'
    14 -> 'e'
    15 -> 'f'
    _  -> '0'

black :: Color
black = rgb 0 0 0

red :: Color
red = rgb 1 0 0

green :: Color
green = rgb 0 0.7 0

white :: Color
white = rgb 1 1 1

transparent :: Color
transparent = rgba 0 0 0 0

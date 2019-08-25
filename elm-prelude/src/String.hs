module String
  ( concat
  , dropLeft
  , fromInt
  , fromList
  , intersperse
  , join
  , left
  , length
  , padRight
  , repeat
  , replace
  , split
  , toList
  ) where

import           Char        (Char)
import qualified Data.String
import qualified Data.Text   as T
import           Kernel      (fromIntegral, show)
import qualified List

fromList :: List Char -> String
fromList = Data.String.fromString

toList :: String -> List Char
toList = T.unpack

fromInt :: Int -> String
fromInt = fromList . show

intersperse :: String -> List String -> String
intersperse = T.intercalate

length :: String -> Int
length = fromIntegral . T.length

concat :: List String -> String
concat = T.concat

padRight :: Int -> Char -> String -> String
padRight w c s = concat [s, fromList $ List.repeat (w - length s) c]

left :: Int -> String -> String
left = T.take . fromIntegral

dropLeft :: Int -> String -> String
dropLeft = T.drop . fromIntegral

-- | Repeat a string /n/ times.
--
-- >>> repeat 3 "ha"
-- "hahaha"
repeat :: Int -> String -> String
repeat = T.replicate . fromIntegral

replace :: String -> String -> String -> String
replace = T.replace

split :: String -> String -> List String
split = T.splitOn

join :: String -> List String -> String
join = T.intercalate

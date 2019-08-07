module String
  ( concat
  , dropLeft
  , fromInt
  , fromList
  , intersperse
  , left
  , length
  , padRight
  , toList
  ) where

import qualified Data.String
import qualified Data.Text   as T
import qualified List
import           Protolude

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

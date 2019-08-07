{-# LANGUAGE PackageImports #-}

module String
  ( String
  , concat
  , dropLeft
  , fromInt
  , fromList
  , intersperse
  , left
  , length
  , padRight
  ) where

import qualified Data.Text       as T
import           Kernel
import qualified List
import qualified "base" Prelude  as P
import           String.Internal

fromInt :: Int -> String
fromInt i = fromList $ P.show i

lift :: (T.Text -> T.Text) -> String -> String
lift f (String s) = String (f s)

intersperse :: String -> List String -> String
intersperse (String i) xs = String (T.intercalate i (List.map runString xs))

length :: String -> Int
length (String s) = P.fromIntegral (T.length s)

concat :: List String -> String
concat xs = String (T.concat (List.map runString xs))

fromList :: List Char -> String
fromList = String . T.pack

padRight :: Int -> Char -> String -> String
padRight w c s = concat [s, fromList $ List.repeat (w - length s) c]

left :: Int -> String -> String
left i = lift (T.take (P.fromIntegral i))

dropLeft :: Int -> String -> String
dropLeft i = lift (T.drop (P.fromIntegral i))

module CLI.Types
  ( Attribute
  , CLI
  , Cmd
  , InputType(..)
  , Program(..)
  , Sub
  , TextAlign(..)
  , VerticalAlign(..)
  , button
  , cell
  , input
  , singleColumn
  , singleRow
  , row
  , table
  , text
  ) where

import           CLI.Types.Internal (Attribute, CLI (..), Cmd, InputType (..),
                                     NodeType (..), Sub, TextAlign (..),
                                     VerticalAlign (..))
import qualified List

data Program flags model msg =
  Program
    (flags -> (model, Cmd msg))
    (model -> CLI msg)
    (msg -> model -> (model, Cmd msg))
    (model -> Sub msg)

button :: List (Attribute msg) -> List (CLI msg) -> CLI msg
button = Node Button

cell :: List (Attribute msg) -> List (CLI msg) -> CLI msg
cell = Node Cell

input :: List (Attribute msg) -> List (CLI msg) -> CLI msg
input = Node Input

row :: List (Attribute msg) -> List (CLI msg) -> CLI msg
row = Node Row

singleColumn :: List (Attribute msg) -> List (CLI msg) -> CLI msg
singleColumn attrs cells =
  table attrs $ List.map (\c -> row [] [cell [] [c]]) cells

singleRow :: List (Attribute msg) -> List (CLI msg) -> CLI msg
singleRow attrs cells =
  table attrs [row [] $ List.map (\c -> cell [] [c]) cells]

table :: List (Attribute msg) -> List (CLI msg) -> CLI msg
table = Node Table

text :: String -> CLI msg
text = Text

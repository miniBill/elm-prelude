{-# LANGUAGE ImplicitPrelude #-}

module CLI.Types
  ( AlignmentType(..)
  , Attribute(..)
  , CLI(..)
  , InputType(..)
  , LayoutType(..)
  , Program(..)
  , attributes
  , border
  , button
  , column
  , input
  , row
  , text
  ) where

import           Color (Color)

data Attribute msg
  = OnClick msg
  | Foreground Color
  | Background Color

data CLI msg
  = Text String
  | Container LayoutType AlignmentType (List (CLI msg))
  | Attributes (List (Attribute msg)) (CLI msg)
  | Border (CLI msg)
  | Input InputType String (String -> msg)

data LayoutType
  = LayoutRow
  | LayoutColumn

data AlignmentType
  = AlignStart
  | AlignCenter
  | AlignEnd

data InputType
  = TypeText
  | TypePassword

data Program flags model msg =
  Program
    (flags -> model)
    (model -> CLI msg)
    (msg -> model -> (model, IO (List msg)))

attributes :: List (Attribute msg) -> CLI msg -> CLI msg
attributes = Attributes

button :: List (Attribute msg) -> CLI msg -> CLI msg
button attrs = Attributes attrs . Border

border :: CLI msg -> CLI msg
border = Border

column :: AlignmentType -> List (CLI msg) -> CLI msg
column = Container LayoutColumn

input :: InputType -> String -> (String -> msg) -> CLI msg
input = Input

row :: AlignmentType -> List (CLI msg) -> CLI msg
row = Container LayoutRow

text :: String -> CLI msg
text = Text

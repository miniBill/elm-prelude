module CLI.Types.Internal
  ( Attribute(..)
  , CLI(..)
  , Cmd(..)
  , Focus(..)
  , InputType(..)
  , NodeType(..)
  , Sub(..)
  , TextAlign(..)
  , VerticalAlign(..)
  ) where

import           Color (Color)

data Focus
  = ChildFocus Int Focus
  | This Int

data NodeType
  = Button
  | Cell
  | Input
  | Row
  | Table

data InputType
  = TypeText
  | TypePassword

data TextAlign
  = AlignLeft
  | AlignCenter
  | AlignRight

data VerticalAlign
  = AlignTop
  | AlignMiddle
  | AlignBottom

data Attribute msg
  = BackgroundColor Color
  | ForegroundColor Color
  | InpuType InputType
  | OnClick msg
  | OnInput (String -> msg)
  | TextAlign TextAlign
  | VerticalAlign VerticalAlign

data CLI msg
  = Text String
  | Node NodeType (List (Attribute msg)) (List (CLI msg))

newtype Cmd msg =
  Cmd
    { runCmd :: List (IO msg)
    }

newtype Sub msg =
  Sub
    { runSub :: ()
    }

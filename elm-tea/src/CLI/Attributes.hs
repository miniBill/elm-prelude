module CLI.Attributes
  ( Attribute(..)
  , backgroundColor
  , foregroundColor
  , onClick
  ) where

import           CLI.Types (Attribute (..))
import           Color     (Color)

onClick :: msg -> Attribute msg
onClick = OnClick

foregroundColor :: Color -> Attribute msg
foregroundColor = Foreground

backgroundColor :: Color -> Attribute msg
backgroundColor = Background

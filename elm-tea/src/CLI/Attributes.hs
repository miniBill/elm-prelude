module CLI.Attributes
  ( Attribute
  , backgroundColor
  , foregroundColor
  , onClick
  ) where

import           CLI.Types.Internal (Attribute (..))
import           Color              (Color)

onClick :: msg -> Attribute msg
onClick = OnClick

foregroundColor :: Color -> Attribute msg
foregroundColor = ForegroundColor

backgroundColor :: Color -> Attribute msg
backgroundColor = BackgroundColor

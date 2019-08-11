module CLI.Layout
  ( display
  , getSize
  , childrenPositions
  ) where

import           CLI.Types    (AlignmentType (..), Attribute (..), CLI (..),
                               InputType (..), LayoutType (..))
import           Color        (Color)
import qualified Color
import qualified Compat
import           Graphics.Vty (Attr, Image, Picture)
import qualified Graphics.Vty as Vty
import qualified List
import qualified Maybe
import qualified String
import qualified Tuple

display :: CLI msg -> Picture
display = Vty.picForImage . displayWidget Vty.defAttr

displayWidget :: Attr -> CLI msg -> Image
displayWidget attr widget =
  case widget of
    Text s -> Vty.text' attr s -- A piece of text is simply written
    Container layout alignment children ->
      displayContainer layout alignment attr children
    Border child -> displayBorder attr child
    Attributes attrs child -> displayAttrs attr attrs child
    Input t v _ ->
      let displayString =
            case t of
              TypeText -> v
              TypePassword ->
                String.fromList $ List.repeat (String.length v) '*'
       in displayBorder attr $
          Text $ String.padRight 10 ' ' displayString ++ " "

getSize :: CLI msg -> (Int, Int)
getSize widget = imageSize $ displayWidget Vty.defAttr widget

imageSize :: Image -> (Int, Int)
imageSize image =
  ( Compat.fromIntegral $ Vty.imageWidth image
  , Compat.fromIntegral $ Vty.imageHeight image)

pad :: AlignmentType -> Int -> Int -> Int
pad AlignStart _ _          = 0
pad AlignCenter maxWidth cw = (maxWidth - cw) // 2
pad AlignEnd maxWidth cw    = maxWidth - cw

childrenPositions ::
     LayoutType -> AlignmentType -> List (CLI msg) -> List ((Int, Int), CLI msg)
childrenPositions LayoutRow    = rowPositions
childrenPositions LayoutColumn = columnPositions

columnPositions :: AlignmentType -> List (CLI msg) -> List ((Int, Int), CLI msg)
columnPositions alignment children =
  let maxWidth =
        children & List.map getSize & List.map Tuple.first & List.maximum &
        Maybe.withDefault 0
   in children &
      List.foldl
        (\child (y, acc) ->
           let (cw, ch) = getSize child
            in (y + ch + 1, ((pad alignment maxWidth cw, y), child) : acc))
        (0, []) &
      Tuple.second &
      List.reverse

rowPositions :: AlignmentType -> List (CLI msg) -> List ((Int, Int), CLI msg)
rowPositions alignment children =
  let maxHeight =
        children & List.map getSize & List.map Tuple.second & List.maximum &
        Maybe.withDefault 0
   in children &
      List.foldl
        (\child (x, acc) ->
           let (cw, ch) = getSize child
            in (x + cw + 1, ((x, pad alignment maxHeight ch), child) : acc))
        (0, []) &
      Tuple.second &
      List.reverse

displayContainer ::
     LayoutType -> AlignmentType -> Attr -> List (CLI msg) -> Image
displayContainer LayoutRow    = displayRow
displayContainer LayoutColumn = displayColumn

displayRow :: AlignmentType -> Attr -> List (CLI msg) -> Image
displayRow alignment attr children =
  let raw = Vty.horizCat $ List.map (displayWidget attr) children
      (_, height) = imageSize raw
      displayPadded child =
        let childPicture = displayWidget attr child
            (cwidth, cheight) = imageSize childPicture
            missing = pad alignment height cheight
            tpad =
              Vty.backgroundFill
                (Compat.fromIntegral cwidth)
                (Compat.fromIntegral missing)
            bpad =
              Vty.backgroundFill
                (Compat.fromIntegral cwidth)
                (Compat.fromIntegral $ height - cheight - missing)
         in Vty.vertCat [tpad, childPicture, bpad]
   in Vty.horizCat $
      List.map displayPadded $ List.intersperse (Text "") children

displayColumn :: AlignmentType -> Attr -> List (CLI msg) -> Image
displayColumn alignment attr children =
  let raw = Vty.vertCat $ List.map (displayWidget attr) children
      (width, _) = imageSize raw
      displayPadded child =
        let childPicture = displayWidget attr child
            (cwidth, cheight) = imageSize childPicture
            missing = pad alignment width cwidth
            lpad =
              Vty.backgroundFill
                (Compat.fromIntegral missing)
                (Compat.fromIntegral cheight)
            rpad =
              Vty.backgroundFill
                (Compat.fromIntegral $ width - cwidth - missing)
                (Compat.fromIntegral cheight)
         in Vty.horizCat [lpad, childPicture, rpad]
   in Vty.vertCat $ List.map displayPadded $ List.intersperse (Text "") children

toVtyColor :: Color -> Vty.Color
toVtyColor c =
  let (r, g, b, _) = Color.toRgba c
   in Vty.rgbColor (round $ r * 255) (round $ g * 255) (round $ b * 255)

displayAttrs :: Attr -> List (Attribute msg) -> CLI msg -> Image
displayAttrs attr attrs child =
  let attr' = List.foldr step attr attrs
      step :: Attribute msg -> Attr -> Attr
      step (OnClick _) a    = a
      step (Foreground f) a = Vty.withForeColor a $ toVtyColor f
      step (Background b) a = Vty.withBackColor a $ toVtyColor b
   in displayWidget attr' child

displayBorder :: Attr -> CLI msg -> Image
displayBorder attr child =
  let (width, height) = getSize child
      char = Vty.char attr
      hline c w = Vty.charFill attr c w 1
      vline c h = Vty.charFill attr c 1 h
   in grid
        [ [char '┌', hline '─' width, char '┐']
        , [vline '│' height, displayWidget attr child, vline '│' height]
        , [char '└', hline '─' width, char '┘']
        ]

grid :: List (List Image) -> Image
grid = Vty.vertCat . List.map Vty.horizCat

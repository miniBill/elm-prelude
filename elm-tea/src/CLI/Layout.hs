module CLI.Layout where

--  ( display
--  --, getSize
--  --, childrenPositions
--  ) where
--import           CLI.Types          (InputType (..), TextAlign (..),
--                                     VerticalAlign (..))
import           CLI.Types.Internal (Attribute (..), CLI (..), NodeType (..))
import           Graphics.Vty       (Attr, Image, Picture)
import qualified Graphics.Vty       as Vty
import qualified List
import qualified Maybe
import qualified String
import qualified Tuple

--import           Color              (Color)
--import qualified Color
--import qualified Compat
display :: CLI msg -> Picture
display widget =
  let measured = measure widget
   in Vty.picForImage $ displayWidget Vty.defAttr measured

data Measured msg
  = MText (Int, Int) (List String)
  | MNode (Int, Int) NodeType (List (Attribute msg)) (List (Measured msg))

size :: Measured msg -> (Int, Int)
size (MText wsize _)     = wsize
size (MNode wsize _ _ _) = wsize

measure :: CLI msg -> Measured msg
measure (Text s) =
  let lines = s & String.split "\n"
      width = lines & List.map String.length & List.maximum0
      height = List.length lines
   in MText (width, height) lines
measure (Node Cell attrs children) =
  let (wsize, children') = measureRow children
   in MNode wsize Cell attrs children'
measure (Node Row attrs children) =
  let (wsize, children') = measureRow children
   in MNode wsize Row attrs children'
measure (Node Button attrs children) =
  let ((width, height), children') = measureRow children
   in MNode (width + 2, height + 2) Button attrs children'
measure (Node Input attrs children) =
  let value =
        List.foldl
          (\e a ->
             case e of
               Value v -> v
               _       -> a)
          ""
          attrs
   in MNode (String.length value + 3, 3) Input attrs []
measure (Node Table attrs children) =
  let asRow (Node Row rattrs cells) =
        Just (rattrs, cells & List.filterMap asCell)
      asRow _ = Nothing
      asCell (Node Cell _ cs) = Just $ measureRow cs
      asCell _                = Nothing
      rawRows = children & List.filterMap asRow
      columnCount = rawRows & List.map List.length & List.maximum0
      rows =
        rawRows &
        List.map
          (\row ->
             row ++ List.repeat (columnCount - List.length row) ((0, 0), []))
      cols = List.transpose rows
      maximumWidth = List.maximum0 . List.map (\((width, _), _) -> width)
      maximumHeight = List.maximum0 . List.map (\((_, height), _) -> height)
      width =
        if columnCount == 0
          then 2
          else cols & List.map maximumWidth & List.sum &
               (\s -> s + columnCount + 1)
      height =
        if List.isEmpty rows
          then 2
          else rows & List.map maximumHeight & List.sum &
               (\s -> s + List.length rows + 1)
      rowToMeasured row = MNode (rwidth, rheight) Row [] cells'
      children' = List.map rowToMeasured rows
   in MNode (width, height) Table attrs children'

measureRow children =
  let children' = List.map measure children
      (width, height) =
        children' & List.map size & List.unzip & Tuple.mapFirst List.sum &
        Tuple.mapSecond List.maximum0
   in ((width, height), children')

reflow :: Int -> Int -> String -> Measured msg
reflow 0 _ _ = MText (0, 0) []
reflow width maxh s =
  let step ::
           String
        -> (Int, List String, List String)
        -> (Int, List String, List String)
      step word (budget, currentLine, accum) =
        if String.length word <= budget
          then (budget - String.length word, word : currentLine, accum)
          else if List.isEmpty currentLine
                 then if budget < 3
                        then step
                               (String.repeat budget ".")
                               (budget, currentLine, accum)
                        else step
                               (String.left (budget - 3) word ++ "...")
                               (budget, currentLine, accum)
                 else step
                        word
                        ( width
                        , []
                        , String.join " " (List.reverse currentLine) : accum)
      reflowLine :: String -> List String
      reflowLine line =
        line & String.split " " & List.foldl (step) (width, [], []) &
        (\(_, currentLine, accum) ->
           String.join " " (List.reverse currentLine) : accum) &
        List.reverse
      lines = s & String.split "\n" & List.concatMap reflowLine & List.take maxh
   in MText
        (List.map String.length lines & List.maximum0, List.length lines)
        lines

displayWidget :: Attr -> Measured msg -> Image
displayWidget attr (MText _ text) = displayText attr text
displayWidget attr (MNode _ _ _ _) -- nodeType attrs children) =
 = displayText attr ["Todo"]

{-
  case nodeType of
    Text s -> displayText size s
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
-}
displayText :: Attr -> List String -> Image
displayText attr s =
  Vty.vertCat $ List.map (Vty.text' attr . String.replace "\r" "") s -- A piece of text is simply written
{-
getSize :: (Int,Int) -> CLI msg -> (Int,Int)
getSize size widget = imageSize $ displayWidget size Vty.defAttr widget

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
        children & List.map getSize & List.map Tuple.first & List.maximum0
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
        children & List.map getSize & List.map Tuple.second & List.maximum0
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

displayColumn ::
     (Int,Int) -> AlignmentType -> Attr -> List (CLI msg) -> Image
displayColumn size alignment attr children =
  let raw = Vty.vertCat $ List.map (displayWidget size attr) children
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

displayAttrs ::
     (Int,Int) -> Attr -> List (Attribute msg) -> CLI msg -> Image
displayAttrs size attr attrs child =
  let attr' = List.foldr step attr attrs
      step :: Attribute msg -> Attr -> Attr
      step _ a = a
      --step (OnClick _) a         = a
      --step (ForegroundColor f) a = Vty.withForeColor a $ toVtyColor f
      --step (BackgroundColor b) a = Vty.withBackColor a $ toVtyColor b
   in displayWidget size attr' child

displayBorder :: (Int,Int) -> Attr -> CLI msg -> Image
displayBorder size attr child =
  let displayedChild = displayWidget size attr child
      (width, height) = imageSize displayedChild
      char = Vty.char attr
      hline c w = Vty.charFill attr c w 1
      vline c h = Vty.charFill attr c 1 h
      store = "├┤┴┼┬"
   in grid
        [ [char '┌', hline '─' width, char '┐']
        , [vline '│' height, displayedChild, vline '│' height]
        , [char '└', hline '─' width, char '┘']
        ]

grid :: List (List Image) -> Image
grid = Vty.vertCat . List.map Vty.horizCat
-}

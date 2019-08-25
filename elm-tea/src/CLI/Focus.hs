module CLI.Focus
  ( finalFocus
  , getFocusPosition
  , initialFocus
  , nextFocus
  , previousFocus
  ) where

--import qualified CLI.Layout         as Layout
import           CLI.Types.Internal (CLI (..), Focus (..))

--import qualified List
--import qualified Maybe
--import qualified String
initialFocus :: CLI msg -> Maybe Focus
initialFocus = \_ -> Nothing {- widget =
  let childrenFocus children =
        children &
        List.indexedMap
          (\i child -> initialFocus child & Maybe.map (ChildFocus i)) &
        List.filterMap identity &
        List.head
   in case widget of
        Attributes _ child     -> initialFocus child
        Text _                 -> Nothing
        Border child           -> initialFocus child
        Input _ text _         -> Just $ This $ String.length text
        Container _ _ children -> childrenFocus children -}

finalFocus :: CLI msg -> Maybe Focus
finalFocus = \_ -> Nothing {- widget =
  let childrenFocus children =
        children &
        List.indexedMap
          (\i child -> finalFocus child & Maybe.map (ChildFocus i)) &
        List.filterMap identity &
        List.reverse &
        List.head
   in case widget of
        Attributes _ child     -> finalFocus child
        Text _                 -> Nothing
        Border child           -> finalFocus child
        Input _ text _         -> Just $ This $ String.length text
        Container _ _ children -> childrenFocus children -}

nextFocus :: CLI msg -> Focus -> Maybe Focus
nextFocus = \_ _ -> Nothing {-
  let containerNextFocus children i focus =
        case children & List.drop i of
          [] -> Nothing
          (x:xs) ->
            case go x focus of
              Just focus' -> Just $ ChildFocus i $ focus'
              Nothing ->
                List.indexedMap
                  (\j e -> Maybe.map (ChildFocus $ i + j + 1) $ initialFocus e)
                  xs &
                List.filterMap identity &
                List.head
      go (Border child) focus = go child focus
      go (Attributes _ child) focus = go child focus
      go (Input _ _ _) (This _) = Nothing
      go (Container _ _ children) (ChildFocus i cfocus) =
        containerNextFocus children i cfocus
      go (Text _) _ = Nothing
      go _ _ = Nothing
   in go -}

previousFocus :: CLI msg -> Focus -> Maybe Focus
previousFocus = \_ _ -> Nothing {-
  let containerPreviousFocus children i focus =
        case List.take (i + 1) children & List.reverse of
          [] -> Nothing
          (x:xs) ->
            case go x focus of
              Just focus' -> Just $ ChildFocus i $ focus'
              Nothing ->
                List.indexedMap
                  (\j e -> Maybe.map (ChildFocus (i - j - 1)) $ finalFocus e)
                  xs &
                List.filterMap identity &
                List.head
      go (Border child) focus = go child focus
      go (Attributes _ child) focus = go child focus
      go (Input _ _ _) (This _) = Nothing
      go (Container _ _ children) (ChildFocus i cfocus) =
        containerPreviousFocus children i cfocus
      go (Text _) _ = Nothing
      go _ _ = Nothing
   in go -}

getFocusPosition :: CLI msg -> Focus -> Maybe (Int, Int)
getFocusPosition = \_ _ -> Nothing {-
  let containerFocus layout alignment children i cfocus =
        children & Layout.childrenPositions layout alignment & List.drop i &
        List.head &
        Maybe.andThen
          (\((cx, cy), child) ->
             getFocusPosition child cfocus &
             Maybe.map (\(x, y) -> (x + cx, y + cy)))
      go (Border child) focus =
        Maybe.map (\(x, y) -> (x + 1, y + 1)) $ getFocusPosition child focus
      go (Text _) _ = Nothing
      go (Attributes _ child) focus = getFocusPosition child focus
      go (Input _ _ _) (This i) = Just (i, 1)
      go (Container layout alignment children) (ChildFocus i cfocus) =
        containerFocus layout alignment children i cfocus
      go _ _ = Nothing
   in go
-}

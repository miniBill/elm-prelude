{-# LANGUAGE FlexibleInstances #-}

module CLI
  ( AlignmentType(..)
  , CLI
  , Cmd
  , InputType(..)
  , Program(..)
  , Sub
  , attributes
  , border
  , button
  , column
  , element
  , input
  , row
  , run
  , run_
  , sandbox
  , text
  ) where

import qualified Char
import           CLI.Attributes     (Attribute (..))
import qualified CLI.Focus          as Focus
import qualified CLI.Layout         as Layout
import           CLI.Types          (AlignmentType (..), CLI (..),
                                     InputType (..), Program (..), attributes,
                                     border, button, column, input, row, text)
import           CLI.Types.Internal (Cmd (..), Focus (..), Sub (..))
import qualified Cmd
import           Compat             (Monad (..))
import qualified Compat
import           Graphics.Vty       (Vty)
import qualified Graphics.Vty       as Vty
import qualified List
import qualified Maybe
import qualified String
import qualified Sub
import qualified Tuple

run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update subscriptions) =
  let (model, _) = init flags -- TODO: use initialCmd
   in do cfg <- Vty.standardIOConfig
         vty <- Vty.mkVty $ cfg {Vty.mouseMode = Just True}
         mainLoop vty view update subscriptions model
         Vty.shutdown vty

mainLoop ::
     Vty
  -> (model -> CLI msg)
  -> (msg -> model -> (model, Cmd msg))
  -> (model -> Sub msg)
  -> model
  -> IO ()
mainLoop vty view update _ initialModel =
  let go focus model = do
        let root = view model
        Vty.update vty $ Layout.display root
        case Maybe.andThen (Focus.getFocusPosition root) focus of
          Just (r, c) -> do
            Vty.showCursor $ Vty.outputIface vty
            Vty.setCursorPos
              (Vty.outputIface vty)
              (Compat.fromIntegral r)
              (Compat.fromIntegral c)
          Nothing -> Vty.hideCursor $ Vty.outputIface vty
        event <- Vty.nextEvent vty
        let maybeMsgs = eventToMsgs root focus event
        case maybeMsgs of
          Nothing -> return () -- Exit
          Just (msgs, focus') -> do
            let (model', _) = List.foldl step (model, []) msgs
            -- TODO: use cmd
            -- TODO: use subscriptions
            go focus' model'
      step msg (mod, cmds) =
        let (mod', cmd) = update msg mod
         in (mod', cmd : cmds)
   in go (Focus.initialFocus $view initialModel) initialModel

-- Returns Nothing to exit, Just msgs for messages
eventToMsgs ::
     CLI msg -> Maybe Focus -> Vty.Event -> Maybe (List msg, Maybe Focus)
eventToMsgs root _ (Vty.EvMouseUp x y _) =
  Just $ onClick (Compat.fromIntegral x) (Compat.fromIntegral y) root
eventToMsgs _ _ (Vty.EvKey Vty.KEsc []) = Nothing
eventToMsgs root (Just focus) (Vty.EvKey (Vty.KChar '\t') []) =
  Just
    ( []
    , case Focus.nextFocus root focus of
        Just f  -> Just f
        Nothing -> Focus.initialFocus root)
eventToMsgs root Nothing (Vty.EvKey (Vty.KChar '\t') []) =
  Just ([], Focus.initialFocus root)
eventToMsgs root (Just focus) (Vty.EvKey (Vty.KBackTab) []) =
  Just
    ( []
    , case Focus.previousFocus root focus of
        Just f  -> Just f
        Nothing -> Focus.finalFocus root)
eventToMsgs root Nothing (Vty.EvKey (Vty.KBackTab) []) =
  Just ([], Focus.finalFocus root)
eventToMsgs root (Just focus) (Vty.EvKey key modifiers) =
  Just $ onKeyUp key modifiers root focus
eventToMsgs _ focus _ = Just ([], focus)

onKeyUp ::
     Vty.Key -> List Vty.Modifier -> CLI msg -> Focus -> (List msg, Maybe Focus)
onKeyUp key modifiers =
  let containerKeyUp ::
           List (CLI msg) -> Int -> Focus -> (List msg, Maybe Focus)
      containerKeyUp children i cfocus =
        children & List.drop i & List.head &
        (\h ->
           case h of
             Just x  -> go x cfocus
             Nothing -> ([], Just cfocus)) &
        Tuple.mapSecond (Maybe.map $ ChildFocus i)
      go (Border child) focus = go child focus
      go (Attributes _ child) focus = go child focus
      go (Input _ v onInput) (This i) = onInputKeyUp v onInput i key modifiers
      go (Container _ _ children) (ChildFocus i cfocus) =
        containerKeyUp children i cfocus
      go (Text _) _ = ([], Nothing)
      go _ _ = ([], Nothing)
   in go

onInputKeyUp ::
     String
  -> (String -> msg)
  -> Int
  -> Vty.Key
  -> List Vty.Modifier
  -> (List msg, Maybe Focus)
onInputKeyUp v onInput i key modifiers =
  let inner Vty.KBS
        | i > 0 =
          ( [onInput $ String.left (i - 1) v ++ String.dropLeft i v]
          , Just $ This (i - 1))
      inner (Vty.KChar char) =
        let char' =
              if List.any
                   (\m ->
                      case m of
                        Vty.MShift -> True
                        _          -> False)
                   modifiers
                then Char.toUpper char
                else char
         in ( [ onInput $
                String.left i v ++
                String.fromList [char'] ++ String.dropLeft i v
              ]
            , Just $ This (i + 1))
      inner Vty.KLeft
        | i > 0 = ([], Just $ This $ i - 1)
      inner Vty.KRight
        | i < String.length v = ([], Just $ This $ i + 1)
      inner Vty.KHome = ([], Just $ This 0)
      inner Vty.KEnd = ([], Just $ This $ String.length v)
      inner _ = ([], Just $ This i)
   in inner key

onClick :: Int -> Int -> CLI msg -> (List msg, Maybe Focus)
onClick _ _ (Text _) = ([], Nothing)
onClick relx rely (Container layout alignment children) =
  Layout.childrenPositions layout alignment children &
  List.indexedMap (\i (pos, child) -> (i, pos, child)) &
  List.filter
    (\(_, (cx, cy), child) ->
       let (cwidth, cheight) = Layout.getSize child
        in cx <= relx && relx < cx + cwidth && cy <= rely && rely < cy + cheight) &
  List.head &
  (\found ->
     case found of
       Nothing -> ([], Nothing)
       Just (i, (cx, cy), child) ->
         onClick (relx - cx) (rely - cy) child &
         Tuple.mapSecond (Maybe.map $ ChildFocus i))
onClick relx rely (Border child) =
  let (w, h) = Layout.getSize child
   in if relx < (w + 2) && rely < (h + 2)
        then onClick (relx - 1) (rely - 1) child
        else ([], Nothing)
onClick relx rely (Input _ v _) =
  let (w, h) = (max 10 $ String.length v + 1, 1)
   in if relx < (w + 2) && rely < (h + 2)
        then ([], Just $ This 0)
        else ([], Nothing)
onClick relx rely (Attributes as child) =
  let (w, h) = Layout.getSize child
   in if relx < w && rely < h
        then let (_, f) = onClick relx rely child
              in ( List.filterMap
                     (\attr ->
                        case attr of
                          OnClick msg -> Just msg
                          _           -> Nothing)
                     as
                 , f)
        else ([], Nothing)

sandbox ::
     model
  -> (model -> CLI msg)
  -> (msg -> model -> model)
  -> Program () model msg
sandbox init view update =
  let init' _ = (init, Cmd.none)
      update' msg model = (update msg model, Cmd.none)
      subscriptions _ = Sub.none
   in Program init' view update' subscriptions

element ::
     (flags -> (model, Cmd msg))
  -> (model -> CLI msg)
  -> (msg -> model -> (model, Cmd msg))
  -> (model -> Sub msg)
  -> Program flags model msg
element = Program
